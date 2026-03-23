make_symmetric_grid_file <- function() {
  grid_mat <- as.matrix(read.csv(example_data_path("example_grid.csv"), row.names = 1, check.names = FALSE))
  grid_mat <- (grid_mat + t(grid_mat)) / 2

  symmetric_grid <- tempfile(fileext = ".csv")
  symmetric_grid_df <- data.frame(zone = rownames(grid_mat), as.data.frame(grid_mat, check.names = FALSE), check.names = FALSE)
  write.csv(symmetric_grid_df, symmetric_grid, row.names = FALSE, quote = TRUE)
  symmetric_grid
}

example_input_config <- function(weights = list(), grid_file = make_symmetric_grid_file()) {
  list(
    inputs = list(
      wind_file = example_data_path("example_wind.csv"),
      solar_file = example_data_path("example_solar.csv"),
      demand_file = example_data_path("example_demand.csv"),
      grid_file = grid_file
    ),
    weights = weights
  )
}

test_that("time-series inputs require datetime plus matching zones", {
  inputs <- mick:::read_input_data(example_input_config())

  expect_true("datetime" %in% names(inputs$wind))
  expect_identical(names(inputs$wind), names(inputs$solar))
  expect_identical(names(inputs$wind), names(inputs$demand))

  wind_df <- read.csv(example_data_path("example_wind.csv"), check.names = FALSE)
  solar_df <- read.csv(example_data_path("example_solar.csv"), check.names = FALSE)

  zone_mismatch_file <- tempfile(fileext = ".csv")
  zone_mismatch_df <- wind_df
  zone_mismatch_df$Q <- zone_mismatch_df$A
  zone_mismatch_df$A <- NULL
  write.csv(zone_mismatch_df, zone_mismatch_file, row.names = FALSE, quote = TRUE)

  zone_mismatch_config <- example_input_config()
  zone_mismatch_config$inputs$wind_file <- zone_mismatch_file

  expect_error(mick:::read_input_data(zone_mismatch_config), "matching zone columns|identical datetimes")

  datetime_mismatch_file <- tempfile(fileext = ".csv")
  datetime_mismatch_df <- solar_df
  datetime_mismatch_df$datetime <- as.character(as.POSIXct(datetime_mismatch_df$datetime, tz = "UTC") + 3600)
  write.csv(datetime_mismatch_df, datetime_mismatch_file, row.names = FALSE, quote = TRUE)

  datetime_mismatch_config <- example_input_config()
  datetime_mismatch_config$inputs$solar_file <- datetime_mismatch_file

  expect_error(mick:::read_input_data(datetime_mismatch_config), "matching zone columns|identical datetimes")
})

test_that("grid matrix is square and symmetric", {
  inputs <- mick:::read_input_data(example_input_config())

  expect_true(is.matrix(inputs$grid))
  expect_equal(nrow(inputs$grid), ncol(inputs$grid))
  expect_true(isTRUE(all.equal(inputs$grid, t(inputs$grid))))
  expect_identical(colnames(inputs$grid), names(inputs$wind)[-1])

  bad_grid <- tempfile(fileext = ".csv")
  bad_grid_mat <- as.matrix(read.csv(example_data_path("example_grid.csv"), row.names = 1, check.names = FALSE))
  bad_grid_mat <- (bad_grid_mat + t(bad_grid_mat)) / 2
  bad_grid_mat[1, 2] <- bad_grid_mat[1, 2] + 1
  bad_grid_df <- data.frame(zone = rownames(bad_grid_mat), as.data.frame(bad_grid_mat, check.names = FALSE), check.names = FALSE)
  write.csv(bad_grid_df, bad_grid, row.names = FALSE, quote = TRUE)

  bad_config <- example_input_config()
  bad_config$inputs$grid_file <- bad_grid

  expect_error(mick:::read_input_data(bad_config), "square|symmetric")
})

test_that("clustering view applies weights without changing original data", {
  weighted_config <- example_input_config(list(
    demand = list(A = 2, B = 3),
    wind = list(A = 4),
    solar = list(B = 5)
  ))
  views <- mick:::build_input_views(weighted_config)

  original_inputs <- mick:::read_input_data(example_input_config())

  expect_identical(views$clustering$demand$A, views$original$demand$A * 2)
  expect_identical(views$clustering$demand$B, views$original$demand$B * 3)
  expect_identical(views$clustering$wind$A, views$original$wind$A * 4)
  expect_identical(views$clustering$solar$B, views$original$solar$B * 5)
  expect_identical(views$clustering$demand$C, views$original$demand$C)
  expect_identical(views$original$wind, original_inputs$wind)
  expect_identical(views$original$solar, original_inputs$solar)
  expect_identical(views$original$demand, original_inputs$demand)
})

test_that("unknown weight zones fail fast", {
  bad_config <- example_input_config(list(
    demand = list(A = 2, Z = 3)
  ))

  expect_error(mick:::build_input_views(bad_config), "unknown|zone")
})

test_that("non-finite weight values fail fast", {
  bad_configs <- list(
    example_input_config(list(demand = list(A = NA_real_))),
    example_input_config(list(wind = list(B = Inf))),
    example_input_config(list(solar = list(C = NaN)))
  )

  for (config in bad_configs) {
    expect_error(mick:::build_input_views(config), "finite|weight|numeric")
  }
})

test_that("reordered symmetric grids are accepted and aligned to the time-series zones", {
  reordered_grid <- tempfile(fileext = ".csv")
  grid_mat <- as.matrix(read.csv(example_data_path("example_grid.csv"), row.names = 1, check.names = FALSE))
  grid_mat <- (grid_mat + t(grid_mat)) / 2
  reordered_zones <- rev(rownames(grid_mat))
  grid_mat <- grid_mat[reordered_zones, reordered_zones]
  reordered_df <- data.frame(zone = rownames(grid_mat), as.data.frame(grid_mat, check.names = FALSE), check.names = FALSE)
  write.csv(reordered_df, reordered_grid, row.names = FALSE, quote = TRUE)

  config <- example_input_config(grid_file = reordered_grid)
  inputs <- mick:::read_input_data(config)

  expect_identical(rownames(inputs$grid), names(inputs$wind)[-1])
  expect_identical(colnames(inputs$grid), names(inputs$wind)[-1])
  expect_true(isTRUE(all.equal(inputs$grid, t(inputs$grid))))
})

test_that("named list grids align row values by key and reject mismatched rows", {
  raw <- list(
    A = list(A = 0, B = 1),
    B = list(B = 0, A = 1)
  )

  parsed <- mick:::.mick_as_grid_matrix(raw, "inline")

  expect_identical(rownames(parsed), c("A", "B"))
  expect_identical(colnames(parsed), c("A", "B"))
  expect_equal(parsed, matrix(
    c(0, 1, 1, 0),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("A", "B"), c("A", "B"))
  ))

  malformed <- list(
    A = list(A = 0, B = 1),
    B = list(A = 1, C = 2)
  )

  expect_error(
    mick:::.mick_as_grid_matrix(malformed, "inline"),
    "matching row names|zone names|grid"
  )
})

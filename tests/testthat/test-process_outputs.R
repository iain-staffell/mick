make_symmetric_grid_file <- function() {
  grid_mat <- as.matrix(read.csv(example_data_path("example_grid.csv"), row.names = 1, check.names = FALSE))
  grid_mat <- (grid_mat + t(grid_mat)) / 2

  symmetric_grid <- tempfile(fileext = ".csv")
  symmetric_grid_df <- data.frame(zone = rownames(grid_mat), as.data.frame(grid_mat, check.names = FALSE), check.names = FALSE)
  write.csv(symmetric_grid_df, symmetric_grid, row.names = FALSE, quote = TRUE)
  symmetric_grid
}

make_output_config <- function(format = "csv", reserved_time_slices = NULL) {
  cfg <- read_mick_config(example_config_path("yaml"))
  cfg$inputs <- lapply(cfg$inputs, function(path) {
    normalizePath(path, winslash = "/", mustWork = TRUE)
  })
  cfg$inputs$grid_file <- make_symmetric_grid_file()
  cfg$outputs$directory <- tempfile("mick-output-")
  cfg$outputs$format <- format
  if (!is.null(reserved_time_slices)) {
    cfg$clustering$reserved_time_slices <- reserved_time_slices
  }
  cfg
}

test_that("run_mick exports csv tables with expected schemas", {
  cfg <- make_output_config(format = "csv")

  result <- run_mick(cfg)

  expect_type(result, "list")
  expect_true(all(c("schema_version", "run_timestamp", "config", "inputs", "spatial", "temporal", "outputs") %in% names(result)))
  expect_true(is.character(result$run_timestamp))
  expect_match(result$run_timestamp, "^\\d{4}-\\d{2}-\\d{2}T")

  metadata <- result$outputs$run_metadata
  written_files <- result$outputs$written_files

  expect_true(is.list(metadata))
  expect_identical(metadata$schema_version, result$schema_version)
  expect_identical(metadata$run_timestamp, result$run_timestamp)
  expect_length(metadata$input_md5, 4)
  expect_true(all(c("wind_file", "solar_file", "demand_file", "grid_file") %in% names(metadata$input_md5)))
  expect_true(all(c("original_dimensions", "reduced_dimensions", "reserved_slices", "slice_weights") %in% names(metadata)))

  expect_true(is.list(written_files))
  expect_true(all(c("spatial_clusters", "temporal_clusters", "reduced_wind", "reduced_solar", "reduced_demand", "reduced_grid", "run_metadata") %in% names(written_files)))
  expect_true(all(file.exists(unlist(written_files, use.names = FALSE))))

  spatial_clusters <- read.csv(written_files$spatial_clusters, check.names = FALSE, stringsAsFactors = FALSE)
  temporal_clusters <- read.csv(written_files$temporal_clusters, check.names = FALSE, stringsAsFactors = FALSE)
  reduced_wind <- read.csv(written_files$reduced_wind, check.names = FALSE, stringsAsFactors = FALSE)
  reduced_solar <- read.csv(written_files$reduced_solar, check.names = FALSE, stringsAsFactors = FALSE)
  reduced_demand <- read.csv(written_files$reduced_demand, check.names = FALSE, stringsAsFactors = FALSE)
  run_metadata <- read.csv(written_files$run_metadata, check.names = FALSE, stringsAsFactors = FALSE)

  expect_identical(names(spatial_clusters), c("zone", "cluster"))
  expect_true(all(c("hour_index", "temporal_cluster") %in% names(temporal_clusters)))
  expect_identical(names(reduced_wind)[1], "temporal_cluster")
  expect_identical(names(reduced_solar)[1], "temporal_cluster")
  expect_identical(names(reduced_demand)[1], "temporal_cluster")
  expect_true(ncol(reduced_wind) > 1L)
  expect_true(ncol(reduced_solar) > 1L)
  expect_true(ncol(reduced_demand) > 1L)
  expect_identical(names(reduced_wind), names(reduced_solar))
  expect_identical(names(reduced_wind), names(reduced_demand))
  expect_true(any(startsWith(run_metadata$field, "reserved_slices")))
  expect_true(any(run_metadata$field == "input_md5.wind_file"))
  expect_false(identical(names(spatial_clusters), c("field", "value")))
  expect_false(identical(names(temporal_clusters), c("field", "value")))
  expect_false(identical(names(reduced_wind), c("field", "value")))
  expect_false(identical(names(reduced_solar), c("field", "value")))
  expect_false(identical(names(reduced_demand), c("field", "value")))
})

test_that("run_mick writes canonical rds output when requested", {
  cfg <- make_output_config(format = "csv")
  cfg$outputs$save_rds <- TRUE

  result <- run_mick(cfg)

  expect_true("result_rds" %in% names(result$outputs$written_files))
  expect_true(file.exists(result$outputs$written_files$result_rds))
  expect_identical(result$outputs$written_files$result_rds, result$outputs$run_metadata$written_files$result_rds)

  saved <- readRDS(result$outputs$written_files$result_rds)
  expect_identical(saved$schema_version, result$schema_version)
  expect_identical(saved$run_timestamp, result$run_timestamp)
})

test_that("run_mick handles empty reserved slices and preserves metadata sections", {
  cfg <- make_output_config(format = "csv", reserved_time_slices = character(0))

  result <- run_mick(cfg)

  expect_s3_class(result$outputs$run_metadata$reserved_slices, "data.frame")
  expect_equal(nrow(result$outputs$run_metadata$reserved_slices), 0L)

  run_metadata <- read.csv(result$outputs$written_files$run_metadata, check.names = FALSE, stringsAsFactors = FALSE)
  expect_true(any(run_metadata$field == "reserved_slices"))
  expect_identical(run_metadata$value[run_metadata$field == "reserved_slices"][1], "[]")
})

test_that("run_mick writes json metadata with expected top-level keys", {
  cfg <- make_output_config(format = "json", reserved_time_slices = character(0))

  result <- run_mick(cfg)

  run_metadata <- jsonlite::fromJSON(result$outputs$written_files$run_metadata, simplifyVector = FALSE)

  expect_true(all(c("schema_version", "run_timestamp", "input_paths", "input_md5", "reserved_slices", "slice_weights", "written_files") %in% names(run_metadata)))
  expect_length(run_metadata$reserved_slices, 0L)
  expect_false("result_rds" %in% names(result$outputs$written_files))
})

test_that("run_mick writes yaml metadata with expected top-level keys", {
  cfg <- make_output_config(format = "yaml", reserved_time_slices = character(0))

  result <- run_mick(cfg)

  run_metadata <- yaml::read_yaml(result$outputs$written_files$run_metadata)

  expect_true(all(c("schema_version", "run_timestamp", "input_paths", "input_md5", "reserved_slices", "slice_weights", "written_files") %in% names(run_metadata)))
  expect_true(is.list(run_metadata$reserved_slices))
  expect_true(all(vapply(run_metadata$reserved_slices, length, integer(1)) == 0L))
  expect_false("result_rds" %in% names(result$outputs$written_files))
})

make_spatial_test_views <- function() {
  datetimes <- as.POSIXct(
    c("2023-01-01 00:00:00", "2023-01-01 01:00:00"),
    tz = "UTC"
  )

  demand <- data.frame(
    datetime = datetimes,
    A = c(10, 12),
    B = c(11, 13),
    C = c(30, 32),
    D = c(31, 33),
    check.names = FALSE
  )
  wind <- data.frame(
    datetime = datetimes,
    A = c(2, 2),
    B = c(2, 2),
    C = c(5, 5),
    D = c(5, 5),
    check.names = FALSE
  )
  solar <- data.frame(
    datetime = datetimes,
    A = c(0, 1),
    B = c(0, 1),
    C = c(3, 4),
    D = c(3, 4),
    check.names = FALSE
  )

  grid <- matrix(
    c(
      0, 10, 1, 0,
      10, 0, 1, 0,
      1, 1, 0, 10,
      0, 0, 10, 0
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D"))
  )

  original <- list(
    wind = wind,
    solar = solar,
    demand = demand,
    grid = grid
  )
  original$net_demand <- mick:::.mick_compute_net_demand(
    original$demand,
    original$wind,
    original$solar
  )

  list(
    original = original,
    clustering = original
  )
}

candidate_pairs <- function(candidates) {
  if (nrow(candidates) == 0L) {
    return(character())
  }

  vapply(seq_len(nrow(candidates)), function(i) {
    paste(sort(c(candidates$cluster_a[[i]], candidates$cluster_b[[i]])), collapse = "|")
  }, character(1))
}

test_that("only adjacent clusters are considered for merging", {
  grid <- make_spatial_test_views()$clustering$grid
  clusters <- list(
    `A+B` = c("A", "B"),
    C = "C",
    D = "D"
  )

  candidates <- mick:::enumerate_spatial_candidates(
    clusters = clusters,
    grid = grid
  )

  expect_setequal(candidate_pairs(candidates), c("A+B|C", "C|D"))
  expect_true(all(candidates$boundary_capacity > 0))
  expect_false("A+B|D" %in% candidate_pairs(candidates))
})

test_that("spatial reduction rejects unknown spatial feature weights", {
  expect_error(
    mick:::run_spatial_reduction(
      views = make_spatial_test_views(),
      n_space = 2L,
      spatial_weights = list(shape = 1, connectivty = 2)
    ),
    "Unknown spatial feature weights"
  )
})

test_that("spatial reduction rejects impossible n_space values", {
  expect_error(
    mick:::run_spatial_reduction(
      views = make_spatial_test_views(),
      n_space = 99L
    ),
    "n_space|exceed|zones"
  )
})

test_that("spatial reduction records deterministic merge history", {
  spatial <- mick:::run_spatial_reduction(
    views = make_spatial_test_views(),
    n_space = 2L,
    spatial_weights = list(
      shape = 5,
      magnitude = 5,
      connectivity = 1
    )
  )

  expect_identical(spatial$merge_history$step, 1:2)
  expect_identical(spatial$merge_history$cluster_a, c("C", "A"))
  expect_identical(spatial$merge_history$cluster_b, c("D", "B"))
  expect_identical(spatial$merge_history$merged_cluster, c("C+D", "A+B"))
  expect_equal(spatial$merge_history$boundary_capacity, c(10, 10))
  expect_equal(spatial$merge_history$total_score, c(0, 0))
})

test_that("spatial aggregation sums member-zone series and boundary capacities", {
  spatial <- mick:::run_spatial_reduction(
    views = make_spatial_test_views(),
    n_space = 2L,
    spatial_weights = list(
      shape = 5,
      magnitude = 5,
      connectivity = 1
    )
  )

  membership <- setNames(spatial$membership$cluster, spatial$membership$zone)
  expect_identical(unname(membership[c("A", "B", "C", "D")]), c("A+B", "A+B", "C+D", "C+D"))

  expect_equal(spatial$original_view$demand$`A+B`, c(21, 25))
  expect_equal(spatial$original_view$demand$`C+D`, c(61, 65))
  expect_equal(spatial$original_view$wind$`A+B`, c(4, 4))
  expect_equal(spatial$original_view$solar$`C+D`, c(6, 8))
  expect_equal(spatial$clustering_view$net_demand$`A+B`, c(17, 19))
  expect_equal(spatial$clustering_view$net_demand$`C+D`, c(45, 47))

  expect_equal(spatial$reduced_grid["A+B", "C+D"], 2)
  expect_equal(spatial$reduced_grid["C+D", "A+B"], 2)
  expect_equal(unname(diag(spatial$reduced_grid)), c(0, 0))
})

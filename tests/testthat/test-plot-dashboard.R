test_that("plot_clustering_dashboard returns all plot families", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  dashboard <- plot_clustering_dashboard(fixture$result)

  expect_type(dashboard, "list")
  expect_identical(names(dashboard), c("spatial", "duration", "dependence", "chronology"))
})

test_that("plot_clustering_dashboard falls back to placeholder spatial plots when igraph is unavailable", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .package = "mick",
    plot_spatial_clusters =
      function(...) stop("igraph is required for network mode.", call. = FALSE)
  )

  dashboard <- plot_clustering_dashboard(fixture$result)

  expect_s3_class(dashboard$spatial$original, "ggplot")
  expect_s3_class(dashboard$spatial$clustered, "ggplot")
})

test_that("plot_clustering_dashboard forwards map arguments to plot_spatial_clusters", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .package = "mick",
    plot_spatial_clusters = function(result, map = NULL, column = NULL) {
      captured$map <- map
      captured$column <- column
      leaf <- structure(list(), class = "ggplot")
      list(original = leaf, clustered = leaf)
    }
  )

  dashboard <- plot_clustering_dashboard(
    fixture$result,
    map = "zones.geojson",
    column = "zone_id"
  )

  expect_identical(captured$map, "zones.geojson")
  expect_identical(captured$column, "zone_id")
  expect_identical(names(dashboard), c("spatial", "duration", "dependence", "chronology"))
})

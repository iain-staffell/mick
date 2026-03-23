make_plot_spatial_map_fixture <- function(zones, column_name = "zone_name", values = zones) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf is required for this helper.", call. = FALSE)
  }

  polygons <- vector("list", length(zones))
  for (i in seq_along(zones)) {
    x <- i - 1
    polygons[[i]] <- sf::st_polygon(list(matrix(
      c(
        x, 0,
        x + 1, 0,
        x + 1, 1,
        x, 1,
        x, 0
      ),
      ncol = 2,
      byrow = TRUE
    )))
  }

  spatial <- sf::st_sf(
    data.frame(stats::setNames(list(values), column_name), stringsAsFactors = FALSE),
    geometry = sf::st_sfc(polygons)
  )

  map_path <- tempfile("plot-spatial-", fileext = ".gpkg")
  sf::st_write(spatial, map_path, quiet = TRUE)
  map_path
}

test_that("network fallback returns original and clustered ggplots", {
  skip_if_not_installed("igraph")

  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  plots <- plot_spatial_clusters(fixture$result)

  expect_type(plots, "list")
  expect_identical(names(plots), c("original", "clustered"))
  expect_s3_class(plots$original, "ggplot")
  expect_s3_class(plots$clustered, "ggplot")
  expect_null(plots$original$labels$x)
  expect_null(plots$original$labels$y)
  expect_s3_class(plots$original$theme$panel.grid.major, "element_blank")
  expect_s3_class(plots$original$theme$panel.grid.minor, "element_blank")
  expect_s3_class(plots$original$theme$axis.text, "element_blank")
  expect_s3_class(plots$original$theme$axis.ticks, "element_blank")
  expect_s3_class(plots$original$theme$axis.title, "element_blank")
})

test_that("missing map path mentions the network fallback", {
  skip_if_not_installed("sf")

  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    plot_spatial_clusters(fixture$result, map = tempfile("missing-map-", fileext = ".gpkg"), column = "zone"),
    "network fallback"
  )
})

test_that("map-backed plotting rejects missing columns and zone mismatches", {
  skip_if_not_installed("sf")

  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  zones <- fixture$result$spatial$membership$zone
  map_path <- make_plot_spatial_map_fixture(zones, column_name = "zone_name")

  expect_error(
    plot_spatial_clusters(fixture$result, map = map_path),
    "network fallback"
  )

  expect_error(
    plot_spatial_clusters(fixture$result, map = map_path, column = "missing_column"),
    "network fallback"
  )

  mismatch_map <- make_plot_spatial_map_fixture(zones, column_name = "zone_name", values = c(zones[-1], "ZZZ"))
  expect_error(
    plot_spatial_clusters(fixture$result, map = mismatch_map, column = "zone_name"),
    "network fallback"
  )
})

test_that("map-backed plotting returns original and clustered ggplots", {
  skip_if_not_installed("sf")

  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  zones <- fixture$result$spatial$membership$zone
  map_path <- make_plot_spatial_map_fixture(zones, column_name = "zone_name")

  plots <- plot_spatial_clusters(fixture$result, map = map_path, column = "zone_name")

  expect_type(plots, "list")
  expect_identical(names(plots), c("original", "clustered"))
  expect_s3_class(plots$original, "ggplot")
  expect_s3_class(plots$clustered, "ggplot")
  expect_null(plots$original$labels$x)
  expect_null(plots$original$labels$y)
})

test_that("require_plot_package reports the network fallback guidance", {
  expect_error(
    mick:::require_plot_package(
      "definitely-not-installed",
      "sf is required for map mode; call plot_spatial_clusters(result) without map to use the network fallback."
    ),
    "network fallback"
  )
})

test_that("synthetic year reconstruction preserves hour count and reduced-node columns", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  result <- fixture$result
  synthetic <- mick:::reconstruct_synthetic_year(result)

  expected_columns <- setdiff(names(result$temporal$representatives$demand), "temporal_cluster")
  expect_identical(names(synthetic$demand)[1L], "datetime")
  expect_setequal(names(synthetic$demand)[-1L], expected_columns)
  expect_equal(nrow(synthetic$demand), nrow(result$temporal$mapping))
  expect_identical(synthetic$demand$datetime, result$temporal$mapping$datetime)

  first_cluster <- result$temporal$mapping$temporal_cluster[1]
  representative_row <- result$temporal$representatives$demand[
    result$temporal$representatives$demand$temporal_cluster == first_cluster,
    ,
    drop = FALSE
  ]
  expect_equal(
    unname(unlist(synthetic$demand[1, expected_columns, drop = FALSE])),
    unname(unlist(representative_row[1, expected_columns, drop = FALSE]))
  )
})

test_that("duration selector rejects conflicting node and var", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    mick:::validate_duration_selector(fixture$result, node = "total", var = "demand"),
    "either `node` or `var`, not both"
  )
})

test_that("duration selector accepts var-only mode when node is omitted", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_silent(mick:::validate_duration_selector(fixture$result, var = "demand"))
})

test_that("plot result validation rejects malformed result objects", {
  expect_error(
    mick:::validate_plot_result(list()),
    "result|spatial|temporal|inputs"
  )
})

test_that("plot result validation rejects incomplete representative series", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  bad <- fixture$result
  bad$temporal$representatives$net_demand <- NULL

  expect_error(
    mick:::validate_plot_result(bad),
    "representatives|net_demand"
  )
})

test_that("dashboard flattening returns ggplot leaves with stable names", {
  leaf <- structure(list(), class = "ggplot")
  nested <- list(
    spatial = list(
      original = leaf,
      clustered = leaf
    ),
    temporal = list(
      duration = list(
        total = leaf,
        demand = leaf
      ),
      note = "ignore"
    )
  )

  flattened <- flatten_dashboard_plots(nested)

  expect_identical(
    names(flattened),
    c(
      "spatial.original",
      "spatial.clustered",
      "temporal.duration.total",
      "temporal.duration.demand"
    )
  )
  expect_true(all(vapply(flattened, inherits, logical(1), what = "ggplot")))
})

test_that("dashboard flattening accepts a result object and forwards map arguments", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .package = "mick",
    plot_clustering_dashboard = function(result, map = NULL, column = NULL) {
      captured$map <- map
      captured$column <- column
      leaf <- structure(list(), class = "ggplot")
      list(
        spatial = list(
          original = leaf,
          clustered = leaf
        )
      )
    }
  )

  flattened <- flatten_dashboard_plots(
    fixture$result,
    map = "zones.geojson",
    column = "zone_id"
  )

  expect_identical(captured$map, "zones.geojson")
  expect_identical(captured$column, "zone_id")
  expect_identical(names(flattened), c("spatial.original", "spatial.clustered"))
})

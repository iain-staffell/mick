test_that("plot_temporal_duration_curves defaults to total-node duration curves", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  plot <- plot_temporal_duration_curves(fixture$result)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_temporal_duration_curves rejects conflicting node and var selectors", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    plot_temporal_duration_curves(fixture$result, node = "total", var = "wind"),
    "either `node` or `var`, not both"
  )
})

test_that("plot_temporal_duration_curves supports var mode and validates selectors", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_silent(plot_temporal_duration_curves(fixture$result, var = "wind"))

  expect_error(
    plot_temporal_duration_curves(fixture$result, node = "missing-node"),
    "Unknown node"
  )

  expect_error(
    plot_temporal_duration_curves(fixture$result, var = "missing-var"),
    "Unknown var"
  )
})

test_that("plot_temporal_duration_curves rejects malformed selector inputs cleanly", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    plot_temporal_duration_curves(fixture$result, node = c("total", "other")),
    "node must be a non-empty string or NULL"
  )

  expect_error(
    plot_temporal_duration_curves(fixture$result, var = c("wind", "solar")),
    "var must be a non-empty string or NULL"
  )
})

test_that("prepare_duration_curve_data in node mode facets all variables including total", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  data <- mick:::prepare_duration_curve_data(fixture$result, node = "total", var = NULL)

  expect_true(is.data.frame(data))
  expect_setequal(as.character(unique(data$facet)), c("demand", "wind", "solar", "net_demand"))
})

test_that("prepare_duration_curve_data in var mode facets reduced nodes only", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  data <- mick:::prepare_duration_curve_data(fixture$result, node = NULL, var = "wind")
  reduced_nodes <- unique(fixture$result$spatial$membership$cluster)

  expect_true(is.data.frame(data))
  expect_false("total" %in% as.character(unique(data$facet)))
  expect_setequal(as.character(unique(data$facet)), reduced_nodes)
})

test_that("duration annual-axis helper detects supported yearlike timestep counts", {
  expect_true(mick:::.mick_duration_use_year_ticks(8760))
  expect_true(mick:::.mick_duration_use_year_ticks(8786))
  expect_false(mick:::.mick_duration_use_year_ticks(8784))
  expect_false(mick:::.mick_duration_use_year_ticks(100))
})

test_that("duration annual-axis breaks and labels match month and quarter spacing", {
  breaks <- mick:::.mick_duration_year_breaks(8760)
  labels <- mick:::.mick_duration_year_labels(breaks)

  expect_equal(breaks, seq(730, 8760, by = 730))
  expect_identical(
    labels,
    c("", "", "2190", "", "", "4380", "", "", "6570", "", "", "8760")
  )
})

test_that("yearlike duration plots disable minor x gridline breaks", {
  testthat::local_mocked_bindings(
    .package = "mick",
    .mick_prepare_duration_curve_data_impl = function(...) {
      list(
        data = data.frame(
          duration_rank = seq_len(8760),
          value = rep(1, 8760),
          source = factor(rep("Original", 8760), levels = c("Original", "Reconstructed")),
          facet = factor(rep("demand", 8760), levels = "demand")
        ),
        mode = "node",
        selector = "total",
        facets = c("demand")
      )
    }
  )

  plot <- plot_temporal_duration_curves(result = list())
  x_scale <- plot$scales$get_scales("x")

  expect_equal(x_scale$breaks, seq(730, 8760, by = 730))
  expect_null(x_scale$minor_breaks)
})

test_that("plot_temporal_chronology returns the expected plot bundle", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  plots <- plot_temporal_chronology(fixture$result)

  expect_type(plots, "list")
  expect_identical(names(plots), c("calendar", "by_month", "by_hour"))
  expect_s3_class(plots$calendar, "ggplot")
  expect_s3_class(plots$by_month, "ggplot")
  expect_s3_class(plots$by_hour, "ggplot")
})

test_that("prepare_calendar_membership_data flags reserved timeslices", {
  fixture <- make_reserved_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  calendar_data <- mick:::prepare_calendar_membership_data(fixture$result)

  expect_true("is_reserved" %in% names(calendar_data))
  expect_true(any(calendar_data$is_reserved))
  expect_identical(calendar_data$is_reserved, fixture$result$temporal$mapping$slice_type == "reserved")
})

test_that("calendar plot y-axis is clamped to the data date range", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  plots <- plot_temporal_chronology(fixture$result)
  calendar_data <- mick:::prepare_calendar_membership_data(fixture$result)
  y_scale <- plots$calendar$scales$get_scales("y")

  expect_s3_class(y_scale, "ScaleContinuousDate")
  expect_identical(as.Date(y_scale$limits), range(calendar_data$date, na.rm = TRUE))
})

test_that("calendar plot removes base tile borders", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  plots <- plot_temporal_chronology(fixture$result)
  base_layer <- plots$calendar$layers[[1L]]

  expect_true(is.na(base_layer$aes_params$colour))
  expect_identical(base_layer$aes_params$linewidth, 0)
  expect_identical(base_layer$aes_params$width, 1)
  expect_identical(base_layer$aes_params$height, 0.98)
})

test_that("reserved pch values follow the configured ordering", {
  values <- mick:::.mick_chronology_reserved_shape_values(
    cluster_levels = c(1, 2, 3, 4, 5),
    reserved_clusters = c(2, 4, 5)
  )

  expect_identical(
    values,
    c("1" = NA, "2" = 3, "3" = NA, "4" = 4, "5" = 8)
  )
})

test_that("calendar plot highlights reserved hours with symbols", {
  fixture <- make_reserved_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  plots <- plot_temporal_chronology(fixture$result)
  reserved_layer <- plots$calendar$layers[[2L]]
  fill_scale <- plots$calendar$scales$get_scales("fill")
  shape_scale <- plots$calendar$scales$get_scales("shape")

  expect_identical(reserved_layer$aes_params$colour, "black")
  expect_true(reserved_layer$aes_params$size >= 2)
  expect_true(reserved_layer$aes_params$stroke > 0)
  expect_gt(nrow(reserved_layer$data), 0)
  expect_identical(fill_scale$name, "Timeslice")
  expect_identical(shape_scale$name, "Timeslice")
  expect_identical(fill_scale$limits, shape_scale$limits)
})

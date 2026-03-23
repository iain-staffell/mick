make_temporal_spatial_result <- function() {
  datetimes <- as.POSIXct(
    sprintf("2023-01-01 %02d:00:00", 0:5),
    tz = "UTC"
  )

  demand <- data.frame(
    datetime = datetimes,
    North = c(100, 120, 95, 90, 80, 110),
    South = c(100, 120, 105, 110, 120, 90),
    check.names = FALSE
  )
  wind <- data.frame(
    datetime = datetimes,
    North = c(10, 10, 20, 15, 5, 0),
    South = c(10, 10, 15, 20, 0, 5),
    check.names = FALSE
  )
  solar <- data.frame(
    datetime = datetimes,
    North = c(0, 0, 10, 15, 20, 5),
    South = c(0, 0, 5, 10, 15, 20),
    check.names = FALSE
  )

  reduced_view <- list(
    demand = demand,
    wind = wind,
    solar = solar,
    grid = matrix(
      c(0, 50, 50, 0),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("North", "South"), c("North", "South"))
    )
  )
  reduced_view$net_demand <- mick:::.mick_compute_net_demand(
    reduced_view$demand,
    reduced_view$wind,
    reduced_view$solar
  )

  list(
    membership = data.frame(
      zone = c("North", "South"),
      cluster = c("North", "South"),
      stringsAsFactors = FALSE
    ),
    original_view = reduced_view,
    clustering_view = reduced_view,
    reduced_demand = demand,
    reduced_wind = wind,
    reduced_solar = solar,
    reduced_grid = reduced_view$grid
  )
}

manual_max_transfer_scores <- function(reduced_view) {
  zone_names <- names(reduced_view$demand)[-1L]
  gross_means <- vapply(zone_names, function(zone) {
    mean(reduced_view$demand[[zone]])
  }, numeric(1))

  normalized_net <- vapply(zone_names, function(zone) {
    reduced_view$net_demand[[zone]] / gross_means[[zone]]
  }, numeric(nrow(reduced_view$net_demand)))
  anomalies <- sweep(
    normalized_net,
    MARGIN = 2L,
    STATS = colMeans(normalized_net),
    FUN = "-"
  )

  vapply(seq_len(nrow(anomalies)), function(i) {
    stats::weighted.mean(
      (anomalies[i, ] - stats::weighted.mean(anomalies[i, ], gross_means)) ^ 2,
      gross_means
    ) ^ 0.5
  }, numeric(1))
}

test_that("reserved rules select unique hours with backfill", {
  temporal <- mick:::run_temporal_reduction(
    spatial_result = make_temporal_spatial_result(),
    n_time = 4L,
    reserved_rules = c("max_net_demand", "max_demand"),
    temporal_method = "k_medians"
  )

  expect_equal(nrow(temporal$reserved), 2L)
  expect_equal(length(unique(temporal$reserved$datetime)), 2L)
  expect_identical(temporal$reserved$rule, c("max_net_demand", "max_demand"))
  expect_identical(temporal$reserved$hour_index, c(2L, 1L))
})

test_that("max_transfer is based on weighted anomaly dispersion", {
  reduced_view <- make_temporal_spatial_result()$clustering_view
  expected_scores <- manual_max_transfer_scores(reduced_view)
  scores <- mick:::score_max_transfer_hours(reduced_view)

  expect_type(scores, "double")
  expect_equal(length(scores), nrow(reduced_view$demand))
  expect_equal(scores, expected_scores, tolerance = 1e-10)
  expect_identical(which.max(scores), which.max(expected_scores))
})

test_that("cluster_remaining_hours repairs duplicate-seed empty clusters deterministically", {
  state_matrix <- rbind(
    c(0, 0),
    c(0, 0),
    c(10, 10),
    c(10, 10)
  )
  colnames(state_matrix) <- c("x", "y")

  expect_identical(mick:::.mick_temporal_initial_indices(state_matrix, 3L), c(1L, 2L, 4L))

  clustered <- mick:::cluster_remaining_hours(
    state_matrix = state_matrix,
    k = 3L,
    method = "k_means"
  )

  expect_identical(clustered$cluster, c(2L, 1L, 3L, 3L))
  expect_equal(
    unname(clustered$centers),
    matrix(
      c(
        0, 0,
        0, 0,
        10, 10
      ),
      nrow = 3,
      byrow = TRUE
    )
  )
})

test_that("temporal reduction returns exactly n_time slices and weights", {
  spatial_result <- make_temporal_spatial_result()
  temporal <- mick:::run_temporal_reduction(
    spatial_result = spatial_result,
    n_time = 4L,
    reserved_rules = c("max_net_demand", "min_net_demand"),
    temporal_method = "k_means"
  )

  expect_equal(length(unique(temporal$mapping$temporal_cluster)), 4L)
  expect_equal(nrow(temporal$mapping), nrow(spatial_result$original_view$demand))
  expect_equal(sum(temporal$weights$weight_hours), nrow(spatial_result$original_view$demand))
  expect_equal(nrow(temporal$weights), 4L)
  expect_equal(nrow(temporal$representatives$demand), 4L)
  expect_equal(nrow(temporal$representatives$wind), 4L)
  expect_equal(nrow(temporal$representatives$solar), 4L)
  expect_equal(nrow(temporal$representatives$net_demand), 4L)
})

test_that("k_means temporal reduction produces deterministic mapping and representatives", {
  temporal <- mick:::run_temporal_reduction(
    spatial_result = make_temporal_spatial_result(),
    n_time = 4L,
    reserved_rules = c("max_net_demand", "min_net_demand"),
    temporal_method = "k_means"
  )

  expect_identical(temporal$reserved$hour_index, c(2L, 4L))
  expect_identical(temporal$mapping$temporal_cluster, c(3L, 1L, 3L, 2L, 3L, 4L))
  expect_identical(
    temporal$mapping$slice_type,
    c("cluster", "reserved", "cluster", "reserved", "cluster", "cluster")
  )
  expect_identical(temporal$weights$weight_hours, c(1L, 1L, 3L, 1L))
  expect_equal(
    temporal$representatives$demand[, c("North", "South")],
    data.frame(
      North = c(120, 90, 91.66666666666667, 110),
      South = c(120, 110, 108.33333333333333, 90),
      check.names = FALSE
    )
  )
  expect_equal(
    temporal$representatives$net_demand[, c("North", "South")],
    data.frame(
      North = c(110, 60, 70, 105),
      South = c(110, 80, 93.33333333333333, 65),
      check.names = FALSE
    )
  )
})

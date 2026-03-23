#' Run the MICK reduction pipeline
#'
#' Executes the full MICK workflow: read and validate configuration, build input
#' views, perform spatial and temporal reduction, and export outputs.
#'
#' @param config Either a canonical config list or a path to a JSON/YAML config
#'   file.
#'
#' @return A named list containing the full in-memory result object, including
#'   inputs, spatial and temporal reductions, and output metadata.
#'
#' @examples
#' \donttest{
#' files <- mick_example_files(format = "yaml")
#' result <- run_mick(files$config)
#' names(result$outputs$written_files)
#' }
#' @export
run_mick <- function(config) {
  if (!(is.list(config) || (is.character(config) && length(config) == 1L && !is.na(config) && nzchar(config)))) {
    stop("config must be a list or a non-empty character path.", call. = FALSE)
  }

  config <- read_mick_config(config)
  validate_config(config)
  input_views <- build_input_views(config)
  spatial <- run_spatial_reduction(
    views = input_views,
    n_space = config$clustering$n_space,
    spatial_weights = config$weights$spatial_features
  )
  temporal <- if (length(config$clustering$reserved_time_slices) == 0L) {
    run_temporal_reduction_no_reserved_rules(
      spatial_result = spatial,
      n_time = config$clustering$n_time,
      temporal_method = config$clustering$temporal_method
    )
  } else {
    run_temporal_reduction(
      spatial_result = spatial,
      n_time = config$clustering$n_time,
      reserved_rules = config$clustering$reserved_time_slices,
      temporal_method = config$clustering$temporal_method
    )
  }

  result <- assemble_mick_result(
    config = config,
    inputs = input_views,
    spatial = spatial,
    temporal = temporal
  )

  export_mick_outputs(result)
}

run_temporal_reduction_no_reserved_rules <- function(spatial_result, n_time, temporal_method = "k_medians") {
  .mick_temporal_validate_inputs(
    spatial_result = spatial_result,
    n_time = n_time,
    reserved_rules = character(0),
    temporal_method = temporal_method
  )

  original_view <- .mick_temporal_view(spatial_result, "original_view")
  clustering_view <- .mick_temporal_view(spatial_result, "clustering_view")
  n_hours <- nrow(clustering_view$demand)

  if (n_time > n_hours) {
    stop("n_time cannot exceed the number of original hours.", call. = FALSE)
  }

  state_matrix <- .mick_temporal_state_matrix(clustering_view)
  n_cluster_slices <- n_time

  if (n_cluster_slices > 0L) {
    clustered <- cluster_remaining_hours(
      state_matrix = state_matrix,
      k = n_cluster_slices,
      method = temporal_method
    )
  } else {
    clustered <- cluster_remaining_hours(
      state_matrix = state_matrix[FALSE, , drop = FALSE],
      k = 0L,
      method = temporal_method
    )
  }

  mapping <- data.frame(
    hour_index = seq_len(n_hours),
    datetime = original_view$demand$datetime,
    temporal_cluster = clustered$cluster,
    slice_type = "cluster",
    stringsAsFactors = FALSE
  )

  cluster_slice_info <- if (n_cluster_slices > 0L) {
    data.frame(
      rule = rep(NA_character_, n_cluster_slices),
      hour_index = rep(NA_integer_, n_cluster_slices),
      datetime = as.POSIXct(rep(NA_real_, n_cluster_slices), origin = "1970-01-01", tz = "UTC"),
      score = rep(NA_real_, n_cluster_slices),
      temporal_cluster = seq_len(n_time),
      slice_type = rep("cluster", n_cluster_slices),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      rule = character(),
      hour_index = integer(),
      datetime = as.POSIXct(character(), tz = "UTC"),
      score = numeric(),
      temporal_cluster = integer(),
      slice_type = character(),
      stringsAsFactors = FALSE
    )
  }

  reserved <- data.frame(
    rule = character(),
    hour_index = integer(),
    datetime = as.POSIXct(character(), tz = "UTC"),
    score = numeric(),
    stringsAsFactors = FALSE
  )
  reserved$temporal_cluster <- integer(0)

  slice_info <- cluster_slice_info[order(cluster_slice_info$temporal_cluster), , drop = FALSE]
  representatives <- .mick_temporal_representatives(
    original_view = original_view,
    slice_info = slice_info,
    mapping = mapping,
    temporal_method = temporal_method
  )
  weights <- .mick_temporal_weights(mapping, slice_info)

  list(
    reserved = reserved[, c("rule", "hour_index", "datetime", "score", "temporal_cluster"), drop = FALSE],
    slices = slice_info,
    mapping = mapping,
    weights = weights,
    representatives = representatives,
    reduced_demand = representatives$demand,
    reduced_wind = representatives$wind,
    reduced_solar = representatives$solar,
    reduced_net_demand = representatives$net_demand,
    method = temporal_method
  )
}

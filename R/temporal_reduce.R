.mick_supported_reserved_rules <- function() {
  c(
    "max_net_demand",
    "min_net_demand",
    "max_transfer",
    "max_demand",
    "min_demand",
    "max_solar",
    "min_solar",
    "max_wind",
    "min_wind"
  )
}

.mick_temporal_view <- function(spatial_result, slot) {
  view <- spatial_result[[slot]]

  if (is.null(view) && identical(slot, "original_view")) {
    view <- spatial_result$original
  }
  if (is.null(view) && identical(slot, "clustering_view")) {
    view <- spatial_result$clustering
  }
  if (!is.list(view)) {
    stop("spatial_result must include ", slot, ".", call. = FALSE)
  }

  .mick_spatial_prepare_view(view)
}

.mick_temporal_validate_inputs <- function(spatial_result, n_time, reserved_rules, temporal_method) {
  if (!is.list(spatial_result)) {
    stop("spatial_result must be a list.", call. = FALSE)
  }
  if (!is.numeric(n_time) || length(n_time) != 1L || is.na(n_time) || n_time < 1L || n_time != floor(n_time)) {
    stop("n_time must be a positive integer.", call. = FALSE)
  }
  if (!is.character(reserved_rules) || anyNA(reserved_rules)) {
    stop("reserved_rules must be a character vector.", call. = FALSE)
  }

  unknown_rules <- setdiff(reserved_rules, .mick_supported_reserved_rules())
  if (length(unknown_rules)) {
    stop(
      "Unsupported reserved rules: ",
      paste(unknown_rules, collapse = ", "),
      call. = FALSE
    )
  }

  if (!identical(temporal_method, "k_medians") && !identical(temporal_method, "k_means")) {
    stop("temporal_method must be one of: k_medians, k_means.", call. = FALSE)
  }
}

.mick_temporal_totals <- function(series_df) {
  rowSums(series_df[, -1L, drop = FALSE])
}

score_max_transfer_hours <- function(reduced_view) {
  reduced_view <- .mick_spatial_prepare_view(reduced_view)
  zone_names <- names(reduced_view$demand)[-1L]

  gross_means <- vapply(zone_names, function(zone) {
    mean(reduced_view$demand[[zone]])
  }, numeric(1))

  normalized_net_demand <- vapply(zone_names, function(zone) {
    reduced_view$net_demand[[zone]] / max(gross_means[[zone]], .Machine$double.eps)
  }, numeric(nrow(reduced_view$net_demand)))

  anomalies <- sweep(
    normalized_net_demand,
    MARGIN = 2L,
    STATS = colMeans(normalized_net_demand),
    FUN = "-"
  )

  vapply(seq_len(nrow(anomalies)), function(i) {
    row_values <- anomalies[i, ]
    row_mean <- stats::weighted.mean(row_values, gross_means)
    stats::weighted.mean((row_values - row_mean) ^ 2, gross_means) ^ 0.5
  }, numeric(1))
}

.mick_reserved_rule_scores <- function(reduced_view, rule) {
  switch(
    rule,
    max_net_demand = .mick_temporal_totals(reduced_view$net_demand),
    min_net_demand = -.mick_temporal_totals(reduced_view$net_demand),
    max_transfer = score_max_transfer_hours(reduced_view),
    max_demand = .mick_temporal_totals(reduced_view$demand),
    min_demand = -.mick_temporal_totals(reduced_view$demand),
    max_solar = .mick_temporal_totals(reduced_view$solar),
    min_solar = -.mick_temporal_totals(reduced_view$solar),
    max_wind = .mick_temporal_totals(reduced_view$wind),
    min_wind = -.mick_temporal_totals(reduced_view$wind),
    stop("Unsupported reserved rule: ", rule, call. = FALSE)
  )
}

select_reserved_hours <- function(reduced_view, reserved_rules) {
  reduced_view <- .mick_spatial_prepare_view(reduced_view)

  if (length(reserved_rules) == 0L) {
    return(data.frame(
      rule = character(),
      hour_index = integer(),
      datetime = as.POSIXct(character(), tz = "UTC"),
      score = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  selected_hours <- integer()
  reserved_rows <- vector("list", length(reserved_rules))

  for (i in seq_along(reserved_rules)) {
    rule <- reserved_rules[[i]]
    scores <- .mick_reserved_rule_scores(reduced_view, rule)
    ordered_hours <- order(-scores, seq_along(scores))
    candidate_hours <- ordered_hours[!ordered_hours %in% selected_hours]

    if (!length(candidate_hours)) {
      stop("Could not backfill a distinct reserved hour for rule: ", rule, call. = FALSE)
    }

    hour_index <- candidate_hours[[1L]]
    selected_hours <- c(selected_hours, hour_index)
    reserved_rows[[i]] <- data.frame(
      rule = rule,
      hour_index = hour_index,
      datetime = reduced_view$demand$datetime[[hour_index]],
      score = scores[[hour_index]],
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, reserved_rows)
}

.mick_temporal_state_matrix <- function(reduced_view) {
  reduced_view <- .mick_spatial_prepare_view(reduced_view)
  zone_names <- names(reduced_view$demand)[-1L]
  series_names <- c("demand", "wind", "solar", "net_demand")

  blocks <- lapply(series_names, function(series_name) {
    block <- as.matrix(reduced_view[[series_name]][, zone_names, drop = FALSE])
    colnames(block) <- paste(series_name, zone_names, sep = "::")
    block
  })

  out <- do.call(cbind, blocks)
  rownames(out) <- as.character(reduced_view$demand$datetime)
  out
}

.mick_temporal_initial_indices <- function(state_matrix, k) {
  n <- nrow(state_matrix)
  ordered <- order(rowSums(state_matrix), seq_len(n))
  proposed <- ordered[round(seq(1, n, length.out = k))]
  proposed <- unique(proposed)

  if (length(proposed) < k) {
    proposed <- c(proposed, ordered[!ordered %in% proposed][seq_len(k - length(proposed))])
  }

  proposed
}

.mick_distance_matrix <- function(state_matrix, centers, method) {
  out <- matrix(
    0,
    nrow = nrow(state_matrix),
    ncol = nrow(centers)
  )

  for (i in seq_len(nrow(centers))) {
    deltas <- sweep(state_matrix, MARGIN = 2L, STATS = centers[i, ], FUN = "-")
    out[, i] <- if (identical(method, "k_means")) {
      rowSums(deltas ^ 2)
    } else {
      rowSums(abs(deltas))
    }
  }

  out
}

.mick_temporal_assign <- function(state_matrix, centers, method) {
  distances <- .mick_distance_matrix(state_matrix, centers, method)
  max.col(-distances, ties.method = "first")
}

.mick_temporal_center <- function(state_matrix, method) {
  if (identical(method, "k_means")) {
    colMeans(state_matrix)
  } else {
    apply(state_matrix, 2L, stats::median)
  }
}

.mick_repair_empty_temporal_clusters <- function(state_matrix, assignment, centers, method) {
  counts <- tabulate(assignment, nbins = nrow(centers))
  empty_clusters <- which(counts == 0L)

  if (!length(empty_clusters)) {
    return(list(assignment = assignment, centers = centers))
  }

  distances <- .mick_distance_matrix(state_matrix, centers, method)

  for (cluster_id in empty_clusters) {
    counts <- tabulate(assignment, nbins = nrow(centers))
    donor_clusters <- which(counts > 1L)

    if (!length(donor_clusters)) {
      stop("Temporal clustering could not maintain non-empty clusters.", call. = FALSE)
    }

    donor_cluster <- donor_clusters[order(-counts[donor_clusters], donor_clusters)][1L]
    donor_members <- which(assignment == donor_cluster)
    donor_distances <- distances[donor_members, donor_cluster]
    moved_index <- donor_members[order(-donor_distances, donor_members)][1L]

    assignment[moved_index] <- cluster_id
    centers[cluster_id, ] <- state_matrix[moved_index, ]
  }

  list(assignment = assignment, centers = centers)
}

cluster_remaining_hours <- function(state_matrix, k, method = "k_medians", max_iter = 100L) {
  state_matrix <- as.matrix(state_matrix)

  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 0L || k != floor(k)) {
    stop("k must be a non-negative integer.", call. = FALSE)
  }
  if (!identical(method, "k_medians") && !identical(method, "k_means")) {
    stop("method must be one of: k_medians, k_means.", call. = FALSE)
  }
  if (k == 0L) {
    return(list(
      cluster = integer(nrow(state_matrix)),
      centers = state_matrix[FALSE, , drop = FALSE],
      method = method,
      iterations = 0L
    ))
  }
  if (nrow(state_matrix) < k) {
    stop("Temporal reduction requires at least as many non-reserved hours as non-reserved slices.", call. = FALSE)
  }
  if (k == nrow(state_matrix)) {
    centers <- state_matrix
    rownames(centers) <- paste0("cluster_", seq_len(k))
    return(list(
      cluster = seq_len(k),
      centers = centers,
      method = method,
      iterations = 0L
    ))
  }

  centers <- state_matrix[.mick_temporal_initial_indices(state_matrix, k), , drop = FALSE]
  rownames(centers) <- paste0("cluster_", seq_len(nrow(centers)))

  last_assignment <- NULL

  for (iter in seq_len(max_iter)) {
    assignment <- .mick_temporal_assign(state_matrix, centers, method)
    repaired <- .mick_repair_empty_temporal_clusters(
      state_matrix = state_matrix,
      assignment = assignment,
      centers = centers,
      method = method
    )
    assignment <- repaired$assignment
    centers <- repaired$centers

    new_centers <- centers
    for (cluster_id in seq_len(k)) {
      members <- state_matrix[assignment == cluster_id, , drop = FALSE]
      new_centers[cluster_id, ] <- .mick_temporal_center(members, method)
    }

    if (identical(assignment, last_assignment) &&
        isTRUE(all.equal(unname(new_centers), unname(centers), tolerance = 1e-8))) {
      centers <- new_centers
      break
    }

    last_assignment <- assignment
    centers <- new_centers
  }

  rownames(centers) <- paste0("cluster_", seq_len(nrow(centers)))

  list(
    cluster = assignment,
    centers = centers,
    method = method,
    iterations = iter
  )
}

.mick_temporal_series_center <- function(series_df, row_indices, method) {
  values <- as.matrix(series_df[row_indices, -1L, drop = FALSE])
  if (nrow(values) == 1L) {
    return(as.numeric(values[1L, ]))
  }
  if (identical(method, "k_means")) {
    return(colMeans(values))
  }
  apply(values, 2L, stats::median)
}

.mick_bind_temporal_rows <- function(rows, zone_names) {
  out <- data.frame(
    temporal_cluster = seq_along(rows),
    matrix(unlist(rows, use.names = FALSE), nrow = length(rows), byrow = TRUE),
    check.names = FALSE
  )
  names(out) <- c("temporal_cluster", zone_names)
  out
}

.mick_temporal_representatives <- function(original_view, slice_info, mapping, temporal_method) {
  series_names <- c("demand", "wind", "solar", "net_demand")
  zone_names <- names(original_view$demand)[-1L]
  representatives <- vector("list", length(series_names))
  names(representatives) <- series_names

  for (series_name in series_names) {
    series_rows <- lapply(seq_len(nrow(slice_info)), function(i) {
      slice <- slice_info[i, , drop = FALSE]

      if (identical(slice$slice_type[[1L]], "reserved")) {
        return(as.numeric(original_view[[series_name]][slice$hour_index[[1L]], zone_names, drop = FALSE]))
      }

      member_indices <- mapping$hour_index[mapping$temporal_cluster == slice$temporal_cluster[[1L]]]
      if (!length(member_indices)) {
        stop("Temporal cluster has no mapped hours: ", slice$temporal_cluster[[1L]], call. = FALSE)
      }

      .mick_temporal_series_center(
        series_df = original_view[[series_name]],
        row_indices = member_indices,
        method = temporal_method
      )
    })

    representatives[[series_name]] <- .mick_bind_temporal_rows(series_rows, zone_names)
  }

  representatives
}

.mick_temporal_weights <- function(mapping, slice_info) {
  weights <- data.frame(
    temporal_cluster = slice_info$temporal_cluster,
    weight_hours = as.integer(tabulate(mapping$temporal_cluster, nbins = nrow(slice_info))),
    slice_type = slice_info$slice_type,
    stringsAsFactors = FALSE
  )

  weights
}

run_temporal_reduction <- function(spatial_result, n_time, reserved_rules = character(), temporal_method = "k_medians") {
  .mick_temporal_validate_inputs(
    spatial_result = spatial_result,
    n_time = n_time,
    reserved_rules = reserved_rules,
    temporal_method = temporal_method
  )

  original_view <- .mick_temporal_view(spatial_result, "original_view")
  clustering_view <- .mick_temporal_view(spatial_result, "clustering_view")
  n_hours <- nrow(clustering_view$demand)

  if (n_time > n_hours) {
    stop("n_time cannot exceed the number of original hours.", call. = FALSE)
  }

  reserved <- select_reserved_hours(clustering_view, reserved_rules)
  n_reserved <- nrow(reserved)

  if (n_reserved > n_time) {
    stop("n_time must be at least the number of reserved rules.", call. = FALSE)
  }

  state_matrix <- .mick_temporal_state_matrix(clustering_view)
  reserved_hours <- reserved$hour_index
  remaining_hours <- setdiff(seq_len(n_hours), reserved_hours)
  n_cluster_slices <- n_time - n_reserved

  if (n_cluster_slices > length(remaining_hours)) {
    stop("Temporal reduction requires enough non-reserved hours to form the requested slices.", call. = FALSE)
  }

  if (n_cluster_slices > 0L) {
    clustered <- cluster_remaining_hours(
      state_matrix = state_matrix[remaining_hours, , drop = FALSE],
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

  reserved$temporal_cluster <- seq_len(n_reserved)
  reserved$slice_type <- "reserved"

  mapping <- data.frame(
    hour_index = seq_len(n_hours),
    datetime = original_view$demand$datetime,
    temporal_cluster = integer(n_hours),
    slice_type = character(n_hours),
    stringsAsFactors = FALSE
  )

  if (n_reserved > 0L) {
    mapping$temporal_cluster[reserved_hours] <- reserved$temporal_cluster
    mapping$slice_type[reserved_hours] <- "reserved"
  }

  if (n_cluster_slices > 0L) {
    cluster_ids <- clustered$cluster + n_reserved
    mapping$temporal_cluster[remaining_hours] <- cluster_ids
    mapping$slice_type[remaining_hours] <- "cluster"
  } else if (length(remaining_hours)) {
    reserved_centers <- state_matrix[reserved_hours, , drop = FALSE]
    nearest_reserved <- .mick_temporal_assign(
      state_matrix = state_matrix[remaining_hours, , drop = FALSE],
      centers = reserved_centers,
      method = temporal_method
    )
    mapping$temporal_cluster[remaining_hours] <- nearest_reserved
    mapping$slice_type[remaining_hours] <- "reserved"
  }

  cluster_slice_info <- if (n_cluster_slices > 0L) {
    data.frame(
      rule = rep(NA_character_, n_cluster_slices),
      hour_index = rep(NA_integer_, n_cluster_slices),
      datetime = as.POSIXct(rep(NA_real_, n_cluster_slices), origin = "1970-01-01", tz = "UTC"),
      score = rep(NA_real_, n_cluster_slices),
      temporal_cluster = seq.int(n_reserved + 1L, n_time),
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

  slice_info <- rbind(
    reserved[, c("rule", "hour_index", "datetime", "score", "temporal_cluster", "slice_type"), drop = FALSE],
    cluster_slice_info
  )

  slice_info <- slice_info[order(slice_info$temporal_cluster), , drop = FALSE]
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

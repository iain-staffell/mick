.mick_spatial_prepare_view <- function(view) {
  out <- view

  if (is.null(out$net_demand)) {
    out$net_demand <- .mick_compute_net_demand(
      out$demand,
      out$wind,
      out$solar
    )
  }

  out
}

.mick_initial_clusters <- function(zone_names) {
  stats::setNames(as.list(zone_names), zone_names)
}

.mick_membership_from_clusters <- function(clusters, zone_order) {
  cluster_by_zone <- stats::setNames(character(length(zone_order)), zone_order)

  for (cluster_name in names(clusters)) {
    cluster_by_zone[clusters[[cluster_name]]] <- cluster_name
  }

  data.frame(
    zone = zone_order,
    cluster = unname(cluster_by_zone[zone_order]),
    stringsAsFactors = FALSE
  )
}

.mick_boundary_capacity <- function(cluster_a, cluster_b, grid) {
  sum(grid[cluster_a, cluster_b, drop = FALSE])
}

.mick_spatial_feature_weights <- function(weights = NULL) {
  defaults <- c(shape = 1, magnitude = 1, connectivity = 1)

  if (is.null(weights) || length(weights) == 0L) {
    return(defaults)
  }

  if (is.list(weights) && !is.data.frame(weights)) {
    weights <- unlist(weights, use.names = TRUE)
  }

  if (!is.numeric(weights)) {
    stop("spatial feature weights must be numeric.", call. = FALSE)
  }
  if (is.null(names(weights)) || any(!nzchar(names(weights)))) {
    stop("spatial feature weights must be named.", call. = FALSE)
  }
  if (anyNA(weights) || any(!is.finite(weights))) {
    stop("spatial feature weights must be finite numeric values.", call. = FALSE)
  }
  if (any(weights < 0)) {
    stop("spatial feature weights must be zero or positive.", call. = FALSE)
  }

  unknown <- setdiff(names(weights), names(defaults))
  if (length(unknown)) {
    stop(
      "Unknown spatial feature weights: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  matched <- intersect(names(defaults), names(weights))
  defaults[matched] <- as.numeric(weights[matched])
  defaults
}

.mick_shape_feature_row <- function(series) {
  mean_scale <- max(abs(mean(series)), .Machine$double.eps)
  normalized <- series / mean_scale
  stats::quantile(
    normalized,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    names = FALSE
  )
}

.mick_spatial_cluster_view <- function(clustering_view, clusters, zone_order) {
  membership <- .mick_membership_from_clusters(clusters, zone_order)
  aggregate_spatial_view(clustering_view, membership)
}

.mick_score_candidates <- function(candidates, features, weights) {
  if (nrow(candidates) == 0L) {
    return(candidates)
  }

  feature_distance <- function(feature_df, cluster_a, cluster_b) {
    row_a <- feature_df[match(cluster_a, feature_df$cluster), -1L, drop = FALSE]
    row_b <- feature_df[match(cluster_b, feature_df$cluster), -1L, drop = FALSE]
    sqrt(sum((as.numeric(row_a) - as.numeric(row_b)) ^ 2))
  }

  normalize_component <- function(x) {
    if (length(x) == 0L || all(abs(x - x[[1L]]) < .Machine$double.eps)) {
      return(rep(0, length(x)))
    }
    (x - min(x)) / (max(x) - min(x))
  }

  candidates$shape_raw <- vapply(seq_len(nrow(candidates)), function(i) {
    feature_distance(features$shape, candidates$cluster_a[[i]], candidates$cluster_b[[i]])
  }, numeric(1))
  candidates$magnitude_raw <- vapply(seq_len(nrow(candidates)), function(i) {
    feature_distance(features$magnitude, candidates$cluster_a[[i]], candidates$cluster_b[[i]])
  }, numeric(1))
  candidates$shape_score <- normalize_component(candidates$shape_raw)
  candidates$magnitude_score <- normalize_component(candidates$magnitude_raw)
  candidates$connectivity_score <- normalize_component(max(candidates$boundary_capacity) - candidates$boundary_capacity)
  candidates$total_score <- (
    weights[["shape"]] * candidates$shape_score +
      weights[["magnitude"]] * candidates$magnitude_score +
      weights[["connectivity"]] * candidates$connectivity_score
  )

  candidates
}

.mick_select_best_candidate <- function(candidates) {
  ordered <- order(
    candidates$total_score,
    candidates$shape_score,
    candidates$magnitude_score,
    candidates$connectivity_score,
    -candidates$boundary_capacity,
    candidates$cluster_a,
    candidates$cluster_b
  )

  candidates[ordered[1L], , drop = FALSE]
}

.mick_merge_clusters <- function(clusters, cluster_a, cluster_b) {
  merged_zones <- sort(unique(c(clusters[[cluster_a]], clusters[[cluster_b]])))
  merged_name <- paste(merged_zones, collapse = "+")

  remaining <- clusters[setdiff(names(clusters), c(cluster_a, cluster_b))]
  remaining[[merged_name]] <- merged_zones
  remaining[order(names(remaining))]
}

.mick_reduce_to_target_clusters <- function(clustering_view, n_space, spatial_weights) {
  zone_order <- names(clustering_view$demand)[-1L]

  if (n_space > length(zone_order)) {
    stop("n_space cannot exceed the number of zones.", call. = FALSE)
  }

  clusters <- .mick_initial_clusters(zone_order)
  merge_rows <- list()
  step <- 0L

  if (n_space < 1L) {
    stop("n_space must be a positive integer.", call. = FALSE)
  }

  while (length(clusters) > n_space) {
    features <- build_spatial_features(clustering_view, clusters)
    candidates <- enumerate_spatial_candidates(clusters, clustering_view$grid)

    if (nrow(candidates) == 0L) {
      stop("Spatial reduction could not reach the requested n_space under adjacency constraints.", call. = FALSE)
    }

    scored <- .mick_score_candidates(candidates, features, spatial_weights)
    best <- .mick_select_best_candidate(scored)
    next_clusters <- .mick_merge_clusters(clusters, best$cluster_a[[1L]], best$cluster_b[[1L]])
    merged_name <- setdiff(names(next_clusters), names(clusters))

    step <- step + 1L
    merge_rows[[step]] <- data.frame(
      step = step,
      cluster_a = best$cluster_a[[1L]],
      cluster_b = best$cluster_b[[1L]],
      merged_cluster = merged_name[[1L]],
      boundary_capacity = best$boundary_capacity[[1L]],
      shape_score = best$shape_score[[1L]],
      magnitude_score = best$magnitude_score[[1L]],
      connectivity_score = best$connectivity_score[[1L]],
      total_score = best$total_score[[1L]],
      stringsAsFactors = FALSE
    )

    clusters <- next_clusters
  }

  list(
    clusters = clusters,
    merge_history = if (length(merge_rows)) do.call(rbind, merge_rows) else {
      data.frame(
        step = integer(),
        cluster_a = character(),
        cluster_b = character(),
        merged_cluster = character(),
        boundary_capacity = numeric(),
        shape_score = numeric(),
        magnitude_score = numeric(),
        connectivity_score = numeric(),
        total_score = numeric(),
        stringsAsFactors = FALSE
      )
    }
  )
}

build_spatial_features <- function(clustering_view, clusters = NULL) {
  clustering_view <- .mick_spatial_prepare_view(clustering_view)
  zone_order <- names(clustering_view$demand)[-1L]

  if (is.null(clusters)) {
    clusters <- .mick_initial_clusters(zone_order)
  }

  aggregated <- .mick_spatial_cluster_view(clustering_view, clusters, zone_order)
  cluster_names <- names(aggregated$demand)[-1L]

  magnitude_rows <- lapply(cluster_names, function(cluster_name) {
    net_demand <- aggregated$net_demand[[cluster_name]]
    data.frame(
      cluster = cluster_name,
      mean_demand = mean(aggregated$demand[[cluster_name]]),
      mean_wind = mean(aggregated$wind[[cluster_name]]),
      mean_solar = mean(aggregated$solar[[cluster_name]]),
      mean_net_demand = mean(net_demand),
      net_demand_p10 = stats::quantile(net_demand, probs = 0.10, names = FALSE),
      net_demand_p50 = stats::quantile(net_demand, probs = 0.50, names = FALSE),
      net_demand_p90 = stats::quantile(net_demand, probs = 0.90, names = FALSE),
      check.names = FALSE
    )
  })

  shape_rows <- lapply(cluster_names, function(cluster_name) {
    shape_values <- c(
      .mick_shape_feature_row(aggregated$demand[[cluster_name]]),
      .mick_shape_feature_row(aggregated$wind[[cluster_name]]),
      .mick_shape_feature_row(aggregated$solar[[cluster_name]]),
      .mick_shape_feature_row(aggregated$net_demand[[cluster_name]])
    )

    data.frame(
      cluster = cluster_name,
      matrix(shape_values, nrow = 1L, dimnames = list(NULL, c(
        paste0("demand_q", c("05", "25", "50", "75", "95")),
        paste0("wind_q", c("05", "25", "50", "75", "95")),
        paste0("solar_q", c("05", "25", "50", "75", "95")),
        paste0("net_demand_q", c("05", "25", "50", "75", "95"))
      ))),
      check.names = FALSE
    )
  })

  list(
    magnitude = do.call(rbind, magnitude_rows),
    shape = do.call(rbind, shape_rows)
  )
}

enumerate_spatial_candidates <- function(clusters, grid) {
  cluster_names <- names(clusters)

  if (length(cluster_names) < 2L) {
    return(data.frame(
      cluster_a = character(),
      cluster_b = character(),
      boundary_capacity = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  candidates <- list()
  next_row <- 1L

  for (i in seq_len(length(cluster_names) - 1L)) {
    for (j in seq.int(i + 1L, length(cluster_names))) {
      boundary_capacity <- .mick_boundary_capacity(
        clusters[[cluster_names[[i]]]],
        clusters[[cluster_names[[j]]]],
        grid
      )

      if (boundary_capacity > 0) {
        candidates[[next_row]] <- data.frame(
          cluster_a = cluster_names[[i]],
          cluster_b = cluster_names[[j]],
          boundary_capacity = boundary_capacity,
          stringsAsFactors = FALSE
        )
        next_row <- next_row + 1L
      }
    }
  }

  if (!length(candidates)) {
    return(data.frame(
      cluster_a = character(),
      cluster_b = character(),
      boundary_capacity = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, candidates)
}

aggregate_spatial_series <- function(series_df, membership) {
  cluster_names <- unique(membership$cluster)
  out <- data.frame(datetime = series_df$datetime, check.names = FALSE)

  for (cluster_name in cluster_names) {
    zones <- membership$zone[membership$cluster == cluster_name]
    out[[cluster_name]] <- rowSums(series_df[, zones, drop = FALSE])
  }

  out
}

aggregate_reduced_grid <- function(grid, membership) {
  cluster_names <- unique(membership$cluster)
  clusters <- split(membership$zone, membership$cluster)
  reduced <- matrix(
    0,
    nrow = length(cluster_names),
    ncol = length(cluster_names),
    dimnames = list(cluster_names, cluster_names)
  )

  if (length(cluster_names) < 2L) {
    return(reduced)
  }

  for (i in seq_len(length(cluster_names) - 1L)) {
    for (j in seq.int(i + 1L, length(cluster_names))) {
      capacity <- .mick_boundary_capacity(
        clusters[[cluster_names[[i]]]],
        clusters[[cluster_names[[j]]]],
        grid
      )
      reduced[i, j] <- capacity
      reduced[j, i] <- capacity
    }
  }

  reduced
}

aggregate_spatial_view <- function(view, membership) {
  out <- list(
    wind = aggregate_spatial_series(view$wind, membership),
    solar = aggregate_spatial_series(view$solar, membership),
    demand = aggregate_spatial_series(view$demand, membership),
    grid = aggregate_reduced_grid(view$grid, membership)
  )
  out$net_demand <- .mick_compute_net_demand(out$demand, out$wind, out$solar)
  out
}

run_spatial_reduction <- function(views, n_space, spatial_weights = NULL) {
  clustering_view <- .mick_spatial_prepare_view(views$clustering)
  original_view <- .mick_spatial_prepare_view(views$original)
  zone_order <- names(clustering_view$demand)[-1L]
  spatial_weights <- .mick_spatial_feature_weights(spatial_weights)

  reduction <- .mick_reduce_to_target_clusters(
    clustering_view = clustering_view,
    n_space = n_space,
    spatial_weights = spatial_weights
  )
  membership <- .mick_membership_from_clusters(reduction$clusters, zone_order)
  reduced_original <- aggregate_spatial_view(original_view, membership)
  reduced_clustering <- aggregate_spatial_view(clustering_view, membership)

  list(
    membership = membership,
    merge_history = reduction$merge_history,
    reduced_wind = reduced_original$wind,
    reduced_solar = reduced_original$solar,
    reduced_demand = reduced_original$demand,
    reduced_grid = reduced_original$grid,
    original_view = reduced_original,
    clustering_view = reduced_clustering,
    weights = spatial_weights
  )
}

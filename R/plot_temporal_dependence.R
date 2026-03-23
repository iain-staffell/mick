.mick_dependence_series_names <- c("demand", "wind", "solar", "net_demand")

.mick_dependence_node_names <- function(result) {
  nodes <- setdiff(names(result$spatial$original_view$demand), "datetime")

  if (!length(nodes)) {
    stop("result$spatial$original_view must contain reduced nodes.", call. = FALSE)
  }

  nodes
}

.mick_dependence_add_total_column <- function(series_df) {
  node_names <- setdiff(names(series_df), "datetime")

  if (!length(node_names)) {
    stop("Series data must contain at least one reduced node.", call. = FALSE)
  }

  out <- series_df
  out$total <- rowSums(out[, node_names, drop = FALSE])
  out
}

.mick_dependence_prepare_source_view <- function(view) {
  out <- view

  for (series_name in intersect(.mick_dependence_series_names, names(view))) {
    out[[series_name]] <- .mick_dependence_add_total_column(out[[series_name]])
  }

  out
}

.mick_dependence_source_views <- function(result) {
  validate_plot_result(result)

  list(
    original = .mick_dependence_prepare_source_view(result$spatial$original_view),
    clustered = .mick_dependence_prepare_source_view(reconstruct_synthetic_year(result))
  )
}

.mick_dependence_feature_table <- function(source_view, node = NULL) {
  nodes <- if (is.null(node)) {
    .mick_dependence_node_names(list(spatial = list(original_view = source_view)))
  } else {
    node
  }

  feature_columns <- lapply(.mick_dependence_series_names, function(series_name) {
    series_df <- source_view[[series_name]]
    values <- as.data.frame(series_df[, nodes, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
    names(values) <- paste(nodes, series_name, sep = "::")
    values
  })

  do.call(cbind, feature_columns)
}

.mick_dependence_correlation_long <- function(feature_df, scope_label) {
  corr <- stats::cor(feature_df, use = "pairwise.complete.obs")
  feature_names <- colnames(corr)
  grid <- expand.grid(
    feature_x = feature_names,
    feature_y = feature_names,
    stringsAsFactors = FALSE
  )
  grid$correlation <- corr[cbind(match(grid$feature_x, feature_names), match(grid$feature_y, feature_names))]
  grid$node <- scope_label
  grid
}

prepare_dependence_correlation_data <- function(result, node = NULL) {
  validate_plot_result(result)

  if (!is.null(node)) {
    validate_duration_selector(result, node = node, var = NULL)
  }

  source_views <- .mick_dependence_source_views(result)
  scope_label <- if (is.null(node)) "all" else node
  original <- .mick_dependence_correlation_long(
    .mick_dependence_feature_table(source_views$original, node = node),
    scope_label = scope_label
  )
  clustered <- .mick_dependence_correlation_long(
    .mick_dependence_feature_table(source_views$clustered, node = node),
    scope_label = scope_label
  )

  delta <- merge(
    original,
    clustered,
    by = c("feature_x", "feature_y", "node"),
    suffixes = c("_original", "_clustered"),
    sort = FALSE
  )
  delta <- delta[, c("feature_x", "feature_y", "node")]
  delta$correlation <- clustered$correlation - original$correlation

  list(
    original = original,
    clustered = clustered,
    delta = delta
  )
}

.mick_dependence_adjacent_node_pairs <- function(result) {
  grid <- result$spatial$reduced_grid
  if (!is.matrix(grid) || !nrow(grid)) {
    return(list())
  }

  node_names <- rownames(grid)
  if (is.null(node_names)) {
    return(list())
  }

  pairs <- list()
  pair_index <- 1L
  for (i in seq_len(nrow(grid) - 1L)) {
    for (j in seq.int(i + 1L, ncol(grid))) {
      if (isTRUE(grid[i, j] > 0)) {
        pairs[[pair_index]] <- list(
          name = paste(node_names[[i]], node_names[[j]], "net_demand", sep = " | "),
          x_var = "net_demand",
          x_node = node_names[[i]],
          y_var = "net_demand",
          y_node = node_names[[j]]
        )
        pair_index <- pair_index + 1L
      }
    }
  }

  pairs
}

default_dependence_pairs <- function(result) {
  validate_plot_result(result)

  nodes <- .mick_dependence_node_names(result)
  pairs <- list(
    list(
      name = paste(nodes[[1L]], "demand vs wind"),
      x_var = "demand",
      x_node = nodes[[1L]],
      y_var = "wind",
      y_node = nodes[[1L]]
    ),
    list(
      name = paste("total net_demand vs", nodes[[1L]], "net_demand"),
      x_var = "net_demand",
      x_node = "total",
      y_var = "net_demand",
      y_node = nodes[[1L]]
    )
  )

  adjacency_pairs <- .mick_dependence_adjacent_node_pairs(result)
  if (length(adjacency_pairs)) {
    pairs <- c(pairs, adjacency_pairs[1L])
  }

  unique_pairs <- list()
  seen_names <- character()
  for (pair in pairs) {
    if (!pair$name %in% seen_names) {
      unique_pairs[[length(unique_pairs) + 1L]] <- pair
      seen_names <- c(seen_names, pair$name)
    }
  }

  unique_pairs
}

.mick_validate_dependence_pair <- function(result, pair, index) {
  if (!is.list(pair) || is.null(names(pair))) {
    stop("Each pair must be a named list.", call. = FALSE)
  }

  required_fields <- c("name", "x_var", "x_node", "y_var", "y_node")
  if (!all(required_fields %in% names(pair))) {
    stop("Each pair must include name, x_var, x_node, y_var, and y_node.", call. = FALSE)
  }

  valid_nodes <- c(.mick_dependence_node_names(result), "total")
  for (field in c("name", "x_var", "x_node", "y_var", "y_node")) {
    value <- pair[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop("pair `", index, "` field `", field, "` must be a non-empty string.", call. = FALSE)
    }
  }

  if (!pair$x_var %in% .mick_dependence_series_names) {
    stop("pair `", index, "` has unsupported x_var: ", pair$x_var, call. = FALSE)
  }
  if (!pair$y_var %in% .mick_dependence_series_names) {
    stop("pair `", index, "` has unsupported y_var: ", pair$y_var, call. = FALSE)
  }
  if (!pair$x_node %in% valid_nodes) {
    stop("pair `", index, "` has unsupported x_node: ", pair$x_node, call. = FALSE)
  }
  if (!pair$y_node %in% valid_nodes) {
    stop("pair `", index, "` has unsupported y_node: ", pair$y_node, call. = FALSE)
  }

  pair
}

.mick_validate_dependence_pairs <- function(result, pairs) {
  if (is.null(pairs)) {
    return(default_dependence_pairs(result))
  }
  if (!is.list(pairs) || !length(pairs)) {
    stop("pairs must be a non-empty list when supplied.", call. = FALSE)
  }

  validated <- vector("list", length(pairs))
  for (i in seq_along(pairs)) {
    validated[[i]] <- .mick_validate_dependence_pair(result, pairs[[i]], i)
  }
  validated
}

.mick_dependence_pair_data <- function(result, pair) {
  source_views <- .mick_dependence_source_views(result)
  representatives <- .mick_dependence_prepare_source_view(result$temporal$representatives)
  weights <- result$temporal$weights

  original_cloud <- data.frame(
    x = source_views$original[[pair$x_var]][[pair$x_node]],
    y = source_views$original[[pair$y_var]][[pair$y_node]],
    stringsAsFactors = FALSE
  )

  representative_points <- data.frame(
    temporal_cluster = representatives[[pair$x_var]]$temporal_cluster,
    x = representatives[[pair$x_var]][[pair$x_node]],
    y = representatives[[pair$y_var]][[pair$y_node]],
    stringsAsFactors = FALSE
  )
  representative_points <- merge(
    representative_points,
    weights[, c("temporal_cluster", "weight_hours"), drop = FALSE],
    by = "temporal_cluster",
    all.x = TRUE,
    sort = FALSE
  )

  list(
    original = original_cloud,
    representatives = representative_points
  )
}

.mick_dependence_plot_correlation <- function(data, title) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = feature_x, y = feature_y, fill = correlation)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "R",
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
}

.mick_dependence_plot_pair <- function(pair_data, pair_name) {
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = pair_data$original,
      ggplot2::aes(x = x, y = y),
      colour = "grey40",
      alpha = 0.15,
      size = 1
    ) +
    ggplot2::geom_point(
      data = pair_data$representatives,
      ggplot2::aes(x = x, y = y, size = weight_hours),
      colour = "#b2182b",
      alpha = 0.85
    ) +
    ggplot2::scale_size_continuous(name = "Hours") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = pair_name
    ) +
    ggplot2::theme_minimal(base_size = 10)
}

#' Plot temporal dependence preservation diagnostics
#'
#' Generates correlation heatmaps and pairwise scatter diagnostics that compare
#' original and clustered temporal dependence.
#'
#' @param result A result object returned by [run_mick()].
#' @param node Optional reduced-node selector for node-scoped diagnostics.
#' @param pairs Optional list of custom pair definitions.
#'
#' @return A named list containing correlation and pairwise ggplot objects.
#' @export
plot_temporal_dependence <- function(result, node = NULL, pairs = NULL) {
  correlation_data <- prepare_dependence_correlation_data(result, node = node)
  validated_pairs <- .mick_validate_dependence_pairs(result, pairs)

  pair_plots <- stats::setNames(
    lapply(validated_pairs, function(pair) {
      .mick_dependence_plot_pair(.mick_dependence_pair_data(result, pair), pair$name)
    }),
    vapply(validated_pairs, `[[`, character(1), "name")
  )

  list(
    correlation = list(
      original = .mick_dependence_plot_correlation(correlation_data$original, "Original correlation"),
      clustered = .mick_dependence_plot_correlation(correlation_data$clustered, "Clustered correlation"),
      delta = .mick_dependence_plot_correlation(correlation_data$delta, "Delta correlation")
    ),
    pairs = pair_plots
  )
}

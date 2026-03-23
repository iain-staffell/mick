.mick_duration_series_names <- c("demand", "wind", "solar", "net_demand")
.mick_duration_yearlike_timesteps <- c(8760L, 8785L)

.mick_duration_use_year_ticks <- function(n_timesteps) {
  if (!is.numeric(n_timesteps) || length(n_timesteps) != 1L || is.na(n_timesteps)) {
    return(FALSE)
  }

  as.integer(n_timesteps) %in% .mick_duration_yearlike_timesteps
}

.mick_duration_year_breaks <- function(n_timesteps) {
  seq.int(730L, as.integer(n_timesteps), by = 730L)
}

.mick_duration_year_labels <- function(x) {
  ifelse(x %% 2190L == 0L, as.character(as.integer(x)), "")
}

.mick_duration_node_names <- function(result) {
  unique_names <- setdiff(names(result$spatial$original_view$demand), "datetime")

  if (!length(unique_names)) {
    stop("result$spatial$original_view must contain reduced nodes.", call. = FALSE)
  }

  unique_names
}

.mick_duration_add_total_column <- function(series_df) {
  node_names <- setdiff(names(series_df), "datetime")

  if (!length(node_names)) {
    stop("Series data must contain at least one reduced node.", call. = FALSE)
  }

  out <- series_df
  out$total <- rowSums(out[, node_names, drop = FALSE])
  out
}

.mick_duration_selector_info <- function(result, node, var, node_missing, var_missing) {
  validate_plot_result(result)

  valid_nodes <- .mick_duration_node_names(result)
  var_active <- !is.null(var)
  effective_node <- if (node_missing) {
    if (var_active) NULL else "total"
  } else {
    node
  }

  validate_duration_selector(result, node = effective_node, var = var)

  if (var_active) {
    return(list(
      mode = "var",
      selector = var,
      facets = valid_nodes
    ))
  }

  list(
    mode = "node",
    selector = effective_node,
    facets = .mick_duration_series_names
  )
}

.mick_duration_duration_curve_rows <- function(values, source, facet) {
  ordered_index <- order(-values, seq_along(values))
  sorted_values <- values[ordered_index]

  data.frame(
    duration_rank = seq_along(sorted_values),
    value = sorted_values,
    source = source,
    facet = facet,
    stringsAsFactors = FALSE
  )
}

.mick_duration_prepare_source_view <- function(view) {
  out <- view

  for (series_name in intersect(.mick_duration_series_names, names(view))) {
    out[[series_name]] <- .mick_duration_add_total_column(out[[series_name]])
  }

  out
}

.mick_duration_build_data <- function(result, selector) {
  source_views <- list(
    original = .mick_duration_prepare_source_view(result$spatial$original_view),
    reconstructed = .mick_duration_prepare_source_view(reconstruct_synthetic_year(result))
  )

  rows <- list()
  row_index <- 1L

  for (source_name in names(source_views)) {
    source_view <- source_views[[source_name]]

    for (facet_value in selector$facets) {
      series_name <- if (identical(selector$mode, "node")) facet_value else selector$selector
      series_df <- source_view[[series_name]]
      if (!is.data.frame(series_df)) {
        stop("Series data must be a data frame.", call. = FALSE)
      }

      values <- series_df[[if (identical(selector$mode, "node")) selector$selector else facet_value]]
      rows[[row_index]] <- .mick_duration_duration_curve_rows(values, source_name, facet_value)
      row_index <- row_index + 1L
    }
  }

  data <- do.call(rbind, rows)
  data$source <- factor(
    data$source,
    levels = c("original", "reconstructed"),
    labels = c("Original", "Reconstructed")
  )
  data$facet <- factor(data$facet, levels = selector$facets)
  data
}

.mick_prepare_duration_curve_data_impl <- function(result, node, var, node_missing, var_missing) {
  selector <- .mick_duration_selector_info(
    result = result,
    node = node,
    var = var,
    node_missing = node_missing,
    var_missing = var_missing
  )

  data <- .mick_duration_build_data(result, selector)

  list(
    data = data,
    mode = selector$mode,
    selector = selector$selector,
    facets = selector$facets
  )
}

prepare_duration_curve_data <- function(result, node = "total", var = NULL) {
  .mick_prepare_duration_curve_data_impl(
    result = result,
    node = node,
    var = var,
    node_missing = missing(node),
    var_missing = missing(var)
  )$data
}

#' Plot temporal duration curves
#'
#' Compares original and reconstructed duration curves. Use `node` mode to facet
#' over variables, or `var` mode to facet over reduced nodes.
#'
#' @param result A result object returned by [run_mick()].
#' @param node Node selector (default `"total"`). Set `NULL` when using `var`.
#' @param var Variable selector (`"demand"`, `"wind"`, `"solar"`,
#'   `"net_demand"`). Set `NULL` when using `node`.
#'
#' @return A ggplot object.
#' @export
plot_temporal_duration_curves <- function(result, node = "total", var = NULL) {
  prepared <- .mick_prepare_duration_curve_data_impl(
    result = result,
    node = node,
    var = var,
    node_missing = missing(node),
    var_missing = missing(var)
  )

  plot <- ggplot2::ggplot(
    prepared$data,
    ggplot2::aes(
      x = duration_rank,
      y = value,
      colour = source,
      linetype = source,
      group = source
    )
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::facet_wrap(~ facet, scales = "free_y") +
    ggplot2::labs(
      x = "Duration rank",
      y = "MW",
      colour = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal()

  n_timesteps <- max(prepared$data$duration_rank, na.rm = TRUE)
  if (.mick_duration_use_year_ticks(n_timesteps)) {
    plot <- plot +
      ggplot2::scale_x_continuous(
        breaks = .mick_duration_year_breaks(n_timesteps),
        labels = .mick_duration_year_labels,
        minor_breaks = NULL
      )
  }

  plot
}

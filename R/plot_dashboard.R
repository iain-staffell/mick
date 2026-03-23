.mick_dashboard_placeholder_plot <- function(title, message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 4, hjust = 0.5) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void()
}

#' Build a bundled clustering dashboard
#'
#' Convenience wrapper that collects spatial, duration, dependence, and
#' chronology plots into one nested list.
#'
#' @param result A result object returned by [run_mick()].
#' @param map Optional path to a spatial file readable by `sf::st_read()`.
#' @param column Optional map column containing original zone identifiers.
#'
#' @return A named list of plot families.
#' @export
plot_clustering_dashboard <- function(result, map = NULL, column = NULL) {
  validate_plot_result(result)
  spatial_plots <- tryCatch(
    plot_spatial_clusters(result, map = map, column = column),
    error = function(e) {
      message_text <- conditionMessage(e)
      if (!grepl("igraph", message_text, ignore.case = TRUE)) {
        stop(e)
      }

      list(
        original = .mick_dashboard_placeholder_plot(
          "Spatial Clusters",
          "Install igraph to enable the network spatial plots."
        ),
        clustered = .mick_dashboard_placeholder_plot(
          "Spatial Clusters",
          "Install igraph to enable the network spatial plots."
        )
      )
    }
  )

  list(
    spatial = spatial_plots,
    duration = plot_temporal_duration_curves(result),
    dependence = plot_temporal_dependence(result),
    chronology = plot_temporal_chronology(result)
  )
}

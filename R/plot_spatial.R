.mick_plot_spatial_fallback_message <- function(details) {
  paste0(details, " Call plot_spatial_clusters(result) without map to use the network fallback.")
}

.mick_plot_spatial_validate_map_column <- function(map_sf, column) {
  if (!is.character(column) || length(column) != 1L || is.na(column) || !nzchar(column)) {
    stop(
      .mick_plot_spatial_fallback_message("column must be supplied when map is provided."),
      call. = FALSE
    )
  }

  if (!column %in% names(map_sf)) {
    stop(
      .mick_plot_spatial_fallback_message(paste0("Map column `", column, "` was not found.")),
      call. = FALSE
    )
  }
}

.mick_plot_spatial_validate_map_zones <- function(map_values, zones) {
  map_values <- as.character(map_values)
  zones <- as.character(zones)

  if (length(map_values) != length(zones) ||
      anyDuplicated(map_values) ||
      !setequal(map_values, zones)) {
    stop(
      .mick_plot_spatial_fallback_message(
        "Map zone values must match the original zone names exactly."
      ),
      call. = FALSE
    )
  }
}

.mick_plot_spatial_read_map <- function(map) {
  tryCatch(
    sf::st_read(map, quiet = TRUE),
    error = function(e) {
      stop(
        .mick_plot_spatial_fallback_message(paste0("Unable to read map `", map, "`.")),
        call. = FALSE
      )
    }
  )
}

.mick_plot_spatial_network_data <- function(result) {
  require_plot_package(
    "igraph",
    "igraph is required for network mode."
  )

  membership <- result$spatial$membership
  zones <- membership$zone
  grid <- as.matrix(result$inputs$original$grid)

  if (is.null(rownames(grid)) || is.null(colnames(grid))) {
    stop("result$inputs$original$grid must have row and column names.", call. = FALSE)
  }

  if (!all(zones %in% rownames(grid)) || !all(zones %in% colnames(grid))) {
    stop("result$inputs$original$grid must contain the original zone names.", call. = FALSE)
  }

  grid <- grid[zones, zones, drop = FALSE]
  graph <- igraph::graph_from_adjacency_matrix(
    grid,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )

  layout <- igraph::layout_in_circle(graph)
  node_df <- data.frame(
    zone = igraph::V(graph)$name,
    x = layout[, 1L],
    y = layout[, 2L],
    cluster = membership$cluster[match(igraph::V(graph)$name, membership$zone)],
    stringsAsFactors = FALSE
  )

  edges <- igraph::as_data_frame(graph, what = "edges")
  if (nrow(edges) > 0L) {
    edges$x <- node_df$x[match(edges$from, node_df$zone)]
    edges$y <- node_df$y[match(edges$from, node_df$zone)]
    edges$xend <- node_df$x[match(edges$to, node_df$zone)]
    edges$yend <- node_df$y[match(edges$to, node_df$zone)]
  }

  list(nodes = node_df, edges = edges)
}

.mick_plot_spatial_network_plot <- function(node_df, edge_df, label_column, point_mapping = NULL, point_colour = "grey35") {
  p <- ggplot2::ggplot()

  if (nrow(edge_df) > 0L) {
    p <- p + ggplot2::geom_segment(
      data = edge_df,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE,
      colour = "grey80",
      linewidth = 0.4
    )
  }

  if (is.null(point_mapping)) {
    p <- p +
      ggplot2::geom_point(
        data = node_df,
        ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE,
        colour = point_colour,
        size = 5
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        data = node_df,
        ggplot2::aes(x = x, y = y, colour = .data[[point_mapping]]),
        inherit.aes = FALSE,
        size = 5
      )
  }

  p <- p +
    ggplot2::geom_text(
      data = node_df,
      ggplot2::aes(x = x, y = y, label = .data[[label_column]]),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )

  p
}

.mick_plot_spatial_map_data <- function(result, map, column) {
  require_plot_package(
    "sf",
    "sf is required for map mode; call plot_spatial_clusters(result) without map to use the network fallback."
  )

  map_sf <- .mick_plot_spatial_read_map(map)
  .mick_plot_spatial_validate_map_column(map_sf, column)

  membership <- result$spatial$membership
  zones <- membership$zone
  map_values <- map_sf[[column]]
  .mick_plot_spatial_validate_map_zones(map_values, zones)

  map_sf$zone <- as.character(map_values)
  map_sf$cluster <- membership$cluster[match(as.character(map_values), membership$zone)]
  centroids <- sf::st_centroid(sf::st_geometry(map_sf))
  centroid_xy <- sf::st_coordinates(centroids)
  label_df <- data.frame(
    x = centroid_xy[, 1L],
    y = centroid_xy[, 2L],
    zone = map_sf$zone,
    cluster = map_sf$cluster,
    stringsAsFactors = FALSE
  )

  list(map = map_sf, labels = label_df)
}

.mick_plot_spatial_map_plot <- function(map_sf, label_df, label_column, fill_column) {
  p <- ggplot2::ggplot()

  p <- p +
    ggplot2::geom_sf(
      data = map_sf,
      ggplot2::aes(fill = .data[[fill_column]]),
      colour = "grey60",
      linewidth = 0.3
    )

  p <- p +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = x, y = y, label = .data[[label_column]]),
      inherit.aes = FALSE,
      size = 3
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal()

  p
}

#' Plot original and clustered spatial topology
#'
#' Produces a pair of spatial plots (`original`, `clustered`) from a MICK
#' result. Without a map, it renders an igraph-based network fallback.
#'
#' @param result A result object returned by [run_mick()].
#' @param map Optional path to a spatial file readable by `sf::st_read()`.
#' @param column Optional map column containing original zone identifiers.
#'
#' @return A named list with `original` and `clustered` ggplot objects.
#' @export
plot_spatial_clusters <- function(result, map = NULL, column = NULL) {
  validate_plot_result(result)

  if (is.null(map)) {
    network <- .mick_plot_spatial_network_data(result)
    return(list(
      original = .mick_plot_spatial_network_plot(
        node_df = network$nodes,
        edge_df = network$edges,
        label_column = "zone",
        point_mapping = "zone",
        point_colour = "grey35"
      ),
      clustered = .mick_plot_spatial_network_plot(
        node_df = network$nodes,
        edge_df = network$edges,
        label_column = "cluster",
        point_mapping = "cluster"
      )
    ))
  }

  map_data <- .mick_plot_spatial_map_data(result, map = map, column = column)
  labels <- map_data$labels

  list(
    original = .mick_plot_spatial_map_plot(
      map_sf = map_data$map,
      label_df = labels,
      label_column = "zone",
      fill_column = "zone"
    ),
    clustered = .mick_plot_spatial_map_plot(
      map_sf = map_data$map,
      label_df = labels,
      label_column = "cluster",
      fill_column = "cluster"
    )
  )
}

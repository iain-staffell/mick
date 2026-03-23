.mick_chronology_reserved_clusters <- function(result) {
  reserved <- result$temporal$reserved

  if (!is.data.frame(reserved) || !"temporal_cluster" %in% names(reserved) || !nrow(reserved)) {
    return(integer())
  }

  unique(stats::na.omit(reserved$temporal_cluster))
}

.mick_chronology_reserved_pch <- c(3, 4, 8, 1, 0, 5, 2, 6, 7, 9:19)

.mick_chronology_reserved_shape_values <- function(cluster_levels, reserved_clusters) {
  cluster_levels_chr <- as.character(cluster_levels)
  values <- rep(NA_real_, length(cluster_levels_chr))
  names(values) <- cluster_levels_chr

  if (!length(reserved_clusters)) {
    return(values)
  }

  reserved_chr <- as.character(reserved_clusters)
  match_index <- match(reserved_chr, cluster_levels_chr)
  valid_index <- match_index[!is.na(match_index)]

  if (!length(valid_index)) {
    return(values)
  }

  values[valid_index] <- rep(.mick_chronology_reserved_pch, length.out = length(valid_index))
  values
}

.mick_chronology_time_components <- function(datetime) {
  if (!inherits(datetime, "POSIXt")) {
    datetime <- as.POSIXct(datetime, tz = "UTC")
  }

  date <- as.Date(datetime, tz = "UTC")
  list(
    datetime = datetime,
    date = date,
    day_index = match(date, unique(date)),
    month = as.integer(format(datetime, "%m", tz = "UTC")),
    hour_of_day = as.integer(format(datetime, "%H", tz = "UTC"))
  )
}

.mick_chronology_cluster_levels <- function(result) {
  cluster_levels <- sort(unique(result$temporal$mapping$temporal_cluster))

  if (!length(cluster_levels)) {
    stop("result$temporal$mapping must contain temporal clusters.", call. = FALSE)
  }

  cluster_levels
}

.mick_chronology_validate_result <- function(result) {
  if (!is.list(result) || is.null(names(result)) || !all(c("spatial", "temporal", "inputs") %in% names(result))) {
    stop("result must contain spatial, temporal, and inputs components.", call. = FALSE)
  }

  if (!is.list(result$temporal) || !is.data.frame(result$temporal$mapping)) {
    stop("result$temporal$mapping must be a data frame.", call. = FALSE)
  }

  if (!all(c("hour_index", "datetime", "temporal_cluster") %in% names(result$temporal$mapping))) {
    stop("result$temporal$mapping must include hour_index, datetime, and temporal_cluster.", call. = FALSE)
  }
}

prepare_calendar_membership_data <- function(result) {
  .mick_chronology_validate_result(result)

  mapping <- result$temporal$mapping
  reserved_clusters <- .mick_chronology_reserved_clusters(result)
  time_components <- .mick_chronology_time_components(mapping$datetime)
  reserved_mask <- if ("slice_type" %in% names(mapping)) {
    mapping$slice_type == "reserved"
  } else {
    mapping$temporal_cluster %in% reserved_clusters
  }

  data.frame(
    hour_index = mapping$hour_index,
    datetime = time_components$datetime,
    date = time_components$date,
    day_index = time_components$day_index,
    month = time_components$month,
    hour_of_day = time_components$hour_of_day,
    temporal_cluster = mapping$temporal_cluster,
    is_reserved = reserved_mask,
    stringsAsFactors = FALSE
  )
}

.mick_prepare_month_occupancy_data <- function(result) {
  calendar_data <- prepare_calendar_membership_data(result)
  cluster_levels <- .mick_chronology_cluster_levels(result)

  counts <- stats::aggregate(
    list(n_hours = rep(1L, nrow(calendar_data))),
    by = list(
      month = calendar_data$month,
      temporal_cluster = calendar_data$temporal_cluster
    ),
    FUN = length
  )

  all_combinations <- expand.grid(
    month = 1:12,
    temporal_cluster = cluster_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  data <- merge(all_combinations, counts, by = c("month", "temporal_cluster"), all.x = TRUE, sort = FALSE)
  data$n_hours[is.na(data$n_hours)] <- 0L
  data$month <- as.integer(data$month)
  data$temporal_cluster <- factor(data$temporal_cluster, levels = cluster_levels)
  data[order(data$month, data$temporal_cluster), , drop = FALSE]
}

prepare_month_occupancy_data <- function(result) {
  .mick_prepare_month_occupancy_data(result)
}

.mick_prepare_hour_occupancy_data <- function(result) {
  calendar_data <- prepare_calendar_membership_data(result)
  cluster_levels <- .mick_chronology_cluster_levels(result)

  counts <- stats::aggregate(
    list(n_hours = rep(1L, nrow(calendar_data))),
    by = list(
      hour_of_day = calendar_data$hour_of_day,
      temporal_cluster = calendar_data$temporal_cluster
    ),
    FUN = length
  )

  all_combinations <- expand.grid(
    hour_of_day = 0:23,
    temporal_cluster = cluster_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  data <- merge(all_combinations, counts, by = c("hour_of_day", "temporal_cluster"), all.x = TRUE, sort = FALSE)
  data$n_hours[is.na(data$n_hours)] <- 0L
  data$hour_of_day <- as.integer(data$hour_of_day)
  data$temporal_cluster <- factor(data$temporal_cluster, levels = cluster_levels)
  data[order(data$hour_of_day, data$temporal_cluster), , drop = FALSE]
}

prepare_hour_occupancy_data <- function(result) {
  .mick_prepare_hour_occupancy_data(result)
}

#' Plot temporal chronology and occupancy diagnostics
#'
#' Produces calendar and occupancy plots to inspect temporal cluster assignment
#' over time.
#'
#' @param result A result object returned by [run_mick()].
#'
#' @return A named list with `calendar`, `by_month`, and `by_hour` ggplot
#'   objects.
#' @export
plot_temporal_chronology <- function(result) {
  .mick_chronology_validate_result(result)

  cluster_levels <- .mick_chronology_cluster_levels(result)
  cluster_levels_chr <- as.character(cluster_levels)
  reserved_clusters <- .mick_chronology_reserved_clusters(result)
  shape_values <- .mick_chronology_reserved_shape_values(cluster_levels, reserved_clusters)
  calendar_data <- prepare_calendar_membership_data(result)
  month_data <- .mick_prepare_month_occupancy_data(result)
  hour_data <- .mick_prepare_hour_occupancy_data(result)
  date_limits <- range(calendar_data$date, na.rm = TRUE)
  calendar_data$cluster_label <- factor(
    as.character(calendar_data$temporal_cluster),
    levels = cluster_levels_chr
  )
  reserved_data <- calendar_data[calendar_data$is_reserved, , drop = FALSE]

  calendar_plot <- ggplot2::ggplot(
    calendar_data,
    ggplot2::aes(x = hour_of_day, y = date, fill = cluster_label)
  ) +
    ggplot2::geom_tile(colour = NA, linewidth = 0, width = 1, height = 0.98, na.rm = TRUE) +
    ggplot2::geom_point(
      data = reserved_data,
      ggplot2::aes(x = hour_of_day, y = date, shape = cluster_label, fill = cluster_label),
      inherit.aes = FALSE,
      colour = "black",
      size = 2.4,
      stroke = 0.65,
      show.legend = TRUE
    ) +
    ggplot2::scale_fill_discrete(name = "Timeslice", limits = cluster_levels_chr, drop = FALSE) +
    ggplot2::scale_shape_manual(
      values = shape_values,
      name = "Timeslice",
      limits = cluster_levels_chr,
      drop = FALSE,
      na.translate = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3), expand = c(0, 0)) +
    ggplot2::scale_y_date(
      limits = date_limits,
      date_breaks = "1 month",
      date_labels = "%b",
      expand = c(0, 0)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 1, override.aes = list(colour = "black", size = 2.8))
    ) +
    ggplot2::labs(
      x = "Hour of day",
      y = "Date"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  month_plot <- ggplot2::ggplot(
    month_data,
    ggplot2::aes(x = month, y = temporal_cluster, fill = n_hours)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.15) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0, 0)) +
    ggplot2::scale_fill_gradient(low = "grey95", high = "#2b6cb0", name = "Hours") +
    ggplot2::labs(
      x = "Month",
      y = "Timeslice"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  hour_plot <- ggplot2::ggplot(
    hour_data,
    ggplot2::aes(x = hour_of_day, y = temporal_cluster, fill = n_hours)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.15) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3), expand = c(0, 0)) +
    ggplot2::scale_fill_gradient(low = "grey95", high = "#2b6cb0", name = "Hours") +
    ggplot2::labs(
      x = "Hour of day",
      y = "Timeslice"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  list(
    calendar = calendar_plot,
    by_month = month_plot,
    by_hour = hour_plot
  )
}

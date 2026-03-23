require_plot_package <- function(pkg, message_text) {
  if (!is.character(pkg) || length(pkg) != 1L || is.na(pkg) || !nzchar(pkg)) {
    stop("pkg must be a non-empty string.", call. = FALSE)
  }
  if (!is.character(message_text) || length(message_text) != 1L || is.na(message_text) || !nzchar(message_text)) {
    stop("message_text must be a non-empty string.", call. = FALSE)
  }

  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(message_text, call. = FALSE)
  }

  invisible(TRUE)
}

validate_plot_result <- function(result) {
  required_components <- c("spatial", "temporal", "inputs")
  required_series <- c("demand", "wind", "solar", "net_demand")

  if (!is.list(result) || is.null(names(result)) || !all(required_components %in% names(result))) {
    stop("result must contain spatial, temporal, and inputs components.", call. = FALSE)
  }

  if (!is.list(result$spatial) || !is.list(result$temporal) || !is.list(result$inputs)) {
    stop("result must contain spatial, temporal, and inputs components.", call. = FALSE)
  }

  if (!is.data.frame(result$spatial$membership) ||
      !all(c("zone", "cluster") %in% names(result$spatial$membership))) {
    stop("result must contain spatial, temporal, and inputs components.", call. = FALSE)
  }

  if (!is.data.frame(result$temporal$mapping) ||
      !all(c("hour_index", "datetime", "temporal_cluster") %in% names(result$temporal$mapping))) {
    stop("result must contain spatial, temporal, and inputs components.", call. = FALSE)
  }

  if (!is.list(result$temporal$representatives) || is.null(names(result$temporal$representatives))) {
    stop("result must contain spatial, temporal, and inputs components.", call. = FALSE)
  }

  if (!is.list(result$inputs$original) || !is.list(result$inputs$clustering)) {
    stop("result must contain spatial, temporal, and inputs components.", call. = FALSE)
  }

  if (!all(required_series %in% names(result$temporal$representatives))) {
    stop(
      "result$temporal$representatives must contain demand, wind, solar, and net_demand.",
      call. = FALSE
    )
  }

  representative_ok <- vapply(required_series, function(series_name) {
    representative <- result$temporal$representatives[[series_name]]
    is.data.frame(representative) &&
      "temporal_cluster" %in% names(representative) &&
      ncol(representative) >= 2L
  }, logical(1))
  if (!all(representative_ok)) {
    stop(
      "result$temporal$representatives entries must be data frames with temporal_cluster and node columns.",
      call. = FALSE
    )
  }

  input_original_required <- c("demand", "wind", "solar", "grid")
  if (!all(input_original_required %in% names(result$inputs$original))) {
    stop("result$inputs$original must contain demand, wind, solar, and grid.", call. = FALSE)
  }

  if (!all(c(input_original_required, "net_demand") %in% names(result$inputs$clustering))) {
    stop("result$inputs$clustering must contain demand, wind, solar, grid, and net_demand.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_duration_selector <- function(result, node = "total", var = NULL) {
  validate_plot_result(result)

  node_missing <- missing(node)
  node_active <- !is.null(node)
  var_active <- !is.null(var)

  if (var_active && node_missing) {
    node <- NULL
    node_active <- FALSE
  }

  if (node_active && var_active) {
    stop("Specify either `node` or `var`, not both.", call. = FALSE)
  }

  if (!node_active && !var_active) {
    stop("Specify either `node` or `var`.", call. = FALSE)
  }

  if (node_active && (!is.character(node) || length(node) != 1L || is.na(node) || !nzchar(node))) {
    stop("node must be a non-empty string or NULL.", call. = FALSE)
  }

  if (var_active && (!is.character(var) || length(var) != 1L || is.na(var) || !nzchar(var))) {
    stop("var must be a non-empty string or NULL.", call. = FALSE)
  }

  if (node_active && !identical(node, "total")) {
    valid_nodes <- unique(result$spatial$membership$cluster)
    if (!node %in% valid_nodes) {
      stop("Unknown node: ", node, call. = FALSE)
    }
  }

  if (var_active) {
    valid_vars <- c("demand", "wind", "solar", "net_demand")
    if (!var %in% valid_vars) {
      stop("Unknown var: ", var, call. = FALSE)
    }
  }

  invisible(TRUE)
}

reconstruct_synthetic_year <- function(result) {
  validate_plot_result(result)

  if (!is.data.frame(result$temporal$mapping)) {
    stop("result$temporal$mapping must be a data frame.", call. = FALSE)
  }
  if (!is.list(result$temporal$representatives)) {
    stop("result$temporal$representatives must be a list.", call. = FALSE)
  }
  if (!"temporal_cluster" %in% names(result$temporal$mapping)) {
    stop("result$temporal$mapping must include temporal_cluster.", call. = FALSE)
  }

  synthetic <- lapply(names(result$temporal$representatives), function(name) {
    representative <- result$temporal$representatives[[name]]
    if (!is.data.frame(representative)) {
      stop("result$temporal$representatives must contain data frames.", call. = FALSE)
    }
    if (!"temporal_cluster" %in% names(representative)) {
      stop("result$temporal$representatives entries must include temporal_cluster.", call. = FALSE)
    }

    value_columns <- setdiff(names(representative), "temporal_cluster")
    cluster_index <- match(result$temporal$mapping$temporal_cluster, representative$temporal_cluster)
    if (anyNA(cluster_index)) {
      stop("result$temporal$mapping refers to an unknown temporal_cluster.", call. = FALSE)
    }

    out <- representative[cluster_index, value_columns, drop = FALSE]
    rownames(out) <- NULL
    out <- cbind(
      datetime = result$temporal$mapping$datetime,
      out,
      stringsAsFactors = FALSE
    )
    out
  })

  stats::setNames(synthetic, names(result$temporal$representatives))
}

.mick_is_plot_result <- function(x) {
  is.list(x) &&
    !is.null(names(x)) &&
    all(c("spatial", "temporal", "inputs") %in% names(x))
}

#' Flatten dashboard plots into a single named list
#'
#' Traverses nested dashboard structures and returns only ggplot leaves with
#' stable dot-delimited names.
#'
#' @param x A dashboard list, ggplot object, or MICK result object.
#' @param prefix Optional name prefix used during recursive flattening.
#' @param map Optional map path forwarded when `x` is a result object.
#' @param column Optional map key column forwarded when `x` is a result object.
#'
#' @return A named list of ggplot objects.
#' @export
flatten_dashboard_plots <- function(x, prefix = NULL, map = NULL, column = NULL) {
  if (is.null(prefix) && .mick_is_plot_result(x)) {
    x <- plot_clustering_dashboard(x, map = map, column = column)
  }

  if (inherits(x, "ggplot")) {
    name <- if (is.null(prefix) || !nzchar(prefix)) "plot" else prefix
    return(stats::setNames(list(x), name))
  }

  if (!is.list(x) || length(x) == 0L) {
    return(list())
  }

  child_names <- names(x)
  if (is.null(child_names)) {
    child_names <- rep("", length(x))
  }

  out <- list()
  for (i in seq_along(x)) {
    child_name <- child_names[[i]]
    next_prefix <- if (is.null(prefix)) {
      if (nzchar(child_name)) child_name else as.character(i)
    } else {
      if (nzchar(child_name)) paste0(prefix, ".", child_name) else paste0(prefix, ".", i)
    }

    flattened <- flatten_dashboard_plots(x[[i]], next_prefix)
    if (length(flattened)) {
      out <- c(out, flattened)
    }
  }

  out
}

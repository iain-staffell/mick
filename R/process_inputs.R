.mick_input_file_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!nzchar(ext)) {
    stop("Input file has no extension: ", path, call. = FALSE)
  }
  ext
}

.mick_read_input_raw <- function(path, kind = c("time-series", "grid")) {
  kind <- match.arg(kind)
  format <- .mick_input_file_format(path)

  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }

  if (format == "csv") {
    if (kind == "grid") {
      return(utils::read.csv(path, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
    }
    return(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE))
  }

  if (format == "json") {
    return(jsonlite::fromJSON(path, simplifyVector = TRUE))
  }

  if (format %in% c("yaml", "yml")) {
    return(yaml::read_yaml(path))
  }

  stop("Unsupported input file format: ", format, call. = FALSE)
}

.mick_as_time_series_frame <- function(raw, path) {
  if (is.data.frame(raw)) {
    return(as.data.frame(raw, check.names = FALSE, stringsAsFactors = FALSE))
  }

  if (is.matrix(raw)) {
    return(as.data.frame(raw, check.names = FALSE, stringsAsFactors = FALSE))
  }

  if (is.list(raw) && length(raw) > 0L && all(vapply(raw, is.list, logical(1)))) {
    rows <- lapply(raw, function(row) {
      as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }

  if (is.list(raw)) {
    return(as.data.frame(raw, check.names = FALSE, stringsAsFactors = FALSE))
  }

  stop("Could not parse time-series input: ", path, call. = FALSE)
}

.mick_as_grid_matrix <- function(raw, path) {
  if (is.matrix(raw)) {
    return(raw)
  }

  if (is.data.frame(raw)) {
    if (all(vapply(raw, is.numeric, logical(1)))) {
      return(as.matrix(raw))
    }

    if (ncol(raw) >= 2L) {
      row_names <- as.character(raw[[1]])
      grid_df <- raw[-1]
      if (!all(vapply(grid_df, is.numeric, logical(1)))) {
        stop("Grid input must be numeric.", call. = FALSE)
      }
      grid_mat <- as.matrix(grid_df)
      rownames(grid_mat) <- row_names
      return(grid_mat)
    }
  }

  if (is.list(raw) && length(raw) > 0L && !is.null(names(raw)) && all(vapply(raw, is.list, logical(1)))) {
    row_names <- names(raw)
    grid_cols <- NULL
    rows <- lapply(seq_along(raw), function(i) {
      row_vec <- unlist(raw[[i]], use.names = TRUE)
      row_names_i <- names(row_vec)

      if (is.null(row_names_i) || anyNA(row_names_i) || any(!nzchar(row_names_i))) {
        stop("Grid rows must use non-empty zone names.", call. = FALSE)
      }
      if (anyDuplicated(row_names_i)) {
        stop("Grid rows must not repeat zone names.", call. = FALSE)
      }

      if (is.null(grid_cols)) {
        grid_cols <<- row_names_i
      } else if (!setequal(row_names_i, grid_cols)) {
        stop("Grid rows must have matching zone names.", call. = FALSE)
      }

      row_vec <- row_vec[grid_cols]
      if (!is.numeric(row_vec)) {
        stop("Grid input must be numeric.", call. = FALSE)
      }

      as.numeric(row_vec)
    })
    grid_mat <- do.call(rbind, rows)
    colnames(grid_mat) <- grid_cols
    rownames(grid_mat) <- row_names
    return(grid_mat)
  }

  stop("Could not parse grid input: ", path, call. = FALSE)
}

.mick_parse_datetime_column <- function(x, series_name) {
  parsed <- as.POSIXct(x, tz = "UTC")
  if (anyNA(parsed)) {
    stop(series_name, " contains invalid datetime values.", call. = FALSE)
  }
  parsed
}

.mick_validate_time_series_frame <- function(df, series_name) {
  if (!is.data.frame(df)) {
    stop(series_name, " must be a data frame.", call. = FALSE)
  }
  if (ncol(df) < 2L) {
    stop(series_name, " must contain datetime plus at least one zone column.", call. = FALSE)
  }
  if (!identical(names(df)[1L], "datetime")) {
    stop(series_name, " must contain datetime in the first column.", call. = FALSE)
  }

  zone_names <- names(df)[-1L]
  if (anyNA(zone_names) || any(!nzchar(zone_names))) {
    stop(series_name, " must use non-empty zone column names.", call. = FALSE)
  }
  if (anyDuplicated(zone_names)) {
    stop(series_name, " must not repeat zone column names.", call. = FALSE)
  }

  df$datetime <- .mick_parse_datetime_column(df$datetime, series_name)

  non_numeric <- !vapply(df[-1L], is.numeric, logical(1))
  if (any(non_numeric)) {
    stop(series_name, " data columns must be numeric.", call. = FALSE)
  }

  missing_values <- vapply(df[-1L], function(col) anyNA(col), logical(1))
  if (any(missing_values) || anyNA(df$datetime)) {
    stop(series_name, " must not contain missing values.", call. = FALSE)
  }

  df
}

.mick_validate_grid_matrix <- function(grid, zone_names) {
  if (!is.matrix(grid)) {
    stop("Grid input must be a matrix.", call. = FALSE)
  }
  if (!is.numeric(grid)) {
    stop("Grid input must be numeric.", call. = FALSE)
  }
  if (anyNA(grid)) {
    stop("Grid input must not contain missing values.", call. = FALSE)
  }
  if (any(grid < 0)) {
    stop("Grid input must be non-negative.", call. = FALSE)
  }
  if (nrow(grid) != ncol(grid)) {
    stop("Grid input must be square and symmetric.", call. = FALSE)
  }
  if (is.null(rownames(grid)) || is.null(colnames(grid))) {
    stop("Grid input must include zone names on rows and columns.", call. = FALSE)
  }
  if (!setequal(rownames(grid), colnames(grid))) {
    stop("Grid input must be square and symmetric.", call. = FALSE)
  }
  if (!setequal(rownames(grid), zone_names)) {
    stop("Grid zones must match the time-series zone columns.", call. = FALSE)
  }
  if (!isTRUE(all.equal(grid, t(grid)))) {
    stop("Grid input must be square and symmetric.", call. = FALSE)
  }

  ordered <- zone_names
  grid <- grid[ordered, ordered, drop = FALSE]

  grid
}

.mick_align_time_series_inputs <- function(wind, solar, demand) {
  zone_names <- names(wind)[-1L]

  if (!identical(names(solar), names(wind)) || !identical(names(demand), names(wind))) {
    stop("Time-series inputs must share identical datetimes and matching zone columns.", call. = FALSE)
  }

  if (!identical(wind$datetime, solar$datetime) || !identical(wind$datetime, demand$datetime)) {
    stop("Time-series inputs must share identical datetimes and matching zone columns.", call. = FALSE)
  }

  zone_names
}

.mick_parse_time_series_input <- function(path, series_name) {
  raw <- .mick_read_input_raw(path, kind = "time-series")
  .mick_validate_time_series_frame(.mick_as_time_series_frame(raw, path), series_name)
}

.mick_parse_grid_input <- function(path) {
  raw <- .mick_read_input_raw(path, kind = "grid")
  .mick_as_grid_matrix(raw, path)
}

.mick_validate_zone_weights <- function(weights, zones, series_name) {
  out <- stats::setNames(rep(1, length(zones)), zones)
  if (is.null(weights) || length(weights) == 0L) {
    return(out)
  }

  if (is.list(weights) && !is.data.frame(weights)) {
    weights <- unlist(weights, use.names = TRUE)
  }

  if (!is.numeric(weights)) {
    stop(series_name, " weights must be numeric by zone.", call. = FALSE)
  }
  if (is.null(names(weights)) || any(!nzchar(names(weights)))) {
    stop(series_name, " weights must be named by zone.", call. = FALSE)
  }
  if (anyDuplicated(names(weights))) {
    stop(series_name, " weights must not repeat zone names.", call. = FALSE)
  }
  if (any(!names(weights) %in% zones)) {
    unknown <- setdiff(names(weights), zones)
    stop(series_name, " weights contain unknown zones: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (anyNA(weights) || any(!is.finite(weights))) {
    stop(series_name, " weights must be finite numeric values.", call. = FALSE)
  }
  if (any(weights < 0)) {
    stop(series_name, " weights must be zero or positive.", call. = FALSE)
  }

  matched <- intersect(zones, names(weights))
  out[matched] <- as.numeric(weights[matched])

  out
}

.mick_apply_zone_weights <- function(df, weights, series_name) {
  out <- df
  zones <- names(out)[-1L]
  zone_weights <- .mick_validate_zone_weights(weights, zones, series_name)

  for (zone in zones) {
    out[[zone]] <- out[[zone]] * zone_weights[[zone]]
  }

  out
}

.mick_compute_net_demand <- function(demand, wind, solar) {
  out <- demand
  zones <- names(demand)[-1L]

  for (zone in zones) {
    out[[zone]] <- demand[[zone]] - wind[[zone]] - solar[[zone]]
  }

  out
}

read_input_data <- function(config) {
  config <- normalize_config(config)
  inputs <- config$inputs

  if (!is.list(inputs)) {
    stop("config$inputs must be a list.", call. = FALSE)
  }

  required <- c("wind_file", "solar_file", "demand_file", "grid_file")
  missing <- required[!vapply(required, function(name) .mick_scalar_string(inputs[[name]]), logical(1))]
  if (length(missing)) {
    stop("Missing required input paths: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  wind <- .mick_parse_time_series_input(inputs$wind_file, "wind")
  solar <- .mick_parse_time_series_input(inputs$solar_file, "solar")
  demand <- .mick_parse_time_series_input(inputs$demand_file, "demand")
  zone_names <- .mick_align_time_series_inputs(wind, solar, demand)
  grid <- .mick_validate_grid_matrix(.mick_parse_grid_input(inputs$grid_file), zone_names)

  list(
    wind = wind,
    solar = solar,
    demand = demand,
    grid = grid
  )
}

build_input_views <- function(config) {
  config <- normalize_config(config)
  original <- read_input_data(config)

  clustering <- list(
    wind = .mick_apply_zone_weights(original$wind, config$weights$wind, "wind"),
    solar = .mick_apply_zone_weights(original$solar, config$weights$solar, "solar"),
    demand = .mick_apply_zone_weights(original$demand, config$weights$demand, "demand"),
    grid = original$grid
  )
  clustering$net_demand <- .mick_compute_net_demand(
    clustering$demand,
    clustering$wind,
    clustering$solar
  )

  list(
    original = original,
    clustering = clustering
  )
}

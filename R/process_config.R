.mick_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.mick_positive_integer <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x > 0 &&
    x == floor(x)
}

.mick_character_vector <- function(x) {
  is.character(x) && all(!is.na(x)) && all(nzchar(x))
}

.mick_normalize_reserved_time_slices <- function(x) {
  if (is.null(x)) {
    return(c("max_net_demand", "min_net_demand"))
  }
  if (is.list(x)) {
    if (length(x) == 0L) {
      return(character(0))
    }
    if (all(vapply(x, .mick_scalar_string, logical(1)))) {
      return(unlist(x, use.names = FALSE))
    }
  }
  x
}

.mick_section_or_list <- function(cfg, name) {
  section <- cfg[[name]]
  if (is.null(section)) {
    list()
  } else {
    section
  }
}

.mick_is_absolute_path <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
    (startsWith(path, "/") ||
      startsWith(path, "\\\\") ||
      grepl("^[A-Za-z]:[\\\\/]", path) ||
      startsWith(path, "~"))
}

.mick_resolve_run_path <- function(path, base_dir) {
  if (!.mick_scalar_string(path)) {
    return(path)
  }

  expanded <- path.expand(path)
  if (.mick_is_absolute_path(expanded)) {
    return(normalizePath(expanded, winslash = "/", mustWork = FALSE))
  }

  normalizePath(file.path(base_dir, expanded), winslash = "/", mustWork = FALSE)
}

.mick_resolve_run_config <- function(cfg, base_dir) {
  cfg <- normalize_config(cfg)

  if (is.list(cfg$inputs)) {
    cfg$inputs$wind_file <- .mick_resolve_run_path(cfg$inputs$wind_file, base_dir)
    cfg$inputs$solar_file <- .mick_resolve_run_path(cfg$inputs$solar_file, base_dir)
    cfg$inputs$demand_file <- .mick_resolve_run_path(cfg$inputs$demand_file, base_dir)
    cfg$inputs$grid_file <- .mick_resolve_run_path(cfg$inputs$grid_file, base_dir)
  }

  if (is.list(cfg$outputs)) {
    cfg$outputs$directory <- .mick_resolve_run_path(cfg$outputs$directory, base_dir)
  }

  cfg
}

normalize_config <- function(cfg) {
  if (!is.list(cfg)) {
    stop("cfg must be a list.", call. = FALSE)
  }

  inputs <- .mick_section_or_list(cfg, "inputs")
  clustering <- .mick_section_or_list(cfg, "clustering")
  weights <- .mick_section_or_list(cfg, "weights")
  outputs <- .mick_section_or_list(cfg, "outputs")

  if (is.list(clustering)) {
    clustering$reserved_time_slices <- .mick_normalize_reserved_time_slices(clustering$reserved_time_slices)
  }
  if (is.list(clustering) && is.null(clustering$temporal_method)) {
    clustering$temporal_method <- "k_medians"
  }
  if (is.null(weights)) {
    weights <- list()
  }

  list(
    inputs = inputs,
    clustering = clustering,
    weights = weights,
    outputs = outputs
  )
}

#' Read a MICK config file or list
#'
#' Reads JSON/YAML configs or normalizes an in-memory list config to MICK's
#' canonical structure. File-based configs resolve relative paths from the
#' config file directory.
#'
#' @param x Either a list config object, or a path to a `.json`, `.yaml`, or
#'   `.yml` config file.
#'
#' @return A canonical config list.
#'
#' @examples
#' cfg <- read_mick_config(mick_example_path("config", "example_config.yaml"))
#' names(cfg)
#' @export
read_mick_config <- function(x) {
  if (is.list(x)) {
    return(normalize_config(x))
  }

  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("config must be a list or a non-empty character path.", call. = FALSE)
  }
  config_path <- path.expand(x)
  if (!file.exists(config_path)) {
    stop("Config file not found: ", x, call. = FALSE)
  }
  config_path <- normalizePath(config_path, winslash = "/", mustWork = TRUE)

  ext <- tolower(tools::file_ext(config_path))
  raw_config <- if (ext == "json") {
    jsonlite::fromJSON(config_path, simplifyVector = FALSE)
  } else if (ext %in% c("yaml", "yml")) {
    yaml::read_yaml(config_path)
  } else {
    stop("Unsupported config file format: ", ext, call. = FALSE)
  }

  .mick_resolve_run_config(
    raw_config,
    dirname(config_path)
  )
}

validate_config <- function(cfg) {
  cfg <- normalize_config(cfg)
  errors <- character()

  add_error <- function(message) {
    errors <<- c(errors, message)
  }

  inputs <- cfg$inputs
  clustering <- cfg$clustering
  weights <- cfg$weights
  outputs <- cfg$outputs

  inputs_is_list <- is.list(inputs)
  clustering_is_list <- is.list(clustering)
  weights_is_list <- is.list(weights)
  outputs_is_list <- is.list(outputs)

  if (!inputs_is_list) {
    add_error("inputs must be a list.")
  }
  if (!clustering_is_list) {
    add_error("clustering must be a list.")
  }
  if (!weights_is_list) {
    add_error("weights must be a list.")
  }
  if (!outputs_is_list) {
    add_error("outputs must be a list.")
  }

  if (inputs_is_list) {
    if (!.mick_scalar_string(inputs$wind_file)) {
      add_error("inputs$wind_file must be a non-empty string.")
    }
    if (!.mick_scalar_string(inputs$solar_file)) {
      add_error("inputs$solar_file must be a non-empty string.")
    }
    if (!.mick_scalar_string(inputs$demand_file)) {
      add_error("inputs$demand_file must be a non-empty string.")
    }
    if (!.mick_scalar_string(inputs$grid_file)) {
      add_error("inputs$grid_file must be a non-empty string.")
    }

    for (field in c("wind_file", "solar_file", "demand_file", "grid_file")) {
      path <- inputs[[field]]
      if (.mick_scalar_string(path) && !file.exists(path)) {
        add_error(paste0("inputs$", field, " does not exist: ", path))
      }
    }
  }

  if (clustering_is_list) {
    if (!.mick_positive_integer(clustering$n_space)) {
      add_error("clustering$n_space must be a positive integer.")
    }
    if (!.mick_positive_integer(clustering$n_time)) {
      add_error("clustering$n_time must be a positive integer.")
    }
    if (!.mick_character_vector(clustering$reserved_time_slices)) {
      add_error("clustering$reserved_time_slices must be a character vector of non-empty strings.")
    }
    if (!.mick_scalar_string(clustering$temporal_method)) {
      add_error("clustering$temporal_method must be a non-empty string.")
    } else if (!clustering$temporal_method %in% c("k_medians", "k_means")) {
      add_error("clustering$temporal_method must be one of: k_medians, k_means.")
    }
    if (.mick_character_vector(clustering$reserved_time_slices)) {
      unsupported_reserved <- setdiff(
        clustering$reserved_time_slices,
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
      )
      if (length(unsupported_reserved)) {
        add_error(paste0(
          "clustering$reserved_time_slices contains unsupported values: ",
          paste(unsupported_reserved, collapse = ", ")
        ))
      }
    }
  }

  if (outputs_is_list) {
    if (!.mick_scalar_string(outputs$directory)) {
      add_error("outputs$directory must be a non-empty string.")
    }
    if (!.mick_scalar_string(outputs$format) || !(tolower(outputs$format) %in% c("csv", "json", "yaml"))) {
      add_error("outputs$format must be csv, json, or yaml.")
    }
  }

  if (length(errors)) {
    stop(paste(unique(errors), collapse = "\n"), call. = FALSE)
  }

  invisible(cfg)
}

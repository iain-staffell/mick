.mick_extdata_root <- function() {
  root <- system.file("extdata", package = "mick")
  if (!is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)) {
    stop("Could not locate bundled example files under extdata.", call. = FALSE)
  }

  normalizePath(root, winslash = "/", mustWork = TRUE)
}

#' Locate bundled example files
#'
#' Returns absolute paths for files shipped in `inst/extdata`.
#'
#' @param ... Optional path components inside `extdata`.
#'
#' @return Absolute path to the requested example file or directory.
#'
#' @examples
#' mick_example_path("config", "example_config.yaml")
#' @export
mick_example_path <- function(...) {
  parts <- list(...)
  root <- .mick_extdata_root()

  if (length(parts) == 0L) {
    return(root)
  }

  rel_path <- do.call(file.path, as.list(parts))
  full_path <- file.path(root, rel_path)

  if (!file.exists(full_path)) {
    stop("Bundled example file not found: ", rel_path, call. = FALSE)
  }

  normalizePath(full_path, winslash = "/", mustWork = TRUE)
}

#' Generate runnable example inputs and config
#'
#' Copies bundled example files to a writable directory and writes a config file
#' that points at those copied inputs.
#'
#' @param directory Target working directory for copied examples.
#' @param format Config format to write, either `"yaml"` or `"json"`.
#'
#' @return A list with absolute paths for `root`, `config`, `inputs`, `map`, and
#'   `output_dir`.
#'
#' @examples
#' files <- mick_example_files(format = "yaml")
#' basename(files$config)
#' @export
mick_example_files <- function(directory = tempfile("mick-example-"), format = c("yaml", "json")) {
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) || !nzchar(directory)) {
    stop("directory must be a non-empty string.", call. = FALSE)
  }

  format <- match.arg(format)
  directory <- normalizePath(path.expand(directory), winslash = "/", mustWork = FALSE)

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  inputs_dir <- file.path(directory, "examples")
  config_dir <- file.path(directory, "config")
  output_dir <- file.path(directory, "output", "example_run")

  dir.create(inputs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)

  input_files <- c(
    "example_wind.csv",
    "example_solar.csv",
    "example_demand.csv",
    "example_grid.csv",
    "example_map.gpkg"
  )
  source_files <- vapply(input_files, function(name) {
    mick_example_path("examples", name)
  }, character(1))
  copied <- file.copy(source_files, file.path(inputs_dir, input_files), overwrite = TRUE)
  if (!all(copied)) {
    stop("Failed to copy one or more bundled example files.", call. = FALSE)
  }

  config <- normalize_config(list(
    inputs = list(
      wind_file = file.path(inputs_dir, "example_wind.csv"),
      solar_file = file.path(inputs_dir, "example_solar.csv"),
      demand_file = file.path(inputs_dir, "example_demand.csv"),
      grid_file = file.path(inputs_dir, "example_grid.csv")
    ),
    clustering = list(
      n_space = 5L,
      n_time = 12L
    ),
    weights = list(),
    outputs = list(
      directory = output_dir,
      format = "csv"
    )
  ))

  config_path <- file.path(config_dir, paste0("example_config.", format))
  if (identical(format, "json")) {
    jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
  } else {
    yaml::write_yaml(config, config_path)
  }

  list(
    root = normalizePath(directory, winslash = "/", mustWork = TRUE),
    config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
    inputs = normalizePath(inputs_dir, winslash = "/", mustWork = TRUE),
    map = normalizePath(file.path(inputs_dir, "example_map.gpkg"), winslash = "/", mustWork = TRUE),
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  )
}

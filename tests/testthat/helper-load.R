helper_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NULL
)

project_root <- if (is.character(helper_file) && length(helper_file) == 1L && !is.na(helper_file) && nzchar(helper_file)) {
  normalizePath(file.path(dirname(helper_file), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

example_data_path <- function(name) {
  mick_example_path("examples", name)
}

example_config_path <- function(format = c("yaml", "json")) {
  format <- match.arg(format)
  mick_example_path("config", paste0("example_config.", format))
}

make_example_config_fixture <- function() {
  fixture_root <- tempfile("mick-config-")
  config_dir <- file.path(fixture_root, "config")
  examples_dir <- file.path(fixture_root, "examples")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(examples_dir, recursive = TRUE, showWarnings = FALSE)

  for (name in c("example_wind.csv", "example_solar.csv", "example_demand.csv", "example_grid.csv")) {
    file.copy(example_data_path(name), file.path(examples_dir, name), overwrite = TRUE)
  }

  config <- read_mick_config(example_config_path("yaml"))
  config$inputs <- list(
    wind_file = "../examples/example_wind.csv",
    solar_file = "../examples/example_solar.csv",
    demand_file = "../examples/example_demand.csv",
    grid_file = "../examples/example_grid.csv"
  )
  config$outputs$directory <- "output/example_run"

  config_path <- file.path(config_dir, "example_config.yaml")
  yaml::write_yaml(config, config_path)

  list(
    fixture_root = fixture_root,
    config_dir = config_dir,
    config_path = config_path,
    output_dir = file.path(config_dir, "output", "example_run"),
    examples_dir = examples_dir
  )
}

make_plot_result_fixture <- function() {
  fixture <- make_example_config_fixture()
  result <- run_mick(fixture$config_path)

  fixture$result <- result
  fixture
}

make_reserved_plot_result_fixture <- function() {
  fixture <- make_example_config_fixture()
  cfg <- yaml::read_yaml(fixture$config_path)
  cfg$clustering$reserved_time_slices <- c("max_net_demand", "min_net_demand")
  yaml::write_yaml(cfg, fixture$config_path)

  result <- run_mick(fixture$config_path)

  fixture$result <- result
  fixture
}

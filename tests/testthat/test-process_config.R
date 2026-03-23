test_that("JSON and YAML configs normalize to the same canonical shape", {
  yaml_cfg <- read_mick_config(example_config_path("yaml"))
  json_cfg <- read_mick_config(example_config_path("json"))

  expect_equal(yaml_cfg, json_cfg)
  expect_identical(
    yaml_cfg$inputs,
    list(
      wind_file = normalizePath(example_data_path("example_wind.csv"), winslash = "/", mustWork = TRUE),
      solar_file = normalizePath(example_data_path("example_solar.csv"), winslash = "/", mustWork = TRUE),
      demand_file = normalizePath(example_data_path("example_demand.csv"), winslash = "/", mustWork = TRUE),
      grid_file = normalizePath(example_data_path("example_grid.csv"), winslash = "/", mustWork = TRUE)
    )
  )
  expect_identical(
    yaml_cfg$outputs$directory,
    normalizePath(file.path(dirname(example_config_path("yaml")), "..", "output", "example_run"), winslash = "/", mustWork = FALSE)
  )
})

test_that("list config receives defaults", {
  cfg <- mick:::normalize_config(list(
    inputs = list(
      wind_file = "examples/example_wind.csv",
      solar_file = "examples/example_solar.csv",
      demand_file = "examples/example_demand.csv",
      grid_file = "examples/example_grid.csv"
    ),
    clustering = list(
      n_space = 3,
      n_time = 4
    ),
    outputs = list(
      directory = "out",
      format = "csv"
    )
  ))

  expect_identical(cfg$clustering$reserved_time_slices, c("max_net_demand", "min_net_demand"))
  expect_identical(cfg$clustering$temporal_method, "k_medians")
  expect_identical(cfg$weights, list())
})

test_that("validated list configs accept integer scalars", {
  cfg <- list(
    inputs = list(
      wind_file = example_data_path("example_wind.csv"),
      solar_file = example_data_path("example_solar.csv"),
      demand_file = example_data_path("example_demand.csv"),
      grid_file = example_data_path("example_grid.csv")
    ),
    clustering = list(
      n_space = 6L,
      n_time = 12L
    ),
    outputs = list(
      directory = "out",
      format = "csv"
    )
  )

  expect_silent(mick:::validate_config(cfg))
})

test_that("validated list configs allow empty reserved_time_slices", {
  cfg <- list(
    inputs = list(
      wind_file = example_data_path("example_wind.csv"),
      solar_file = example_data_path("example_solar.csv"),
      demand_file = example_data_path("example_demand.csv"),
      grid_file = example_data_path("example_grid.csv")
    ),
    clustering = list(
      n_space = 6L,
      n_time = 12L,
      reserved_time_slices = character(0)
    ),
    outputs = list(
      directory = "out",
      format = "csv"
    )
  )

  expect_silent(mick:::validate_config(cfg))
})

test_that("file-backed configs accept empty reserved_time_slices", {
  fixture_root <- tempfile("mick-config-")
  config_dir <- file.path(fixture_root, "config")
  examples_dir <- file.path(fixture_root, "examples")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(examples_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    c(
      example_data_path("example_wind.csv"),
      example_data_path("example_solar.csv"),
      example_data_path("example_demand.csv"),
      example_data_path("example_grid.csv")
    ),
    c(
      file.path(examples_dir, "example_wind.csv"),
      file.path(examples_dir, "example_solar.csv"),
      file.path(examples_dir, "example_demand.csv"),
      file.path(examples_dir, "example_grid.csv")
    ),
    overwrite = TRUE
  )
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  json_path <- file.path(config_dir, "example_config.json")
  yaml_path <- file.path(config_dir, "example_config.yaml")

  json_config <- list(
    inputs = list(
      wind_file = "../examples/example_wind.csv",
      solar_file = "../examples/example_solar.csv",
      demand_file = "../examples/example_demand.csv",
      grid_file = "../examples/example_grid.csv"
    ),
    clustering = list(
      n_space = 6L,
      n_time = 12L,
      reserved_time_slices = list()
    ),
    outputs = list(
      directory = "out",
      format = "csv"
    )
  )

  yaml_config <- json_config

  jsonlite::write_json(json_config, json_path, auto_unbox = TRUE, pretty = TRUE)
  yaml::write_yaml(yaml_config, yaml_path)

  expect_silent(mick:::validate_config(read_mick_config(json_path)))
  expect_silent(mick:::validate_config(read_mick_config(yaml_path)))
  expect_identical(read_mick_config(json_path)$clustering$reserved_time_slices, character(0))
  expect_identical(read_mick_config(yaml_path)$clustering$reserved_time_slices, character(0))
})

test_that("file-backed configs resolve relative paths from their own directory", {
  fixture <- make_example_config_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- read_mick_config(fixture$config_path)

  expect_identical(
    cfg$inputs$wind_file,
    normalizePath(file.path(fixture$config_dir, "../examples/example_wind.csv"), winslash = "/", mustWork = TRUE)
  )
  expect_identical(
    cfg$inputs$solar_file,
    normalizePath(file.path(fixture$config_dir, "../examples/example_solar.csv"), winslash = "/", mustWork = TRUE)
  )
  expect_identical(
    cfg$outputs$directory,
    normalizePath(file.path(fixture$config_dir, "output/example_run"), winslash = "/", mustWork = FALSE)
  )
})

test_that("wrong-type top-level sections are reported as section errors", {
  bad <- list(
    inputs = "oops",
    clustering = list(
      n_space = 6L,
      n_time = 12L
    ),
    weights = 123,
    outputs = list(
      directory = "out",
      format = "csv"
    )
  )

  expect_error(
    mick:::validate_config(bad),
    "inputs must be a list|weights must be a list"
  )
})

test_that("validation aggregates multiple config errors", {
  bad <- list(
    inputs = list(
      wind_file = "",
      solar_file = "examples/example_solar.csv"
    ),
    clustering = list(
      n_space = 0,
      n_time = NA
    ),
    outputs = list(
      directory = "",
      format = "txt"
    )
  )

  expect_error(
    mick:::validate_config(bad),
    "wind_file|demand_file|grid_file|n_space|n_time|directory|format"
  )
})

test_that("validate_config rejects unsupported temporal and reserved slice values", {
  bad <- list(
    inputs = list(
      wind_file = example_data_path("example_wind.csv"),
      solar_file = example_data_path("example_solar.csv"),
      demand_file = example_data_path("example_demand.csv"),
      grid_file = example_data_path("example_grid.csv")
    ),
    clustering = list(
      n_space = 6L,
      n_time = 12L,
      temporal_method = "bogus",
      reserved_time_slices = c("max_net_demand", "bogus_rule")
    ),
    outputs = list(
      directory = "out",
      format = "csv"
    )
  )

  expect_error(
    mick:::validate_config(bad),
    "temporal_method|Unsupported reserved rules|reserved_time_slices"
  )
})

test_that("validate_config aggregates missing input files after resolution", {
  fixture_root <- tempfile("mick-config-")
  config_dir <- file.path(fixture_root, "config")
  examples_dir <- file.path(fixture_root, "examples")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(examples_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    c(
      example_data_path("example_solar.csv"),
      example_data_path("example_demand.csv")
    ),
    c(
      file.path(examples_dir, "example_solar.csv"),
      file.path(examples_dir, "example_demand.csv")
    ),
    overwrite = TRUE
  )
  config_path <- file.path(config_dir, "example_config.yaml")

  yaml::write_yaml(list(
    inputs = list(
      wind_file = "../missing_wind.csv",
      solar_file = "../examples/example_solar.csv",
      demand_file = "../examples/example_demand.csv",
      grid_file = "../missing_grid.csv"
    ),
    clustering = list(
      n_space = 6L,
      n_time = 12L
    ),
    outputs = list(
      directory = "../output/example_run",
      format = "csv"
    )
  ), config_path)

  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    mick:::validate_config(read_mick_config(config_path)),
    "missing_wind\\.csv|missing_grid\\.csv"
  )
})

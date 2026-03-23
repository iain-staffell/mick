test_that("run_mick rejects unsupported input types", {
  expect_error(run_mick(123), "config")
})

test_that("mick_example_files returns runnable assets", {
  fixture <- mick_example_files()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_true(file.exists(fixture$config))
  expect_true(file.exists(fixture$map))
  expect_true(dir.exists(fixture$inputs))
  expect_true(dir.exists(dirname(fixture$output_dir)))
})

test_that("run_mick executes the shipped example config end-to-end with temp output", {
  fixture <- make_example_config_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  result <- run_mick(fixture$config_path)

  expect_type(result, "list")
  expect_true(all(c("schema_version", "run_timestamp", "config", "inputs", "spatial", "temporal", "outputs") %in% names(result)))
  expect_match(result$run_timestamp, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_identical(result$outputs$directory, normalizePath(fixture$output_dir, winslash = "/", mustWork = TRUE))
  expect_true(is.list(result$outputs$run_metadata))

  written_files <- result$outputs$written_files
  expect_true(is.list(written_files))
  expect_true(all(c("spatial_clusters", "temporal_clusters", "reduced_wind", "reduced_solar", "reduced_demand", "reduced_grid", "run_metadata") %in% names(written_files)))
  expect_true(all(file.exists(unlist(written_files, use.names = FALSE))))

  spatial_clusters <- read.csv(written_files$spatial_clusters, check.names = FALSE, stringsAsFactors = FALSE)
  temporal_clusters <- read.csv(written_files$temporal_clusters, check.names = FALSE, stringsAsFactors = FALSE)
  reduced_wind <- read.csv(written_files$reduced_wind, check.names = FALSE, stringsAsFactors = FALSE)
  run_metadata <- read.csv(written_files$run_metadata, check.names = FALSE, stringsAsFactors = FALSE)

  expect_identical(names(spatial_clusters), c("zone", "cluster"))
  expect_true(all(c("hour_index", "temporal_cluster") %in% names(temporal_clusters)))
  expect_identical(names(reduced_wind)[1], "temporal_cluster")
  expect_true(nrow(reduced_wind) > 0L)
  expect_true(any(run_metadata$field == "schema_version"))
  expect_true(any(run_metadata$field == "input_paths.wind_file"))
  expect_true(any(run_metadata$field == "written_files.reduced_grid"))
  expect_identical(result$outputs$run_metadata$written_files$run_metadata, written_files$run_metadata)
})

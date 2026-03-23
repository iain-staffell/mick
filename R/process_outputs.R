.mick_schema_version <- "1.0"

.mick_run_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

.mick_output_extension <- function(format) {
  switch(
    tolower(format),
    csv = "csv",
    json = "json",
    yaml = "yaml",
    stop("Unsupported output format: ", format, call. = FALSE)
  )
}

.mick_output_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.mick_input_paths <- function(config) {
  input_names <- c("wind_file", "solar_file", "demand_file", "grid_file")
  paths <- vapply(input_names, function(name) {
    normalizePath(config$inputs[[name]], winslash = "/", mustWork = TRUE)
  }, character(1))
  stats::setNames(paths, input_names)
}

.mick_input_md5 <- function(paths) {
  hashes <- tools::md5sum(unname(paths))
  stats::setNames(unname(hashes), names(paths))
}

.mick_dimensions <- function(x) {
  list(
    rows = nrow(x),
    cols = ncol(x)
  )
}

.mick_stringify_datetimes <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L || ncol(df) == 0L) {
    return(df)
  }

  out <- df
  datetime_cols <- vapply(out, inherits, logical(1), what = "POSIXct")
  if (any(datetime_cols)) {
    out[datetime_cols] <- lapply(out[datetime_cols], function(col) {
      format(col, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    })
  }
  out
}

.mick_spatial_clusters_table <- function(spatial) {
  spatial$membership
}

.mick_temporal_clusters_table <- function(temporal) {
  .mick_stringify_datetimes(temporal$mapping)
}

.mick_reduced_grid_table <- function(grid) {
  grid_df <- as.data.frame(grid, check.names = FALSE, stringsAsFactors = FALSE)
  data.frame(
    cluster = rownames(grid),
    grid_df,
    row.names = NULL,
    check.names = FALSE
  )
}

.mick_flatten_metadata <- function(x, prefix = NULL) {
  if (inherits(x, "POSIXct")) {
    return(data.frame(
      field = prefix,
      value = format(x, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      stringsAsFactors = FALSE
    ))
  }

  if (is.data.frame(x)) {
    if (nrow(x) == 0L) {
      return(data.frame(
        field = prefix,
        value = "[]",
        stringsAsFactors = FALSE
      ))
    }
    rows <- lapply(seq_len(nrow(x)), function(i) {
      .mick_flatten_metadata(as.list(x[i, , drop = FALSE]), paste0(prefix, "[", i, "]"))
    })
    return(do.call(rbind, rows))
  }

  if (is.list(x) && !is.null(names(x))) {
    if (length(x) == 0L) {
      return(data.frame(
        field = prefix,
        value = "[]",
        stringsAsFactors = FALSE
      ))
    }
    rows <- lapply(names(x), function(name) {
      child_prefix <- if (is.null(prefix)) name else paste0(prefix, ".", name)
      .mick_flatten_metadata(x[[name]], child_prefix)
    })
    return(do.call(rbind, rows))
  }

  if (is.list(x)) {
    if (length(x) == 0L) {
      return(data.frame(
        field = prefix,
        value = "[]",
        stringsAsFactors = FALSE
      ))
    }
    rows <- lapply(seq_along(x), function(i) {
      child_prefix <- if (is.null(prefix)) paste0("[", i, "]") else paste0(prefix, "[", i, "]")
      .mick_flatten_metadata(x[[i]], child_prefix)
    })
    return(do.call(rbind, rows))
  }

  if (length(x) > 1L) {
    rows <- lapply(seq_along(x), function(i) {
      child_prefix <- if (is.null(prefix)) paste0("[", i, "]") else paste0(prefix, "[", i, "]")
      .mick_flatten_metadata(x[[i]], child_prefix)
    })
    return(do.call(rbind, rows))
  }

  value <- if (is.logical(x)) {
    if (is.na(x)) "NA" else if (x) "TRUE" else "FALSE"
  } else {
    as.character(x)
  }

  data.frame(
    field = prefix,
    value = value,
    stringsAsFactors = FALSE
  )
}

.mick_run_metadata <- function(config, inputs, spatial, temporal, run_timestamp, schema_version) {
  input_paths <- .mick_input_paths(config)
  input_md5 <- .mick_input_md5(input_paths)

  list(
    run_timestamp = run_timestamp,
    schema_version = schema_version,
    input_paths = as.list(input_paths),
    input_md5 = as.list(input_md5),
    original_dimensions = list(
      wind = .mick_dimensions(inputs$original$wind),
      solar = .mick_dimensions(inputs$original$solar),
      demand = .mick_dimensions(inputs$original$demand),
      grid = .mick_dimensions(inputs$original$grid)
    ),
    reduced_dimensions = list(
      spatial_clusters = length(unique(spatial$membership$cluster)),
      temporal_slices = nrow(temporal$slices),
      reduced_wind = .mick_dimensions(temporal$reduced_wind),
      reduced_solar = .mick_dimensions(temporal$reduced_solar),
      reduced_demand = .mick_dimensions(temporal$reduced_demand),
      reduced_grid = .mick_dimensions(spatial$reduced_grid)
    ),
    reserved_slices = .mick_stringify_datetimes(temporal$reserved),
    slice_weights = temporal$weights
  )
}

assemble_mick_result <- function(config, inputs, spatial, temporal, run_timestamp = .mick_run_timestamp(), schema_version = .mick_schema_version) {
  run_metadata <- .mick_run_metadata(config, inputs, spatial, temporal, run_timestamp, schema_version)

  list(
    schema_version = schema_version,
    run_timestamp = run_timestamp,
    config = config,
    inputs = inputs,
    spatial = spatial,
    temporal = temporal,
    outputs = list(
      format = tolower(config$outputs$format),
      directory = config$outputs$directory,
      tables = list(
        spatial_clusters = .mick_spatial_clusters_table(spatial),
        temporal_clusters = .mick_temporal_clusters_table(temporal),
        reduced_wind = temporal$reduced_wind,
        reduced_solar = temporal$reduced_solar,
        reduced_demand = temporal$reduced_demand,
        reduced_grid = .mick_reduced_grid_table(spatial$reduced_grid)
      ),
      run_metadata = run_metadata
    )
  )
}

.mick_output_path <- function(directory, name, format) {
  file.path(directory, paste0(name, ".", .mick_output_extension(format)))
}

.mick_write_value <- function(value, path, format) {
  if (identical(tolower(format), "csv")) {
    if (is.matrix(value)) {
      value <- .mick_reduced_grid_table(value)
    }

    if (is.data.frame(value)) {
      value <- as.data.frame(value, check.names = FALSE, stringsAsFactors = FALSE)
    } else if (is.list(value)) {
      value <- .mick_flatten_metadata(value)
    }

    utils::write.csv(value, path, row.names = FALSE, quote = TRUE)
    return(invisible(TRUE))
  }

  if (is.data.frame(value)) {
    value <- as.data.frame(value, check.names = FALSE, stringsAsFactors = FALSE)
  } else if (is.matrix(value)) {
    value <- .mick_reduced_grid_table(value)
  }

  if (identical(tolower(format), "json")) {
    json <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, null = "null")
    writeLines(json, path, useBytes = TRUE)
    return(invisible(TRUE))
  }

  if (identical(tolower(format), "yaml")) {
    yaml::write_yaml(value, path)
    return(invisible(TRUE))
  }

  stop("Unsupported output format: ", format, call. = FALSE)
}

export_mick_outputs <- function(result) {
  format <- tolower(result$outputs$format)
  directory <- .mick_output_directory(result$outputs$directory)
  save_rds <- isTRUE(result$config$outputs$save_rds)

  tables <- result$outputs$tables
  written_files <- list(
    spatial_clusters = .mick_output_path(directory, "spatial_clusters", format),
    temporal_clusters = .mick_output_path(directory, "temporal_clusters", format),
    reduced_wind = .mick_output_path(directory, "reduced_wind", format),
    reduced_solar = .mick_output_path(directory, "reduced_solar", format),
    reduced_demand = .mick_output_path(directory, "reduced_demand", format),
    reduced_grid = .mick_output_path(directory, "reduced_grid", format),
    run_metadata = .mick_output_path(directory, "run_metadata", format)
  )
  if (save_rds) {
    written_files$result_rds <- file.path(directory, "mick_result.rds")
  }

  metadata <- result$outputs$run_metadata
  metadata$written_files <- written_files

  .mick_write_value(tables$spatial_clusters, written_files$spatial_clusters, format)
  .mick_write_value(tables$temporal_clusters, written_files$temporal_clusters, format)
  .mick_write_value(tables$reduced_wind, written_files$reduced_wind, format)
  .mick_write_value(tables$reduced_solar, written_files$reduced_solar, format)
  .mick_write_value(tables$reduced_demand, written_files$reduced_demand, format)
  .mick_write_value(tables$reduced_grid, written_files$reduced_grid, format)

  .mick_write_value(metadata, written_files$run_metadata, format)

  result$outputs$directory <- directory
  result$outputs$run_metadata <- metadata
  result$outputs$written_files <- written_files
  if (save_rds) {
    saveRDS(result, written_files$result_rds)
  }

  result
}

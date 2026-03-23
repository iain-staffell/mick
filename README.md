# MICK

MICK, the Mosaic Intelligent Clustering Kernel, reduces high-resolution spatial and hourly energy data into a smaller model that is easier to solve while keeping the original inputs, cluster assignments, reduced series, and run metadata together.

## Quick Start

To install:

```r
install.packages("remotes")
remotes::install_github("iain-staffell/mick")
library(mick)
```

Minimal runnable example with bundled inputs:

```r
files <- mick_example_files(format = "yaml")
result <- run_mick(files$config)
result$outputs$written_files
```

Run with your own config file or config list:

```r
result <- run_mick("path/to/config.yaml")
# or
result <- run_mick(list(...))
```

## Plotting

Plotting uses the in-memory `result` object returned by `run_mick()`, not the exported files on disk.

Direct plot helpers:

```r
plots <- plot_spatial_clusters(result)
duration <- plot_temporal_duration_curves(result)
dependence <- plot_temporal_dependence(result)
chronology <- plot_temporal_chronology(result)
```

Spatial plots default to the network fallback in v1. Pass `map` and `column` to `plot_spatial_clusters()` only if you want map-backed plots and have `sf` installed. The dashboard uses that same network view by default; if `igraph` is unavailable, it shows a placeholder in the spatial slot instead of installing anything at runtime.

Duration curves use `node = "total"` by default. Pass either `node` or `var`, not both. When `var` is supplied, the plot facets over the reduced spatial clusters; when `node` is supplied, it facets over the four time-series families.

For a quick interactive walk-through (including dashboard playback), run `Rscript examples/example_run.R`.

## What It Expects

- Wind, solar, and demand inputs are time-series tables with `datetime` in the first column and matching zone columns after that.
- Grid input is a square, symmetric matrix with the same zone names on rows and columns as the time-series files.
- Reduced temporal outputs are wide tables with `temporal_cluster` in the first column.
- `run_metadata` records the run timestamp, input hashes and paths, source/reduced dimensions, reserved slices, slice weights, and the written output files.

## Docs

- [Getting Started](docs/index.md)
- [Output Schema](docs/output_schema.md)

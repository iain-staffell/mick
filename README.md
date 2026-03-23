# MOSAIC Intelligent Clustering Kit (MICK)

MICK is a lightweight R package which reduces high-resolution power system data down to a smaller set of zones and timesteps, making it easier to use in energy systems models.

MICK takes zonal electricity demand and supply from wind and solar farms, identifies which zones can be merged while still capturing the main structure of the original system, and then groups hours into representative time slices. It can preserve important conditions such as minimum and maximum net demand, while retaining the main spatial and temporal patterns needed for planning models.


## Quick Start

Install it using `remotes`:

```r
install.packages("remotes")
remotes::install_github("iain-staffell/mick")
library(mick)
```

Run a minimal example for Great Britain, reducing 8760 hours of data for 14 zones down to 12 time slices for 6 zones.
This example gives you a local copy of the files that MICK works with (so you can see the example format), and saves the output to disk.

```r
# populate a sub-folder called "gb_exapmle" with the necessary input files
files <- mick_example_files(directory = "gb_example")

# run MICK with the default config file
result <- run_mick(files$config)

# output files are saved to this location
result$outputs$written_files
```

There are many ways to visualise the resulting clustered data (see [Plotting](#plotting)).

One approach is to view the distribution of your variables in the original and clustered data:

```r
# by default, show the duration curve for each variable, summed over all zone:
plot_temporal_duration_curves(result)

# or look at one variable across all zones
plot_temporal_duration_curves(result, var="net_demand")

# you can look at individual zones (e.g. southern scotland)
plot_temporal_duration_curves(result, node="N")

```

Another approach is to see how the zones were clustered together:

```r
# by default, show a graph with lines showing which zones are connected
network <- plot_spatial_clusters(result)

# show the original, and then the clustered zones
network[[1]]
network[[2]]

# show these zones on a map if you supply a GIS file that can be read by `sf`
# a map of Britain's GSP zones is provided within `mick_example_files()`
network <- plot_spatial_clusters(result, map=files$map, column='GSPGroup')
network[[1]]
network[[2]]
```



## Input Data

- Wind, solar, and demand inputs are time-series tables with `datetime` in the first column and matching zone columns after that.
- Grid input is a square, symmetric matrix with the same zone names on rows and columns as the time-series files.
- Reduced temporal outputs are wide tables with `temporal_cluster` in the first column.
- `run_metadata` records the run timestamp, input hashes and paths, source/reduced dimensions, reserved slices, slice weights, and the written output files.


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

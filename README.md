
<!-- README.md is generated from README.Rmd. Without Pandoc, run Rscript scripts/render_readme.R. -->



# WateRAT

Water Rights Allocation Tool for R (`WateRAT`) provides methods for retrieving and analyzing water allocation and modeled streamflow data.

Detailed setup, filtering, schema, targets, network-routing, and GeoPackage
documentation is published at <https://soilwaterfish.github.io/WateRAT/>.

The package uses Montana [Points of Diversion (POD)](https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip) and [FlowMet](https://www.fs.usda.gov/rm/boise/AWAE/projects/modeled_stream_flow_metrics.shtml) streamflow outputs to quantify water allocations relative to modeled August streamflow.

This work was originally developed as part of a master's thesis at Montana State University (Oestreich, 2023).

The package also includes a [{targets}](https://books.ropensci.org/targets/) pipeline for reproducibly updating and processing POD, FlowMet, NHDPlus, and administrative-boundary data. Parallel execution is managed with [`crew`](https://wlandau.github.io/crew/), with individual stream reaches processed as dynamic `targets` branches.

## Installation

Install the development version of `WateRAT` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("soilwaterfish/WateRAT")
```

To run the development pipeline, install the workflow packages if needed:

```r
install.packages(c(
  "targets",
  "tarchetypes",
  "crew"
))
```

The remaining spatial and data-processing dependencies are installed with `WateRAT` or can be installed separately if needed.

## Methods

The workflow accepts one or more watershed boundaries and combines local spatial data and external services using functions including:

* `get_flowmet()` for FlowMet streamflow data;
* `get_mtwr()` for Montana water-right POD data;
* `get_pod_basin()` for retrieving an upstream drainage basin for a single COMID;
* `fs_logic()` for separating Forest Service and private water allocations; and
* `capture_sites_within()` for summarizing PODs and allocated flow within each drainage area.

FlowMet stream reaches are joined to NHDPlus using the common COMID identifier. The analysis is restricted to greater than first-order Strahler streams and calculates water allocations relative to modeled August streamflow.

The resulting dataset includes attributes such as:

* `intersecting_sites`
* `intersecting_flow_all_together`
* `intersecting_flow_fs`
* `intersecting_flow_private`
* `maug_hist`
* `gnis_name`
* `intersecting_flow_all_together_percent`
* `intersecting_flow_fs_percent`
* `intersecting_flow_private_percent`

<div class="figure">
<img src="inst/www/flow_chart.png" alt="plot of chunk unnamed-chunk-2" width="200%" />
<p class="caption">plot of chunk unnamed-chunk-2</p>
</div>

<table>
  <tr>
    <td valign="top"><img src="inst/www/animated_wf.gif"/></td>
    <td valign="top"><img src="inst/www/animated_wf2.gif"/></td>
  </tr>
</table>

## Targets workflow

The project uses `targets` to manage dependencies, caching, parallel execution, and reproducible processing.

Run the pipeline from R with:


``` r
library(targets)

tar_make()
```

or from the command line:

```bash
Rscript -e 'targets::tar_make()'
```

Parallel execution is configured directly in `_targets.R` with `crew`, so the standard `tar_make()` command runs the parallel pipeline.

Useful commands for inspecting the workflow include:


``` r
# Visualize the dependency graph
tar_visnetwork()

# Show targets that need to be rebuilt
tar_outdated()

# Inspect pipeline progress
tar_progress()

# Inspect the target manifest
tar_manifest()

# Summarize crew worker use after a run
tar_crew()
```

### Starting from scratch

Normally, rerun:


``` r
tar_make()
```

and `targets` will reuse completed targets and only rebuild portions of the pipeline that are outdated.

To deliberately delete the entire target store and force a complete rebuild:


``` r
tar_destroy()
tar_make()
```

Use `tar_destroy()` only when a complete rebuild is desired because it removes the existing `_targets` data store and cached results.

## Parallel processing

Parallel execution is managed entirely by `targets` and `crew`.

The workflow uses two levels of branching:

1. `tar_map()` creates a static pipeline for each major watershed.
2. COMIDs within each watershed are exposed as dynamic branches for computationally independent operations.

Two `crew` worker pools are used because the NLDI basin retrieval and local spatial calculations have different resource requirements:

```text
100 available CPU cores

├── 8 workers  -> NLDI basin retrieval
├── 88 workers -> local spatial computation
└── 4 cores    -> operating system / targets / overhead
```

The `nldi` controller handles calls to `nhdplusTools::get_nldi_basin()`. Limiting this stage to a smaller number of workers prevents the workflow from sending dozens of simultaneous requests to the remote NLDI service.

The `compute` controller handles the remaining pipeline and provides up to 88 concurrent workers for local computation, including `capture_sites_within()`.

Conceptually, the pipeline is:

```text
Major watersheds
│
├── Kootenai
├── Clark Fork
├── Missouri
├── Yellowstone
└── Little Missouri
       │
       ▼
   FlowMet + NHDPlus
       │
       ▼
     COMIDs
       │
       ├── COMID 1 ── get_pod_basin()
       ├── COMID 2 ── get_pod_basin()
       ├── COMID 3 ── get_pod_basin()
       └── ...
              │
              ▼
           basins
              │
              ├── COMID 1 ── capture_sites_within()
              ├── COMID 2 ── capture_sites_within()
              ├── COMID 3 ── capture_sites_within()
              └── ...
                     │
                     ▼
               combined output
```

Each COMID branch is independently tracked and cached by `targets`.

This is particularly useful for the NLDI stage because a failed request is associated with a specific COMID branch instead of being hidden inside a larger mapping operation. Likewise, completed `capture_sites_within()` branches do not need to be recalculated when unrelated branches are rerun.

## Single-COMID basin retrieval

Basin retrieval is defined at the level of a single COMID so that `targets` can manage the parallel mapping.

The package helper has the following structure:


``` r
get_pod_basin <- function(comid, crs) {

  basin <- nhdplusTools::get_nldi_basin(
    list(
      featureSource = "comid",
      featureID = as.character(comid)
    )
  )

  # Some COMIDs may legitimately return no basin.
  if (is.null(basin) || nrow(basin) == 0L) {
    return(NULL)
  }

  basin <- sf::st_zm(basin)

  basin <- basin[
    !sf::st_is_empty(basin),
    ,
    drop = FALSE
  ]

  if (nrow(basin) == 0L) {
    return(NULL)
  }

  basin %>%
    dplyr::mutate(
      comid = as.character(comid)
    ) %>%
    sf::st_transform(
      crs = crs
    )
}
```

Network or service errors are allowed to propagate to `targets`. The corresponding dynamic branch is then recorded as failed rather than silently storing an incomplete result.

The NLDI dynamic target uses `error = "continue"` so other independent branches can continue running if an individual basin request fails.

A subsequent:

```r
tar_make()
```

can rerun unfinished or failed work while retaining successfully completed branches.

## Example `_targets.R`

The following shows the pipeline configuration for processing the major Montana basins.

Project-relative paths are used below. Adjust the input paths as needed for the local installation.


``` r
# -------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------

library(targets)
library(tarchetypes)
library(crew)


# -------------------------------------------------------------------
# Parallel worker pools
# -------------------------------------------------------------------

# Primary worker pool for local spatial and data-processing operations.
controller_compute <- crew::crew_controller_local(
  name = "compute",
  workers = 88
)

# Smaller worker pool for remote NLDI requests.
controller_nldi <- crew::crew_controller_local(
  name = "nldi",
  workers = 8
)

# Combine both pools into a single controller group.
controller <- crew::crew_controller_group(
  controller_compute,
  controller_nldi
)


# -------------------------------------------------------------------
# Targets options
# -------------------------------------------------------------------

tar_option_set(
  packages = c(
    "WateRAT",
    "nhdplusTools",
    "dplyr",
    "purrr",
    "sf"
  ),

  controller = controller,

  # Local compute pool is the default for pipeline targets.
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "compute"
    )
  )
)


# -------------------------------------------------------------------
# Major watershed boundaries
# -------------------------------------------------------------------

values <- tibble::tribble(
  ~basin_name,       ~basin_path,
  "kootenai",        "data/kootenai.shp",
  "clark_fork",      "data/clark_fork.shp",
  "missouri",        "data/missouri.shp",
  "yellowstone",     "data/yellowstone.shp",
  "little_missouri", "data/little_missouri.shp"
)


# -------------------------------------------------------------------
# Pipeline
# -------------------------------------------------------------------

targets <- tar_map(
  values = values,
  names = basin_name,

  # -----------------------------------------------------------------
  # Basin input
  # -----------------------------------------------------------------

  tar_target(
    basin,
    sf::read_sf(basin_path)
  ),

  tar_target(
    basin_crs,
    sf::st_crs(basin)
  ),


  # -----------------------------------------------------------------
  # Forest Service administrative boundary
  # -----------------------------------------------------------------

  tar_target(
    admin_int,
    suppressMessages(
      sf::read_sf("data/admin.shp") %>%
        sf::st_set_crs(4326) %>%
        sf::st_transform(crs = basin_crs) %>%
        sf::st_make_valid() %>%
        sf::st_intersection(basin) %>%
        sf::st_union() %>%
        sf::st_as_sf()
    )
  ),


  # -----------------------------------------------------------------
  # FlowMet
  # -----------------------------------------------------------------

  tar_target(
    flowmet_intersect,
    get_flowmet(
      filter_geom = basin,
      layer = "mean_summer_flow_historical_hires",
      local_path = "data/flowmet.gpkg"
    ) %>%
      sf::read_sf() %>%
      sf::st_zm() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_set_crs(4326) %>%
      dplyr::select(
        maug_hist,
        comid
      ) %>%
      sf::st_transform(crs = basin_crs) %>%
      sf::st_intersection(basin)
  ),


  # -----------------------------------------------------------------
  # NHDPlus
  # -----------------------------------------------------------------

  tar_target(
    nhdplus,
    nhdplusTools::get_nhdplus(
      sf::st_as_sfc(
        sf::st_bbox(flowmet_intersect)
      ),
      streamorder = 2
    )
  ),

  tar_target(
    flowmet_join_nhdplus,
    flowmet_intersect %>%
      dplyr::select(
        maug_hist,
        comid
      ) %>%
      dplyr::left_join(
        nhdplus %>%
          sf::st_drop_geometry() %>%
          dplyr::mutate(
            comid = as.character(comid)
          ),
        by = "comid"
      )
  ),


  # -----------------------------------------------------------------
  # Montana water-right PODs
  # -----------------------------------------------------------------

  tar_target(
    pou_pod_together,
    get_mtwr(
      basin,
      layer = "WRQS_PODS",
      local_path = "data/WRQS_Dataset_GDB.gdb"
    ) %>%
      dplyr::group_by(WRKEY) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  ),

  tar_target(
    pou_pod_together_sf,
    date_cleaning(
      pou_pod_together
    )
  ),


  # -----------------------------------------------------------------
  # Stream filtering
  # -----------------------------------------------------------------

  tar_target(
    flowmet_grt_strahler_1_order,
    flowmet_join_nhdplus %>%
      dplyr::filter(
        streamorde > 1
      )
  ),

  tar_target(
    crs,
    sf::st_crs(
      pou_pod_together_sf
    )
  ),


  # -----------------------------------------------------------------
  # COMIDs for basin delineation
  # -----------------------------------------------------------------

  tar_target(
    comids,
    unique(
      as.character(
        flowmet_grt_strahler_1_order$comid
      )
    )
  ),


  # -----------------------------------------------------------------
  # NLDI basin retrieval
  #
  # Each COMID is an independent dynamic branch.
  #
  # These branches are routed to the smaller "nldi" controller rather
  # than the primary compute pool.
  # -----------------------------------------------------------------

  tar_target(
    pod_basin,
    get_pod_basin(
      comids,
      crs
    ),

    pattern = map(comids),
    iteration = "list",
    error = "continue",

    resources = tar_resources(
      crew = tar_resources_crew(
        controller = "nldi"
      )
    )
  ),


  # -----------------------------------------------------------------
  # Combine successfully retrieved basins
  # -----------------------------------------------------------------

  tar_target(
    basins,
    pod_basin %>%
      purrr::compact() %>%
      dplyr::bind_rows() %>%
      sf::st_as_sf()
  ),


  # -----------------------------------------------------------------
  # Forest Service / private allocation logic
  # -----------------------------------------------------------------

  tar_target(
    pou_pod_together_fs_intersection,
    fs_logic(
      pou_pod_together_sf,
      admin_int
    )
  ),


  # -----------------------------------------------------------------
  # COMID-level spatial processing
  # -----------------------------------------------------------------

  tar_target(
    basin_comids,
    split(
      basins,
      basins$comid
    ),
    iteration = "list"
  ),


  # -----------------------------------------------------------------
  # Capture PODs and allocated flow within each basin
  #
  # Each basin is an independent dynamic branch.
  #
  # These branches use the primary "compute" worker pool.
  # -----------------------------------------------------------------

  tar_target(
    captured_sites,
    capture_sites_within(
      basin_comids,
      pou_pod_together_fs_intersection
    ),

    pattern = map(basin_comids),
    iteration = "list"
  ),


  # -----------------------------------------------------------------
  # Recombine COMID results
  # -----------------------------------------------------------------

  tar_target(
    adding_intersecting_flows,
    captured_sites %>%
      dplyr::bind_rows() %>%
      sf::st_as_sf()
  ),


  # -----------------------------------------------------------------
  # Final FlowMet join and allocation percentages
  # -----------------------------------------------------------------

  tar_target(
    pou_pod_together_sf_final_joined,
    adding_intersecting_flows %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(
        flowmet_grt_strahler_1_order %>%
          dplyr::select(
            comid,
            maug_hist,
            gnis_name,
            qe_08
          ),
        by = "comid"
      ) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(
        intersecting_flow_all_together_percent =
          (intersecting_flow_all_together / maug_hist) * 100,

        intersecting_flow_fs_percent =
          (intersecting_flow_fs / maug_hist) * 100,

        intersecting_flow_private_percent =
          (intersecting_flow_private / maug_hist) * 100
      )
  )
)

list(targets)
```

## Parallel execution architecture

With the configuration above, `targets` manages both worker pools:

```text
                       tar_make()
                           │
                           ▼
                    targets scheduler
                           │
              ┌────────────┴────────────┐
              │                         │
              ▼                         ▼
       NLDI controller          Compute controller
          8 workers                88 workers
              │                         │
              ▼                         ▼
       get_pod_basin()        Pipeline computation
       one COMID each                  +
                                  capture_sites_within()
                                   one COMID each
```

The maximum configured worker count is therefore:

```text
8 + 88 = 96 workers
```

on a system with 100 available CPU cores.

The remaining cores provide capacity for the primary R process, the `targets` scheduler, operating-system activity, and filesystem overhead.

`crew` scales each controller up to its configured worker limit according to the amount of work available. If only a small number of branches are ready, fewer workers are used.

## Why COMIDs are dynamic targets

Both basin retrieval and POD allocation are naturally independent at the COMID level.

For basin retrieval:

```text
COMID
  │
  └── get_pod_basin()
```

For water-right summarization:

```text
COMID basin
  │
  └── capture_sites_within()
```

Exposing these operations to `targets` rather than running an internal parallel map provides several benefits:

* individual COMIDs are independently cached;
* completed COMIDs remain complete if the workflow is interrupted;
* failed NLDI requests are visible as failed branches;
* failed work can be rerun without repeating successful branches;
* `targets` controls the global worker allocation;
* independent watersheds and COMIDs can run concurrently when dependencies allow; and
* worker pools can be matched to different types of work.

This keeps parallel execution at the pipeline level rather than creating nested worker pools inside individual functions.

## Monitoring parallel execution

While the workflow is running, standard system tools can be used to monitor CPU and memory utilization.

For example:

```bash
htop
```

Within R, pipeline status can be inspected with:

```r
tar_progress()
```

After the run, worker usage can be summarized with:

```r
tar_crew()
```

Because the pipeline contains dynamic targets, individual branch progress can also be inspected when debugging COMID-level failures.

## Running the full workflow

From the project root:

```bash
Rscript -e 'targets::tar_make()'
```

or from an interactive R session:

```r
library(targets)

tar_make()
```

Completed branches are stored by `targets`, so interrupted or partially completed pipelines can generally be resumed by running:

```r
tar_make()
```

again.

If an NLDI branch fails because of a temporary network or service issue, other independent branches can continue because the basin target uses:

```r
error = "continue"
```

A subsequent run can then attempt the unfinished work while retaining successfully completed COMID branches.

## Updating water-right data incrementally

Before replacing a Montana water-right download, create a snapshot that includes both active and retired PODs. Snapshots are local operational data and should stay outside the repository (or under the ignored `data/` directory).

```r
old_pods <- get_mtwr(
  basin,
  layer = "WRQS_PODS",
  local_path = "data/WRQS_Dataset_GDB.gdb",
  active_only = FALSE
) |>
  water_right_snapshot()

write_water_right_snapshot(old_pods, "data/snapshots/pods_previous.gpkg")
```

After downloading the replacement dataset, create a new snapshot, detect changes, and select the COMID basins to rebuild:

```r
previous <- read_water_right_snapshot("data/snapshots/pods_previous.gpkg")

current <- get_mtwr(
  basin,
  layer = "WRQS_PODS",
  local_path = "data/WRQS_Dataset_GDB.gdb",
  active_only = FALSE
) |>
  water_right_snapshot()

changes <- detect_water_right_changes(previous, current)
affected_comids <- affected_downstream_comids(changes, basins)
affected_basins <- affected_downstream_basins(changes, basins)
```

`changes` labels each POD as `added` or `retired`. A COMID is selected when its upstream basin contains a changed POD, so `affected_comids` is precisely the downstream set whose allocation metrics must be recalculated. Pass `affected_basins` to the `capture_sites_within()` dynamic branches instead of the full `basins` object, then merge the recalculated COMIDs into the previously published result.

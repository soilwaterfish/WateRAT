
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wrqur

Water Rights Quantification/Uses for R provides methods for retrieving
water allocation and flow data. It uses functions that call API’s or use
local data to compare [Points of Diversion
(POD)](https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip)
in Montana with
[FlowMet](https://www.fs.usda.gov/rm/boise/AWAE/projects/modeled_stream_flow_metrics.shtml)
flow outputs for the month of August. This work was developed by Brianna
Niehoff for her master’s thesis (Oestreich, 2023) at the Montana State
University.

In addition, the package uses a
[{targets}](https://books.ropensci.org/targets/) pipeline to help with
updating POD or FlowMet derivatives.

## Installation

You can install the development version of wrqur from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soilwaterfish/wrqur")
```

## Methods

The model takes in any basin configuration and then calls local files or
APIs with FlowMet `get_flowmet()`, Forest Service Administration
Boundaries `get_adminboundaries()`, and PODs `get_mtwr()`. From there,
utility functions help generate the intersecting FlowMet, Administration
Boundaries, and POD values via common identifiers (COMID) and basins
(see GIF below). This then relates all flow allocation metrics to \> 1st
order (Strahler) streamlines via COMID and provides the following
attributes:
`intersecting_sites, intersecting_flow_all_together, intersecting_flow_fs, intersecting_flow_private, MAUG_HIST, gnis_name, intersecting_flow_all_together_percent, intersecting_flow_fs_percent, intersecting_flow_private_percent`.

<img src="inst/www/flow_chart.png" alt="" width="200%" />

<table>

<tr>

<td valign="top">

<img src="inst/www/animated_wf.gif"/>
</td>

<td valign="top">

<img src="inst/www/animated_wf2.gif"/>
</td>

</tr>

</table>

## Example

The example below shows how the to call the `targets` package.

``` r
library(targets)

#This will run the targets workflow
tar_destroy()
tar_make() 

# or for parallel processing

tar_make_future(workers = 5)
```

If needed, you can change the `_targets.R` file to adjust for local/API
calls or starting basins.

``` r

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)

# Set target options:
tar_option_set(packages = c( "wrqur", "nhdplusTools", "furrr","tidyverse",  "sf")
)

### This `basin_entry` will depend on the user defined watershed boundary.
values = dplyr::tibble(values = c("data/kootenai.shp",
                                  "data/clark_fork.shp",
                                  "data/missouri.shp",
                                  "data/yellowstone.shp"
                                  "data/little_missouri.shp"
                                  ))
targets <- tar_map(

values = values,

tar_target(basin, sf::read_sf(values)),

tar_target(basin_crs, sf::st_crs(basin)),

tar_target(admin_int, suppressMessages(get_adminboundaries(filter_geom = sf::st_bbox(sf::st_transform(basin, 4269)),
                                                           where = "OWNERCLASSIFICATION='USDA FOREST SERVICE' OR OWNERCLASSIFICATION='UNPARTITIONED RIPARIAN INTEREST'")%>%
                                         sf::st_transform(crs = basin_crs) %>%
                                         sf::st_make_valid() %>%
                                         sf::st_intersection(basin) %>%
                                         sf::st_union() %>%
                                         sf::st_as_sf())),

tar_target(flowmet_intersect, get_flowmet(filter_geom = basin, local_path = r"{Z:\Downloads\S_USA.Hydro_FlowMet_1990s.gdb\S_USA.Hydro_FlowMet_1990s.gdb}")  %>%
             sf::read_sf()%>%
             sf::st_zm() %>%
             dplyr::select(c("MAUG_HIST", "COMID")) %>%
                                      sf::st_transform(crs = basin_crs) %>%
                                      sf::st_intersection(basin)),

tar_target(nhdplus, nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(flowmet_intersect)))),

tar_target(flowmet_join_nhdplus, flowmet_intersect %>% dplyr::select(MAUG_HIST, COMID) %>%
    dplyr::left_join(nhdplus %>%
                       sf::st_drop_geometry() %>%
                       dplyr::mutate(comid = as.character(comid)), by = c('COMID' = 'comid'))
),

tar_target(pou, get_mtwr(basin, layer = 'WR1POU', local_path =  r'{Z:\Downloads\MTWaterRights.gdb\MTWaterRights.gdb}') %>%
             sf::read_sf() %>%
             dplyr::group_by(WRKEY) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup()),

tar_target(pod, get_mtwr(basin, layer = 'WR1DIV', local_path =  r'{Z:\Downloads\MTWaterRights.gdb\MTWaterRights.gdb}') %>%
             sf::read_sf() %>%
             dplyr::group_by(WRKEY) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup() %>%
             dplyr::filter(WRKEY %in% pou$WRKEY)),

tar_target(pou_pod_together, suppressMessages(pod %>%
                                                  dplyr::left_join(pou %>%
                                                                     sf::st_drop_geometry() %>%
                                                                     dplyr::select(c("WRKEY", "PURPOSE", "IRRTYPE", "MAXACRES", "FLWRTGPM", "FLWRTCFS", "VOL", "ACREAGE"))))
),

tar_target(pou_pod_together_sf, date_cleaning(pou_pod_together)),

tar_target(flowmet_grt_strahler_1_order, flowmet_join_nhdplus %>% filter(streamorde > 1)),

tar_target(crs, sf::st_crs(pou_pod_together_sf)),

tar_target(basins, get_pod_basins(flowmet_grt_strahler_1_order, crs)),

tar_target(pou_pod_together_fs_intersection, fs_logic(pou_pod_together_sf, admin_int)),

tar_target(adding_intersecting_flows, basins %>% split(.$COMID) %>%
             furrr::future_map(
               ~capture_sites_within(.x, pou_pod_together_fs_intersection)) %>%
             dplyr::bind_rows() %>%
             sf::st_as_sf()),
tar_target(pou_pod_together_sf_final_joined, adding_intersecting_flows %>%
             st_drop_geometry() %>%
             left_join(flowmet_grt_strahler_1_order %>% select(COMID,MAUG_HIST, gnis_name, qe_08)) %>%
             st_as_sf() %>%
             mutate(
               intersecting_flow_all_together_percent = (intersecting_flow_all_together/MAUG_HIST)*100,
               intersecting_flow_fs_percent = (intersecting_flow_fs/MAUG_HIST)*100,
               intersecting_flow_private_percent = (intersecting_flow_private/MAUG_HIST)*100
             ))
)


list(targets)
```

## Running the `targets` Pipeline in Parallel

This pipeline uses [`targets`](https://books.ropensci.org/targets/) with
`crew` to parallelize processing across watershed COMIDs.

The computationally expensive `capture_sites_within()` operation is
implemented as a **dynamic target**, so individual COMIDs can be
distributed across workers.

### 1. Install required packages

From R:

``` r
install.packages(c(
  "targets",
  "tarchetypes",
  "crew"
))
```

The project-specific packages must also be installed and available to
the workers:

``` r
install.packages(c(
  "nhdplusTools",
  "furrr",
  "tidyverse",
  "sf"
))
```

The `wrqur` package must also be installed.

------------------------------------------------------------------------

## 2. Configure parallel workers

The HPC node has approximately 100 CPU cores available.

The pipeline is configured to use **96 concurrent workers**, leaving
several cores available for the operating system, the main `targets`
process, filesystem operations, and other overhead.

At the beginning of `_targets.R`:

``` r
library(targets)
library(tarchetypes)
library(crew)

tar_option_set(
  packages = c(
    "wrqur",
    "nhdplusTools",
    "tidyverse",
    "sf"
  ),
  controller = crew::crew_controller_local(
    workers = 96
  )
)
```

`future`, `future.callr`, and `furrr` are not required for
pipeline-level parallelism.

In particular, do **not** use:

``` r
plan(callr)
```

or run the pipeline with:

``` r
tar_make_future(workers = 50)
```

The `crew` controller now manages parallel execution.

------------------------------------------------------------------------

## 3. Parallelize `capture_sites_within()`

Instead of running `furrr::future_map()` inside one large target, split
the basin into individual COMIDs and expose those COMIDs to `targets` as
dynamic branches.

Replace:

``` r
tar_target(
  adding_intersecting_flows,
  basins %>%
    split(.$comid) %>%
    furrr::future_map(
      ~ capture_sites_within(
        .x,
        pou_pod_together_fs_intersection
      )
    ) %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf()
)
```

with:

``` r
tar_target(
  basin_comids,
  split(
    basins,
    basins$comid
  ),
  iteration = "list"
),

tar_target(
  captured_sites,
  capture_sites_within(
    basin_comids,
    pou_pod_together_fs_intersection
  ),
  pattern = map(basin_comids),
  iteration = "list"
),

tar_target(
  adding_intersecting_flows,
  captured_sites %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf()
)
```

Because this code occurs inside the existing `tar_map()`, each major
watershed still has its own pipeline while the individual COMIDs within
each watershed become dynamic branches.

Conceptually:

``` text
Kootenai
   ├── COMID 1 ── capture_sites_within()
   ├── COMID 2 ── capture_sites_within()
   ├── COMID 3 ── capture_sites_within()
   └── ...

Clark Fork
   ├── COMID 1 ── capture_sites_within()
   ├── COMID 2 ── capture_sites_within()
   └── ...

Missouri
   ├── COMID 1 ── capture_sites_within()
   └── ...

Yellowstone
   └── ...

Little Missouri
   └── ...
```

`targets` can then schedule up to 96 ready COMID branches
simultaneously.

------------------------------------------------------------------------

## 4. Run the pipeline

From the project directory:

``` bash
Rscript -e 'targets::tar_make()'
```

Or interactively from R:

``` r
library(targets)

tar_make()
```

Do **not** use:

``` r
tar_make_future()
```

Parallel execution is controlled by the `crew` controller defined in
`_targets.R`.

------------------------------------------------------------------------

## 5. Inspect the pipeline before running

To visualize the target dependency graph:

``` r
targets::tar_visnetwork()
```

To list targets:

``` r
targets::tar_manifest()
```

To see which targets are currently outdated and need to run:

``` r
targets::tar_outdated()
```

------------------------------------------------------------------------

## 6. Re-running the pipeline

One major advantage of making `capture_sites_within()` a dynamic target
is that each COMID is independently cached.

If the pipeline stops partway through, simply run:

``` r
tar_make()
```

again.

Completed branches remain cached and `targets` will continue with
branches that still need to run.

Similarly, if an upstream dependency changes, `targets` determines which
COMID branches need to be rebuilt.

------------------------------------------------------------------------

## Worker count

Default:

``` r
workers = 96
```

If memory or filesystem I/O becomes limiting, reduce the worker count:

``` r
controller = crew::crew_controller_local(
  workers = 64
)
```

or:

``` r
controller = crew::crew_controller_local(
  workers = 80
)
```

For `sf` operations, increasing CPU workers can substantially increase
memory use because multiple workers may simultaneously load or operate
on large spatial objects.

A good initial test is therefore:

``` r
workers = 96
```

while monitoring CPU utilization, RAM, and I/O.

If all cores remain busy and memory usage is acceptable, retain 96
workers. If the node spends substantial time in I/O wait or memory
pressure increases, reduce the number of workers.

------------------------------------------------------------------------

## Full execution

Once `_targets.R` has been configured:

``` bash
cd /path/to/project

Rscript -e 'targets::tar_make()'
```

The intended parallelization hierarchy is:

``` text
targets
   │
   ├── major watersheds
   │
   └── COMID dynamic branches
          │
          └── capture_sites_within()
                 │
                 └── up to 96 concurrent workers
```

`targets`/`crew` should control the worker pool globally. Avoid starting
additional `future`, `furrr`, or other nested parallel worker pools
inside `capture_sites_within()`.

## References

Oestreich, B.L. (2023). QUANTIFYING WATER SUPPLY AND DEMAND ACROSS
NATIONAL FOREST SYSTEM LANDS WITHIN THE CLARK FORK RIVER WATERSHED,
MONTANA. Montana State University.

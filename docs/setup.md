---
title: Setup and local data
---

# Setup and local data

## Install the package

```r
remotes::install_github("soilwaterfish/WateRAT")
```

For development, open the project and load the package with:

```r
devtools::load_all()
```

The pipeline also needs `targets`, `tarchetypes`, and `crew`.

## Keep source data local

Large geodatabases, GeoPackages, target stores, scratch files, and development
artifacts belong under ignored local paths such as `data/` and `dev/`. Do not
commit them. The README should link to the authoritative dataset provider
instead.

The current workflow expects local copies of:

| Input | Purpose |
| --- | --- |
| Montana water-right geodatabase | Montana `WRQS_PODS` source layer |
| Idaho IDWR geodatabase | Idaho `PODRight` source layer |
| FlowMet GeoPackage | historical mean-August flow (`maug_hist`) |
| `states.gpkg`, layer `states` | analysis boundaries with `state_abbr` |
| Forest Service administration layer | Forest Service/private allocation split |

Update the local paths in `_targets.R` to match the machine running the
workflow. They are intentionally not portable repository assets.

## Build Idaho’s canonical cache deliberately

Idaho requires requests to individual `WRReport` pages to obtain the Water Uses
table. Build its canonical cache explicitly before a full pipeline run:

```r
cache_idwr_water_rights(
  local_path = "data/idwr.gdb",
  filter_geom = idaho_boundary,
  cache_path = "data/idaho_water_rights.gpkg"
)
```

This separates a potentially long external scrape from ordinary `targets`
rebuilds. The resulting GeoPackage is local and ignored by Git.

## Smoke mode

For a small local check, first create the ignored smoke inputs, then run:

```bash
WATERAT_RUN_MODE=smoke Rscript -e 'targets::tar_make()'
```

Full mode is the default. Smoke mode reduces worker counts and uses the smoke
boundary/cache paths defined in `_targets.R`.

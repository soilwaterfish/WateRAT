# State adapters

WateRAT analyzes a single canonical water-right schema. State data must be
converted to this schema before it enters `fs_logic()` or
`capture_sites_within()`; those utilities do not contain state-specific field
names or conversions.

## Workflow

1. Register a state input in `_targets.R` with a two-letter `state` code, its
   local dataset path, and (when required) a canonical cache path. Boundaries
   are read from `data/states.gpkg`, layer `states`, using `state_abbr`.
2. Retrieve and filter the state's source data before any remote report
   requests. For Idaho, use `get_idwr_pods()` with `source`, `status`,
   `exclude_source`, and `exclude_status`.
3. Expand any one-to-many source attributes. Idaho's Water Uses table produces
   one record per beneficial use with `expand_idwr_pods_by_use()`.
4. Standardize with a state adapter. `standardize_idwr_water_rights()` maps IDWR
   records to the fields listed by `water_right_schema()`; it preserves reported
   units and only populates `max_flow_cfs` when a diversion rate is reported in
   CFS.
5. Apply the analysis-month filter. `filter_water_rights_month()` retains the
   rate active on the 15th of the requested month. Idaho's cache builder uses
   August by default, so a 12-row monthly minimum-streamflow schedule contributes
   only its August rate to August FlowMet comparisons. Annual records remain.
6. Validate with `validate_water_rights()`. Adapters must supply a point
   geometry, unique `record_id`, stable `right_id` and `site_id`, numeric
   `max_flow_cfs`, and logical `is_instream`.
7. Run common analysis. The canonical output goes through `fs_logic()` and the
   COMID-level `capture_sites_within()` branches unchanged for every state.
8. Add conversion rules explicitly before treating non-CFS rates as comparable.
   Missing `max_flow_cfs` is intentional: it prevents unsupported unit
   conversions from entering allocation totals.

## Idaho example

```r
idaho_pods <- get_idwr_pods(
  local_path = "data/idwr_pod_rights.gdb",
  filter_geom = idaho_boundary,
  exclude_source = "GROUND WATER"
)

idaho_water_rights <- idaho_pods |>
  expand_idwr_pods_by_use() |>
  standardize_idwr_water_rights()
```

`idaho_water_rights` is the object supplied to the common pipeline. For a
small test, filter to a compact boundary first: each retained distinct IDWR
report requires one request to the IDWR website.

## Idaho cache used by targets

The `_targets.R` state branch reads `data/idaho_water_rights.gpkg`, layer
`water_rights`. Create or refresh it separately so `tar_make()` never starts a
state-wide scrape unexpectedly:

```r
idaho_boundary <- sf::read_sf("data/states.gpkg", layer = "states") |>
  dplyr::filter(state_abbr == "ID")

cache_idwr_water_rights(
  local_path = "data/_ags_data25175638F3334C56901BED08795AB38A.gdb",
  filter_geom = idaho_boundary,
  cache_path = "data/idaho_water_rights.gpkg",
  exclude_source = "GROUND WATER",
  month = 8
)
```

## End-to-end smoke test

Prepare a compact, ignored test dataset. This selects three eligible PODs in
each state, buffers them by ten kilometres, and makes only the selected Idaho
report requests:

```r
prepare_smoke_test()
```

Then start a fresh R session and run the pipeline in smoke mode:

```r
Sys.setenv(WATERAT_RUN_MODE = "smoke")
targets::tar_destroy()
targets::tar_make()
```

Use `tar_visnetwork()` or `tar_progress()` to inspect the two compact state
branches. Set `WATERAT_RUN_MODE` back to `"full"` and call `tar_destroy()`
before switching back to the full pipeline; the state input paths differ by
mode.

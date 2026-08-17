---
title: Targets framework
---

# Targets framework

`_targets.R` makes the state workflow reproducible and cache-aware. A static
`tar_map()` branch is created for each state, using the state boundary and its
registered adapter.

```text
state boundary
  ├─ FlowMet clip + NHDPlus attributes
  ├─ Forest Service ownership clip
  └─ canonical state water rights
       └─ capture/accumulation output
```

The pipeline uses two `crew` worker pools:

| Pool | Full mode | Smoke mode | Purpose |
| --- | ---: | ---: | --- |
| `compute` | 88 | 2 | local spatial/data processing |
| `nldi` | 8 | 2 | rate-limited NLDI work |

Run the pipeline with:

```r
targets::tar_make()
```

Useful inspection commands:

```r
targets::tar_visnetwork()
targets::tar_outdated()
targets::tar_progress()
targets::tar_manifest()
```

`targets` rebuilds only outdated work. Use `targets::tar_destroy()` only when a
complete cache reset is intended.

## Established and network paths

The established targets path retrieves one upstream NLDI basin per reporting
COMID and calls `capture_sites_within()` to total PODs within that polygon. It
is retained as the validation baseline.

The network functions on this branch are the replacement path under
development. They prepare POD COMIDs once, route allocations through NHDPlus
edges, and then produce the same final allocation field names. Keeping both
methods available makes line-by-line validation possible before the network
path replaces the basin branches in `_targets.R`.

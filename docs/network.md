---
title: NHDPlus network accumulation
---

# NHDPlus network accumulation

The network method avoids repeating an upstream-basin polygon request for every
flowline. Instead it starts from local allocations and propagates them through
the directed NHDPlus network.

```text
canonical POD point
  → NLDI hydrolocation
  → routable flowline COMID
  → local COMID allocation totals
  → Hydroseq / DnHydroseq edges
  → downstream cumulative totals
  → publish order 2+ reaches
```

## POD-to-COMID assignment

`nldi_comid_function()` uses NLDI’s `hydrolocation` endpoint. It returns a
flowline COMID by snapping a nearby point or tracing downhill to a connected
flowline. This differs from a catchment-only point-in-polygon lookup and avoids
assigning a POD to a non-routable catchment identifier.

`index_water_right_comids()` can checkpoint its results to an RDS file and use
a small number of parallel workers:

```r
index_water_right_comids(
  water_rights,
  cache_path = "data/state_hydrolocation.rds",
  workers = 4L
)
```

The cache records the endpoint used. A cache from an older lookup method is
ignored rather than mixed with hydrolocation results.

## Routing rules

`nhdplus_network_edges()` connects each reach’s `dnhydroseq` to the downstream
reach’s `hydroseq`. `accumulate_downstream_allocations()` processes those edges
in topological order:

- at a confluence, upstream allocation totals are added;
- at a divergence, totals propagate along each supplied downstream path; and
- a cycle in the supplied network is an error.

Fetch **all** stream orders for routing. A first-order reach may contain a POD
whose allocation must travel into a higher-order reporting reach. Only after
accumulation should `filter_reportable_reaches(minimum_streamorder = 2)` remove
first-order rows from the published layer.

## Comparable field names

`aggregate_local_allocations()` computes the same all/instream/non-instream and
Forest Service/private measures as `capture_sites_within()`. Then
`format_capture_sites_output()` converts cumulative totals to the established
output names. This is intentional: the basin and network methods can be tested
on the same reporting lines without a column-translation step.

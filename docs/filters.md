---
title: State adapters and filtering
---

# State adapters and filtering

State-specific retrieval belongs in an adapter. Downstream analysis receives
only validated records in the canonical schema, so it does not need to know
whether a record originated in Montana, Idaho, or a future state source.

## Montana

`get_state_water_rights("MT", ...)` reads the `WRQS_PODS` layer through
`get_mtwr()`, then applies the Montana filtering and date normalization used by
the package:

- `WR_STATUS = ACTIVE`;
- a non-missing `MAX_FLOW_CFS`;
- surface-water source type; and
- an August diversion-period selection where a period is supplied.

The adapter converts Montana field names into the canonical schema and creates
stable IDs such as `MT:<PODV_ID_SEQ>`.

## Idaho

`get_idwr_pods()` filters `PODRight` records before any web requests:

- by an optional spatial boundary;
- `Status = Active` by default;
- optional included/excluded source and status values; and
- ground water is normally excluded when building the cache.

`expand_idwr_pods_by_use()` then scrapes the report page’s **Water Uses** table.
One POD can produce several records: one per beneficial use and diversion
period. The `TOTAL` row is excluded. Diversion-rate and volume text are split
into numeric values and their reported units.

`filter_water_rights_month(month = 8)` retains annual records plus records
whose reported diversion period includes August 15. This is essential for
month-by-month Idaho authorizations: a January rate must not be substituted for
the August rate.

## Forest Service classification

`fs_logic()` adds `fs_intersection` by spatially intersecting canonical POD
points with USDA Forest Service ownership polygons. Ownership-class matching is
case-insensitive (`USDA FOREST SERVICE`) because source capitalization varies.

The classification is not a water-right status. It is used only to split each
allocation into Forest Service and private totals in the final output.

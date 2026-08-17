---
title: Final GeoPackage contract
---

# Final GeoPackage contract

The final network GeoPackage is intended for GIS review and comparison with the
established basin output. It has a published flowline layer and optional audit
layers for PODs and their COMID assignments.

## Published flowline layer

The final join deliberately keeps only the fields needed for interpretation;
it does **not** copy the full NHDPlus attribute table.

| Field | Description |
| --- | --- |
| `comid` | Reporting NHDPlus flowline COMID |
| `intersecting_flow_all_together` | Total upstream authorized allocation, CFS |
| `intersecting_flow_all_together_instream` | Instream component, CFS |
| `intersecting_flow_all_together_non_instream` | Non-instream component, CFS |
| `intersecting_flow_fs` | Forest Service component, CFS |
| `intersecting_flow_fs_instream` | Forest Service instream component, CFS |
| `intersecting_flow_fs_non_instream` | Forest Service non-instream component, CFS |
| `intersecting_flow_private` | Private component, CFS |
| `intersecting_flow_private_instream` | Private instream component, CFS |
| `intersecting_flow_private_non_instream` | Private non-instream component, CFS |
| `maug_hist` | FlowMet historical mean-August flow |
| `qe_08` | NHDPlus August flow attribute retained for reference |
| `intersecting_flow_all_together_percent` | `100 × total / maug_hist` |
| `intersecting_flow_fs_percent` | `100 × FS / maug_hist` |
| `intersecting_flow_private_percent` | `100 × private / maug_hist` |
| geometry | Reporting flowline geometry |

Values above 100% are not automatically errors. They identify reaches where
the summed authorized allocation exceeds the selected modeled August-flow
metric and deserve data and hydrologic review.

## Audit layers

When written, the POD layer contains the canonical POD attributes plus:

- `comid`: the hydrolocation flowline assignment; and
- `nldi_error`: an NLDI failure message, if one occurred.

The COMID index layer is a compact audit table with `record_id`, `comid`, and
`nldi_error`. It allows a GIS or R review to trace every published allocation
back to the POD records that entered the network.

## Join logic

The final formatting step joins only FlowMet `maug_hist` to the accumulated
network result and retains NHDPlus `qe_08`. This prevents the output from being
cluttered by NHDPlus routing/geometry metadata while preserving the two flow
metrics used for interpretation.

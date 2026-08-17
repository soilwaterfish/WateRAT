---
title: Canonical water-right schema
---

# Canonical water-right schema

Every state adapter must return an `sf` point layer that passes
`validate_water_rights()`. These fields are the data contract for all shared
utilities and targets.

| Field | Meaning |
| --- | --- |
| `state` | Two-letter state abbreviation |
| `right_id` | State-issued water-right identifier |
| `site_id` | Stable point-of-diversion identifier |
| `record_id` | Unique POD/use record identifier; non-missing and unique |
| `status` | Source-system right status |
| `source` | Source waterbody or source type |
| `beneficial_use` | Use associated with this record |
| `diversion_start`, `diversion_end` | `MM/DD` diversion period, when supplied |
| `max_flow_cfs` | Authorized rate in cubic feet per second |
| `diversion_rate`, `diversion_rate_unit` | Original reported rate and unit |
| `volume`, `volume_unit` | Original reported volume and unit |
| `is_instream` | Logical instream-use flag |
| `report_url` | Source report URL, when available |
| geometry | POD point geometry |

## Rules that protect comparability

`max_flow_cfs` must be numeric and `is_instream` must be logical. Do not make
an unsupported unit conversion merely to fill `max_flow_cfs`: Idaho rates not
reported as CFS remain missing until a documented conversion is added.

State-specific source fields may remain on an intermediate object, but shared
functions must use only canonical fields. This keeps allocation, ownership, and
network code independent of the state data source.

## Optional network-preparation fields

After preparation, a POD layer may also contain:

| Field | Meaning |
| --- | --- |
| `comid` | Routable NHDPlus flowline COMID returned by NLDI hydrolocation |
| `nldi_error` | Request error when a COMID could not be assigned |
| `fs_intersection` | Forest Service ownership flag |

These are derived fields, not part of the minimal state-adapter contract.

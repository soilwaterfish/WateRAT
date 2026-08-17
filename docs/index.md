---
title: WateRAT documentation
---

# WateRAT

WateRAT (Water Rights Allocation Tool for R) standardizes state water-right
points of diversion (PODs), relates their authorized allocations to modeled
August flow, and produces reviewable geospatial outputs.

This site describes the package and workflow currently developed on the
`network-accumulation` branch. Source datasets are deliberately **not** kept in
the repository. Obtain them locally from their publishers, then configure the
paths described in the setup guide.

## Documentation

- [Setup and local data](setup.html)
- [State adapters and filtering](filters.html)
- [Canonical water-right schema](schema.html)
- [Targets pipeline](targets.html)
- [NHDPlus network accumulation](network.html)
- [Final GeoPackage contract](output-gpkg.html)

## What the output means

Each published flowline has a COMID, modeled August flow (`maug_hist`), NHDPlus
August flow (`qe_08`), and the water-right allocation totals that reach it.
The allocation fields use the same names as the established
`capture_sites_within()` workflow, making basin-based and network-based results
comparable during validation.

The network method is being developed alongside the established basin method.
It routes each POD’s allocation through NHDPlus connectivity instead of
retrieving and intersecting a separate upstream basin polygon for every
reporting reach.

## Publishing this site

The repository includes a GitHub Actions Pages workflow. In the GitHub
repository settings, set **Pages → Build and deployment → Source** to
**GitHub Actions**. A push to the configured branch then deploys this `docs/`
site to the repository’s GitHub Pages URL.

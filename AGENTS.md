# WaterRAT contributor instructions

## Versioning

Before creating any Git commit, increment `Version` in `DESCRIPTION` and
include that change in the same commit. For routine development work, increase
the final development component (for example, `0.0.0.9001` to
`0.0.0.9002`). Use a deliberate semantic version change only when a release is
being prepared.

## Local data

Do not commit local source datasets, caches, targets stores, or derived GIS
outputs. Keep them under the ignored `data/` or `dev/` paths. Small fixtures
needed by package tests belong in `inst/extdata/`.

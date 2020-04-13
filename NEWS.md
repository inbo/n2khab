## n2khab 0.1.2

#### Hotfix

- `read_GRTSmh()`, `read_GRTSmh_base4frac()`, `read_GRTSmh_diffres()`: avoided CRS error with more recent GDAL/PROJ.4 installations (#61)

## n2khab 0.1.1

#### Hotfix

- `read_habitatmap_terr()`: fixed non-functioning argument `keep_aq_types=FALSE` (#60)

## n2khab 0.1.0

#### Features of the first stable release

- Core reading and preprocessing functions
- Several built-in checklists (among which: `types` and `env_pressures`)
- Function documentation
- 4 tutorials (vignettes / articles)
- A reexport of `download_zenodo()` from `inborutils`
- Website built with `pkgdown`

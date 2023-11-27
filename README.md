<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3631579.svg)](https://doi.org/10.5281/zenodo.3631579)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/inbo/n2khab/workflows/R-CMD-check/badge.svg)](https://github.com/inbo/n2khab/actions?query=workflow%3AR-CMD-check)
[![inbo r-universe-name](https://inbo.r-universe.dev/badges/:name?color=c04384)](https://inbo.r-universe.dev)
[![inbo r-universe package status](https://inbo.r-universe.dev/badges/n2khab)](https://inbo.r-universe.dev)
<!-- badges: end -->

## Welcome

The **n2khab** R package provides preprocessing functions and standard reference data, supporting _reproducible_ and _transparent_ analyses on Flemish Natura 2000 (**n2k**) habitats (**hab**) and regionally important biotopes (RIBs).

The package's core aim is to **provide readily standardized (preprocessed) data** in your R environment.
This facilitates collaboration and reproducibility.

The standard reference data include: checklists, spatial habitat distribution, administrative & environmental layers, GRTSmaster_habitats.

## Installing and using the _n2khab_ package

To install the current package version from the `main` branch (latest stable release), run:

```r
install.packages("n2khab", repos = c(inbo = "https://inbo.r-universe.dev", 
                                     CRAN = "https://cloud.r-project.org"))
```

The above provides a pre-compiled package for Windows and macOS, which should be faster than below approach.
INBO staff should have the INBO repository enabled already (check with `getOption("repos")`), in which case **`install.packages("n2khab")`** is all you need!

If you want to install from the source repository, run:

```r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true") # as a precaution
remotes::install_github("inbo/n2khab",
                        build_vignettes = TRUE,
                        upgrade = TRUE)
```

Note that this will install the package from the `main` branch.
If you need a version from another branch, add the `ref` argument in the above function to provide the branch name.

Repeat the installation when you wish to upgrade.

Have a look at the vignettes to quickly find your way!

```r
help(package = "n2khab")
# vignettes only: browseVignettes("n2khab")
# documentation of whole package: package?n2khab
```

### Data setup

Please take note that you must set up the needed data sources as explained in `vignette("v020_datastorage")` and demonstrated in `vignette("v022_example")`.
There is a major distinction between:

- **raw data** ([Zenodo-link](https://zenodo.org/communities/n2khab-data-raw)), to be stored in a folder `n2khab_data/10_raw`;
- **processed data** ([Zenodo-link](https://zenodo.org/communities/n2khab-data-processed)), to be stored in a folder `n2khab_data/20_processed`.


## You are welcome to contribute!

Please have a look at our [contributing guide](.github/CONTRIBUTING.md)!


## Intention for the future

At a later date, the intention is to incorporate functionality to enhance workflow reproducibility and ease the setup:

- let a user declare which versions of which data sources are used in an R workflow (at the beginning of a script or R markdown file);
- perform checks whether the needed versions of those data sources are locally present;
- if missing, download the needed data from the Zenodo collections.

Currently these aspects must be taken care of in a more manual fashion.
See `vignette("v022_example")` for example code to currently accomplish specific aspects.

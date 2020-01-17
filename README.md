## Welcome

[![Build Status](https://travis-ci.com/inbo/n2khab.svg?branch=master)](https://travis-ci.com/inbo/n2khab)

The **n2khab** R package provides data definitions, standard checklists (reference data) and preprocessing functions, supporting _reproducible_ and _transparent_ analyses on Flemish Natura 2000 (**n2k**) habitats (**hab**) and regionally important biotopes (RIBs).

The package's core aim is to **provide readily standardized (preprocessed) data** in your R environment.
This facilitates collaboration and reproducibility.


## Installing, testing and using the _n2khab_ package

To install, run:

```r
remotes::install_github("inbo/n2khab", build_vignettes = TRUE)
```

Note that this will install the package from the `master` branch.
If you need a version from another branch, add the `ref` argument in the above function to provide the branch name.

Have a look at the vignettes to quickly find your way!

```r
help(package = "n2khab")
# vignettes only: browseVignettes("n2khab")
# documentation of whole package: package?n2khab
```


## You are welcome to contribute!

Please have a look at our [contributing guide](.github/CONTRIBUTING.md)!


## Background information

From its aim, it follows that the package supports various _N2KHAB_ projects, i.e. projects that focus on Natura 2000 habitat (and which may as well use the _n2khab_-prefix for their git repository name, such as [this one](https://github.com/inbo/n2khab-monitoring)).

The package is a companion to the [n2khab-preprocessing](https://github.com/inbo/n2khab-preprocessing) repo.
Beside direct preprocessing functions, **n2khab** provides several functions that return datasets, generated in `n2khab-preprocessing`, as standardized R-objects.
Having processed datasets readily available is useful in the case of time-consuming calculations to produce them.

The data definitions & standard checklists (reference data) provided by the package are textual files in the [vc-format](https://ropensci.github.io/git2rdata/index.html).
Some of them come over from another repository.
Others may be written as the result of a synchronization script to give them a start.
The code to reproduce these files is inside the `src` folder.
The package provides the necessary functions to return these as standardized tibbles (`vignette("v010_reference_lists")`).

This package is the successor of the 'n2khabutils' package.
Previous to commit `c8608d9`, the code was part of the [n2khab-monitoring](https://github.com/inbo/n2khab-monitoring) repo (formerly 'n2khab-inputs'), where the original version history remains stored.
At that time, the package was called 'n2khabutils'.
As a convenience, the **n2khab** repo still holds the rewritten (shrinked) package history from before commit `c8608d9`, as defined by the related files and folders.
See [this](https://github.com/inbo/n2khab-monitoring/issues/28) issue in the 'n2khab-monitoring' repo, where the migration is documented.


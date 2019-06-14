## Welcome

The **n2khab** R package provides data definitions, standard checklists (reference data) and preprocessing functions, supporting _reproducible_ and _transparent_ analyses on Flemish Natura 2000 (**n2k**) habitats (**hab**) and regionally important biotopes (RIBs).
It consequently supports various _N2KHAB_ projects, i.e. projects that focus on Natura 2000 habitat (and which may as well use the _n2khab_-prefix for their git repository name, such as [this one](https://github.com/inbo/n2khab-monitoring)).

The package is a companion to the [n2khab-preprocessing](https://github.com/inbo/n2khab-preprocessing) repo.
**n2khab** provides several functions that return datasets, generated in that repository, as standardized R-objects.

The data definitions & standard checklists (reference data) provided by the package are textual files in the [vc-format](https://ropensci.github.io/git2rdata/index.html).
Some of them come over from another repository.
Others may be written as the result of a synchronization script to give them a start.
The code to reproduce these files is inside the `src` folder.
The package provides the necessary functions to return these as standardized tibbles (`vignette("vign-010_reference_lists"`)).

This package is the successor of the 'n2khabutils' package; more information [at the bottom](#package-history).


### Find your way: repository structure

This is the structure of the repo:

```
├── src                 <- Package-related scripts / R markdown files. Rbuild-ignored!
                           Contains a script on package management + a bookdown 
                           project to reproduce the included textual data + a 
                           script to upgrade vc-formatted files.
├── inst
    └── textdata        <- Textual data delivered with the package (in vc-format).
                           They can be read into R by package functions or with
                           git2rdata::read_vc().
├── man
├── R                   <- Package functions are to be made here.
├── DESCRIPTION
├── LICENSE
├── n2khab.Rproj        <- RStudio project file
├── NAMESPACE
└── README.md
```

### Installing, testing and using the _n2khab_ package

To install, run:

```r
remotes::install_github("inbo/n2khab", 
                        build_opts = c("--no-resave-data", "--no-manual"))
```

Have a look at the vignettes to quickly find your way!

```r
help(package = "n2khab")
# vignettes only: browseVignettes("n2khab")
```


### You are welcome to contribute!

It is wise to first think about the scope of your function (or your proposed enhancement of an exisiting one), and talk it through with other collaborators:

- functions that are of broader interest than Natura 2000, better go to [inborutils](https://inbo.github.io/inborutils/) or a separate package;
- functions that will only be relevant to a specific _N2KHAB_ project, are better developed in the project-specific repo.

For more inspiration on where to put your own function, look into the [n2khab-monitoring](https://github.com/inbo/n2khab-monitoring) repo (which centralizes planning and workflow documentation in N2KHAB monitoring).

You will want to look at the file `src/manage_package.R` to get some useful packaging commands and developing tips.


#### Coding tools: it's never too late for learning!

When writing functions for `n2khab`:

- please use `tidyverse`, `sf` and `raster` packages for data reading.
Discover the human-friendly way of coding a data processing pipeline through the use of [pipes](https://r4ds.had.co.nz/pipes.html)!
Organise data in R in a [tidy](https://r4ds.had.co.nz/tidy-data.html#tidy-data-1) way in order to avoid troubles later on.
Recommended resources to get started are:
    - [R for Data Science](https://r4ds.had.co.nz/)
    - [Geocomputation with R](https://geocompr.robinlovelace.net)
    - [R packages](http://r-pkgs.had.co.nz/) (by Hadley Wickham 2015; an extended/updated [version](https://r-pkgs.org/) is still under development)
    - `vignette("formatting", package = "roxygen2")` for documentation syntax
- have a quick look at the [tidyverse style guide](https://style.tidyverse.org/).
There you see how to style object, variable and function names, as well as the documentation.
At least keep in mind: **use lower case and 'snake_case'** for object, variable and function names.
- if your function returns a dataframe, use `dplyr::as_tibble()` to return it as a tibble instead.
A tibble is a dataframe that makes working in the tidyverse a little [easier](https://r4ds.had.co.nz/tibbles.html).


#### How can I contribute code?

More detailed info on git workflows at INBO: <https://inbo.github.io/tutorials/tags/git/>.
See also [these git workshop materials](https://inbo.github.io/git-course/index.html).

1. Make commits (in your local clone of the remote repo on Github) _in your own git branch_, branched off from the `master` branch.
(But see this in a relative manner: exactly the same process can be repeated by someone else in turn, relative to your branch.
So '`master`' in this protocol can be replaced by another branch name!)
You can push your branch to the remote as often as you like, as it will not influence other branches (first time: do `git push -u origin yourbranchname`; afterwards `git push` suffices). It serves as a backup and enables others to work with you on that branch.
1. Meanwhile, make sure that your branch stays up to date with evolutions in `master` (i.e. in your local repo, update `master` with `git checkout master && git pull` and then, with your own branch checked out again, do `git merge --no-ff master`), in order to prevent merge conflicts with `master` later on.
At this stage, you need to resolve any merge conflicts that may arise in your own branch.
1. Propose to merge your commits into `master`: this starts with making a 'pull request' (PR; actually this is a merge request) and assign at least one reviewer before a merge can be decided. At that moment, open online discussion in the repo is possible on your changes (for other open discussion that you want to start, make an _issue_). As long as no merge is performed, more commits can be added to this PR with `git push`, e.g. to implement requested changes by others.
    - note that, if you branched off another (reference) branch than `master`, make sure to change the reference branch in the pull request (the default reference is `master`).
1. After your PR is merged, pull the reference branch (usually `master`) and clean up your local repo in order to keep up with the remote.


### Package history

Previous to commit `c8608d9`, the code was part of the [n2khab-monitoring](https://github.com/inbo/n2khab-monitoring) repo (formerly 'n2khab-inputs'), where the original version history remains stored.
At that time, the package was called 'n2khabutils'.
As a convenience, the **n2khab** repo still holds the rewritten (shrinked) package history from before commit `c8608d9`, as defined by the related files and folders.
See [this](https://github.com/inbo/n2khab-monitoring/issues/28) issue in the 'n2khab-monitoring' repo, where the migration is documented.


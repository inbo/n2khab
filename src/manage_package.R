## Use this script FROM WITHIN THE PACKAGE RSTUDIO PROJECT,
## i.e. the n2khab.Rproj file in the repository root.

install.packages("devtools")

# Some history:
#
# library(usethis)
# usethis::create_package(path = "../n2khab")
# use_gpl3_license(name = "Research Institute for Nature and Forest")
# usethis::use_vignette("v010_reference_lists")



# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B' (same as RStudio button 'install & restart')
#       (this installs the package from your local source directory - i.e. devtools::install() - +
#       restarts R + loads the package with library(packagename))

#   Check Package:             'Ctrl + Shift + E' (same as RStudio button 'check')
        # Preferrably perform the package check before pushing! Add extra commits if needed.
        # At least the master branch should always have a package with successful package check result,
        # i.e. without warnings/errors/notes. So try to solve in your own branch and ask help where needed.
        # The feedback from the check is often very helpful.

#   Load Package:              'Ctrl + Shift + L' (makes current package state available
#                                                   in your current R session, without installing. Very
#                                                   helpful for your own testing)
devtools::load_all() # same as 'Ctrl + Shift + L'


#   Generate ROxgen2 docu:     'Ctrl + Shift + D' (this is also needed, separately from load_all()
#                                                   if you also want to have your updated documentation
#                                                   available in your environment)
#       Definitely needed whenever you change or add documentation of a function in the 'R' subfolder.
#       It triggers the generation of *.Rd files which are the basis for the function documentation.
#       And this is important: the Rd files (and changes therein) are TO BE COMMITTED into the repo!
devtools::document() # same as 'Ctrl + Shift + D'


#   (Test Package:              'Ctrl + Shift + T' (no automated tests implemented in current stage))


# Doing a local package installation (from here):

devtools::install()

    # With vignettes:
    devtools::install(build_vignettes = TRUE)

# Doing a github-based package installation (defaults to the master branch):

remotes::install_github("inbo/n2khab")

    # With vignettes:
    remotes::install_github("inbo/n2khab",
                            build_opts = c("--no-resave-data", "--no-manual"))



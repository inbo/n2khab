#' @details
#' See the \href{00Index.html}{Index} for vignettes and
#' for documentation on functions and datasets delivered with the package.
#'
#' For contributing, see the README on
#' \href{https://github.com/inbo/n2khab/blob/master/README.md}{GitHub}.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("."))

#' @importFrom utils packageVersion packageDescription
#' @importFrom curl nslookup
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Attaching n2khab ",
    packageVersion("n2khab"),
    "."
  )
  invisible(n2khab_options())
  packageStartupMessage("Will use sf ", packageDescription("sf")$Version, ".")
  if (
    length(find.package("raster", quiet = TRUE) > 0) &&
      packageVersion("raster") >= package_version("3.6-3") &&
      length(find.package("terra", quiet = TRUE) > 0)
  ) {
    packageStartupMessage(
      "Will use terra ",
      packageDescription("terra")$Version,
      ifelse(isTRUE(n2khab_using_raster()), " through raster.", ".")
    )
  }
  if (!is.null(nslookup("api.github.com", error = FALSE))) {
    tryCatch(
      {
        ref <- remotes::github_remote(
          "inbo/n2khab",
          ref = remotes::github_release()
        )$ref
        release <- package_version(gsub("\\p{L}*", "", ref, perl = TRUE))
        if (packageVersion("n2khab") < release) {
          packageStartupMessage(
            "\n",
            rep("=", getOption("width")),
            "\nIt is advised to upgrade n2khab to its current version ",
            release,
            ". Run:\n\n",
            'detach("package:n2khab", unload = TRUE)\n',
            'install.packages("n2khab", repos = c(inbo = "https://inbo.r-universe.dev",
                                     CRAN = "https://cloud.r-project.org"))',
            "\n",
            "library(n2khab)\n",
            rep("=", getOption("width"))
          )
        }
      },
      error = function(e) {}
    )
  }
}

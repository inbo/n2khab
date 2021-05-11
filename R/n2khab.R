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

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Attaching n2khab version ",
                          packageVersion("n2khab"), ".\n")
    packageStartupMessage(
        "When working with raster objects returned by n2khab, you can safely\n",
        "mute proj4string degradation warnings:",
        "\n\noptions(rgdal_show_exportToProj4_warnings = \"none\")\n\n",
        "You must do this before using the n2khab functions ",
        "depending on \nrgdal or raster, and before loading ",
        "the latter (or sp).\n\n",
        "Do note that those warnings are applicable: in the returned raster\n",
        "objects, the proj4string is effectively degraded and should not ",
        "be used.\n",
        "See https://inbo.github.io/n2khab/#suppressing-rgdal-warnings-about-",
        "proj4string-degradation\nfor more information.")
}

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
  packageStartupMessage(
    "Attaching n2khab version ",
    packageVersion("n2khab")
  )
}

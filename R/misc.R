#' Check availability of required packages
#'
#' Takes a vector of package names and passes each name to
#' \code{\link[base:ns-load]{requireNamespace()}};
#' if package(s) are missing, returns an error message providing the basic
#' \code{install.packages()} command to install them.
#'
#' @param pkgs A character vector of package names.
#'
#' @examples
#' \dontrun{
#' require_pkgs(c("a", "base", "b", "c"))
#' }
#'
#' @importFrom purrr
#' map_lgl
#' @importFrom assertthat
#' assert_that
#' @keywords internal
require_pkgs <- function(pkgs) {
  assert_that(is.character(pkgs))
  available <- map_lgl(pkgs, ~ requireNamespace(., quietly = TRUE))
  if (!all(available)) {
    multiple <- sum(!available) > 1
    stop(ifelse(multiple, "Multiple", "A"),
      " package",
      ifelse(multiple, "s", ""),
      " needed for this function ",
      ifelse(multiple, "are", "is"),
      " missing.\nPlease install as follows: install.packages(",
      deparse(pkgs[!available]),
      ")",
      call. = FALSE
    )
  }
}

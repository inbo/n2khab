#' Deprecated functions
#'
#' - [`read_schemes()`] has moved to package `n2khabmon`.
#' Use [`n2khabmon::read_schemes()`] instead.
#' - [`read_scheme_types()`] has moved to package `n2khabmon`.
#' Use [`n2khabmon::read_scheme_types()`] instead.
#'
#' @param ... arguments passed to the new function
#'
#' @md
#'
#' @name n2khab-deprecated

#' @rdname n2khab-deprecated
#' @export
#' @keywords internal
read_schemes <- function(...) {
  .Deprecated("n2khabmon::read_schemes")
  require_pkgs("n2khabmon")
  n2khabmon::read_schemes(...)
}



#' @rdname n2khab-deprecated
#' @export
#' @keywords internal
read_scheme_types <- function(...) {
  .Deprecated("n2khabmon::read_scheme_types")
  require_pkgs("n2khabmon")
  n2khabmon::read_scheme_types(...)
}

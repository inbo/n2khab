#' Deprecated functions
#'
#' - [`read_schemes()`] has moved to package `n2khabmon`.
#' Use [`n2khabmon::read_schemes()`] instead.
#' - [`read_scheme_types()`] has moved to package `n2khabmon`.
#' Use [`n2khabmon::read_scheme_types()`] instead.
#'
#' An efficient way with base R to avoid function masking and conflict warnings
#' when attaching both `{n2khab}` and `{n2khabmon}`, regardless of the order
#' in which they're loaded, is by specifying something as below in your
#' script, at least _before_ loading `{n2khab}`:
#'
#' `conflictRules("n2khab", exclude = c("read_schemes", "read_scheme_types"))`
#'
#' @param ... Arguments passed to the new function.
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

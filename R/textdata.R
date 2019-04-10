#' Documentation of included data source 'namelist'
#'
#' 'namelist' is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' names and (optionally) shortnames for IDs/codes used in other data sources.
#' Multiple languages are supported.
#'
#' @format A vc-formatted data source. As such, it corresponds to
#' a dataframe with many rows and 4 variables:
#' \describe{
#'   \item{code}{A code used elsewhere.}
#'   \item{lang}{An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{IETF BCP
#'   47 language tag}, such as \code{"en"} or \code{"nl"}, to identify
#'   the language of \code{name} and \code{shortname}.}
#'   \item{name}{The name corresponding to \code{code} and \code{lang}.}
#'   \item{shortname}{Optionally, a shorter variant of \code{name}.} }
#'
#' @section Typical way of loading:
#'
#'   \code{read_namelist()}
#'
#'   \code{read_namelist(lang = "nl")}
#'
#' @section Corresponding datafiles in the installed package:
#'
#'   \code{textdata/namelist.tsv}
#'
#'   \code{textdata/namelist.yml}
#'
#' @source
#'
#' The 'namelist' data source has got its contents from the sources of
#' several other n2khab-referencelists (see the source of those).
#'
#' @seealso \code{\link{read_namelist}}
#'
#' @family n2khab-referencelists
#'
#' @name namelist
NULL

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



#' Documentation of included data source 'types'
#'
#' 'types' is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' a checklist of types, represented by their codes, together with several
#' attributes.
#' A 'type' refers to either a (main) habitat type, a
#' habitat subtype or a regionally important biotope (RIB).
#' The codes of types, typeclasses and tags are explained in the
#' data source \code{\link{namelist}} (which can accommodate multiple
#' languages).
#'
#' @format A vc-formatted data source. As such, it corresponds to
#' a dataframe with 108 rows and 7 variables:
#' \describe{
#'   \item{type}{Code of the type.
#'   This is the ID for use in diverse workflows and datasets.
#'   Names in multiple languages are to be found in \code{namelist}.
#'   Contains no duplicates!}
#'   \item{typelevel}{A factor that labels the type as
#'    either \code{"main_type"} or \code{"subtype"}.}
#'   \item{main_type}{The main type that corresponds with \code{type}.
#'   Each type is either a subtype of a main type, or is a main type itself.
#'   This is indicated by \code{typelevel}.}
#'   \item{typeclass}{A code explained by \code{namelist},
#'   corresponding to the typeclass.
#'   Is a factor.}
#'   \item{tag_1}{Optional tag, e.g. a categorization ID explained
#'   by \code{namelist}.}
#'   \item{tag_2}{Optional tag, e.g. a categorization ID explained
#'   by \code{namelist}.}
#'   \item{tag_3}{Optional tag, e.g. a categorization ID explained
#'   by \code{namelist}.} }
#'
#' @section Typical way of loading:
#'
#'   \code{read_types()}
#'
#'   \code{read_types(lang = "nl")}
#'
#' @section Corresponding datafiles in the installed package:
#'
#'   \code{textdata/types.tsv}
#'
#'   \code{textdata/types.yml}
#'
#' @source
#'
#' \href{https://docs.google.com/spreadsheets/d/1dK0S1Tt3RlVEh4WrNF5N_-hn0OXiTUVOvsL2JRhSl78}{This googlesheet}.
#' Currently, the googlesheet and the data source are both kept up-to-date.
#'
#' @seealso \code{\link{read_types}}
#'
#' @family n2khab-referencelists
#'
#' @name types
NULL










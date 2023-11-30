#' Documentation of included data source 'namelist'
#'
#' 'namelist' is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' names and (optionally) shortnames for IDs/codes used in other data sources.
#' Multiple languages are supported.
#'
#' @format A vc-formatted data source. As such, it corresponds to
#' a data frame with many rows and 4 variables:
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
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' a checklist of types, represented by their \strong{current} codes, together
#' with several attributes.
#' A 'type' refers to either a (main) habitat type, a
#' habitat subtype or a regionally important biotope (RIB).
#' The codes of types, typeclasses and further attributes and tags are
#' explained in the
#' data source \code{\link{namelist}} (which can accommodate multiple
#' languages).
#'
#' @format A vc-formatted data source. As such, it corresponds to
#' a data frame with several variables:
#' \describe{
#'   \item{type}{Code of the type, as a factor.
#'   This is the ID for use in diverse workflows and datasets.
#'   Names in multiple languages are to be found in \code{\link{namelist}}.
#'   Only \emph{currently active} codes are kept in this list, in order to avoid
#'   confusion (this especially relates to habitat subtypes and RIBs).
#'   Contains no duplicates!}
#'   \item{typelevel}{A factor that labels the type as
#'    either \code{"main_type"} or \code{"subtype"}.}
#'   \item{main_type}{The main type that corresponds with \code{type},
#'   as a factor.
#'   Each type is either a subtype of a main type, or is a main type itself.
#'   This is indicated by \code{typelevel}.}
#'   \item{typeclass}{A code explained by \code{\link{namelist}},
#'   corresponding to the typeclass.
#'   Is a factor.}
#'   \item{hydr_class}{A code explained by \code{\link{namelist}},
#'   corresponding to the hydrological class.
#'   Is a factor.}
#'   \item{groundw_dep}{A code explained by \code{\link{namelist}},
#'   corresponding to the groundwater dependency category.
#'   Is a factor.}
#'   \item{flood_dep}{A code explained by \code{\link{namelist}},
#'   corresponding to the flood dependency category.
#'   Is a factor.
#'   Note that flood dependency is only defined for (semi-)terrestrial types,
#'   hence for aquatic types (hydrological class \code{HC3})
#'   it is \code{NA}.}
#'   \item{tag_1}{Optional tag, e.g. a categorization ID explained
#'   by \code{\link{namelist}}.
#'   Currently used to indicate subcategories within a few typeclasses,
#'   or to differentiate between lotic and lentic aquatic types.}
#'   \item{tag_2}{Optional tag, e.g. a categorization ID explained
#'   by \code{\link{namelist}}.}
#'   \item{tag_3}{Optional tag, e.g. a categorization ID explained
#'   by \code{\link{namelist}}.} }
#'
#' @section Typical way of loading:
#'
#'   \code{read_types()}
#'
#'   \code{read_types(lang = "nl")}
#'
#' @section Corresponding datafiles in the installed package:
#'
#'   \code{textdata/types.csv}
#'
#'   \code{textdata/types.yml}
#'
#' @source
#'
#' Most information comes from
#' \href{https://docs.google.com/spreadsheets/d/1dK0S1Tt3RlVEh4WrNF5N_-hn0OXiTUVOvsL2JRhSl78}{this googlesheet}.
#' Currently, the googlesheet and the data source are both kept up-to-date.
#' However only the 'types' data source is under version control.
#'
#' The source for the hydrological class attribute is a vc-formatted file
#' stored in the package source code.
#' It is read by the 'generate_textdata' bookdown project which generates the
#' 'types' data source.
#' The referred vc-formatted file was derived from a yet unpublished database
#' on the interrelations
#' between types, hydrological classes, environmental compartments and their
#' characteristics, and environmental pressures.
#'
#' The source for the groundwater and flood dependency attributes is
#' \href{https://docs.google.com/spreadsheets/d/1bhXgamK28K--MSWF7goKNOWtWPEI149_QCpU-3V_k_E}{this googlesheet}.
#' Currently, the googlesheet and the data source are both kept up-to-date.
#' However only the 'types' data source is under version control.
#'
#' @seealso \code{\link{read_types}}
#'
#' @family n2khab-referencelists
#'
#' @name types
NULL









#' Documentation of included data source 'env_pressures'
#'
#' 'env_pressures' is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' a checklist of environmental pressures, represented by codes, together
#' with the pressure-class and the textual explanation (with optional remarks).
#' The codes of environmental pressures, pressure-classes and explanations
#' are explained in the
#' data source \code{\link{namelist}} (which can accommodate multiple
#' languages).
#'
#' @format A vc-formatted data source. As such, it corresponds to
#' a data frame with 35 rows and 3 variables:
#' \describe{
#'   \item{ep_code}{Code of the environmental pressure, as a factor.
#'   This is the ID for use in diverse workflows and datasets.
#'   Corresponding names and abbreviations
#'   in multiple languages are stored in \code{\link{namelist}}
#'   (as name and shortname, respectively).
#'   The abbreviation may be seen as an alternative, language-dependent code.
#'   Contains no duplicates!}
#'   \item{ep_class}{A code explained by \code{\link{namelist}},
#'   corresponding to the environmental pressure's class.
#'   Is a factor.}
#'   \item{explanation}{A code explained by \code{\link{namelist}},
#'   corresponding to the explanation on the environmental pressure, and
#'   optional remarks.
#'   Explanation and remarks are stored in \code{\link{namelist}}
#'   (as name and shortname, respectively)}
#' }
#'
#' @section Typical way of loading:
#'
#'   \code{read_env_pressures()}
#'
#'   \code{read_env_pressures(lang = "nl")}
#'
#' @section Corresponding datafiles in the installed package:
#'
#'   \code{textdata/env_pressures.csv}
#'
#'   \code{textdata/env_pressures.yml}
#'
#' @source
#'
#' The latest worksheet of
#' \href{https://docs.google.com/spreadsheets/d/1PH6InqJk0ijQF_N7v7IZjarijlqHRBCKhTbDn44skZU}{
#' this googlesheet}.
#' Currently, the googlesheet and the data source are both kept up-to-date.
#'
#' @seealso \code{\link{read_env_pressures}}
#'
#' @family n2khab-referencelists
#'
#' @name env_pressures
NULL

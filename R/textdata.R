#' Documentation of included data source 'namelist'
#'
#' 'namelist' is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
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
#' a dataframe with several variables:
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
#'   \code{textdata/types.tsv}
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
#' a dataframe with 35 rows and 3 variables:
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
#'   \code{textdata/env_pressures.tsv}
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









#' Documentation of included data source 'schemes'
#'
#' 'schemes' is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' a list of (monitoring) schemes for N2KHAB monitoring programmes or
#' other N2KHAB projects, together
#' with defining attributes and optional information.
#' The codes of schemes, programmes, attributes and tags
#' are explained in the
#' data source \code{\link{namelist}} (which can accommodate multiple
#' languages).
#'
#'
#' @format A vc-formatted data source. As such, it corresponds to
#' a dataframe with 10 variables:
#' \describe{
#'   \item{scheme}{Code of the scheme, as a factor.
#'   This is the ID for use in diverse workflows and datasets.
#'   Corresponding names and shortnames
#'   in multiple languages are to be found in \code{\link{namelist}}.
#'   Contains no duplicates!
#'
#'   A 'scheme' refers to a monitoring or research setup that determines
#'   which types (habitat/RIBs) are to be investigated for a question or for
#'   a bunch of related questions.
#'   }
#'   \item{programme}{Code of the programme to which the scheme belongs.
#'   The programme's code is explained by \code{\link{namelist}}.
#'   Is a factor.
#'
#'   A 'programme' refers to a N2KHAB monitoring programme or research project.
#'   One programme can correspond to multiple schemes.
#'   At least the monitoring programmes MHQ and MNE are present.
#'   }
#'   \item{attribute_1}{The first defining attribute of the scheme.
#'   Typically, the code is explained by \code{\link{namelist}}.
#'   Is a factor.
#'   \itemize{
#'     \item{In MNE, this is used to declare the 'compartment superscheme' (scheme
#'     collection) to which the scheme belongs, and which is named after the
#'     environmental compartment (however note that the surfacewater superscheme
#'     comprises two environmental compartments).}
#'     \item{In MHQ, this is used to define the target habitat type of the
#'     monitoring scheme.}
#'   }
#'   }
#'   \item{attribute_2}{A second defining attribute of the scheme (if needed).
#'   Typically, the code is explained by \code{\link{namelist}}.
#'   Is a factor.
#'   \itemize{
#'     \item{In MNE, this provides the code of the environmental pressure to
#'     which the scheme is related.
#'     It must be represented in \code{\link{env_pressures}}.}
#'   }
#'   }
#'   \item{attribute_3}{A third defining attribute of the scheme (if needed).
#'   Typically, the code is explained by \code{\link{namelist}}.
#'   Is a factor.
#'   \itemize{
#'     \item{In MNE, this is used to declare an \strong{optional} partitioning of the
#'     scheme that would be constituted by attributes 1 and 2 alone
#'     (environmental compartment and pressure), which will hence divide the
#'     concerned types in at least two groups.
#'     This typically has to do with differences in the considered
#'     environmental variables or compatibility of interpretation.
#'
#'     It is \emph{not} meant to distinguish between different types or
#'     typegroups for inferences, so types are kept in the same scheme as
#'     long as the measured values have compatibility of interpretation.
#'     To declare which types or typegroups should be
#'     distinguished in inferences, use the \code{typegroup} variable in the
#'     \code{\link{scheme_types}} data source!}
#'   }
#'   }
#'   \item{spatial_restriction}{Optional further defining specification of a
#'   scheme (in English):
#'   spatial restrictions, superposed on the restrictions already
#'   generated by the attributes.}
#'   \item{notes}{Optional. In English. For additional notes on the scheme
#'   definition, if necessary.}
#'   \item{tag_1}{Optional tag, e.g. a categorization ID explained
#'   by \code{\link{namelist}}.
#'   \itemize{
#'     \item{In MNE, this is used to tag which schemes are focal schemes and
#'     which ones are secondary schemes.}
#'     \item{In MHQ, this is used to separate aquatic and terrestial
#'     monitoring schemes.}
#'   }
#'   }
#'   \item{tag_2}{Optional tag, e.g. a categorization ID explained
#'   by \code{\link{namelist}}.}
#'   \item{tag_3}{Optional tag, e.g. a categorization ID explained
#'   by \code{\link{namelist}}.}
#' }
#'
#' @section Typical way of loading:
#'
#'   \code{read_schemes()}
#'
#'   \code{read_schemes(lang = "nl")}
#'
#' @section Corresponding datafiles in the installed package:
#'
#'   \code{textdata/schemes.tsv}
#'
#'   \code{textdata/schemes.yml}
#'
#' @source
#'
#' \itemize{
#'     \item{For the MNE-schemes: a vc-formatted, Dutch-language data source
#'     which can be found in the
#'     \href{https://github.com/inbo/n2khab}{
#'     n2khab Github repository}:
#'     \code{misc/generate_textdata/rawraw_data/10_compmeetnet_types_milieudrukken}.
#'     This data source was originally generated in the
#'     n2khab-mne-selections repository
#'     (\samp{https://gitlab.com/florisvdh/n2khab-mne-selections}).
#'     }
#'   }
#' \itemize{
#'     \item{More information on the MHQ-schemes can be found in
#'     \href{https://pureportal.inbo.be/portal/files/4339795/Westra_etal_2014_MonitoringNatura2000Habitats.pdf}{Westra \emph{et al.} (2014)}.
#'   }
#'  }
#'
#' @seealso \code{\link{read_schemes}}
#'
#' @family n2khab-referencelists
#'
#' @name schemes
NULL









#' Documentation of included data source 'scheme_types'
#'
#' 'scheme_types' is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which lists
#' the types (using the type-code from \code{\link{types}}) that belong to
#' each N2KHAB (monitoring or research) scheme (using the scheme-code from
#' \code{\link{schemes}}).
#' It also defines typegroup memberships of the types within specific schemes,
#' if applicable.
#' The codes of schemes and types (and optionally: typegroups)
#' are explained in the
#' data source \code{\link{namelist}} (which can accommodate multiple
#' languages).
#'
#'
#' @format A vc-formatted data source. As such, it corresponds to
#' a dataframe with 3 variables:
#' \describe{
#'   \item{scheme}{Code of the scheme, as a factor, and explained by
#'   \code{\link{namelist}}.
#'   It must be represented in \code{\link{schemes}}.
#'   }
#'   \item{type}{Code of the type, as a factor, and explained by
#'   \code{\link{namelist}}.
#'   It must be represented in \code{\link{types}}.}
#'   \item{typegroup}{An \emph{optional} code (and optionally explained by
#'   \code{\link{namelist}}),
#'   declaring the typegroup to which a type belongs \emph{within the specified
#'   scheme}.
#'   Typegroups point out which types are considered together as a group in
#'   inferences for the scheme at hand.}
#' }
#'
#' Each combination of \code{scheme} and \code{type} must be unique.
#'
#' @section Typical way of loading:
#'
#'   \code{read_scheme_types()}
#'
#'   \code{read_scheme_types(lang = "nl")}
#'
#' @section Corresponding datafiles in the installed package:
#'
#'   \code{textdata/scheme_types.tsv}
#'
#'   \code{textdata/scheme_types.yml}
#'
#' @source
#'
#' \itemize{
#'     \item{For the MNE-schemes:
#'       \itemize{
#'         \item{the link between schemes and types comes from
#'         a vc-formatted, Dutch-language data source
#'         which can be found in the
#'         \href{https://github.com/inbo/n2khab}{
#'         n2khab Github repository}:
#'         \code{misc/generate_textdata/rawraw_data/10_compmeetnet_types_milieudrukken}.
#'         This data source was originally generated in the
#'         n2khab-mne-selections repository
#'         (\samp{https://gitlab.com/florisvdh/n2khab-mne-selections}).}
#'         \item{the declaration of typegroups comes from
#'         \href{https://docs.google.com/spreadsheets/d/1n2ohvuLEK_anX37gxlanQXRGotweMVonFxKxtfTcuZI}{
#'         this googlesheet}.
#'         Currently, the typegroups are kept up to date both in the
#'         googlesheet and in the data source.}
#'         }
#'     }
#'   }
#'
#' \itemize{
#'     \item{For the MHQ-schemes, the list of sampled types is based on the report of
#'     \href{https://pureportal.inbo.be/portal/files/4339795/Westra_etal_2014_MonitoringNatura2000Habitats.pdf}{Westra \emph{et al.} (2014)}.
#'   }
#'  }
#'
#'
#' @seealso \code{\link{read_scheme_types}}
#'
#' @family n2khab-referencelists
#'
#' @name scheme_types
NULL










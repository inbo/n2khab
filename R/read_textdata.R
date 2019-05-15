#' Return the 'namelist' data source as a tibble
#'
#' Returns the included data source \code{\link{namelist}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}},
#' by default filtered according to English names and shortnames.
#'
#' \code{\link{namelist}} is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' names and (optionally) shortnames for IDs/codes used in other data sources.
#'
#' \code{read_namelist()} reads it and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a dataframe that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and only English
#' names (\code{lang = "en"}) are returned.
#'
#' @param path Location of the data source.
#' The default is to use the location of the data source as delivered by
#' the installed package.
#' @param file The filename of the data source, without extension.
#' The default is to use the file delivered by the installed package.
#' @param lang An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{IETF BCP
#'   47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
#'   the language of \code{name} and \code{shortname} to be returned in the tibble.
#'   If \code{lang = "all"}, the full \code{namelist} tibble is returned, i.e.
#'   containing all languages.
#'
#' @return
#' The \code{namelist} dataframe as a \code{\link[tibble:tbl_df-class]{tibble}},
#' filtered according to the \code{lang} argument.
#' See \code{\link{namelist}} for documentation of the tibble's contents.
#'
#' @section Recommended usage:
#'
#'   \code{read_namelist()}
#'
#'   \code{read_namelist(lang = "nl")}
#'
#' @seealso
#' \code{\link{namelist}}
#'
#' @family reading functions for n2khab-referencelists
#'
#' @examples
#' \dontrun{
#' read_namelist()
#' read_namelist(lang = "nl")
#' }
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom dplyr %>% filter as_tibble
read_namelist <-
    function(path = pkgdatasource_path("textdata/namelist", ".tsv"),
             file = "namelist",
             lang = "en") {

        if (lang == "all") {
            result <-
                read_vc(file = file, root = path)
        } else {
            result <-
                read_vc(file = file, root = path) %>%
                filter(lang == !!lang)
        }

        attr(result, "source") <- NULL

        result %>%
            as_tibble
    }









#' Return the path of a package data source
#'
#' Returns the path for a given file
#' basename in a package. The developer can optionally
#' provide the file's extension separately.
#'
#' @param file The filename of the data source. Can be without extension.
#' Can be a character vector.
#' @param extension The extension of a file if not provided by \code{file}.
#' The dot is to be included, e.g. \code{.csv}.
#'
#' @return A character vector.
#' @importFrom dplyr %>% filter
#' @importFrom stringr str_c
#' @keywords internal
pkgdatasource_path <-
    function(file, extension = "") {
        system.file(str_c(file, extension),
                    package = "n2khabutils"
        ) %>%
            dirname
    }









#' Translates a factor with levels mapped according to the namelist
#'
#' Translates a factor, by default using the 'name' variable from the
#' 'namelist' object to substitute the factor levels. The join on 'namelist' is
#' by the variable 'code'.
#'
#' @param x A factor.
#' @param pick A character, referring to the variable from namelist
#' (\code{"name"} or \code{"shortname"}) that should be used as translated
#' factor levels.
#' @param codelist An object conforming to \code{\link{namelist}}.
#'
#' @return A factor.
#' @importFrom dplyr %>% left_join select
#' @importFrom plyr mapvalues
#' @keywords internal
namelist_factor <-
    function(x, pick = "name", codelist) {

        suppressWarnings(
            mapped_levels <-
                data.frame(code = levels(x)) %>%
                left_join(codelist,
                          by = "code") %>%
                select(.data$code, !!pick)
        )

        x %>%
            mapvalues(from = mapped_levels$code,
                      to = mapped_levels[,2])

    }















#' Return the 'types' data source as a tibble with names & shortnames
#'
#' Returns the included data source \code{\link{types}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names and shortnames from \code{\link{namelist}} are added,
#' in English by default.
#'
#' \code{\link{types}} is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' a checklist of types, represented by their \strong{current} codes, together
#' with several attributes.
#' A 'type' refers to either a (main) habitat type, a
#' habitat subtype or a regionally important biotope (RIB).
#'
#' \code{read_types()} reads the \code{\link{types}} data source, adds
#' names + shortnames
#' and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a dataframe that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' names (\code{lang = "en"}) are returned for types, typeclasses and tags.
#'
#' @param path Location of the data sources \code{types} and \code{namelist}.
#' The default is to use the location of the data sources as delivered by
#' the installed package.
#' @param file The filename of the \code{types} data source, without extension.
#' The default is to use the file delivered by the installed package.
#' @param file_namelist The filename of the \code{namelist} data source,
#' without extension.
#' The default is to use the file delivered by the installed package.
#' @param lang An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{
#'   IETF BCP 47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
#'   the language of names & shortnames to be returned in the tibble.
#'
#' @return
#' The \code{types} dataframe as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with names & shortnames added for types, typeclasses and tags
#' according to the \code{lang} argument.
#' The tibble has 108 rows and 16 variables.
#' See \code{\link{types}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding names (and shortnames).
#'
#' The added names and shortnames are represented by the following variables:
#' \itemize{
#'   \item \code{type_name}
#'   \item \code{type_shortname}
#'   \item \code{typeclass_name}: a factor with the level order
#'   according to that of \code{typeclass}.
#'   \item \code{tag_1_name}
#'   \item \code{tag_1_shortname}
#'   \item \code{tag_2_name}
#'   \item \code{tag_2_shortname}
#'   \item \code{tag_3_name}
#'   \item \code{tag_3_shortname}
#' }
#'
#' @section Recommended usage:
#'
#'   \code{read_types()}
#'
#'   \code{read_types(lang = "nl")}
#'
#' @seealso
#' \code{\link{types}}
#'
#' @family reading functions for n2khab-referencelists
#'
#' @examples
#' \dontrun{
#' read_types()
#' read_types(lang = "nl")
#' }
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom dplyr
#' %>%
#' filter
#' select
#' mutate
#' rename
#' tibble
#' left_join
#' as_tibble
#' @importFrom plyr mapvalues
#' @importFrom rlang .data
read_types <-
    function(path = pkgdatasource_path("textdata/types", ".tsv"),
             file = "types",
             file_namelist = "namelist",
             lang = "en") {

        namelist <-
            read_namelist(path = path,
                          file = file_namelist,
                          lang = lang) %>%
            select(.data$code,
                   .data$name,
                   .data$shortname)

        types_base <-
            read_vc(file = file, root = path)

        suppressWarnings({
        type_levels <-
            tibble(codelevel = types_base$type %>% levels) %>%
            left_join(namelist,
                      by = c("codelevel" = "code")) %>%
            rename(namelevel = .data$name,
                   shortnamelevel = .data$shortname)

                typeclass_levels <-
            tibble(codelevel = types_base$typeclass %>% levels) %>%
            left_join(namelist %>% select(-.data$shortname),
                       by = c("codelevel" = "code")) %>%
            rename(namelevel = .data$name)

        types_base %>%
            left_join(namelist, by = c("type" = "code")) %>%
            rename(type_name = .data$name,
                   type_shortname = .data$shortname) %>%
            mutate(type = factor(.data$type,
                                 levels = types_base$type %>%
                                     levels),
                   type_name =
                       .data$type %>%
                       mapvalues(from = type_levels$codelevel,
                                 to = type_levels$namelevel),
                   type_shortname =
                       .data$type %>%
                       mapvalues(from = type_levels$codelevel,
                                 to = type_levels$shortnamelevel),
                   typeclass_name =
                       .data$typeclass %>%
                       mapvalues(from = typeclass_levels$codelevel,
                                 to = typeclass_levels$namelevel)
                  ) %>%
            left_join(namelist,
                      by = c("tag_1" = "code")) %>%
            rename(tag_1_name = .data$name,
                   tag_1_shortname = .data$shortname) %>%
            left_join(namelist,
                      by = c("tag_2" = "code")) %>%
            rename(tag_2_name = .data$name,
                   tag_2_shortname = .data$shortname) %>%
            left_join(namelist,
                      by = c("tag_3" = "code")) %>%
            rename(tag_3_name = .data$name,
                   tag_3_shortname = .data$shortname) %>%
            select(1:3, 8:9,
                   4, 10,
                   5, 11:12,
                   6, 13:14,
                   7, 15:16) %>%
            as_tibble
        })
    }













#' Return the 'env_pressures' data source as a tibble with human-readable
#' attributes
#'
#' Returns the included data source \code{\link{env_pressures}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names, shortnames and explanations from \code{\link{namelist}} are added,
#' in English by default.
#'
#' \code{\link{env_pressures}} is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' a checklist of environmental pressures, represented by codes, together
#' with the pressure-class and the textual explanation.
#'
#' \code{read_env_pressures()} reads the \code{\link{env_pressures}} data
#' source, adds
#' human-readable attributes and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a dataframe that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' text (\code{lang = "en"}) is returned for names of environmental pressures and
#' pressure-classes, and for textual explanations.
#'
#' @param path Location of the data sources \code{env_pressures} and
#' \code{namelist}.
#' The default is to use the location of the data sources as delivered by
#' the installed package.
#' @param file The filename of the \code{env_pressures} data source, without
#' extension.
#' The default is to use the file delivered by the installed package.
#' @param lang An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{
#'   IETF BCP 47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
#'   the language of human-readable attributes to be returned in the tibble.
#'
#' @inheritParams read_types
#'
#' @return
#' The \code{env_pressures} dataframe as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with human-readable text added for environmental pressures,
#' pressure-classes and textual explanations
#' according to the \code{lang} argument.
#' The tibble has 35 rows and 6 variables.
#' See \code{\link{env_pressures}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding text.
#'
#' The human-readable attributes are represented by the following variables:
#' \describe{
#'   \item{\code{ep_abbrev}}{A (language-dependent) abbreviation (alternative code)
#'   of the environmental pressure.
#'   Is a factor with the level order coinciding with that of
#'   \code{ep_code}.}
#'   \item{\code{ep_name}}{The name of the environmental pressure.
#'   Is a factor with the level order coinciding with that of
#'   \code{ep_code}.}
#'   \item{\code{ep_class_name}}{The name of the environmental pressure's class.
#'   Is a factor with the level order coinciding with that of
#'   \code{ep_class}.}
#'   \item{\code{explanation}}{An explanation of the environmental pressure.
#'   \emph{Beware that this explanation is often shared between multiple
#'   environmental pressures!}
#'   Hence the added explanation may cover more than is revealed by the environmental
#'   pressure's \strong{name}.}
#' }
#'
#' @section Recommended usage:
#'
#'   \code{read_env_pressures()}
#'
#'   \code{read_env_pressures(lang = "nl")}
#'
#' @seealso
#' \code{\link{env_pressures}}
#'
#' @family reading functions for n2khab-referencelists
#'
#' @examples
#' \dontrun{
#' read_env_pressures()
#' read_env_pressures(lang = "nl")
#' }
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom dplyr
#' %>%
#' select
#' mutate
#' rename
#' tibble
#' left_join
#' as_tibble
#' distinct
#' arrange
#' @importFrom rlang .data
read_env_pressures <-
    function(path = pkgdatasource_path("textdata/env_pressures", ".tsv"),
             file = "env_pressures",
             file_namelist = "namelist",
             lang = "en") {

        namelist <-
            read_namelist(path = path,
                          file = file_namelist,
                          lang = lang) %>%
            select(.data$code,
                   .data$name,
                   .data$shortname)

        env_pressures_base <-
            read_vc(file = file, root = path)

        suppressWarnings(
            env_pressures_base2 <-
                read_vc(file = file, root = path) %>%
                left_join(namelist, by = c("ep_code" = "code")) %>%
                rename(ep_name = .data$name,
                       ep_abbrev = .data$shortname) %>%
                mutate(ep_code = .data$ep_code %>%
                           factor(levels = env_pressures_base$ep_code %>% levels)
                )
            )

        ep_levels <-
            env_pressures_base2 %>%
            distinct(.data$ep_code,
                     .data$ep_name,
                     .data$ep_abbrev) %>%
            arrange(.data$ep_code)

        ep_class_levels <-
            tibble(codelevel = env_pressures_base$ep_class %>% levels) %>%
            left_join(namelist %>% select(-.data$shortname),
                      by = c("codelevel" = "code")) %>%
            rename(namelevel = .data$name)

        env_pressures_base2 %>%
            mutate(ep_name = .data$ep_name %>%
                       factor(levels = ep_levels$ep_name),
                   ep_abbrev = .data$ep_abbrev %>%
                       factor(levels = ep_levels$ep_abbrev),
                   ep_class_name =
                       .data$ep_class %>%
                       mapvalues(from = ep_class_levels$codelevel,
                                 to = ep_class_levels$namelevel)
            ) %>%
            left_join(namelist %>% select(-.data$shortname),
                      by = c("explanation" = "code")) %>%
            select(-.data$explanation) %>%
            rename(explanation = .data$name) %>%
            mutate(ep_code = .data$ep_code %>%
                       factor(levels = env_pressures_base$ep_code %>% levels)
            ) %>%
            select(.data$ep_code,
                   .data$ep_abbrev,
                   .data$ep_name,
                   .data$ep_class,
                   .data$ep_class_name,
                   .data$explanation) %>%
            as_tibble
    }














#' Return the 'schemes' data source as a tibble with names & shortnames
#'
#' Returns the included data source \code{\link{schemes}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names and shortnames from \code{\link{namelist}} are added,
#' in English by default.
#'
#' \code{\link{schemes}} is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' a list of (monitoring) schemes for N2KHAB monitoring programmes or
#' other N2KHAB projects, together
#' with defining attributes and optional information.
#' A 'scheme' refers to a monitoring or research setup that determines
#' which types (habitat/RIBs) are to be investigated for a question or for
#' a bunch of related questions.
#'
#' \code{read_schemes()} reads the \code{\link{schemes}} data source, adds
#' names + shortnames and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a dataframe that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' names (\code{lang = "en"}) are returned for scheme, programme, attributes
#' and tags.
#'
#' @param path Location of the data sources \code{schemes} and \code{namelist}.
#' The default is to use the location of the data sources as delivered by
#' the installed package.
#' @param file The filename of the \code{schemes} data source, without extension.
#' The default is to use the file delivered by the installed package.
#'
#' @inheritParams read_types
#' @inheritParams read_env_pressures
#'
#' @return
#' The \code{schemes} dataframe as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with names & shortnames added for scheme, programme, attributes and tags
#' according to the \code{lang} argument.
#' The tibble has 25 variables.
#' See \code{\link{schemes}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding names (and shortnames).
#'
#' The added names and shortnames are represented by the following variables:
#' \itemize{
#'   \item \code{scheme_name}
#'   \item \code{scheme_shortname}
#'   \item \code{programme_name}
#'   \item \code{attribute_1_name}
#'   \item \code{attribute_1_shortname}
#'   \item \code{attribute_2_name}
#'   \item \code{attribute_2_shortname}
#'   \item \code{attribute_3_name}
#'   \item \code{attribute_3_shortname}
#'   \item \code{tag_1_name}
#'   \item \code{tag_1_shortname}
#'   \item \code{tag_2_name}
#'   \item \code{tag_2_shortname}
#'   \item \code{tag_3_name}
#'   \item \code{tag_3_shortname}
#' }
#'
#' The added names and shortnames for scheme, programme and attributes are
#' \emph{factors} with their level order according to that of the
#' scheme, programme or attribute variable.
#'
#' @section Recommended usage:
#'
#'   \code{read_schemes()}
#'
#'   \code{read_schemes(lang = "nl")}
#'
#' @seealso
#' \code{\link{schemes}}
#'
#' @family reading functions for n2khab-referencelists
#'
#' @examples
#' \dontrun{
#' read_schemes()
#' read_schemes(lang = "nl")
#' }
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom dplyr
#' %>%
#' filter
#' select
#' mutate
#' left_join
#' as_tibble
#' contains
#' @importFrom rlang .data
read_schemes <-
    function(path = pkgdatasource_path("textdata/schemes", ".tsv"),
             file = "schemes",
             file_namelist = "namelist",
             lang = "en") {

        namelist <-
            read_namelist(path = path,
                          file = file_namelist,
                          lang = lang) %>%
            select(.data$code,
                   .data$name,
                   .data$shortname)

        suppressWarnings(
        read_vc(file = file, root = path) %>%
                mutate(
                    scheme_name = namelist_factor(.data$scheme,
                                                  codelist = namelist),
                    scheme_shortname = namelist_factor(.data$scheme,
                                                       "shortname",
                                                       codelist = namelist),
                    programme_name = namelist_factor(.data$programme,
                                                     codelist = namelist),
                    attribute_1_name = namelist_factor(.data$attribute_1,
                                                       codelist = namelist),
                    attribute_1_shortname = namelist_factor(.data$attribute_1,
                                                            "shortname",
                                                            codelist = namelist),
                    attribute_2_name = namelist_factor(.data$attribute_2,
                                                       codelist = namelist),
                    attribute_2_shortname = namelist_factor(.data$attribute_2,
                                                            "shortname",
                                                            codelist = namelist),
                    attribute_3_name = namelist_factor(.data$attribute_3,
                                                       codelist = namelist),
                    attribute_3_shortname = namelist_factor(.data$attribute_3,
                                                            "shortname",
                                                            codelist = namelist)
                    ) %>%
                left_join(namelist,
                          by = c("tag_1" = "code")) %>%
                rename(tag_1_name = .data$name,
                       tag_1_shortname = .data$shortname) %>%
                left_join(namelist,
                          by = c("tag_2" = "code")) %>%
                rename(tag_2_name = .data$name,
                       tag_2_shortname = .data$shortname) %>%
                left_join(namelist,
                          by = c("tag_3" = "code")) %>%
                rename(tag_3_name = .data$name,
                       tag_3_shortname = .data$shortname) %>%
                select(contains("scheme"),
                       contains("programme"),
                       contains("attribute"),
                       .data$spatial_restriction,
                       .data$notes,
                       contains("tag")
                       ) %>%
                as_tibble)

    }




















#' Return the 'scheme_types' data source as a tibble
#'
#' Returns the included data source \code{\link{scheme_types}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names and shortnames from \code{\link{namelist}} are optionally added,
#' in English by default.
#'
#' \code{\link{scheme_types}} is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which lists
#' the types (using the type-code from \code{\link{types}}) that belong to
#' each N2KHAB (monitoring or research) scheme (using the scheme-code from
#' \code{\link{schemes}}).
#' It also defines typegroup memberships of the types within specific schemes,
#' if applicable.
#'
#' \code{read_scheme_types()} reads the \code{\link{scheme_types}} data
#' source, optionally adds names + shortnames (always done for the typegroup)
#' and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a dataframe that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' names (\code{lang = "en"}) are returned.
#'
#' @param path Location of the data sources \code{scheme_types},
#' \code{schemes}, \code{types} and \code{namelist}.
#' The default is to use the location of the data sources as delivered by
#' the installed package.
#' @param file The filename of the \code{scheme_types} data source, without extension.
#' The default is to use the file delivered by the installed package.
#'
#' @inheritParams read_types
#' @inheritParams read_env_pressures
#'
#' @param extended Should names & shortnames be added for scheme, programme,
#' scheme attributes, type, typeclass and tags of scheme and type?
#'
#' @return
#' The \code{scheme_types} dataframe as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with names & shortnames added for the typegroup variable and optionally for
#' scheme, programme,
#' scheme attributes, type, typeclass and tags of scheme and type, all
#' according to the \code{lang} argument.
#' The tibble has either 5 or 44 variables, depending on the \code{extended}
#' argument.
#' See \code{\link{scheme_types}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding names (and shortnames).
#'
#' The \emph{optionally} added names and shortnames are represented by the
#' following variables:
#' \itemize{
#'   \item \code{scheme_name}
#'   \item \code{scheme_shortname}
#'   \item \code{programme_name}
#'   \item \code{attribute_1_name}
#'   \item \code{attribute_1_shortname}
#'   \item \code{attribute_2_name}
#'   \item \code{attribute_2_shortname}
#'   \item \code{attribute_3_name}
#'   \item \code{attribute_3_shortname}
#'   \item \code{schemetag_1_name}
#'   \item \code{schemetag_1_shortname}
#'   \item \code{schemetag_2_name}
#'   \item \code{schemetag_2_shortname}
#'   \item \code{schemetag_3_name}
#'   \item \code{schemetag_3_shortname}
#'   \item \code{type_name}
#'   \item \code{type_shortname}
#'   \item \code{typeclass_name}
#'   \item \code{typetag_1_name}
#'   \item \code{typetag_1_shortname}
#'   \item \code{typetag_2_name}
#'   \item \code{typetag_2_shortname}
#'   \item \code{typetag_3_name}
#'   \item \code{typetag_3_shortname}
#' }
#'
#' The added names and shortnames for scheme, programme, typeclass,
#' attributes and typegroup are \emph{factors} with their level order according
#' to that of the scheme, programme, typeclass, attribute or typegroup variable.
#'
#' @section Recommended usage:
#'
#'   \code{read_scheme_types()}
#'
#'   \code{read_scheme_types(lang = "nl")}
#'
#' @seealso
#' \code{\link{scheme_types}}
#'
#' @family reading functions for n2khab-referencelists
#'
#' @examples
#' \dontrun{
#' read_scheme_types()
#' read_scheme_types(lang = "nl")
#' }
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom dplyr
#' %>%
#' select
#' mutate
#' left_join
#' as_tibble
#' contains
#' pull
#' @importFrom tidyr gather spread
#' @importFrom stringr str_c
#' @importFrom rlang .data
read_scheme_types <- function(path = pkgdatasource_path("textdata/scheme_types", ".tsv"),
                              file = "scheme_types",
                              file_namelist = "namelist",
                              lang = "en",
                              extended = FALSE) {

    namelist <-
        read_namelist(path = path,
                      file = file_namelist,
                      lang = lang) %>%
        select(.data$code,
               .data$name,
               .data$shortname)

    scheme_types <- read_vc(file = file, root = path)

    if (extended) {

    schemes <-
        read_schemes(path = path,
                     file = "schemes",
                     file_namelist = file_namelist,
                     lang = lang) %>%
        gather(key = "key",
               value = "value",
               contains("tag")) %>%
        mutate(key = str_c("scheme", .data$key)) %>%
        spread(key = .data$key, value = .data$value)

    types <-
        read_types(path = path,
                   file = "types",
                   file_namelist = file_namelist,
                   lang = lang) %>%
        gather(key = "key",
               value = "value",
               contains("tag")) %>%
        mutate(key = str_c("type", .data$key)) %>%
        spread(key = .data$key, value = .data$value)

    scheme_types %>%
        left_join(schemes,
                  by = "scheme") %>%
        mutate(type = .data$type %>% as.character) %>%
        left_join(types,
                  by = "type") %>%
        mutate(type = .data$type %>%
                        factor(levels =
                                   read_vc(file = file, root = path) %>%
                                   pull(.data$type) %>%
                                   levels
                                   )) %>%
        mutate(typegroup_name = namelist_factor(.data$typegroup,
                                                codelist = namelist),
               typegroup_shortname = namelist_factor(.data$typegroup,
                                                     "shortname",
                                                     codelist = namelist)) %>%
        as_tibble

    } else {

        scheme_types %>%
            mutate(typegroup_name = namelist_factor(.data$typegroup,
                                                    codelist = namelist),
                   typegroup_shortname = namelist_factor(.data$typegroup,
                                                         "shortname",
                                                         codelist = namelist)) %>%
            as_tibble

    }

}







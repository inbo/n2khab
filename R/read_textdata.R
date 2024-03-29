#' Return the 'namelist' data source as a tibble
#'
#' Returns the included data source \code{\link{namelist}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}},
#' by default filtered according to English names and shortnames.
#'
#' \code{\link{namelist}} is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' names and (optionally) shortnames for IDs/codes used in other data sources.
#'
#' \code{read_namelist()} reads it and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a data frame that makes working in the tidyverse a little
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
#' The \code{namelist} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
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
#' read_namelist()
#' read_namelist(lang = "nl")
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr %>% filter as_tibble
read_namelist <-
  function(path = pkgdatasource_path("textdata/namelist", ".yml"),
           file = "namelist",
           lang = "en") {
    assert_that(is.string(lang))

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
      as_tibble()
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
      package = "n2khab"
    ) %>%
      dirname()
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
          by = "code"
        ) %>%
        select(.data$code, !!pick)
    )

    x %>%
      mapvalues(
        from = mapped_levels$code,
        to = mapped_levels[, 2]
      )
  }















#' Return the 'types' data source as a tibble with names & shortnames
#'
#' Returns the included data source \code{\link{types}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names and shortnames from \code{\link{namelist}} are added,
#' in English by default.
#'
#' \code{\link{types}} is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' a checklist of types, represented by their \strong{current} codes, together
#' with several attributes.
#' A 'type' refers to either a (main) habitat type, a
#' habitat subtype or a regionally important biotope (RIB).
#'
#' \code{read_types()} reads the \code{\link{types}} data source, adds
#' names + shortnames
#' and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a data frame that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' names (\code{lang = "en"}) are returned for types, attributes and tags.
#'
#' Note that factors are generated with implicit \code{NA} values (i.e. there is
#' no factor level to represent the missing values).
#' If you want this category to appear in certain results, you can convert
#' such variables with
#' \code{\link[forcats:fct_explicit_na]{forcats::fct_explicit_na()}}.
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
#' The \code{types} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with names & shortnames added for types, attributes and tags
#' according to the \code{lang} argument.
#' See \code{\link{types}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding names (and shortnames).
#'
#' The added names and shortnames are represented by the following variables:
#' \itemize{
#'   \item \code{type_name}
#'   \item \code{type_shortname}
#'   \item \code{typeclass_name}
#'   \item \code{hydr_class_name}
#'   \item \code{hydr_class_shortname}
#'   \item \code{groundw_dep_name}
#'   \item \code{groundw_dep_shortname}
#'   \item \code{flood_dep_name}
#'   \item \code{flood_dep_shortname}
#'   \item \code{tag_1_name}
#'   \item \code{tag_1_shortname}
#'   \item \code{tag_2_name}
#'   \item \code{tag_2_shortname}
#'   \item \code{tag_3_name}
#'   \item \code{tag_3_shortname}
#' }
#' Except for the tags, the names and shortnames are factors with their level
#' order according to that of the corresponding attribute.

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
#' read_types()
#' read_types(lang = "nl")
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr
#' %>%
#' distinct
#' filter
#' select
#' mutate
#' rename
#' tibble
#' left_join
#' as_tibble
#' pull
#' @importFrom plyr mapvalues
#' @importFrom rlang .data
read_types <-
  function(path = pkgdatasource_path("textdata/types", ".yml"),
           file = "types",
           file_namelist = "namelist",
           lang = "en") {
    assert_that(is.string(lang))

    langs <-
      read_namelist(
        path = path,
        file = file_namelist,
        lang = "all"
      ) %>%
      distinct(.data$lang) %>%
      pull(lang)

    assert_that(any(lang %in% langs),
      msg = "Your setting of lang is not supported."
    )

    namelist <-
      read_namelist(
        path = path,
        file = file_namelist,
        lang = lang
      ) %>%
      select(
        .data$code,
        .data$name,
        .data$shortname
      )

    types_base <-
      read_vc(file = file, root = path)

    suppressMessages(suppressWarnings({
      type_levels <-
        tibble(codelevel = types_base$type %>% levels()) %>%
        left_join(namelist,
          by = c("codelevel" = "code")
        ) %>%
        rename(
          namelevel = .data$name,
          shortnamelevel = .data$shortname
        )

      typeclass_levels <-
        tibble(codelevel = types_base$typeclass %>% levels()) %>%
        left_join(namelist %>% select(-.data$shortname),
          by = c("codelevel" = "code")
        ) %>%
        rename(namelevel = .data$name)

      types_base %>%
        left_join(namelist, by = c("type" = "code")) %>%
        rename(
          type_name = .data$name,
          type_shortname = .data$shortname
        ) %>%
        mutate(
          type = factor(.data$type,
            levels = types_base$type %>%
              levels()
          ),
          type_name =
            .data$type %>%
              mapvalues(
                from = type_levels$codelevel,
                to = type_levels$namelevel
              ),
          type_shortname =
            .data$type %>%
              mapvalues(
                from = type_levels$codelevel,
                to = type_levels$shortnamelevel
              ),
          typeclass_name =
            .data$typeclass %>%
              mapvalues(
                from = typeclass_levels$codelevel,
                to = typeclass_levels$namelevel
              ),
          hydr_class_name =
            .data$hydr_class %>%
              mapvalues(
                from = namelist$code,
                to = namelist$name
              ),
          hydr_class_shortname =
            .data$hydr_class %>%
              mapvalues(
                from = namelist$code,
                to = namelist$shortname
              ),
          groundw_dep_name =
            .data$groundw_dep %>%
              mapvalues(
                from = namelist$code,
                to = namelist$name
              ),
          groundw_dep_shortname =
            .data$groundw_dep %>%
              mapvalues(
                from = namelist$code,
                to = namelist$shortname
              ),
          flood_dep_name =
            .data$flood_dep %>%
              mapvalues(
                from = namelist$code,
                to = namelist$name
              ),
          flood_dep_shortname =
            .data$flood_dep %>%
              mapvalues(
                from = namelist$code,
                to = namelist$shortname
              )
        ) %>%
        left_join(namelist,
          by = c("tag_1" = "code")
        ) %>%
        rename(
          tag_1_name = .data$name,
          tag_1_shortname = .data$shortname
        ) %>%
        left_join(namelist,
          by = c("tag_2" = "code")
        ) %>%
        rename(
          tag_2_name = .data$name,
          tag_2_shortname = .data$shortname
        ) %>%
        left_join(namelist,
          by = c("tag_3" = "code")
        ) %>%
        rename(
          tag_3_name = .data$name,
          tag_3_shortname = .data$shortname
        ) %>%
        select(
          1:3, 11:12,
          4, 13,
          5, 14:15,
          6, 16:17,
          7, 18:19,
          8, 20:21,
          9, 22:23,
          10, 24:25
        ) %>%
        as_tibble()
    }))
  }













#' Return the 'env_pressures' data source as a tibble with human-readable
#' attributes
#'
#' Returns the included data source \code{\link{env_pressures}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names, shortnames, explanations and optional remarks from
#' \code{\link{namelist}} are added,
#' in English by default.
#'
#' \code{\link{env_pressures}} is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' a checklist of environmental pressures, represented by codes, together
#' with the pressure-class and the textual explanation.
#'
#' \code{read_env_pressures()} reads the \code{\link{env_pressures}} data
#' source, adds
#' human-readable attributes and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a data frame that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' text (\code{lang = "en"}) is returned for names of environmental pressures and
#' pressure-classes, and for textual explanations and remarks.
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
#' The \code{env_pressures} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with human-readable text added for environmental pressures,
#' pressure-classes and textual explanations and remarks
#' according to the \code{lang} argument.
#' The tibble has 35 rows and 7 variables.
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
#'   \item{\code{explanation}}{An explanation of the environmental pressure.}
#'   \item{\code{remarks}}{Optional remarks about the environmental
#'   pressure.}
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
#' read_env_pressures()
#' read_env_pressures(lang = "nl")
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr
#' %>%
#' select
#' distinct
#' mutate
#' rename
#' tibble
#' left_join
#' as_tibble
#' distinct
#' arrange
#' pull
#' @importFrom rlang .data
read_env_pressures <-
  function(path = pkgdatasource_path("textdata/env_pressures", ".yml"),
           file = "env_pressures",
           file_namelist = "namelist",
           lang = "en") {
    assert_that(is.string(lang))

    langs <-
      read_namelist(
        path = path,
        file = file_namelist,
        lang = "all"
      ) %>%
      distinct(.data$lang) %>%
      pull(lang)

    assert_that(any(lang %in% langs),
      msg = "Your setting of lang is not supported."
    )

    namelist <-
      read_namelist(
        path = path,
        file = file_namelist,
        lang = lang
      ) %>%
      select(
        .data$code,
        .data$name,
        .data$shortname
      )

    env_pressures_base <-
      read_vc(file = file, root = path)

    suppressWarnings(
      env_pressures_base2 <-
        read_vc(file = file, root = path) %>%
        left_join(namelist, by = c("ep_code" = "code")) %>%
        rename(
          ep_name = .data$name,
          ep_abbrev = .data$shortname
        ) %>%
        mutate(ep_code = .data$ep_code %>%
          factor(levels = env_pressures_base$ep_code %>% levels()))
    )

    ep_levels <-
      env_pressures_base2 %>%
      distinct(
        .data$ep_code,
        .data$ep_name,
        .data$ep_abbrev
      ) %>%
      arrange(.data$ep_code)

    ep_class_levels <-
      tibble(codelevel = env_pressures_base$ep_class %>% levels()) %>%
      left_join(namelist %>% select(-.data$shortname),
        by = c("codelevel" = "code")
      ) %>%
      rename(namelevel = .data$name)

    env_pressures_base2 %>%
      mutate(
        ep_name = .data$ep_name %>%
          factor(levels = ep_levels$ep_name),
        ep_abbrev = .data$ep_abbrev %>%
          factor(levels = ep_levels$ep_abbrev),
        ep_class_name =
          .data$ep_class %>%
            mapvalues(
              from = ep_class_levels$codelevel,
              to = ep_class_levels$namelevel
            )
      ) %>%
      left_join(namelist,
        by = c("explanation" = "code")
      ) %>%
      select(-.data$explanation) %>%
      rename(
        explanation = .data$name,
        remarks = .data$shortname
      ) %>%
      mutate(ep_code = .data$ep_code %>%
        factor(levels = env_pressures_base$ep_code %>% levels())) %>%
      select(
        .data$ep_code,
        .data$ep_abbrev,
        .data$ep_name,
        .data$ep_class,
        .data$ep_class_name,
        .data$explanation,
        .data$remarks
      ) %>%
      as_tibble()
  }

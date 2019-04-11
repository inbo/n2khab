#' Return the 'namelist' data source as a tibble
#'
#' Returns the included data source \code{\link{namelist}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}},
#' by default filtered according to English names and shortnames.
#'
#' 'namelist' is a data source in the
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









#' Return the 'types' data source as a tibble with names & shortnames
#'
#' Returns the included data source \code{\link{types}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names and shortnames from \code{\link{namelist}} are added,
#' in English by default.
#'
#' 'types' is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' a checklist of types, represented by their codes, together with several
#' attributes.
#' A 'type' refers to either a (main) habitat type, a
#' habitat subtype or a regionally important biotope (RIB).
#'
#' \code{read_types()} reads the 'types' data source, adds names + shortnames
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
#' See \code{\link{types}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding names (and shortnames).
#'
#' The added names and shortnames are represented by the following variables:
#' \itemize{
#'   \item \code{type_name}
#'   \item \code{type_shortname}
#'   \item \code{typeclass_name}
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

        typeclass_levels <-
            tibble(codelevel = types_base$typeclass %>% levels) %>%
            left_join(namelist %>% select(-.data$shortname),
                       by = c("codelevel" = "code")) %>%
            rename(namelevel = .data$name)

        types_base %>%
            left_join(namelist, by = c("type" = "code")) %>%
            rename(type_name = .data$name,
                   type_shortname = .data$shortname) %>%
            mutate(typeclass_name =
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
    }




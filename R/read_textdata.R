#' Return the 'namelist' dataframe
#'
#' Returns the included data source \code{\link{namelist}} as a dataframe.
#'
#' 'namelist' is a data source in the
#' \href{https://inbo.github.io/git2rdata/index.html}{vc-format} which provides
#' names and (optionally) shortnames for IDs/codes used in other data sources.
#'
#' \code{read_namelist()} reads it and returns it as a dataframe.
#' By default, the version delivered with the package is used and only English
#' names (\code{lang = "en"}) are returned.
#'
#' @param path Location of the data source.
#' The default is to use the location of the data source as delivered by
#' the installed package.
#' @param file The filename of the data source, without extension in the case
#' of multiple files with different extensions.
#' The default is to use the file delivered by the installed package.
#' @param lang An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{IETF BCP
#'   47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
#'   the language of \code{name} and \code{shortname} to be returned in the dataframe.
#'   If \code{lang = "all"}, the full \code{namelist} dataframe is returned, i.e.
#'   containing all languages.
#'
#' @return
#' The \code{namelist} dataframe, filtered according to the \code{lang} argument.
#' See \code{\link{namelist}} for documentation of the dataframe's contents.
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
#' @importFrom dplyr %>% filter
#' @importFrom stringr str_c
read_namelist <-
    function(path = pkgdatasource_path("textdata/namelist", ".tsv"),
             file = "namelist",
             lang = "en") {

        if (lang == "all") {
            read_vc(file = file, root = path)
        } else {
            read_vc(file = file, root = path) %>%
                filter(lang == !!lang)
        }
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
#' @keywords internal
pkgdatasource_path <-
    function(file, extension = "") {
        system.file(str_c(file, extension),
                    package = "n2khabutils"
        ) %>%
            dirname
    }

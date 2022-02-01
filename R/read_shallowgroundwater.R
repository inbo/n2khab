#' Return the data source \code{shallowgroundwater} as an \code{sf}
#' multipolygon layer
#'
#' Returns the raw data source \code{shallowgroundwater}
#' as an \code{sf} multipolygon
#' layer.
#' The coordinate reference system is 'BD72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' The R code for creating the \code{shallowgroundwater} data source,
#' starting from a manually composed layer, can be found in the
#' \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing}
#' repository.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of geometry type \code{MULTIPOLYGON}.
#'
#' @family functions returning environmental data sets
#'
#' @examples
#' \dontrun{
#' shallowgroundwater <- read_shallowgroundwater()
#' shallowgroundwater
#' }
#'
#' @importFrom assertthat
#' assert_that
#' @importFrom sf
#' read_sf
#' @export
read_shallowgroundwater <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "10_raw/shallowgroundwater/shallowgroundwater.gpkg")){

        assert_that(file.exists(file))

        shallowgroundwater <-
            suppressWarnings(
                read_sf(file,
                        crs = 31370)
            )

        return(shallowgroundwater)

    }

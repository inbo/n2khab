#' Return the processed data source \code{watercourse_100mseg}
#'
#' Returns the data source \code{watercourse_100mseg}
#' as a list of two \code{sf} objects:
#' \itemize{
#' \item \code{lines}
#' (\code{LINESTRING} geometry): represents
#' line segments of length 100 m derived from the raw \code{watercourses}
#' data source;
#' \item \code{points} (\code{POINT} geometry):
#' represents the \emph{downstream}
#' endpoints of all segments ('downstream' as defined in \code{watercourses}).
#' }
#' Optionally, only one of these objects is returned.
#' The coordinate reference system is 'Belge 72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' The data source \code{watercourse_100mseg} represents all officially known
#' watercourses
#' of the Flemish Region as line segments of 100 m (or < 100 m, for the most
#' upstream segment of a watercourse).
#' The data source can be used as a base layer of statistical
#' \strong{population units} (line segments) and corresponding anchor points,
#' in the design of monitoring and research of watercourses.
#'
#' The data source is a GeoPackage, available at
#' \href{https://doi.org/10.5281/zenodo.4452577}{Zenodo}, and contains two
#' spatial layers:
#' \itemize{
#'   \item{\code{watercourse_100mseg_lines}: the line segments;}
#'   \item{\code{watercourse_100mseg_points}: the corresponding downstream
#'   endpoints.}
#'   }
#' Both layers have the same number of rows and share the same attributes.
#'
#' The data source was derived from the raw
#' \href{https://doi.org/10.5281/zenodo.4420904}{\code{watercourses}} data
#' source as follows:
#' \enumerate{
#' \item each line ('watercourse') of \code{watercourses} is split into
#' segments of 100 m, where
#' the remaining segment of < 100 m (per original line) is situated most
#' upstream.
#' For this step, the direction of the lines has been reverted (in
#' \code{watercourses} the direction is from upstream to downstream).
#' A unique rank number is assigned to each segment, as well as the
#' VHAG code from the corresponding line in \code{watercourses}.
#' \item the downstream endpoint of each segment is located, and assigned the
#' same attributes (\code{rank} and \code{vhag_code}).
#' }
#'
#' The R and GRASS code for creating the \code{watercourse_100mseg} data source
#' can be found in the
#' \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing}
#' repository.
#'
#' @param element Optional string.
#' The string must be one of two possible values: \code{"lines"} or
#' \code{"points"}.
#' If set, either the \code{lines} or the \code{points} object will be
#' returned, respectively.
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' By default, a list of two \code{sf} objects (see 'Description').
#' The \code{lines} and the \code{points} objects have the same number of
#' rows.
#' They share the same attributes:
#' \describe{
#' \item{\code{rank}}{A unique, incremental number for each segment/endpoint.
#' It just reflects the downstream-to-upstream order of segments within
#' each original line.}
#' \item{\code{vhag_code}}{The VHAG code from the raw \code{watercourses} data
#' source.
#' It distinguishes the different watercourses, so it is common to all
#' segments/points that belong to the same watercourse.}
#' }
#' Optionally, only one of these \code{sf} objects is returned.
#'
#' @family functions involved in processing the 'watercourses' data source
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of the
#' # 'watercourse_100mseg' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' (lines <- read_watercourse_100mseg(element = "lines"))
#' (points <- read_watercourse_100mseg(element = "points"))
#' str(read_watercourse_100mseg(), give.attr = FALSE)
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom sf
#' read_sf
read_watercourse_100mseg <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "20_processed/watercourse_100mseg/watercourse_100mseg.gpkg"),
             element = NULL,
             version = "watercourse_100mseg_20200807v1"){

        assert_that(is.string(version))

        if (!missing(element)) {
            assert_that(is.string(element),
                        element %in% c("lines", "points"))
            res <-
                switch(element,
                       "lines" = read_sf(file,
                                         layer = "watercourse_100mseg_lines"),
                       "points" = read_sf(file,
                                          layer = "watercourse_100mseg_points")
                )
        } else {
            res <-
                list("lines" = read_sf(file,
                                       layer = "watercourse_100mseg_lines"),
                     "points" = read_sf(file,
                                        layer = "watercourse_100mseg_points"))
        }
        return(res)
    }

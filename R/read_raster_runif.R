#' Return the data source \code{raster_runif} as a RasterLayer
#'
#' The \code{raster_runif} data source covers Flanders and the Brussels
#' Capital Region
#' and has a resolution of 32 meters.
#' The raster cells with non-missing values match the value-cells of
#' the \code{GRTSmaster_habitats} data source (see \code{\link{read_GRTSmh}})
#' with a small buffer added.
#' Every raster cell has a random value between 0 and 1 according to the
#' uniform distribution.
#' The coordinate reference system is 'Belge 72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' The \code{raster_runif} data source is a GeoTIFF file (available at
#' \href{https://doi.org/10.5281/zenodo.4745984}{Zenodo}).
#'
#' The R-code for creating the \code{raster_runif} data source can be found in
#' the \href{https://github.com/inbo/n2khab-preprocessing}{
#' n2khab-preprocessing} repository.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A RasterLayer.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has
#' # the 'n2khab_data' folder AND that the latest version of the
#' # 'raster_runif' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can consider
#' # what to do.
#' r <- read_raster_runif()
#' r
#' raster::spplot(r)
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that

read_raster_runif <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "10_raw/raster_runif/raster_runif.tif"),
             version = "raster_runif_v1") {

        assert_that(file.exists(file))

        require_pkgs("raster")

        r <- raster::raster(file)
        raster::crs(r) <- "EPSG:31370"

        return(r)
        }

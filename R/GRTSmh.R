#' Convert a vector of values from the \code{GRTSmaster_habitats} data source to
#' base 4 fractions
#'
#' Converts decimal (i.e. base 10) integer values from the raster data source
#' \code{GRTSmaster_habitats} into base 4 fractions, using a precision of
#' 13 digits behind the decimal mark (as needed to cope with the range of
#' values).
#' For example, the integer \code{16} (\code{= 4^2}) is converted into
#' \code{0.0000000000100} and \code{4^12} is converted into
#' \code{0.1000000000000}.
#'
#' Long base 4 fractions seem to be handled and stored easier than long
#' (base 4) integers.
#' This approach follows the one of Stevens & Olsen (2004) to represent
#' the reverse hierarchical order in a GRTS sample as base-4-fraction addresses.
#'
#' The function works on a vector and retains \code{NA} values.
#' As such, it can be used in \code{raster::calc()}.
#' When writing such a raster to a file, it is needed to use the \code{FLT8S}
#' data type (see \code{\link[raster]{dataType}}).
#' Otherwise several digits will change.
#'
#' The function is based on code from the \code{baseConvert()} function
#' in Will Gray's \href{https://github.com/graywh/r-gmisc}{Gmisc} package.
#'
#' @param x A decimal (i.e. base 10) scalar or vector of integer values from the
#' \code{GRTSmaster_habitats} raster.
#'
#' @return
#' The corresponding base4 scalar or vector, stored as a fraction.
#'
#' @family functions involved in processing the 'GRTSmaster_habitats' data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. \doi{10.1198/016214504000000250}.
#'
#' @examples
#' oldoption <- options(list(digits = 15, scipen = 999))
#' convert_dec_to_base4frac(c(14, 15, NA, 456))
#' options(oldoption)
#'
#' @export
convert_dec_to_base4frac <-
    function(x) {

        sapply(x,

               function(x) {
                   ifelse(is.na(x), NA,
                          as.double(
                              ifelse(x > 0,{
                                  d <- floor(log(x, 4) + 1)
                                  paste(c("0", "1", "2", "3")[
                                      as.integer(abs(diff(x %% 4 ^ seq(d, 0))) %/%
                                                     4 ^ seq(d - 1, 0) + 1)],
                                      collapse = "")
                              },
                              '0'
                              )) / 10 ^ 13
                   )
               }

               )

    }


























#' Convert a vector of base 4 fractions into (truncated) decimal integer
#' values
#'
#' Converts base 4 fractions, representing the full GRTS addresses from the
#' raster data source \code{GRTSmaster_habitats} into decimal (i.e. base 10)
#' integer values.
#' Before the actual conversion happens, leading digits from the full GRTS
#' address can be discarded according to the \code{level} specified by the user.
#' Hence, the result may correspond to the GRTS ranking at a lower spatial
#' resolution.
#'
#' For example, the base 4 fraction \code{0.0000000000100}
#' is converted into decimal integer \code{16} (\code{= 4^2}) as long as the
#' \code{level} argument is \code{10} or lower (if not, it will be \code{0}) and
#'  \code{0.0000000000101} is converted into either \code{17} (
#'  \code{level <= 10}) or \code{1} (if \code{level} is \code{11} or \code{12}).
#'
#' Long base 4 fractions seem to be handled and stored easier than long
#' (base 4) integers.
#' This approach follows the one of Stevens & Olsen (2004) to represent
#' the reverse hierarchical order in a GRTS sample.
#'
#' The function works on a vector and retains \code{NA} values.
#' As such, it can be used in \code{raster::calc()}.
#' When writing such a raster to a file, it is recommended to use the
#' \code{INT4U} data type (see \code{\link[raster]{dataType}}).
#'
#' @param x A scalar or vector of base 4 fractions, originating from the
#' \code{GRTSmaster_habitats} data source.
#' @param level The number of leading digits to discard from the GRTS base 4
#' address, i.e. from the '\code{xxx...}' digits behind the decimal mark in
#' the '\code{0.xxxxxxxxxxxxx}' base 4 fractions.
#' Only values from \code{0} (maintain full address) to \code{12} (only return
#' last digit) are sensible, as the GRTS addresses are \code{13} digits long.
#'
#' @return
#' The corresponding decimal (i.e. base 10) integer scalar or vector.
#'
#' @family functions involved in processing the 'GRTSmaster_habitats' data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. \doi{10.1198/016214504000000250}.
#'
#' @examples
#' oldoption <- options(list(digits = 15, scipen = 999))
#' # one scalar:
#' convert_base4frac_to_dec(0.1010101010101, level = 0)
#' # vector, level 0:
#' convert_base4frac_to_dec(c(NA, 0.1010101010101), level = 0)
#' # vector, level 5:
#' convert_base4frac_to_dec(c(NA, 0.1010101010101), level = 5)
#' # same vector, all sensible levels computed:
#' sapply(0:12, function(i) convert_base4frac_to_dec(c(NA, 0.1010101010101),
#'                                           level = i)
#'       )
#' options(oldoption)
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom withr with_options
#' @importFrom stringr str_sub str_pad str_split
convert_base4frac_to_dec <-
    function(x, level) {

        with_options(
            c(scipen = 999,
              digits = 15), {

        multipliers <- as.matrix(4 ^ ((13 - level - 1):0))

        sapply(x,

               function(x, level2 = level) {
                   ifelse(is.na(x), NA, {
                       a <- x * 10 ^ level2
                       a <- round(a - floor(a), 13 - level2)
                       a <- a %>%
                           as.character %>%
                           str_sub(start = 3) %>%
                           str_pad(width = 13 - level2,
                                   side = "right",
                                   pad = "0") %>%
                           str_split("", simplify = TRUE) %>%
                           as.numeric
                       t(a) %*% multipliers
                   }
                   )
               }

        )

        })

    }


























#' Return the \code{GRTSmaster_habitats} data source as a RasterLayer or a
#' 10-layered variant as a RasterBrick
#'
#' By default, the \code{GRTSmaster_habitats} data source is returned as a
#' RasterLayer with decimal integer ranking numbers as values.
#' If \code{brick = TRUE}, a 10-layered RasterBrick is
#' returned (data source \code{GRTSmh_brick}; resolution 32 m)
#' with the decimal integer ranking numbers of 10 hierarchical levels of the
#' GRTS cell addresses, including the one from \code{GRTSmaster_habitats}
#' (with GRTS cell addresses at the resolution level).
#' The coordinate reference system is 'Belge 72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' The data source \code{GRTSmaster_habitats}, provided and documented in
#' \href{https://doi.org/10.5281/zenodo.2611233}{Zenodo}, is a monolayered GeoTIFF
#' file covering the whole of Flanders and the Brussels Capital Region at a
#' resolution of 32 m.
#' Its values are unique decimal integer ranking numbers from the GRTS
#' algorithm applied to the Flemish and Brussels area.
#' Beware that not all GRTS ranking numbers are present in the data source, as
#' the original GRTS raster has been clipped with the Flemish outer borders
#' (i.e., not excluding the Brussels Capital Region).
#'
#' The GRTS algorithm uses a quadrant-recursive, hierarchically randomized
#' function that maps the unit square to the unit interval, resulting in a
#' base-4 GRTS address for each location (see
#' \code{\link{read_GRTSmh_base4frac}}).
#' The ranking numbers in \code{GRTSmaster_habitats} are base-10 numbers and
#' follow the reverse
#' hierarchical order: each consecutive subset of ranking numbers corresponds
#' to a spatially balanced sample of locations.
#' Hence, it allows dynamical sample sizes.
#' More information on the GRTS algorithm can be found in Stevens & Olsen (2003,
#' 2004) and in the \href{https://github.com/ThierryO/grts}{GRTS} and
#' \href{https://CRAN.R-project.org/package=spsurvey}{spsurvey} packages.
#'
#' Depending on the value of the \code{brick} argument, the function either
#' returns the \code{GRTSmaster_habitats} data source as a
#' RasterLayer (\code{brick = FALSE}), or (\code{brick = TRUE}) returns the
#' 10-layered RasterBrick \code{GRTSmh_brick} (resolution 32 m)
#' with the decimal integer ranking numbers of 10 hierarchical levels of the
#' GRTS cell addresses, including the one from \code{GRTSmaster_habitats}
#' (with GRTS cell addresses at the resolution level).
#' The \code{GRTSmh_brick} data source is a processed dataset (10-layered
#' GeoTIFF), available at
#' \href{https://doi.org/10.5281/zenodo.3354403}{Zenodo}, and can only be
#' returned by the function when it is already present as a file.
#' See R-code in the \href{https://github.com/inbo/n2khab-preprocessing}{
#' n2khab-preprocessing} repository for its creation from
#' the \code{GRTSmaster_habitats} data source.
#'
#' Both GeoTIFFs (\code{GRTSmaster_habitats}, \code{GRTSmh_brick}) use the
#' \code{INT4S} datatype.
#'
#' The higher-level ranking numbers of the RasterBrick allow spatially balanced
#' samples at lower spatial resolution than that of 32 m, and can also be
#' used for aggregation purposes.
#' The provided hierarchical levels correspond to the resolutions vector
#' \code{32 * 2^(0:9)} (minimum: 32 meters, maximum: 16384 meters), with
#' the corresponding RasterBrick layers named as \code{level0} to \code{level9}.
#'
#' @param brick Logical; determines whether the RasterLayer or RasterBrick data
#' source is returned. See the Details section.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' Either a RasterLayer or a 10-layered RasterBrick, always with 21041043 cells.
#'
#' @family functions involved in processing the 'GRTSmaster_habitats' data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2003). Variance estimation for spatially balanced
#' samples of environmental resources. Environmetrics 14 (6): 593–610.
#' \doi{10.1002/env.606}.
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. \doi{10.1198/016214504000000250}.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has
#' # the 'n2khab_data' folder AND that the latest version of the
#' # 'GRTSmaster_habitats' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can consider
#' # what to do.
#' r <- read_GRTSmh()
#' r
#' }
#'
#' @export
#' @importFrom stringr str_c
read_GRTSmh <-
    function(file = file.path(fileman_up("n2khab_data"),
                              c("10_raw/GRTSmaster_habitats/GRTSmaster_habitats.tif",
                                "20_processed/GRTSmh_brick/GRTSmh_brick.tif")),
             brick = FALSE) {

        require_pkgs("raster")

        if (brick) {
            if (missing(file)) {
                    b <- raster::brick(file[2])} else {
                    b <- raster::brick(file)
                    }
            names(b) <- str_c("level", 0:(raster::nlayers(b) - 1))
            result <- b
        } else {
            if (missing(file)) {
                   r <- raster::raster(file[1])} else {
                   r <- raster::raster(file)
                   }
            result <- r
        }
        raster::crs(result) <- "EPSG:31370"
        return(result)
    }






















#' Return the processed data source \code{GRTSmh_base4frac} as a
#' RasterLayer
#'
#' The \code{GRTSmh_base4frac} data source is like a mirror to
#' \code{GRTSmaster_habitats}, holding the ranking numbers as base 4 fractions.
#' The function returns it as a RasterLayer in the Belgian Lambert 72 CRS
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' The data source file, read by the function, is a monolayered GeoTIFF in the
#' \code{FLT8S} datatype and is available at
#' \href{https://doi.org/10.5281/zenodo.3354401}{Zenodo}.
#' In \code{GRTSmh_base4frac}, the decimal (i.e. base 10) integer values from
#' the raster data source \code{GRTSmaster_habitats} (see
#' \code{\link{read_GRTSmh}}) have been converted into base 4 fractions,
#' using a precision
#' of 13 digits behind the decimal mark (as needed to cope with the range of
#' values).
#' For example, the integer \code{16} (\code{= 4^2}) has been converted into
#' \code{0.0000000000100} and \code{4^12} has been converted into
#' \code{0.1000000000000}.
#'
#' Long base 4 fractions seem to be handled and stored easier than long
#' (base 4) integers.
#' This approach follows the one of Stevens & Olsen (2004) to represent
#' the reverse hierarchical order in a GRTS sample as base-4-fraction addresses.
#'
#' See R-code in the \href{https://github.com/inbo/n2khab-preprocessing}{
#' n2khab-preprocessing} repository for the creation from
#' the \code{GRTSmaster_habitats} data source.
#'
#' Beware that not all GRTS ranking numbers are present in the data source, as
#' the original GRTS raster has been clipped with the Flemish outer borders
#' (i.e., not excluding the Brussels Capital Region).
#'
#' Also, be warned that R does not regard the values as base 4, but
#' as base 10.
#' So, what really matters is only the notation with many digits, to be
#' \emph{regarded} as a base 4 fraction.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A RasterLayer with 21041043 cells.
#'
#' @family functions involved in processing the 'GRTSmaster_habitats' data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. \doi{10.1198/016214504000000250}.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has
#' # the 'n2khab_data' folder AND that the latest version of the
#' # 'GRTSmh_base4frac' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can consider
#' # what to do.
#' r <- read_GRTSmh_base4frac()
#' r
#' }
#'
#' @export
read_GRTSmh_base4frac <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "20_processed/GRTSmh_base4frac/GRTSmh_base4frac.tif")) {

        require_pkgs("raster")

        r <- raster::raster(file)
        raster::crs(r) <- "EPSG:31370"
        return(r)
    }




























#' Return a RasterLayer or an \code{sf} polygon layer from the processed data
#' source \code{GRTSmh_diffres}
#'
#' The \code{GRTSmh_diffres} data source is derived from
#' \code{GRTSmh_brick}.
#' It provides the hierarchical levels 1 to 9 of the
#' GRTS cell addresses at the corresponding spatial resolution.
#' The function returns one selected level, either as a RasterLayer or as an
#' \code{sf} polygon layer (in the latter case, only levels 4 to 9 are
#' provided).
#' The coordinate reference system is 'Belge 72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' The \code{GRTSmh_diffres} data source file is a file collection (available
#' at \href{https://doi.org/10.5281/zenodo.3354405}{Zenodo}), composed of
#' nine monolayered GeoTIFF files of the \code{INT4S} datatype plus a GeoPackage
#' with six polygon layers:
#' \itemize{
#' \item{The polygon layers in the GeoPackage are the dissolved, polygonized
#' versions of levels 4 to 9 of the
#' \code{GRTSmh_brick} data source (see \code{\link{read_GRTSmh}}).
#' This means that they provide the decimal (i.e. base 10) integer values of
#' these \emph{higher hierarchical levels} of the GRTS cell addresses
#' of the raw data source \code{GRTSmaster_habitats}.
#' Hence, the polygons are typically squares that correspond to the GRTS cell at
#' the specified hierarchical level.
#' The polygon layer is however restricted to the non-\code{NA} cells of the original
#' \code{GRTSmaster_habitats} raster.
#' Consequently, a part of the polygons is clipped along the Flemish border.
#' Levels 1 to 3 are not provided for the whole of Flanders,
#' because this would inflate the GPKG file.
#' You can look at the \href{https://github.com/inbo/n2khab-preprocessing}{source code}
#' to do such things.
#' }
#' \item{The GeoTIFF files provide the respective levels 1 to 9 of the
#' \code{GRTSmh_brick} data source in a raster format, at the resolution that
#' corresponds to the GRTS cell at the specified hierarchical level.
#' The presence of \code{NA} cells around Flanders at level 0 implies that, with
#' decreasing resolution, the raster's extent increases and larger areas outside
#' Flanders are covered by non-\code{NA} cells along the border.
#' }
#' }
#'
#' The function returns the selected \code{level} either as an \code{sf} polygon
#' layer or as a RasterLayer, depending on the
#' \code{polygon} argument.
#'
#' The higher-level ranking numbers (compared to the original level 0) allow
#' spatially balanced samples at lower spatial resolution than that of 32 m,
#' and can also be used for aggregation purposes.
#'
#' The levels 1 to 9 correspond to the resolutions vector
#' \code{32 * 2^(1:9)} in meters:
#'
#' \tabular{rr}{
#' level \tab    resolution (m)\cr
#' 1 \tab    64\cr
#' 2 \tab   128\cr
#' 3 \tab   256\cr
#' 4 \tab   512\cr
#' 5 \tab  1024\cr
#' 6 \tab  2048\cr
#' 7 \tab  4096\cr
#' 8 \tab  8192\cr
#' 9 \tab 16384
#' }
#'
#' See R-code in the \href{https://github.com/inbo/n2khab-preprocessing}{
#' n2khab-preprocessing} repository for the creation from
#' the \code{GRTSmh_brick} data source.
#'
#' Beware that not all GRTS ranking numbers at the specified level are
#' provided, as the original GRTS raster has been clipped with the Flemish
#' outer borders (i.e., not excluding the Brussels Capital Region).
#'
#' @param dir The data source directory (absolute or relative).
#' The default follows the data management advice in the
#' vignette on data storage (run \code{vignette("v020_datastorage")}).
#' It uses the first \code{n2khab_data} folder that is found when
#' sequentially climbing up 0 to 10 levels in the file system hierarchy,
#' starting from the working directory.
#' @param level Integer in the range from 1 to 9; determines the spatial
#' resolution. See the Details section.
#' @param polygon Logical; determines whether a polygon layer or a
#' RasterLayer is returned. See the Details section.
#'
#' @return
#' Either a RasterLayer or a Simple feature collection of geometry type
#' \code{POLYGON}.
#'
#' @family functions involved in processing the 'GRTSmaster_habitats' data source
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has
#' # the 'n2khab_data' folder AND that the latest version of the
#' # 'GRTSmh_diffres' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can consider
#' # what to do.
#' r <- read_GRTSmh_diffres(level = 7)
#' r
#' raster::spplot(r)
#' p <- read_GRTSmh_diffres(level = 7, polygon = TRUE)
#' p
#' plot(p)
#' }
#'
#' @export
#' @importFrom stringr str_c
#' @importFrom sf
#' read_sf
#' st_crs<-
read_GRTSmh_diffres <-
    function(dir = file.path(fileman_up("n2khab_data"), "20_processed/GRTSmh_diffres"),
             level,
             polygon = FALSE) {

        if (!(level %in% 1:9 & level %% 1 == 0)) {
            stop("level must be an integer in the range 1 to 9.")
        }

        if (polygon) {

            if (!(level %in% 4:9)) {
                stop("When polygon = TRUE, level must be an integer in the range 4 to 9.")
            }

            p <- read_sf(file.path(dir,
                              "GRTSmh_diffres.gpkg"),
                    layer = str_c("GRTSmh_polygonized_level", level))
            suppressWarnings(st_crs(p) <- 31370)
            p

        } else {

            require_pkgs("raster")

            r <- raster::raster(file.path(dir,
                                          str_c("GRTSmh_diffres.",
                                                level, ".tif")))
            names(r) <- str_c("level", level)
            raster::crs(r) <- "EPSG:31370"
            r

        }
    }





















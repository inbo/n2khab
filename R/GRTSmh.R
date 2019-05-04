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
#' @family functions involved in processing the \code{GRTSmaster_habitats} data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. DOI: 10.1198/016214504000000250.
#'
#' @examples
#' \dontrun{
#' oldoption <- getOption("digits")
#' options(digits = 15)
#' dec_to_base4frac(c(14, 15, NA, 456))
#' options(digits = oldoption)
#' }
#'
#' @export
dec_to_base4frac <-
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
#' Hence, the result may correspond to the GRTS ranking at a higher spatial
#' level.
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
#' @family functions involved in processing the \code{GRTSmaster_habitats} data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. DOI: 10.1198/016214504000000250.
#'
#' @examples
#' \dontrun{
#' oldoption <- options(list(digits = 15, scipen = 999))
#' # one scalar:
#' base4frac_to_dec(0.1010101010101, level = 0)
#' # vector, level 0:
#' base4frac_to_dec(c(NA, 0.1010101010101), level = 0)
#' # vector, level 5:
#' base4frac_to_dec(c(NA, 0.1010101010101), level = 5)
#' # same vector, all sensible levels computed:
#' sapply(0:12, function(i) base4frac_to_dec(c(NA, 0.1010101010101),
#'                                           level = i)
#'       )
#' options(oldoption)
#' }
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom withr with_options
#' @importFrom stringr str_sub str_pad str_split
base4frac_to_dec <-
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
#'
#' The data source \code{GRTSmaster_habitats}, provided and documented in
#' \href{https://zenodo.org/record/2611234}{Zenodo}, is a monolayered GeoTIFF
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
#' \href{https://cran.r-project.org/web/packages/spsurvey}{spsurvey} packages.
#'
#' Depending on the value of the \code{brick} argument, the function either
#' returns the \code{GRTSmaster_habitats} data source as a
#' RasterLayer (\code{brick = FALSE}), or (\code{brick = TRUE}) returns the
#' 10-layered RasterBrick \code{GRTSmh_brick} (resolution 32 m)
#' with the decimal integer ranking numbers of 10 hierarchical levels of the
#' GRTS cell addresses, including the one from \code{GRTSmaster_habitats}
#' (with GRTS cell addresses at the resolution level).
#' The \code{GRTSmh_brick} data source is a processed dataset (10-layered
#' GeoTIFF) and can only be
#' returned by the function when it is already present as a file.
#' See R-code in the \href{https://github.com/inbo/n2khab-inputs}{
#' n2khab-inputs} repository for its creation from
#' the \code{GRTSmaster_habitats} data source.
#'
#' Both GeoTIFFs (\code{GRTSmaster_habitats}, \code{GRTSmh_brick}) use the
#' \code{INT4S} datatype.
#'
#' The higher-level ranking numbers of the RasterBrick allow spatially balanced
#' samples at higher spatial levels than the resolution of 32, and can also be
#' used for aggregation purposes.
#' The provided hierarchical levels correspond to the vector
#' \code{32 * 2^(0:9)} (minimum: 32 meters, maximum: 16384 meters), with
#' the corresponding RasterBrick layers named as \code{level0} to \code{level9}.
#'
#' @param path Location of the file.
#' Considering the default value of the \code{file} argument, use this argument
#' in scripts to set the location of the folder '\strong{\code{data}}'.
#' @param file The filename of the data source.
#' May include a path prefix.
#' The default follows the data management advice in the
#' \href{https://github.com/inbo/n2khab-inputs}{n2khab-inputs} repository.
#' @param brick Logical; determines whether the RasterLayer or RasterBrick data
#' source is returned. See the Details section.
#'
#' @return
#' Either a RasterLayer or a 10-layered RasterBrick, always with 21041043 cells.
#'
#' @family functions involved in processing the \code{GRTSmaster_habitats} data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2003). Variance estimation for spatially balanced
#' samples of environmental resources. Environmetrics 14 (6): 593–610.
#' DOI: 10.1002/env.606.
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. DOI: 10.1198/016214504000000250.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory is a folder next to
#' # the 'n2khab-inputs' repository AND that the
#' # 'GRTSmaster_habitats' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can consider
#' # what to do.
#' r <- read_GRTSmh("../n2khab-inputs/data")
#' r
#' }
#'
#' @export
#' @importFrom raster
#' raster
#' brick
#' nlayers
#' @importFrom stringr str_c
read_GRTSmh <-
    function(path,
             file = c("10_raw/GRTSmaster_habitats/GRTSmaster_habitats.tif",
                      "20_processed/GRTSmh_brick/GRTSmh_brick.tif"),
             brick = FALSE) {

        if (brick) {
            if (length(file) == 2) {
                    b <- brick(file.path(path, file[2]))} else {
                    b <- brick(file.path(path, file))
                    }
            names(b) <- str_c("level", 0:(nlayers(b) - 1))
            return(b)
        } else {
            if (length(file) == 2) {
                   raster(file.path(path, file[1]))} else {
                   raster(file.path(path, file))
                   }
        }
    }






















#' Return the processed data source \code{GRTSmh_base4frac} as a
#' RasterLayer
#'
#' The \code{GRTSmh_base4frac} data source is like a mirror to
#' \code{GRTSmaster_habitats}, holding the ranking numbers as base 4 fractions.
#' The function returns it as a RasterLayer.
#'
#' The data source file, read by the function, is a monolayered GeoTIFF in the
#' \code{FLT8S} datatype.
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
#' Beware that not all GRTS ranking numbers are present in the data source, as
#' the original GRTS raster has been clipped with the Flemish outer borders
#' (i.e., not excluding the Brussels Capital Region).
#'
#' @param path Location of the file.
#' Considering the default value of the \code{file} argument, use this argument
#' in scripts to set the location of the folder '\strong{\code{data}}'.
#' @param file The filename of the data source.
#' May include a path prefix.
#' The default follows the data management advice in the
#' \href{https://github.com/inbo/n2khab-inputs}{n2khab-inputs} repository.
#'
#' @return
#' A RasterLayer with 21041043 cells.
#'
#' @family functions involved in processing the \code{GRTSmaster_habitats} data source
#'
#' @references
#'
#' Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
#' Resources. Journal of the American Statistical Association 99 (465):
#' 262–278. DOI: 10.1198/016214504000000250.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory is a folder next to
#' # the 'n2khab-inputs' repository AND that the
#' # 'GRTSmh_base4frac' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can consider
#' # what to do.
#' r <- read_GRTSmh_base4frac("../n2khab-inputs/data")
#' r
#' }
#'
#' @export
#' @importFrom raster raster
read_GRTSmh_base4frac <-
    function(path,
             file = "20_processed/GRTSmh_base4frac/GRTSmh_base4frac.tif") {
        raster(file.path(path, file))
    }





























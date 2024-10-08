% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GRTSmh.R
\name{read_GRTSmh_base4frac}
\alias{read_GRTSmh_base4frac}
\title{Return the processed data source \code{GRTSmh_base4frac} as a
SpatRaster}
\usage{
read_GRTSmh_base4frac(
  file = file.path(locate_n2khab_data(),
    "20_processed/GRTSmh_base4frac/GRTSmh_base4frac.tif")
)
}
\arguments{
\item{file}{The absolute or relative file path of the data source.
The default follows the data management advice in the
vignette on data storage (run \code{vignette("v020_datastorage")}).
It uses the first \code{n2khab_data} folder that is found when
sequentially climbing up 0 to 10 levels in the file system hierarchy,
starting from the working directory.}
}
\value{
A SpatRaster with 21041043 cells.

If the package is configured to use the raster package
(see \code{\link[=n2khab_options]{n2khab_options()}}), a RasterLayer is
returned instead.
}
\description{
The \code{GRTSmh_base4frac} data source is like a mirror to
\code{GRTSmaster_habitats}, holding the ranking numbers as base 4 fractions.
The function returns it as a SpatRaster in the Belgian Lambert 72 CRS
(EPSG-code \href{https://epsg.io/31370}{31370}).
}
\details{
The data source file, read by the function, is a monolayered GeoTIFF in the
\code{FLT8S} datatype and is available at
\href{https://doi.org/10.5281/zenodo.3354401}{Zenodo}.
In \code{GRTSmh_base4frac}, the decimal (i.e. base 10) integer values from
the raster data source \code{GRTSmaster_habitats} (see
\code{\link{read_GRTSmh}}) have been converted into base 4 fractions,
using a precision
of 13 digits behind the decimal mark (as needed to cope with the range of
values).
For example, the integer \code{16} (\code{= 4^2}) has been converted into
\code{0.0000000000100} and \code{4^12} has been converted into
\code{0.1000000000000}.

Long base 4 fractions seem to be handled and stored easier than long
(base 4) integers.
This approach follows the one of Stevens & Olsen (2004) to represent
the reverse hierarchical order in a GRTS sample as base-4-fraction addresses.

See R-code in the \href{https://github.com/inbo/n2khab-preprocessing}{
n2khab-preprocessing} repository for the creation from
the \code{GRTSmaster_habitats} data source.

Beware that not all GRTS ranking numbers are present in the data source, as
the original GRTS raster has been clipped with the Flemish outer borders
(i.e., not excluding the Brussels Capital Region).

Also, be warned that R does not regard the values as base 4, but
as base 10.
So, what really matters is only the notation with many digits, to be
\emph{regarded} as a base 4 fraction.
}
\examples{
\dontrun{
# This example supposes that your working directory or a directory up to 10
# levels above has
# the 'n2khab_data' folder AND that the latest version of the
# 'GRTSmh_base4frac' data source is present in the default subdirectory.
# In all other cases, this example won't work but at least you can consider
# what to do.
oldopt <- options(scipen = 999, digits = 15)
r <- read_GRTSmh_base4frac()
r
options(oldopt)
}

}
\references{
Stevens D.L. & Olsen A.R. (2004). Spatially Balanced Sampling of Natural
Resources. Journal of the American Statistical Association 99 (465):
262–278. \doi{10.1198/016214504000000250}.
}
\seealso{
Other functions involved in processing the 'GRTSmaster_habitats' data source: 
\code{\link{convert_base4frac_to_dec}()},
\code{\link{convert_dec_to_base4frac}()},
\code{\link{read_GRTSmh}()},
\code{\link{read_GRTSmh_diffres}()}
}
\concept{functions involved in processing the 'GRTSmaster_habitats' data source}

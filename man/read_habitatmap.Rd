% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_habitatdata.R
\name{read_habitatmap}
\alias{read_habitatmap}
\title{Return the data source \code{habitatmap} as an \code{sf} multipolygon layer}
\usage{
read_habitatmap(
  file = file.path(locate_n2khab_data(), "10_raw/habitatmap"),
  filter_hab = FALSE,
  fix_geom = FALSE,
  version = c("habitatmap_2023", "habitatmap_2020", "habitatmap_2018")
)
}
\arguments{
\item{file}{The absolute or relative file path of the data source.
The default follows the data management advice in the
vignette on data storage (run \code{vignette("v020_datastorage")}).
It uses the first \code{n2khab_data} folder that is found when
sequentially climbing up 0 to 10 levels in the file system hierarchy,
starting from the working directory.}

\item{filter_hab}{If \code{TRUE} only polygons that (partially) contain habitat or a regionally
important biotope (RIB) are returned. The default value is \code{FALSE}. This
requires the corresponding version of the processed data source
\code{habitatmap_stdized} to be present in its default location inside the
\code{n2khab_data} folder.}

\item{fix_geom}{Logical.
Should invalid or corrupt geometries be fixed in the resulting \code{sf}
object in order to make them valid?
This prevents potential problems in geospatial operations, but beware that
fixed geometries are different from the original ones.
\code{\link[sf:st_make_valid]{sf::st_make_valid()}} is used to fix
geometries (with GEOS as backend).
Defaults to \code{FALSE}.}

\item{version}{Version ID of the data source.
Defaults to the latest available version defined by the package.}
}
\value{
A Simple feature collection of
type \code{MULTIPOLYGON}.
}
\description{
Returns the raw data source \code{habitatmap} (De Saeger et al., 2023)
as a standardized \code{sf} multipolygon layer
(tidyverse-styled, internationalized) in the Belgian Lambert 72 CRS
(EPSG-code \href{https://epsg.io/31370}{31370}).
Given the size of the data source, this function
takes a bit longer than usual to run.
}
\examples{
\dontrun{
# This example supposes that your working directory or a directory up to 10
# levels above has the 'n2khab_data' folder AND that the latest version of
# the 'habitatmap'
# data source is present in the default subdirectory.
# In all other cases, this example won't work but at least you can
# consider what to do.

hm <- read_habitatmap()
hm

hm_valid <- read_habitatmap(fix_geom = TRUE)
hm_valid

all(sf::st_is_valid(hm))
all(sf::st_is_valid(hm_valid))
}

}
\references{
\itemize{
\item De Saeger S., Dhaluin P., Erens R., Guelinckx G., Hennebel D.,
Jacobs I., Kumpen M., Van Oost F., Spanhove T., Leyssen A., Oosterlynck P.,
Van Dam G., Van Hove M., Wils C. (red.) (2023).
Biologische Waarderingskaart en Natura 2000 Habitatkaart, uitgave 2023.
(Rapporten van het Instituut voor Natuur- en Bosonderzoek; Nr. 31).
Instituut voor Natuur- en Bosonderzoek (INBO).
\doi{10.21436/inbor.96375305}.
\item De Saeger, S., Oosterlynck, P. & Paelinckx, D. (2017). The Biological
Valuation Map (BVM): a field-driven survey of land cover and vegetation in
the Flemish Region of Belgium. Documents phytosociologiques - Actes du
colloque de Saint-Mandé 2012 - Prodrome et cartographie des végétations de
France - 2017. Vol. 6: 372-382.
}
}
\seealso{
Other functions involved in processing the 'habitatmap' data source: 
\code{\link{read_habitatmap_stdized}()},
\code{\link{read_habitatmap_terr}()},
\code{\link{read_watersurfaces_hab}()}
}
\concept{functions involved in processing the 'habitatmap' data source}

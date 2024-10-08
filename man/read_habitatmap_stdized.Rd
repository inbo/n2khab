% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_habitatdata.R
\name{read_habitatmap_stdized}
\alias{read_habitatmap_stdized}
\title{Return the data source \code{habitatmap_stdized} as a list of two
objects}
\usage{
read_habitatmap_stdized(
  file = file.path(locate_n2khab_data(),
    "20_processed/habitatmap_stdized/habitatmap_stdized.gpkg"),
  version = c("habitatmap_stdized_2023_v1", "habitatmap_stdized_2020_v1",
    "habitatmap_stdized_2018_v2", "habitatmap_stdized_2018_v1")
)
}
\arguments{
\item{file}{The absolute or relative file path of the data source.
The default follows the data management advice in the
vignette on data storage (run \code{vignette("v020_datastorage")}).
It uses the first \code{n2khab_data} folder that is found when
sequentially climbing up 0 to 10 levels in the file system hierarchy,
starting from the working directory.}

\item{version}{Version ID of the data source.
Defaults to the latest available version defined by the package.}
}
\value{
A list of two objects:
  \itemize{
  \item \code{habitatmap_polygons}: an sf object of \code{habitatmap} polygons with two attribute variables
  \itemize{
    \item \code{polygon_id}
    \item \code{description_orig}: polygon description based on the
    orginal type codes in the raw \code{habitatmap}}
  }
  \itemize{
  \item \code{habitatmap_types}: a tibble with following variables
  \itemize{
    \item \code{polygon_id}
    \item \code{type}: habitat or RIB type listed in \code{\link{types}}.
    \item \code{certain}: \code{TRUE} when the type is certain and
     \code{FALSE} when the type is uncertain.
    \item \code{code_orig}: original type code in raw \code{habitatmap}.
    \item \code{phab}: proportion of polygon covered by type, as a percentage.
    }
    Since version \code{habitatmap_stdized_2020_v1}, rows are unique only
    by the combination of the \code{polygon_id}, \code{type} and
    \code{certain} columns.
    }
}
\description{
\code{read_habitatmap_stdized} returns the data source \code{habitatmap_stdized} as a list of two objects:
\itemize{
  \item \code{habitatmap_polygons}: an sf object in the Belgian Lambert 72
  CRS (EPSG-code \href{https://epsg.io/31370}{31370}) with all polygons
  of the \code{habitatmap} that contain habitat or a regionally
  important biotope (RIB).
  \item \code{habitatmap_types}: a tibble with information on the
  habitat and RIB \link{types} (HAB1, HAB2,..., HAB5) that occur within
  each polygon of \code{habitatmap_polygons}.
  }
}
\details{
The data source \code{habitatmap_stdized} is the processed version
of the raw data source \code{habitatmap} (De Saeger et al., 2023).
Every polygon in the \code{habitatmap} can consist of maximum 5
different types. This information is stored in the
columns 'HAB1', HAB2',..., 'HAB5' of the attribute table. The
fraction of each type within the polygons is stored in
the columns 'PHAB1', 'PHAB2', ..., 'PHAB5'.

The data source \code{habitatmap_stdized} is a GeoPackage, available at
\href{https://doi.org/10.5281/zenodo.3355192}{Zenodo}, that
contains:
\itemize{
  \item \code{habitatmap_polygons}: a spatial layer with every
  \code{habitatmap} polygon that contains a habitat or RIB type
  listed in \code{\link{types}}.
  \item \code{habitatmap_types}: a table with the types that occur in each polygon.
  }

The processing of the \code{habitatmap_types} tibble included
following steps:
\itemize{
  \item For some polygons the type is uncertain, and the
  type code in the raw \code{habitatmap} data source consists
  of 2 or 3 possible types, separated with a ','. The different
  possible types are split up and one row is created for
  each of them, with \code{phab} for each new row simply set to the original
  value of \code{phab}.
  The variable \code{certain} will be \code{FALSE} if
  the original code consists of 2 or 3 possible types, and \code{TRUE}
  if only one type is provided.
  \item Some polygons contain both a standing water habitat type
  and \code{rbbmr}:
  \code{3130_rbbmr},
  \code{3140_rbbmr},
  \code{3150_rbbmr} and
  \code{3160_rbbmr}.
  Since \code{habitatmap_stdized_2020_v1}, the two
  types \code{31xx} and \code{rbbmr} are split up and one row is
  created for each of them, with \code{phab} for each new row simply
  set to the original value of \code{phab}.
  The variable \code{certain} in this case will be \code{TRUE} for both
  types.
  \item After those first two steps, a given polygon could contain the
  same type with the same value for \code{certain} repeated several
  times, e.g. when \code{31xx_rbbmr} is present with \code{phab} = yy\%
  and \code{31xx} is present with \code{phab} = zz\%.
  In that case the rows with the same \code{polygon_id}, \code{type}
  and \code{certain} were gathered into one row and the respective
  \code{phab} values were added up.
  \item For some polygons the original type code in the
  \code{habitatmap} was not consistent with general coding syntax or
  with the type codes from the \code{\link{types}} data source. In that
  case the code was adjusted.

  }

  The R-code for creating the \code{habitatmap_stdized} data source
  can be found in the \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing} repository.
}
\examples{
\dontrun{
# This example supposes that your working directory or a directory up to 10
# levels above has the 'n2khab_data' folder AND that the latest version of
# the 'habitatmap_stdized'
# data source is present in the default subdirectory.
# In all other cases, this example won't work but at least you can
# consider what to do.

hms <- read_habitatmap_stdized()
hms_polygons <- hms$habitatmap_polygons
hms_types <- hms$habitatmap_types
hms_polygons
hms_types
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
\code{\link{read_habitatmap}()},
\code{\link{read_habitatmap_terr}()},
\code{\link{read_watersurfaces_hab}()}
}
\concept{functions involved in processing the 'habitatmap' data source}

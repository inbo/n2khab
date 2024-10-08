% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_habitatdata.R
\name{read_habitatmap_terr}
\alias{read_habitatmap_terr}
\title{Return the data source \code{habitatmap_terr} as a list of two
objects}
\usage{
read_habitatmap_terr(
  file = file.path(locate_n2khab_data(),
    "20_processed/habitatmap_terr/habitatmap_terr.gpkg"),
  keep_aq_types = TRUE,
  drop_7220 = TRUE,
  version = c("habitatmap_terr_2023_v1", "habitatmap_terr_2020_v2",
    "habitatmap_terr_2020_v1", "habitatmap_terr_2018_v2", "habitatmap_terr_2018_v1")
)
}
\arguments{
\item{file}{The absolute or relative file path of the data source.
The default follows the data management advice in the
vignette on data storage (run \code{vignette("v020_datastorage")}).
It uses the first \code{n2khab_data} folder that is found when
sequentially climbing up 0 to 10 levels in the file system hierarchy,
starting from the working directory.}

\item{keep_aq_types}{Logical; \code{TRUE} by default.
The data source \code{habitatmap_terr} aims at delineating all
polygons with at least one (semi-)terrestrial type.
For those polygons, it returns all known habitat types and RIBs as types.
Hence, in several cases polygons do occur with a combination of terrestrial
and aquatic types (see \emph{Details} for a definition of 'aquatic').
Setting \code{keep_aq_types = FALSE} is convenient for use cases where one
only wants to look at the (semi-)terrestrial types: this setting will
discard all aquatic types in 'mixed' aquatic/terrestrial polygons.
As each polygon always has at least one (semi-)terrestrial type,
this will not affect the number of polygons returned,
only the number of types.}

\item{drop_7220}{Logical; \code{TRUE} by default.
Should occurrences of type \code{7220} be dropped from the result?
To get more accurate information about type \code{7220}, notably its
occurrences, surface area and other characteristics, it is advised
to use the \code{habitatsprings} data source and not
\code{habitatmap_terr} - see
\code{\link[=read_habitatsprings]{read_habitatsprings()}}.}

\item{version}{Version ID of the data source.
Defaults to the latest available version defined by the package.}
}
\value{
A list of two objects:
  \itemize{
  \item \code{habitatmap_terr_polygons}: a Simple feature collection of
  geometry type \code{MULTIPOLYGON} with four attribute variables:
  \itemize{
    \item \code{polygon_id}
    \item \code{description_orig}: polygon description based on the
    original type codes in the \code{habitatmap} data source
    \item \code{description}: based on \code{description_orig} but with the
    interpreted type codes
    \item \code{source}: states where \code{description} comes from: either
    \code{habitatmap_stdized} or \code{habitatmap_stdized + interpretation}
  }
  \item \code{habitatmap_terr_types}: a tibble with the following
  variables (the first 4 being identical to those in
  \code{habitatmap_stdized}):
  \itemize{
    \item \code{polygon_id}
    \item \code{type}: the interpreted habitat or RIB type
    \item \code{certain}
    \item \code{code_orig}
    \item \code{phab}
    \item \code{source}: states where \code{type} comes from: either
    \code{habitatmap_stdized} or \code{habitatmap_stdized + interpretation}
    }
    Since version \code{habitatmap_terr_2020_v1}, rows are unique only
    by the combination of the \code{polygon_id}, \code{type} and
    \code{certain} columns.
    }
}
\description{
\code{read_habitatmap_terr()} returns the data source \code{habitatmap_terr}
as a list of two objects: \code{habitatmap_terr_polygons}, having the Belgian
Lambert 72 CRS (EPSG-code \href{https://epsg.io/31370}{31370}), and
\code{habitatmap_terr_types}.
\code{habitatmap_terr} is the further interpreted, terrestrial part of
\code{habitatmap_stdized} (see \code{\link{read_habitatmap_stdized}}),
which, in turn, is derived from the raw data source \code{habitatmap}
(De Saeger et al., 2023).
By default, occurrences of type \code{7220} are dropped because a more
reliable data source is available for this habitat type (see \code{drop_7220}
argument).
Note: a \link[=types]{type} is a habitat (sub)type or a regionally
important biotope (RIB).
}
\details{
\code{habitatmap_terr} was derived from \code{habitatmap_stdized} as
follows:
\itemize{
\item{it excludes all polygons
that are most probably aquatic habitat or RIB.
These are the polygons for which
\strong{all} habitat or RIB types are aquatic.
In the process, a distinction was also made between \code{2190_a} and
\code{2190_overig}.
There is no exclusion of aquatic types when these coexist with
terrestrial types in the same polygon.
The aquatic types are the types for which \code{hydr_class == "HC3"}
in the \code{\link{types}} data source (\code{hydr_class} is the hydrological
class; cf. the output of \code{\link[=read_types]{read_types()}});}
\item{it excludes types which most probably are \emph{no}
habitat or RIB at all.
Those are the types where \code{code_orig} contains \code{"bos"} or is
equal to \code{"6510,gh"} or \code{"9120,gh"};}
\item{it translates several main type codes into a corresponding
subtype which they almost always represent:
\code{6410} -> \code{6410_mo},
\code{6430} -> \code{6430_hf},
\code{6510} -> \code{6510_hu},
\code{7140} -> \code{7140_meso},
\code{9130} -> \code{9130_end};}
\item{it distinguishes types \code{rbbhfl} and \code{rbbhf}.}
}

The data source \code{habitatmap_terr} is a GeoPackage, available at
\href{https://doi.org/10.5281/zenodo.3468948}{Zenodo}, that contains:
\itemize{
  \item{\code{habitatmap_terr_polygons}: a spatial polygon layer}
  \item{\code{habitatmap_terr_types}: a table with the types that occur
  in each polygon.}
  }

The R-code for creating the \code{habitatmap_terr} data source
can be found in the
\href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing}
repository.
}
\examples{
\dontrun{
# This example supposes that your working directory or a directory up to 10
# levels above has the 'n2khab_data' folder AND that the latest version of
# the 'habitatmap_terr'
# data source is present in the default subdirectory.
# In all other cases, this example won't work but at least you can
# consider what to do.

habmap_terr <- read_habitatmap_terr()
habmap_terr$habitatmap_terr_polygons
habmap_terr$habitatmap_terr_types

habmap_terr_noaq <- read_habitatmap_terr(keep_aq_types = FALSE)
habmap_terr_noaq$habitatmap_terr_polygons
habmap_terr_noaq$habitatmap_terr_types
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
\code{\link{read_habitatmap_stdized}()},
\code{\link{read_watersurfaces_hab}()}
}
\concept{functions involved in processing the 'habitatmap' data source}

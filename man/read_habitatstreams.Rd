% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_habitatdata.R
\name{read_habitatstreams}
\alias{read_habitatstreams}
\title{Return the data source \code{habitatstreams} as an \code{sf} linestring
layer or as a list}
\usage{
read_habitatstreams(
  file = file.path(locate_n2khab_data(), "10_raw/habitatstreams"),
  source_text = FALSE
)
}
\arguments{
\item{file}{The absolute or relative file path of the data source.
The default follows the data management advice in the
vignette on data storage (run \code{vignette("v020_datastorage")}).
It uses the first \code{n2khab_data} folder that is found when
sequentially climbing up 0 to 10 levels in the file system hierarchy,
starting from the working directory.}

\item{source_text}{Logical, defaults to \code{FALSE}.
If \code{TRUE}, a list is returned (see \emph{Value}).}
}
\value{
With \code{source_text = FALSE} (default): a Simple feature collection of
type \code{LINESTRING}.

With \code{source_text = TRUE}: a list of two objects:
\itemize{
\item \code{lines}: the same \code{sf} object as with \code{source_text = FALSE}.
\item \code{sources}: textual explanation on the values of the \code{source_id}
variable in the \code{sf} object.
}
}
\description{
Returns the raw data source \code{habitatstreams} (section 'habitat 3260' from
De Saeger et al., 2023) as an \code{sf} linestring
layer or as a list of two objects: the \code{sf} object (CRS:
Belgian Lambert 72 (EPSG-code \href{https://epsg.io/31370}{31370}))
plus a data frame
with textual explanation of the values of the \code{source_id} variable.
}
\details{
For more information about this map, see also the documentation of the
previous version (Leyssen et al., 2020).
}
\examples{
\dontrun{
# This example supposes that your working directory or a directory up to 10
# levels above has the 'n2khab_data' folder AND that the latest version of
# the 'habitatstreams'
# data source is present in the default subdirectory.
# In all other cases, this example won't work but at least you can
# consider what to do.

library(magrittr)
library(sf)
hs <- read_habitatstreams()
hs
hs2 <- read_habitatstreams(source_text = TRUE)
hs2
all.equal(
  hs \%>\% st_drop_geometry(),
  hs2$lines \%>\% st_drop_geometry()
)
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
\item Leyssen A., Smeekens V., Denys L. (2020). Indicatieve situering van het
Natura 2000 habitattype 3260. Submontane en laaglandrivieren met vegetaties
behorend tot het \emph{Ranunculion fluitantis} en het
\emph{Callitricho-Batrachion}.
Uitgave 2020 (versie 1.7). Rapporten van het Instituut voor Natuur- en
Bosonderzoek 2020 (34). Research Institute for Nature and Forest, Brussels.
\doi{10.21436/inbor.18903609}.
}
}

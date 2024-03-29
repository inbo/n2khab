% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_textdata.R
\name{read_env_pressures}
\alias{read_env_pressures}
\title{Return the 'env_pressures' data source as a tibble with human-readable
attributes}
\usage{
read_env_pressures(
  path = pkgdatasource_path("textdata/env_pressures", ".yml"),
  file = "env_pressures",
  file_namelist = "namelist",
  lang = "en"
)
}
\arguments{
\item{path}{Location of the data sources \code{env_pressures} and
\code{namelist}.
The default is to use the location of the data sources as delivered by
the installed package.}

\item{file}{The filename of the \code{env_pressures} data source, without
extension.
The default is to use the file delivered by the installed package.}

\item{file_namelist}{The filename of the \code{namelist} data source,
without extension.
The default is to use the file delivered by the installed package.}

\item{lang}{An
\href{https://www.w3.org/International/articles/language-tags/index.en}{
IETF BCP 47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
the language of human-readable attributes to be returned in the tibble.}
}
\value{
The \code{env_pressures} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
with human-readable text added for environmental pressures,
pressure-classes and textual explanations and remarks
according to the \code{lang} argument.
The tibble has 35 rows and 7 variables.
See \code{\link{env_pressures}} for documentation of the data-source's contents.
See \code{\link{namelist}} for the link between codes or other identifiers
and the corresponding text.

The human-readable attributes are represented by the following variables:
\describe{
  \item{\code{ep_abbrev}}{A (language-dependent) abbreviation (alternative code)
  of the environmental pressure.
  Is a factor with the level order coinciding with that of
  \code{ep_code}.}
  \item{\code{ep_name}}{The name of the environmental pressure.
  Is a factor with the level order coinciding with that of
  \code{ep_code}.}
  \item{\code{ep_class_name}}{The name of the environmental pressure's class.
  Is a factor with the level order coinciding with that of
  \code{ep_class}.}
  \item{\code{explanation}}{An explanation of the environmental pressure.}
  \item{\code{remarks}}{Optional remarks about the environmental
  pressure.}
}
}
\description{
Returns the included data source \code{\link{env_pressures}} as a
\code{\link[tibble:tbl_df-class]{tibble}}.
Names, shortnames, explanations and optional remarks from
\code{\link{namelist}} are added,
in English by default.
}
\details{
\code{\link{env_pressures}} is a data source in the
\href{https://ropensci.github.io/git2rdata}{vc-format} which provides
a checklist of environmental pressures, represented by codes, together
with the pressure-class and the textual explanation.

\code{read_env_pressures()} reads the \code{\link{env_pressures}} data
source, adds
human-readable attributes and returns it as a
\code{\link[tibble:tbl_df-class]{tibble}}.
A tibble is a data frame that makes working in the tidyverse a little
\href{https://r4ds.had.co.nz/tibbles.html}{easier}.
By default, the data version delivered with the package is used and English
text (\code{lang = "en"}) is returned for names of environmental pressures and
pressure-classes, and for textual explanations and remarks.
}
\section{Recommended usage}{


  \code{read_env_pressures()}

  \code{read_env_pressures(lang = "nl")}
}

\examples{
read_env_pressures()
read_env_pressures(lang = "nl")

}
\seealso{
\code{\link{env_pressures}}

Other reading functions for n2khab-referencelists: 
\code{\link{read_namelist}()},
\code{\link{read_types}()}
}
\concept{reading functions for n2khab-referencelists}

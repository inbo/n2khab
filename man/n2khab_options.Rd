% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{n2khab_options}
\alias{n2khab_options}
\title{Package configuration through options and environment variables}
\usage{
n2khab_options()
}
\value{
A data frame with the names and values of possible options.
Missing values are returned as \code{NA}.
}
\description{
The package can be configured by means of options or environment variables.
These will influence the behaviour of certain functions.
Each option has its sibling environment variable. When both have a value,
the \emph{option} will be given priority.

This function queries these options and environment variables, and returns
the resulting state for each of them (not distinguishing between options or
environment variables as the source).
}
\details{
Options are typically harder to
isolate from the R code that you collaborate on and share through a
repository.
This is especially the case when using \code{renv}: it requires
\code{.Rprofile} as part of your project in the working directory, which prevents
\code{.Rprofile} files elsewhere on the system from being used.

Consequently, it is advised to:
\itemize{
\item use \code{\link[=options]{options()}} where this affects behaviour that must be the same across
users and machines for reproducibility.
Put these inside your script, or at least in an \code{.Rprofile} file that is
shared together with the other project files.
Example: which package to use to represent raster objects.
\item use environment variables where behaviour must be machine-specific, e.g.
to override the default location of the \code{n2khab_data} directory (can also be
needed when using \code{\link[reprex:reprex]{reprex::reprex()}}).
For example, you can create an \code{.Renviron} file in your working directory
and ignore it in distributed version control.
Or you can set the environment variable at a higher level, e.g. in an
\code{.Renviron} file in your home directory.
See \link[base:Startup]{base::Startup} for more information.
}
}
\section{Description of options and environment variables}{
\tabular{llll}{
   \strong{option} \tab \strong{environment variable} \tab \strong{type} \tab \strong{description} \cr
   \code{n2khab_data_path} \tab \code{N2KHAB_DATA_PATH} \tab string \tab Path of the \code{n2khab_data} directory. Takes priority over the default locations where reading functions expect this directory. \cr
   \code{n2khab_use_raster} \tab \code{N2KHAB_USE_RASTER} \tab logical \tab Should the \code{raster} package be used to return raster objects? The \code{terra} package is used by default. \cr
}
}

\examples{
n2khab_options()

oldopt <- options(n2khab_use_raster = TRUE)
n2khab_options()
options(oldopt)

# Unacceptable values yield an error message;
# the data frame is still returned with NA:
oldopt <- options(n2khab_data_path = 0, n2khab_use_raster = TRUE)
n2khab_options()
options(oldopt)

}

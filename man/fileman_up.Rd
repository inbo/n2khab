% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filemanagement.R
\name{fileman_up}
\alias{fileman_up}
\title{Climb up in the file system hierarchy to find a file or folder}
\usage{
fileman_up(name, start = ".", levels = 10)
}
\arguments{
\item{name}{Name of file or folder to search for.
An exact match is needed.
The matching is case sensitive.}

\item{start}{String.
Directory to start searching from.}

\item{levels}{Integer.
How many levels to sequentially climb up in the file hierarchy,
if the file or folder is not found in the \code{start} directory?}
}
\value{
The path to the specified folder or file (string), or \code{NULL} if
not found.
}
\description{
Searches for a specific file or folder, starting from the \code{start}
directory and sequentially climbing up one directory level at a time.
The first match causes this sequence to stop
and the full path will be returned.
}
\details{
Symbolic links are matched, and in the returned path they are converted.
}
\examples{
\dontrun{
fileman_up("n2khab_data")
}

}
\seealso{
Other functions regarding file management for N2KHAB projects: 
\code{\link{checksum}()},
\code{\link{download_zenodo}()},
\code{\link{fileman_folders}()},
\code{\link{locate_n2khab_data}()}
}
\concept{functions regarding file management for N2KHAB projects}

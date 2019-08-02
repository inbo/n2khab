#' @title Create a standard data folder structure and return the path to the \code{n2khab_data} folder
#'
#' @description This function will check for the existence of default data folders,
#' create them if necessary, and return the path to the data folder.
#'
#' @details In n2khab projects a standardized folder setup is used for binary
#' data, as explained in the \href{doc/v020_datastorage.html}{vignette} on
#' data storage.
#' The functions creates the folders \code{n2khab_data}, \code{n2khab_data/10_raw}
#' and \code{n2khab_data/20_processed}, or prints a message if these already
#' exist.
#' You can use the value returned by the function as the `path` argument of
#' functions that read particular data.
#'
#' @param root Character string indicating whether the root folder of the current git repository or the root folder of the current Rstudio project should be used as the folder where you want the data folder structure to be created.
#' Can be \code{"rproj"} (the default) for an RStudio R project or \code{"git"}
#' for a git repository.
#'
#' @param path An optional argument to specify a custom path to a folder where you want the data folder structure to be created. Default is \code{NA} (no custom path).
#'
#' @return A character string that gives the absolute path to the \code{n2khab_data/}
#' folder.
#'
#' @family functions regarding file management for N2KHAB projects
#'
#' @importFrom rprojroot
#' find_root
#' is_git_root
#' is_rstudio_project
#'
#' @export
#'
#' @examples
#' \dontrun{
#'fileman_folders()
#'datapath <- fileman_folders(root = "git")
#' }
#'
fileman_folders <- function(root = c("rproj", "git"), path = NA) {
    # directory setup
    if (!is.na(path)) {
        if (dir.exists(path)) {
            datapath <- file.path(path, "n2khab_data")
        } else {
            stop("The specified path does not exist.")
        }
    } else {
        root <- tolower(root)
        root <- match.arg(root)

        if (root == "git") {
            root <- find_root(is_git_root)
        }

        if (root == "rproj") {
            root <- find_root(is_rstudio_project)
        }

        datapath <- file.path(root, "n2khab_data")
    }



    # check for existence of the folder
    if (!dir.exists(datapath)) {
        # create a new directory
        dir.create(file.path(datapath))
    } else {
        message(paste0("The path to ", datapath, " already exists"))
    }

    # create subfolders
    subfolders <- c("10_raw", "20_processed")
    for (subfolder in subfolders) {
        if (!dir.exists(file.path(datapath, subfolder))) {
            dir.create(file.path(datapath, subfolder))
        } else {
            message(paste0("The subfolder ", subfolder, " already exists"))
        }
    }
    datapath
}





#' Get data from a Zenodo archive
#'
#' This function will download data from Zenodo (\url{https://zenodo.org}).
#' It only works for Zenodo created DOI (not when the DOI is for
#' example derived from Zookeys.)
#'
#' @param path local path to where the data need to be written
#' @param doi a doi pointer to the Zenodo archive starting with '10.5281/zenodo.'. See examples.
#'
#' @return Downloaded file(s) in the specified folder.
#'
#' @importFrom stringr fixed str_remove str_split
#' @importFrom curl curl_fetch_memory curl_download
#' @importFrom jsonlite fromJSON
#' @importFrom tools md5sum
#' @importFrom utils tail
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Single zip file deposition
#' fileman_zenodo(".", "10.5281/zenodo.1283345")
#' # Multiple files deposition
#' fileman_zenodo(".", "10.5281/zenodo.1172801")
#' # Single pdf file deposition
#' fileman_zenodo(".", "10.5281/zenodo.168478")
#' }
fileman_zenodo <- function(path, doi) {
    if (missing(path)) {
        stop("Please provide a path to which the data need to be downloaded")
    }
    if (missing(doi)) {
        stop(paste0("Please provide a doi for a Zenodo archive. This is a ",
                   "string starting with '10.5281/zenodo.' followed by a ",
                   "unique number."))
    }

    # check for existence of the folder
    if (!dir.exists(path)) {
        stop("The path does not exist.")
    }

    record <- str_remove(doi, fixed("10.5281/zenodo."))

    # Retrieve file name by records call
    base_url <- 'https://zenodo.org/api/records/'
    req <- curl_fetch_memory(paste0(base_url, record))
    content <- fromJSON(rawToChar(req$content))

    # extract individual file names and urls
    file_urls <- content$files$links$self

    # extract check-sum(s)
    file_md5 <- content$files$checksum

    # donwload each of the files
    for (i in seq_along(file_urls)) {
        file_name <- tail(str_split(file_urls[i], "/")[[1]], 1)
        destfile <- file.path(path, file_name)
        curl_download(url = file_urls[i],
                      destfile = destfile,
                      quiet = FALSE)
        md5 <- unname(md5sum(destfile))
        zenodo_md5 <- str_split(file_md5[i], ":")[[1]][2]
        if (all.equal(md5, zenodo_md5)) {
            print(paste0("md5sum ", md5, " for ", file_name," is correct."))
        } else {
            warning(paste0("md5 sum ",
                           md5,
                           " for file",
                           file_name,
                           " does not match the Zenodo archived md5 sum ",
                           zenodo_md5))
        }
    }
}












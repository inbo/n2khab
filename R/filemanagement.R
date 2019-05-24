#' @title Create a standard data folder structure and return the path to the \code{data} folder
#'
#' @description This function will check for the existence of default data folders,
#' create them if necessary, and return the path to the data folder.
#'
#' @details In n2khab projects a standardized folder setup is used for binary
#' data, as explained in the \href{doc/vign-020_datastorage.html}{vignette} on
#' data storage.
#' The functions creates the folders \code{data}, \code{data/10_raw}
#' and \code{data/20_processed}, or prints a message if these already
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
#' @return A character string that gives the absolute path to the \code{data/}
#' folder.
#'
#' @importFrom rprojroot
#' find_root
#' is_git_root
#' is_rstudio_project
#'
#' @export
#'
#' @examples
#'filemanag_folders()
#'datapath <- filemanag_folders(root = "git")
#'
filemanag_folders <- function(root = c("rproj", "git"), path = NA) {
    # directory setup
    if (!is.na(path)) {
        if (dir.exists(path)) {
            datapath <- file.path(path, "data")
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

        datapath <- file.path(root, "data")
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





#' @title Get raw data from a zenodo archive
#'
#' This function will download data from Zenodo (\href{https://zenodo.org}).
#' It only works for Zenodo created DOI (not when the DOI is for
#' example derived from Zookeys.)
#'
#' @param path local path to where the data need to be written
#' @param doi doi (a pointer to the Zenodo archive)
#'
#' @return Downloaded file(s) in the specified folder.
#'
#' @importFrom stringr fixed str_remove str_split
#' @importFrom curl curl_fetch_memory curl_download
#' @importFrom jsonlite fromJSON
#' @importFrom tools md5sum
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Single zip file deposition
#' filemanag_zenodo(".", "10.5281/zenodo.1283345")
#' # Multiple files deposition
#' filemanag_zenodo(".", "10.5281/zenodo.1172801")
#' # Single pdf file deposition
#' filemanag_zenodo(".", "10.5281/zenodo.168478")
#' }
filemanag_zenodo <- function(path, doi) {
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
        md5 <- md5sum(destfile)
        if (all.equal(unname(md5), str_split(file_md5[i], ":")[[1]][2])) {
            print(paste0("md5sum ", md5, " is correct."))
        } else {
            warning("md5 sum ", md5, " mismatch.")
        }
    }
}



#' @title Get processed data
#'
#' @description This function will download processed data so they become locally available
#'
#' @param path path to where the data need to be written
#' @param filename character vector of filenames
#'
#' @return downloaded files in the specified folder
#'
#' @keywords internal
#'
filemanag_processed <- function(path, filename) {



}










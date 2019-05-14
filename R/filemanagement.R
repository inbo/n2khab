#' @title Create the data folder structure and return the path to the data folder
#'
#' @description This function will check for the existence of data folders, create them if necessary, and return the path to the data folder (\code{datapath} object)
#'
#' @param root Character string indicating whether the root folder of the current git repository or the root folder of the current Rstudio project should be used as the folder where you want the data folder structure to be created. Can be "rproj" (the default) for an Rstudio R project or "git" for a git repository.
#' @param path An optional argument to specify a custom path to a folder where you want the data folder structure to be created. Default is \code{NA} (no custom path).
#'
#' @return A new data folder beneath specified root with subfolders \code{10_raw} and \code{20_processed} and an object \code{datapath} that points to the \code{data/} folder
#'
#' @importFrom rprojroot
#' find_root
#' is_git_root
#' is_rstudio_project
#'
#' @examples
#'filemanag_folders()
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
        message(paste0("The ", datapath, " already exists"))
    }

    # create subfolders
    subfolders <- c("10_raw", "20_processed")
    for (subfolder in subfolders) {
        if (!dir.exists(file.path(datapath, subfolder))) {
            dir.create(file.path(datapath, subfolder))
        } else {
            message(paste0("The ", subfolder, " already exists"))
        }
    }
    datapath
}





#' @title Get raw data from a zenodo archive
#'
#' @description This function will download data from zenodo
#'
#' @param path path to where the data need to be written
#' @param doi doi (a pointer to the zenodo archive)
#'
#' @return downloaded and unzipped file in the folder
#'
#'
#' @examples
#'
filemanag_zenodo <- function(path, doi) {
    if (is.missing(path)) {

    }

    if (is.missing(doi)) {

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
#'
#' @examples
#'
filemanag_processed <- function(path, filename) {



}










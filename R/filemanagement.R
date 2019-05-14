#' @title Create the data folder structure and return the path to the data folder
#'
#' @description This function will check for the existence of data folders, create them if necessary, and return the path to the data folder (datapath object)
#'
#' @param root char either use the git root or rproj root (default)
#'
#' @return A new data folder beneath specified root with subfolders 10_raw and 20_processed and an object datapath that points to the data/ folder
#'
#' @importFrom rprojroot find_root
#' @importFrom rprojroot is_git_root
#' @importFrom rprojroot is_rstudio_project
#'
#' @examples
#'filemanag_folders()
#'
filemanag_folders <- function(root = "rproj") {
    # directory setup
    root <- tolower(root)

    if (root == "git") {
        root <- find_root(is_git_root)
    }

    if (root == "rproj") {
        root <- find_root(is_rstudio_project)
    }

    datapath <- file.path(root, "data")

    # check for existence of the folder
    if (!dir.exists(datapath)) {
        # create a new directory
        dir.create(file.path(datapath))
    }

    # create subfolders
    subfolders <- c("10_raw", "20_processed")
    for (subfolder in subfolders) {
        if (!dir.exists(file.path(datapath, subfolder))) {
            dir.create(file.path(datapath, subfolder))
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










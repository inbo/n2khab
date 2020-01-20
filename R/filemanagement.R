#' @title Create a standard data folder structure and return the path to the \code{n2khab_data} folder
#'
#' @description This function will check for the existence of default data folders,
#' create them if necessary, and return the path to the \code{n2khab_data} folder.
#'
#' @details In n2khab projects a standardized folder setup is used for binary
#' data, as explained in the
#' vignette on data storage (run \code{vignette("v020_datastorage")}).
#' The functions creates the folders \code{n2khab_data}, \code{n2khab_data/10_raw}
#' and \code{n2khab_data/20_processed}, or prints a message if these already
#' exist.
#' The function returns the path to \code{n2khab_data}.
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
            datapath <- normalizePath(file.path(path, "n2khab_data"))
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
        message(paste0("Created ", datapath))
    } else {
        message(paste0("The path to ", datapath, " already exists"))
    }

    # create subfolders
    subfolders <- c("10_raw", "20_processed")
    for (subfolder in subfolders) {
        if (!dir.exists(file.path(datapath, subfolder))) {
            dir.create(file.path(datapath, subfolder))
            message(paste0("Created subfolder ", subfolder))
        } else {
            message(paste0("The subfolder ", subfolder, " already exists"))
        }
    }
    datapath
}





#' Get data from a Zenodo archive
#'
#' @name download_zenodo
#' @keywords documentation
#' @importFrom inborutils download_zenodo
#' @export
inborutils::download_zenodo







#' Climb up in the file system hierarchy to find a file or folder
#'
#' Searches for a specific file or folder, starting from the \code{start}
#' directory and sequentially climbing up one directory level at a time.
#' The first match causes this sequence to stop
#' and the full path will be returned.
#'
#' Symbolic links are matched, and in the returned path they are converted.
#'
#' @param name Name of file or folder to search for.
#' An exact match is needed.
#' The matching is case sensitive.
#' @param start String.
#' Directory to start searching from.
#' @param levels Integer.
#' How many levels to sequentially climb up in the file hierarchy,
#' if the file or folder is not found in the \code{start} directory?
#'
#' @return
#' The path to the specified folder or file (string), or \code{NULL} if
#' not found.
#'
#' @family functions regarding file management for N2KHAB projects
#'
#' @examples
#' \dontrun{
#' fileman_up("n2khab_data")
#' }
#'
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr
#' %>%
#' @export
fileman_up <- function(name,
                       start = ".",
                       levels = 10) {

    assert_that(is.string(name))
    assert_that(dir.exists(start),
                msg = "The start directory does not exist.")
    assert_that(levels %% levels == 0 & levels >= 0,
                msg = "levels must be a positive integer value.")

    path <- start

    for (i in 0:levels) {
        ff <- list.files(path,
                         all.files = TRUE,
                         include.dirs = TRUE)
        if (name %in% ff) break
        path <- file.path(path, "..")
    }

    if (name %in% ff) {
        file.path(path, name) %>%
            normalizePath()
    } else {
        NULL
    }

}






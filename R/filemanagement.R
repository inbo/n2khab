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
#' This function will download an entire archive from Zenodo (\url{https://zenodo.org}).
#' It only works for Zenodo created DOI (not when the DOI is for
#' example derived from Zookeys.)
#'
#' @author Hans Van Calster, \email{hans.vancalster@@inbo.be}
#' @author Floris Vanderhaeghe, \email{floris.vanderhaeghe@@inbo.be}
#'
#' @param path Path where the data must be downloaded.
#' Defaults to the working directory.
#' @param doi a doi pointer to the Zenodo archive starting with '10.5281/zenodo.'. See examples.
#' @param parallel Logical (\code{FALSE} by default).
#' If \code{TRUE}, will run a number of parallel processes, each downloading
#' another file.
#' This is useful when multiple large files are present in the Zenodo
#' record, which otherwise would be downloaded sequentially.
#' Of course, the operation is limited by bandwidth and traffic limitations.
#' @param quiet Logical (\code{FALSE} by default).
#' Do you want to suppress informative messages (not warnings)?
#'
#' @importFrom stringr
#' fixed
#' str_remove
#' str_split
#' str_match
#' @importFrom assertthat
#' assert_that
#' is.string
#' is.flag
#' noNA
#'
#' @export
#' @family functions regarding file management for N2KHAB projects
#'
#' @examples
#' \dontrun{
#' # Example download of an archive containing a single zip
#' download_zenodo(doi = "10.5281/zenodo.1283345")
#' download_zenodo(doi = "10.5281/zenodo.1283345", quiet = TRUE)
#' # Example download of an archive containing multiple files
#' # using parallel download
#' # (multiple files will be simultaneously downloaded)
#' download_zenodo(doi = "10.5281/zenodo.1172801", parallel = TRUE)
#' # Example download of an archive containing a single pdf file
#' download_zenodo(doi = "10.5281/zenodo.168478")
#' }
download_zenodo <- function(doi,
                            path = ".",
                            parallel = FALSE,
                            quiet = FALSE) {

    assert_that(is.string(doi), is.string(path))
    assert_that(is.flag(parallel), noNA(parallel), is.flag(quiet), noNA(quiet))

    require_pkgs(c("jsonlite", "curl", "tools"))

    # check for existence of the folder
    stopifnot(dir.exists(path))

    record <- str_remove(doi, fixed("10.5281/zenodo."))

    # Retrieve file name by records call
    base_url <- 'https://zenodo.org/api/records/'
    req <- curl::curl_fetch_memory(paste0(base_url, record))
    content <- jsonlite::fromJSON(rawToChar(req$content))

    # Calculate total file size
    totalsize <- sum(content$files$size) %>%
        human_filesize()

    # extract individual file names and urls
    file_urls <- content$files$links$self
    filenames <- str_match(file_urls, ".+/([^/]+)")[,2]
    destfiles <- file.path(path, filenames)

    # extract check-sum(s)
    file_md5 <- content$files$checksum

    # download files
    if (!quiet) {
        message("Will download ",
                (nrfiles <- length(filenames)),
                " file",
                ifelse(nrfiles > 1, "s", ""),
                " (total size: ",
                totalsize,
                ") from https://doi.org/",
                doi,
                " (",
                content$metadata$title,
                "; version: ",
                ifelse(!is.null(content$metadata$version),
                       content$metadata$version,
                       content$metadata$relations$version[1, 1]
                ),
                ")\n"
        )
    }

    if (parallel) {

        require_pkgs("parallel")

        nr_nodes <- min(10, length(file_urls))

        if (!quiet) message("Initializing parallel download on ",
                            nr_nodes,
                            " R session nodes...\n")

        clus <- parallel::makeCluster(nr_nodes)

        if (!quiet) {
            message("Starting parallel downloads. ",
                    "This may take a while (and I can't show you the overall progress).\n",
                    "Be patient...\n")
        }

        parallel::clusterMap(clus,
                             function(src, dest) {
                                 curl::curl_download(url = src,
                                                     destfile = dest,
                                                     quiet = quiet)
                             },
                             file_urls,
                             destfiles)

        parallel::stopCluster(clus)

        if (!quiet) message("Ended parallel downloads.")

    } else {

        mapply(curl::curl_download,
               file_urls,
               destfiles,
               MoreArgs = list(quiet = quiet))

    }

    # check each of the files

    if (!quiet) message("\nVerifying file integrity...\n")

    for (i in seq_along(file_urls)) {
        filename <- filenames[i]
        destfile <- destfiles[i]
        md5 <- unname(tools::md5sum(destfile))
        zenodo_md5 <- str_split(file_md5[i], ":")[[1]][2]
        if (all.equal(md5, zenodo_md5)) {
            if (!quiet) message(filename,
                                " was downloaded and its integrity verified (md5sum: ",
                                md5,
                                ")")
        } else {
            warning("Incorrect download! md5sum ",
                    md5,
                    " for file",
                    filename,
                    " does not match the Zenodo archived md5sum ",
                    zenodo_md5)
        }
    }
}



#' Human-readable binary file size
#'
#' Takes an integer (referring to number of bytes) and returns an optimally
#' human-readable
#' \href{https://en.wikipedia.org/wiki/Binary_prefix}{binary-prefixed}
#' byte size (KiB, MiB, GiB, TiB, PiB, EiB).
#' The function is vectorised.
#'
#' @author Floris Vanderhaeghe, \email{floris.vanderhaeghe@@inbo.be}
#'
#' @param x A positive integer, i.e. the number of bytes (B).
#' Can be a vector of file sizes.
#'
#' @return
#' A character vector.
#'
#' @keywords internal
#' @importFrom assertthat
#' assert_that
#' @importFrom dplyr
#' %>%
human_filesize <- function(x) {
    assert_that(is.numeric(x))
    assert_that(all(x %% 1 == 0 & x >= 0))
    magnitude <-
        log(x, base = 1024) %>%
        floor %>%
        pmin(8)
    unit <- factor(magnitude,
                   levels = 0:8,
                   labels = c(
                       "B",
                       "KiB",
                       "MiB",
                       "GiB",
                       "TiB",
                       "PiB",
                       "EiB",
                       "ZiB",
                       "YiB")
    )
    size <- (x / 1024^magnitude) %>% round(1)
    return(paste(size, unit))
}








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
    assert_that(levels %% 1 == 0 & levels >= 0,
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
        stop(name, " was not found. Searched up to ", normalizePath(path))
    }

}





#' Calculate file checksums
#'
#' The functions calculate the checksum (digest; hash value) of
#' one or multiple files.
#' They can be used to verify file integrity.
#'
#' A few cryptographic and non-cryptographic hash functions are implemented,
#' either from the OpenSSL library (through
#' \href{https://CRAN.R-project.org/package=openssl}{\code{openssl}})
#' or as embedded in the
#' \href{https://CRAN.R-project.org/package=digest}{\code{digest}}
#' package.
#'
#' Functions \code{md5sum()} etc. are simple shortcuts to \code{checksum()}
#' with the appropriate hash function preset.
#' Their names were chosen to match those of xxHash and GNU coreutils.
#'
#' The cryptographic algorithms use the OpenSSL implementation and
#' stream-hash the binary
#' contents of the connections to the respective files.
#' They turn the hash-format for binary streams by the \code{openssl} package
#' into a regular hash string.
#' Note that \code{n2khab} will mask
#' \code{\link[tools:md5sum]{tools::md5sum()}},
#' which is a standalone implementation.
#'
#' @param files Character vector of file path(s).
#' File path(s) can be absolute or relative.
#' @param hash_fun String that defines the hash function.
#' See \emph{Usage} for allowed values; defaults to the first.
#'
#' @return
#' Named character vector with the same length as \code{files}
#' and with the file names as names.
#'
#' @family functions regarding file management for N2KHAB projects
#'
#' @examples
#' # creating two different temporary files:
#' file1 <- tempfile()
#' file2 <- tempfile()
#' files <- c(file1, file2)
#' file.create(files)
#' con <- file(file2)
#' writeLines("some text", con)
#' close(con)
#'
#' # computing alternative checksums:
#' checksum(files)
#' xxh64sum(files)
#' md5sum(files)
#' sha256sum(files)
#'
#' \dontrun{
#' # This will error:
#' files <- c(file1, file2, tempfile(), tempfile())
#' checksum(files)
#' }
#'
#' @importFrom purrr
#' map_chr
#' @importFrom stringr
#' str_detect
#' @export
checksum <- function(files,
                     hash_fun = c("xxh64", "md5", "sha256")) {

    assert_that_allfiles_exist(files)
    hash_fun <- match.arg(hash_fun)

    if (str_detect(hash_fun, "^xxh")) {
        require_pkgs("digest")
        checksums <- map_chr(files,
                             ~digest::digest(.,
                                             algo = "xxhash64",
                                             file = TRUE))
    } else {
        require_pkgs("openssl")
        fun <- eval(str2lang(paste0("openssl::", hash_fun)))
        checksums <- map_chr(files, ~paste(fun(file(.))))
    }

    names(checksums) <- basename(files)
    return(checksums)
}

#' @rdname checksum
#' @export
xxh64sum <- function(files) checksum(files, hash_fun = "xxh64")

#' @rdname checksum
#' @export
md5sum <- function(files) checksum(files, hash_fun = "md5")

#' @rdname checksum
#' @export
sha256sum <- function(files) checksum(files, hash_fun = "sha256")


#' @importFrom assertthat
#' assert_that
#' @keywords internal
assert_that_allfiles_exist <- function(x) {
    exist <- file.exists(x)
    assert_that(all(exist),
                msg = paste0("The following path(s) do not exist:\n",
                             paste0(x[!exist], collapse = "\n")))
    isdir <- dir.exists(x)
    assert_that(!any(isdir),
                msg = paste0("Only files are accepted; ",
                             "the following path(s) are directories:\n",
                             paste0(x[isdir], collapse = "\n")))
}




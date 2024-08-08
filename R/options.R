#' Package configuration through options and environment variables
#'
#' @description
#' The package can be configured by means of options or environment variables.
#' These will influence the behaviour of certain functions.
#' Each option has its sibling environment variable. When both have a value,
#' the _option_ will be given priority.
#'
#' This function queries these options and environment variables, and returns
#' the resulting state for each of them (not distinguishing between options or
#' environment variables as the source).
#'
#' @details
#' Options are typically harder to
#' isolate from the R code that you collaborate on and share through a
#' repository.
#' This is especially the case when using `renv`: it requires
#' `.Rprofile` as part of your project in the working directory, which prevents
#' `.Rprofile` files elsewhere on the system from being used.
#'
#' Consequently, it is advised to:
#'
#' - use [`options()`] where this affects behaviour that must be the same across
#' users and machines for reproducibility.
#' Put these inside your script, or at least in an `.Rprofile` file that is
#' shared together with the other project files.
#' Example: which package to use to represent raster objects.
#' - use environment variables where behaviour must be machine-specific, e.g.
#' to override the default location of the `n2khab_data` directory (can also be
#' needed when using [`reprex::reprex()`]).
#' For example, you can create an `.Renviron` file in your working directory
#' and ignore it in distributed version control.
#' Or you can set the environment variable at a higher level, e.g. in an
#' `.Renviron` file in your home directory.
#' See [base::Startup] for more information.
#'
#' @section Description of options and environment variables:
#' **option** | **environment variable** | **type**  | **description**
#' --- | --- | --- | ---
#' `n2khab_data_path` | `N2KHAB_DATA_PATH` | string | Path of the `n2khab_data` directory. Takes priority over the default locations where reading functions expect this directory.
#' `n2khab_use_raster` | `N2KHAB_USE_RASTER` | logical | Should the `raster` package be used to return raster objects? The `terra` package is used by default.
#'
#' @returns
#' A data frame with the names and values of possible options.
#' Missing values are returned as `NA`.
#'
#' @examples
#' n2khab_options()
#'
#' oldopt <- options(n2khab_use_raster = TRUE)
#' n2khab_options()
#' options(oldopt)
#'
#' # Unacceptable values yield an error message;
#' # the data frame is still returned with NA:
#' oldopt <- options(n2khab_data_path = 0, n2khab_use_raster = TRUE)
#' n2khab_options()
#' options(oldopt)
#'
#' @importFrom tidyr tribble
#'
#' @md
#'
#' @name n2khab_options
#' @export
n2khab_options <- function() {
  tribble(
    ~option,
    ~value,
    "n2khab_data_path",
    tryCatch(
      n2khab_data_path(),
      error = function(e) {
        message(as.character(e))
        NA
      }
    ),
    "n2khab_use_raster",
    tryCatch(
      as.character(n2khab_using_raster()),
      error = function(e) {
        message(as.character(e))
        NA
      }
    )
  )
}



#' @keywords internal
n2khab_data_path <- function() {
  opt <- getOption("n2khab_data_path", Sys.getenv("N2KHAB_DATA_PATH"))
  assert_that(
    is.string(opt),
    msg = "Option 'n2khab_data_path' must be a string."
  )

  if (identical(opt, "")) {
    opt <- NA
  } else {
    assert_that(
      dir.exists(opt),
      msg = "Option 'n2khab_data_path' must be an existing directory."
    )
    assert_that(
      identical(basename(opt), "n2khab_data"),
      msg = paste(
        "Option 'n2khab_data_path' must point to a directory",
        "named 'n2khab_data'."
      )
    )
  }
  opt
}

#' @keywords internal
n2khab_using_raster <- function() {
  opt <- getOption("n2khab_use_raster", Sys.getenv("N2KHAB_USE_RASTER"))
  assert_that(
    is.flag(opt) ||
      (is.string(opt) && opt %in% c("", "TRUE", "FALSE", "true", "false")),
    msg = "Option 'n2khab_use_raster' must be 'TRUE' or 'FALSE'."
  )
  if (identical(opt, "")) {
    opt <- NA
  }
  (is.logical(opt) && length(opt) == 1 && opt) ||
    identical(opt, "TRUE") ||
    identical(opt, "true")
}

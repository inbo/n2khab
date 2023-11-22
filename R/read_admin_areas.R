#' Return one of the available geospatial data sources representing
#' administrative areas
#'
#' Returns an administrative geospatial data source.
#' The coordinate reference system is 'BD72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' @details See section \emph{Usage} to see which N2KHAB data sources are
#' available.
#' You get a list of the data source names and related information with
#' \code{XXXXXXXXX}.
#' You are referred to the raw N2KHAB-data collection
#' \href{https://zenodo.org/communities/n2khab-data-raw}{at Zenodo} to learn
#' more about these data sources.
#'
#' @param dsn A string, conforming to one of the data source names listed under
#' \emph{Usage}.
#' Considering the default values of the \code{path} and \code{file} arguments,
#' it is recommended to just use this argument.
#' Defaults to \code{"flanders"}.
#'
#' Different data sources are handled differently by the function.
#' The \code{dsn} argument states which data source is requested.
#' It is also used to determine \code{file} if that argument is not specified,
#' in order to select the right data file(s) from the \code{n2khab_data} folder.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of geometry type \code{MULTIPOLYGON} or
#' \code{POLYGON}.
#'
#' @examples
#' \dontrun{
#' flanders <- read_admin_areas(dsn = "flanders")
#' provinces <- read_admin_areas(dsn = "provinces")
#' sac <- read_admin_areas(dsn = "sac")
#' }
#'
#' @importFrom assertthat
#' assert_that
#' @importFrom sf
#' read_sf
#' @importFrom dplyr
#' %>%
#' select
#' mutate
#' arrange
#' vars
#' mutate_at
#' n
#' @importFrom forcats
#' fct_reorder
#' @importFrom stringr
#' str_detect
#' @importFrom rlang .data
#' @export
read_admin_areas <-
  function(file = file.path(
             fileman_up("n2khab_data"),
             c(
               "10_raw/flanders",
               "10_raw/provinces",
               "10_raw/sac"
             )
           ),
           dsn = c("flanders", "provinces", "sac")) {
    dsn <- match.arg(dsn)

    if (missing(file)) {
      file <- file[str_detect(file, dsn)][1]
    }

    assert_that(file.exists(file))

    suppressWarnings(
      requested <- read_sf(file,
        crs = 31370
      )
    )

    requested <-
      switch(dsn,
        "flanders" = requested %>%
          select(name = .data$NAAM) %>%
          mutate(name = factor(.data$name)),
        "provinces" = requested %>%
          select(
            name = .data$NAAM,
            territory_id = .data$TERRID,
            code_nis = .data$NISCODE,
            code_nuts2 = .data$NUTS2
          ) %>%
          mutate(
            name = fct_reorder(
              .data$name,
              .data$territory_id
            ),
            code_nis = fct_reorder(
              .data$code_nis,
              .data$territory_id
            ),
            code_nuts2 = fct_reorder(
              .data$code_nuts2,
              .data$territory_id
            ),
            territory_id = factor(.data$territory_id)
          ),
        "sac" = requested %>%
          select(
            sac_code = .data$GEBCODE,
            sac_name = .data$NAAM,
            subsac_code = .data$DEELGEBIED,
            polygon_id = .data$POLY_ID
          ) %>%
          arrange(
            .data$sac_code,
            .data$subsac_code,
            .data$polygon_id
          ) %>%
          mutate(
            id = 1:n(),
            sac_code = fct_reorder(
              .data$sac_code,
              .data$id
            ),
            sac_name = fct_reorder(
              .data$sac_name,
              .data$id
            ),
            subsac_code = fct_reorder(
              .data$subsac_code,
              .data$id
            )
          ) %>%
          select(-.data$id)
      )

    return(requested)
  }

#' Return the \code{ecoregions} data source as an \code{sf} object
#'
#' Returns the raw data source \code{ecoregions}, with unique polygon
#' identifiers \code{polygon_code} and \code{polygon_id}.
#' Multiple polygons can have the same \code{region_name}.
#' The coordinate reference system is 'BD72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' Original columns of the raw data source were mapped as:
#' \itemize{
#' \item \code{CODE} -> \code{polygon_code}
#' \item \code{NR} -> \code{polygon_id}
#' \item \code{REGIO} -> \code{region_name}
#' \item \code{DISTRICT} -> \code{district_name}
#' }
#'
#' Apart from the label, there is no complementary information between
#' \code{polygon_code} and \code{polygon_id}.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of geometry type \code{MULTIPOLYGON}.
#'
#' @examples
#' \dontrun{
#' ecoregions <- read_ecoregions()
#' ecoregions
#' }
#'
#' @importFrom sf
#' read_sf
#' st_drop_geometry
#' @importFrom dplyr
#' %>%
#' select
#' mutate
#' arrange
#' @importFrom rlang .data
#' @export
read_ecoregions <-
  function(file = file.path(locate_n2khab_data(), "10_raw/ecoregions")) {
    suppressWarnings(
      ecoregions <- read_sf(file,
        crs = 31370
      )
    )

    ecoregions <-
      ecoregions %>%
      select(
        polygon_code = .data$CODE,
        polygon_id = .data$NR,
        region_name = .data$REGIO,
        district_name = .data$DISTRICT
      ) %>%
      arrange(.data$polygon_code, .data$polygon_id)

    er_levels <-
      ecoregions %>%
      st_drop_geometry() %>%
      select(-.data$district_name)

    ecoregions <-
      ecoregions %>%
      mutate(
        polygon_code = factor(.data$polygon_code,
          levels = er_levels$polygon_code
        ),
        polygon_id = factor(.data$polygon_id,
          levels = er_levels$polygon_id
        ),
        region_name = factor(.data$region_name,
          levels = unique(er_levels$region_name)
        ),
        district_name = factor(.data$district_name)
      )

    return(ecoregions)
  }

#' Return the data source \code{habitatmap_stdized} as a list of two
#' objects
#'
#' \code{read_habitatmap_stdized} returns the data source \code{habitatmap_stdized} as a list of two objects:
#' \itemize{
#'   \item \code{habitatmap_polygons}: an sf object in the Belgian Lambert 72
#'   CRS (EPSG-code \href{https://epsg.io/31370}{31370}) with all polygons
#'   of the \code{habitatmap} that contain habitat or a regionally
#'   important biotope (RIB).
#'   \item \code{habitatmap_types}: a tibble with information on the
#'   habitat and RIB types (HAB1, HAB2,..., HAB5) that occur within
#'   each polygon of \code{habitatmap_polygons}.
#'   }
#'
#' The data source \code{habitatmap_stdized} is the processed version
#' of the raw data source \code{habitatmap} (De Saeger et al., 2018).
#' Every polygon in the \code{habitatmap} can consist of maximum 5
#' different vegetation types. This information is stored in the
#' columns 'HAB1', HAB2',..., 'HAB5' of the attribute table. The
#' fraction of each vegetation type within the polygons is stored in
#' the columns 'PHAB1', 'PHAB2', ..., 'PHAB5'.
#'
#' The data source \code{habitatmap_stdized} is a GeoPackage, available at
#' \href{https://doi.org/10.5281/zenodo.3355192}{Zenodo}, that
#' contains:
#' \itemize{
#'   \item \code{habitatmap_polygons}: a spatial layer with every
#'   \code{habitatmap} polygon that contains a habitat or RIB type
#'   listed in \code{\link{types}}.
#'   \item \code{habitatmap_types}: a table with the types that occur in each polygon.
#'   }
#'
#' The processing of the \code{habitatmap_types} tibble included
#' following steps:
#' \itemize{
#'   \item For some polygons the vegetation type is uncertain, and the
#'   vegetation code in the raw \code{habitatmap} data source consists
#'   of 2 or 3 possible types, separated with a ','. The different
#'   possible vegetation types are split up and one row is created for
#'   each of them. The variable \code{certain} will be \code{FALSE} if
#'   the original code consists of 2 or 3 possible vegetation types, and \code{TRUE}
#'   if only one vegetation type is provided.
#'   \item For some polygons the original vegetation code in the
#'   \code{habitatmap} was not consistent with general coding syntax or
#'   with the type codes from the \code{\link{types}}. In that case the
#'   code was adjusted.
#'
#'   }
#'
#'   The R-code for creating the \code{habitatmap_stdized} data source
#'   can be found in the \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing} repository.
#'
#'
#' @param file The absolute or relative file path of the data source.
#' The default follows the data management advice in the
#' vignette on data storage (run \code{vignette("v020_datastorage")}).
#' It uses the first \code{n2khab_data} folder that is found when
#' sequentially climbing up 0 to 10 levels in the file system hierarchy,
#' starting from the working directory.
#' @param version Version ID of the data source.
#' Defaults to the latest available version defined by the package.
#'
#' @return
#' A list of two objects:
#'   \itemize{
#'   \item \code{habitatmap_polygons}: an sf object of \code{habitatmap} polygons with two attribute variables
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{description_orig}: polygon description based on the
#'     orginal vegetation codes in the raw \code{habitatmap}}
#'   }
#'   \itemize{
#'   \item \code{habitatmap_types}: a tibble with following variables
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{code_orig}: original vegetation code in raw \code{habitatmap}.
#'     \item \code{phab}: proportion of polygon covered by type, as a percentage.
#'     \item \code{certain}: \code{TRUE} when vegetation type is certain and
#'      \code{FALSE} when vegetation type is uncertain.
#'     \item \code{type}: habitat or RIB type listed in \code{\link{types}}.
#'     }
#'     }
#'
#' @family functions involved in processing the 'habitatmap' data source
#'
#' @references
#'
#' De Saeger S., Guelinckx R., Oosterlynck P., De Bruyn A., Debusschere K., Dhaluin P.,
#' Erens R., Hendrickx P., Hendrix R., Hennebel D., et al. (2018). Biologische
#' Waarderingskaart en Natura 2000 Habitatkaart: Uitgave 2018. Rapporten van het
#' Instituut voor Natuur- en Bosonderzoek. Instituut voor Natuur- en Bosonderzoek (INBO).
#' \doi{10.21436/inbor.15138099}.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'habitatmap_stdized'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' hms <- read_habitatmap_stdized()
#' hms_polygons <- hms$habitatmap_polygons
#' hms_types <- hms$habitatmap_types
#' hms_polygons
#' hms_types
#' }
#'
#' @export
#' @importFrom sf
#' read_sf
#' st_crs<-
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#' @importFrom assertthat
#' assert_that
#' is.string
#'
read_habitatmap_stdized <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "20_processed/habitatmap_stdized/habitatmap_stdized.gpkg"),
             version = "habitatmap_stdized_2018_v2"){

        assert_that(is.string(version))

        habmap_polygons <- read_sf(file,
                                   "habitatmap_polygons")

        habmap_polygons <- habmap_polygons %>%
            mutate(polygon_id = factor(.data$polygon_id))

        suppressWarnings(st_crs(habmap_polygons) <- 31370)

        if (version == "habitatmap_stdized_2018_v1") {
            habmap_types <- suppressWarnings(
                read_sf(file,
                        "habitatmap_patches")
            )
        } else {
            habmap_types <- suppressWarnings(
                read_sf(file,
                        "habitatmap_types")
            )
        }

        types <- suppressWarnings(read_types())

        habmap_types <- habmap_types %>%
            mutate( polygon_id = factor(.data$polygon_id,
                                        levels = levels(habmap_polygons$polygon_id)),
                    certain = .data$certain == 1,
                    type = factor(.data$type,
                                  levels = levels(types$type)
                                  )
                    )

        if (version == "habitatmap_stdized_2018_v1") {

            result <- list(habitatmap_polygons = habmap_polygons,
                       habitatmap_patches = habmap_types)

        } else {

           result <- list(habitatmap_polygons = habmap_polygons,
                       habitatmap_types = habmap_types)

        }

        return(result)

    }





#' Return the data source \code{watersurfaces_hab} as a list of two
#' objects
#'
#' \code{read_watersurfaces_hab} returns the data source \code{watersurfaces_hab} as a list of two objects:
#' \itemize{
#'   \item \code{watersurfaces_polygons}: an sf object in the Belgian Lambert 72
#'   CRS (EPSG-code \href{https://epsg.io/31370}{31370}) with all polygons
#'   that contain standing water types (habitat or RIB).
#'   \item \code{watersurfaces_types}: a tibble with information on the
#'   standing water types (HAB1, HAB2,..., HAB5) that occur within
#'   each polygon of \code{watersurfaces_polygons}.
#'   }
#'
#' The data source \code{watersurfaces_hab} is a combination of \code{habitatmap_stdized} (see
#' \code{\link{read_habitatmap_stdized}}) and the \href{https://http://www.geopunt.be/catalogus/datasetfolder/10e87ad3-8235-40e0-8269-42c3c96a884d}{watersurface map of Flanders}.
#' It contains all standing water types in Flanders.
#'
#'
#' The data source \code{watersurfaces_hab} is a GeoPackage, available at
#' \href{https://doi.org/10.5281/zenodo.3374645}{Zenodo}, that contains:
#' \itemize{
#'   \item \code{watersurfaces_hab_polygons}: a spatial layer with all polygons that contain standing water types listed in \code{\link{types}}.
#'   \item \code{watersurfaces_hab_types}: a table in which every row corresponds with a combination of polygon and type.
#'   }
#'
#'The polygons with 2190_a habitat (dune slack ponds) are generated by selecting all watersurface polygons that
#'overlap with dune habitat polygons (21xx) of the standardized habitat map.
#'
#'For each of the other considered habitat types (31xx and rbbah) we select the watersurface polygons that
#'overlap with the selected habitat type polygons of the standardized habitat map. We also select polygons of the
#'standardized habitat map that contain standing water types but do not overlap with any watersurface polygon of the
#'watersurface map.
#'
#'The R-code for creating the \code{watersurfaces_hab} data source can be found in the \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing}
#'repository.
#'
#'
#' @param interpreted If \code{TRUE}, the interpreted subtype is provided when the subtype is missing. This only
#' applies to type 3130. When the subtype is missing for 3130, we interpret it as 3130_aom.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A list of two objects:
#'   \itemize{
#'   \item \code{watersurfaces_polygons}: an sf object of standing water polygons with four attribute variables:
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{polygon_id_ws}: id for the polygon in the \code{watersurface map}
#'     \item \code{polygon_id_habitatmap}: id's of all overlapping polygons of \code{habitatmap_stdized} that
#'     contain standing water habitat. The different id's are separated by '+'.
#'     \item \code{description_orig}: descriptions of all overlapping polygons of \code{habitatmap_stdized} that
#'     contain standing water habitat. The different descriptions are separated by '+'.}
#'   }
#'   \itemize{
#'   \item \code{watersurfaces_types}: a tibble with following variables:
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{code_orig}: original vegetation code in raw \code{habitatmap}.
#'     \item \code{certain}: \code{TRUE} when vegetation type is certain and
#'      \code{FALSE} when vegetation type is uncertain.
#'     \item \code{type}: habitat or RIB type listed in \code{\link{types}}.
#'     }
#'     }
#'
#' @family functions involved in processing the 'habitatmap' data source
#'
#' @family functions involved in processing the 'watersurfaces' data source
#'
#' @references
#' \itemize{
#' \item Packet J., Scheers K., Smeekens V., Leyssen A., Wils C. & Denys L.
#' (2018).
#' Watervlakken versie 1.0: polygonenkaart van stilstaand water in Vlaanderen.
#' Een nieuw instrument voor onderzoek, water-, milieu- en natuurbeleid.
#' Rapporten van het Instituut voor Natuur- en Bosonderzoek 2018 (14).
#' Instituut voor Natuur- en Bosonderzoek, Brussel.
#' \doi{10.21436/inbor.14178464}.
#' \item De Saeger S., Guelinckx R., Oosterlynck P., De Bruyn A., Debusschere K., Dhaluin P.,
#' Erens R., Hendrickx P., Hendrix R., Hennebel D., et al. (2018). Biologische
#' Waarderingskaart en Natura 2000 Habitatkaart: Uitgave 2018. Rapporten van het
#' Instituut voor Natuur- en Bosonderzoek. Instituut voor Natuur- en Bosonderzoek (INBO).
#' \doi{10.21436/inbor.15138099}.
#' }
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'watersurfaces_hab'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' wsh <- read_watersurfaces_hab()
#' wsh_polygons <- wsh$watersurfaces_polygons
#' wsh_types <- wsh$watersurfaces_types
#' wsh_polygons
#' wsh_types
#' }
#'
#' @export
#' @importFrom sf
#' read_sf
#' st_crs<-
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' mutate
#' mutate_at
#' vars
#' @importFrom assertthat
#' assert_that
#' is.string
#'
read_watersurfaces_hab <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "20_processed/watersurfaces_hab/watersurfaces_hab.gpkg"),
             interpreted = FALSE,
             version = "watersurfaces_hab_v3"){

        assert_that(is.string(version))

        watersurfaces_polygons <- read_sf(file,
                                   "watersurfaces_hab_polygons")

        watersurfaces_polygons <- watersurfaces_polygons %>%
            mutate_at(.vars = vars(starts_with("polygon_id")),
                      .funs = factor)

        suppressWarnings(st_crs(watersurfaces_polygons) <- 31370)

        if (version %in% c("watersurfaces_hab_v1", "watersurfaces_hab_v2")) {
            watersurfaces_types <- suppressWarnings(
                read_sf(file,
                        "watersurfaces_hab_patches")
            )
        } else {
            watersurfaces_types <- suppressWarnings(
                read_sf(file,
                        "watersurfaces_hab_types")
            )
        }

        if (interpreted){
          watersurfaces_types <- watersurfaces_types %>%
              mutate(type = ifelse(.data$type == "3130", "3130_aom", .data$type))
        }

        types <- suppressWarnings(read_types())

        watersurfaces_types <- watersurfaces_types %>%
            mutate( polygon_id = factor(.data$polygon_id, levels = levels(watersurfaces_polygons$polygon_id)),
                    certain = .data$certain == 1,
                    type = factor(.data$type,
                                  levels = levels(types$type)
                                  )
                    )

        if (version %in% c("watersurfaces_hab_v1", "watersurfaces_hab_v2")) {

          result <- list(watersurfaces_polygons = watersurfaces_polygons,
                       watersurfaces_patches = watersurfaces_types)

        } else {

           result <- list(watersurfaces_polygons = watersurfaces_polygons,
                       watersurfaces_types = watersurfaces_types)

        }

        return(result)

    }















#' Return the data source \code{watersurfaces} as an \code{sf} polygon layer
#'
#' Returns the raw data source \code{watersurfaces} (Leyssen et al., 2020)
#' as a standardized \code{sf} polygon layer
#' (tidyverse-styled, internationalized) in the Belgian Lambert 72 CRS
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' If \code{file} is not specified, the function will try to read the file
#' in the default folder for data storage (as described in the data management
#' advice in the vignette (run \code{vignette("v020_datastorage")})).
#' If you want to use another file or file structure than the default
#' data storage, you can specify your own \code{file}.
#' In both cases: always make sure to specify the correct \code{version}, that
#' is the version corresponding to the \code{file} (note that the \code{version}
#' defaults to the latest version, that is \code{watersurfaces_v1.1}).
#'
#' See Leyssen et al. (2020) for an elaborate explanation of the data source
#' and its attributes.
#'
#' @param file Optional string. An absolute or relative file path of
#' the data source. If left \code{NULL}, the default follows the data management
#' advice in the vignette on data storage
#' (run \code{vignette("v020_datastorage")}).
#' It uses the first \code{n2khab_data} folder that is found when
#' sequentially climbing up 0 to 10 levels in the file system hierarchy,
#' starting from the working directory.
#' @param extended Logical.
#' Should names or explanations of codes be added as extra
#' variables in the result?
#' Currently only applies to \code{wfd_type} and \code{connectivity};
#' if \code{TRUE}, the variables \code{wfd_type_name} and
#' \code{connectivity_name} are added.
#' Defaults to \code{FALSE}.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of
#' type \code{POLYGON}, sorted by \code{polygon_id}, with the following
#' variables (not mentioning extra 'name' variables for
#' \code{extended = TRUE}):
#' \itemize{
#'   \item \code{polygon_id}: code of the polygon;
#'   \item \code{wfd_code}: optional; Flemish code of the water body with
#'   regard to the Water Framework Directive (WFD);
#'   \item \code{hyla_code}: optional; code of the watersurface according to the
#'   Flemish working group 'Hyla', a working group on amphibians & reptiles;
#'   \item \code{name}: name of the watersurface;
#'   \item \code{area_name}: name of the area;
#'   \item \code{wfd_type}: type code according to the Flemish WFD typology
#'   (Denys, 2009);
#'   \item \code{wfd_type_certain}: Logical.
#'   Is there high confidence about the \code{wfd_type} determination?
#'   \item \code{depth_class}: class of water depth;
#'   \item \code{connectivity}: connectivity class;
#'   \item \code{usage}: usage class.
#' }
#'
#' @family functions involved in processing the 'watersurfaces' data source
#' @family functions returning important environmental data sets
#'
#' @references
#'
#' \itemize{
#' \item Denys L. (2009). Een a posteriori typologie van stilstaande, zoete
#' wateren in Vlaanderen.
#' Rapporten van het Instituut voor Natuur- en Bosonderzoek INBO.R.2009.34.
#' Instituut voor Natuur- en Bosonderzoek, Brussel.
#' \item Leyssen A., Scheers K., Smeekens V., Wils C., Packet J., De Knijf G. &
#' Denys L. (2020).
#' Watervlakken versie 1.1: polygonenkaart van stilstaand water in Vlaanderen.
#' Uitgave 2020. Rapporten van het Instituut voor Natuur- en Bosonderzoek 2020
#' (40). Instituut voor Natuur en Bosonderzoek, Brussel.
#' \doi{10.21436/inbor.19088385}.
#' }
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'watersurfaces' data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' ws <- read_watersurfaces()
#' ws
#' summary(ws)
#'
#' ws2 <- read_watersurfaces(extended = TRUE)
#' ws2
#' }
#'
#' @importFrom sf
#' read_sf
#' @importFrom plyr
#' mapvalues
#' @importFrom rlang
#' .data
#' na_lgl
#' @importFrom dplyr
#' %>%
#' across
#' arrange
#' mutate
#' mutate_at
#' mutate_if
#' rename
#' select
#' left_join
#' everything
#' tribble
#' @importFrom assertthat
#' assert_that
#' @importFrom stringr
#' str_replace
#' @export
read_watersurfaces <-
    function(file = NULL,
             extended = FALSE,
             version = c("watersurfaces_v1.1", "watersurfaces_v1.0")) {

        version <- match.arg(version)

        if (missing(file)) {

            if (version == "watersurfaces_v1.1") {
                file <- file.path(fileman_up("n2khab_data"),
                                  "10_raw/watersurfaces/watersurfaces.gpkg")
                } else {
                    file <- file.path(fileman_up("n2khab_data"),
                                      "10_raw/watersurfaces/watersurfaces.shp")
                }

            assert_that(file.exists(file),
                        msg =  paste("Path", file, "does not exist. Control the",
                                     "path and specify the corresponding version",
                                     "if you do not use", version))
        } else {

            assert_that(file.exists(file))

            if (version == "watersurfaces_v1.1") {
                if (substr(file, nchar(file) - 4, nchar(file)) != ".gpkg") {
                    stop(paste(version, "should be a GeoPackage (.gpkg).",
                               "Control the version and the path."))
                }
            }
        }

        if (version == "watersurfaces_v1.1") {

            suppressWarnings(
                watersurfaces <- read_sf(file,
                                         layer = "Watervlakken",
                                         crs = 31370))

            wfd_typetransl <- read_sf(file, layer = "LktKRWTYPE") %>%
                mutate_if(., is.character,
                          .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
                mutate(across(c(.data$Code), as.factor)) %>%
                dplyr::rename(wfd_type = .data$Code,
                              wfd_type_name = .data$Omschrijving)

        } else {

            suppressWarnings(
                watersurfaces <- read_sf(file,
                                         crs = 31370)
            )

            wfd_typetransl <-
                tribble(~wfd_type, ~wfd_type_name,
                        "B", "sterk brak",
                        "Bzl", "zeer licht brak",
                        "Ad", "alkalisch duinwater",
                        "Ai", "ondiep, alkalisch, ionenrijk",
                        "Ami", "ondiep, alkalisch, matig ionenrijk",
                        "Ami-e", "ondiep, alkalisch, matig ionenrijk, eutroof",
                        "Ami-om", "ondiep, alkalisch, matig ionenrijk, oligo-mesotroof",
                        "Aw", "groot-diep, alkalisch",
                        "Aw-e", "groot-diep, alkalisch, eutroof",
                        "Aw-om", "groot-diep, alkalisch, oligo-mesotroof",
                        "C", "circumneutraal",
                        "Cb", "circumneutraal, sterk gebufferd",
                        "CbFe", "circumneutraal, sterk gebufferd, ijzerrijk",
                        "Czb", "circumneutraal, zwak gebufferd",
                        "Z", "zuur",
                        "Zm", "zwak zuur",
                        "Zs", "sterk zuur"
                ) %>%
                mutate(
                    wfd_type = factor(.data$wfd_type,
                                      levels = .$wfd_type)
                )

        }



        watersurfaces <-
            watersurfaces %>%
            select(polygon_id = .data$WVLC,
                   wfd_code = .data$WTRLICHC,
                   hyla_code = .data$HYLAC,
                   name = .data$NAAM,
                   area_name = .data$GEBIED,
                   wfd_type = .data$KRWTYPE,
                   wfd_type_certain = .data$KRWTYPES,
                   depth_class = .data$DIEPKL,
                   connectivity = .data$CONNECT,
                   usage = .data$FUNCTIE) %>%
            mutate(depth_class = str_replace(string = .data$depth_class,
                                             pattern = "\u2265",
                                             replacement = ">=")) %>%
            mutate(across(c(.data$area_name,
                            .data$depth_class,
                            .data$connectivity,
                            .data$usage),
                          as.factor)) %>%
            mutate(wfd_type = .data$wfd_type %>%
                    factor(levels =
                               levels(wfd_typetransl$wfd_type)),
                hyla_code = ifelse(.data$hyla_code == 0,
                                   NA,
                                   .data$hyla_code)
            ) %>%
            arrange(.data$polygon_id)

        if (version == "watersurfaces_v1.0") {
            watersurfaces <-
                watersurfaces %>%
                mutate_at(.vars = c("wfd_code", "name"),
                          .funs = function(x) {
                              ifelse(x == "<Null>", NA, x)
                          }) %>%
                mutate(wfd_type_certain = ifelse(is.na(.data$wfd_type_certain),
                                                 na_lgl,
                                                 .data$wfd_type_certain %in%
                                                     c("zeker",
                                                       "definitief")))
        } else {
            watersurfaces <-
                watersurfaces %>%
                mutate(wfd_type_certain = ifelse(is.na(.data$wfd_type_certain),
                                                 na_lgl,
                                                 .data$wfd_type_certain ==
                                                     "definitief"))
        }

        if (extended) {

            if (version == "watersurfaces_v1.1") {

                connectivitytransl <- read_sf(file, layer = "LktCONNECT") %>%
                    mutate_if(., is.character,
                              .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
                    mutate(across(c(.data$Code), as.factor)) %>%
                    rename(connectivity = .data$Code,
                           connectivity_name = .data$Omschr)

            } else {

                connectivitytransl <-
                    tribble(~connectivity, ~connectivity_name,
                            paste0("ge","\u00EF","soleerd"),
                            "niet verbonden met een waterloop",
                            "periodiek",
                            paste0("tijdelijk (door peilbeheer of droogte) ",
                                   "in verbinding met minstens ","\u00E9",
                                   "\u00E9","n waterloop"),
                            "permanent",
                            paste0("permanent in verbinding met minstens ",
                                   "\u00E9","\u00E9","n waterloop")
                    ) %>%
                    mutate(
                        connectivity = factor(.data$connectivity,
                                              levels = .$connectivity)
                    )
            }

            watersurfaces <-
                watersurfaces %>%
                left_join(wfd_typetransl, by = "wfd_type") %>%
                mutate(
                    wfd_type_name =
                        .data$wfd_type %>%
                        mapvalues(from = wfd_typetransl$wfd_type,
                                  to = wfd_typetransl$wfd_type_name)
                ) %>%
                left_join(connectivitytransl, by = "connectivity") %>%
                mutate(
                    connectivity_name =
                        .data$connectivity %>%
                        mapvalues(from = connectivitytransl$connectivity,
                                  to = connectivitytransl$connectivity_name)
                ) %>%
                select(1:6,
                       .data$wfd_type_name,
                       7:9,
                       .data$connectivity_name,
                       everything())
        }

        return(watersurfaces)

    }

























#' Return the data source \code{habitatmap} as an \code{sf} multipolygon layer
#'
#' Returns the raw data source \code{habitatmap} (De Saeger et al., 2018)
#' as a standardized \code{sf} multipolygon layer
#' (tidyverse-styled, internationalized) in the Belgian Lambert 72 CRS
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#' Given the size of the data source, this function
#' takes a bit longer than usual to run.
#'
#' @param filter_hab If \code{TRUE} only polygons that (partially) contain habitat or a regionally
#' important biotope (RIB) are returned. The default value is \code{FALSE}.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of
#' type \code{MULTIPOLYGON}.
#'
#' @family functions involved in processing the 'habitatmap' data source
#'
#' @references
#'
#' De Saeger S., Guelinckx R., Oosterlynck P., De Bruyn A., Debusschere K., Dhaluin P.,
#' Erens R., Hendrickx P., Hendrix R., Hennebel D., et al. (2018). Biologische
#' Waarderingskaart en Natura 2000 Habitatkaart: Uitgave 2018. Rapporten van het
#' Instituut voor Natuur- en Bosonderzoek. Instituut voor Natuur- en Bosonderzoek (INBO).
#' \doi{10.21436/inbor.15138099}.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'habitatmap'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' hm <- read_habitatmap()
#' hm
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' @importFrom sf
#' read_sf
#' st_crs<-
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' mutate
#' select
#' filter
#' starts_with
#'
read_habitatmap <-
    function(file = file.path(fileman_up("n2khab_data"), "10_raw/habitatmap"),
             filter_hab = FALSE){

        assert_that(file.exists(file))
        assert_that(is.flag(filter_hab), noNA(filter_hab))

        habitatmap <- read_sf(file,
                              "habitatmap")

        colnames(habitatmap) <- tolower(colnames(habitatmap))

        habitatmap <- habitatmap %>%
            select(polygon_id = .data$tag,
                   .data$eval,
                   starts_with("eenh"),
                   .data$v1,
                   .data$v2,
                   .data$v3,
                   source = .data$herk,
                   .data$info,
                   bwk_label = .data$bwklabel,
                   .data$hab1,
                   .data$phab1,
                   .data$hab2,
                   .data$phab2,
                   .data$hab3,
                   .data$phab3,
                   .data$hab4,
                   .data$phab4,
                   .data$hab5,
                   .data$phab5,
                   source_hab = .data$herkhab,
                   source_phab = .data$herkphab,
                   hab_legend = .data$hablegende,
                   area_m2 = .data$oppervl)

        habitatmap <- habitatmap %>%
            mutate(eval = factor(.data$eval),
                   hab_legend = factor(.data$hab_legend)
                   )

        if (filter_hab) {

            # we only select polygons with habitat or RIB, i.e. polygons in habitatmap_stdized data source
            hab_stdized <- read_habitatmap_stdized()
            hab_stdized <- hab_stdized$habitatmap_polygons

            habitatmap <- habitatmap %>%
            filter(.data$polygon_id %in% hab_stdized$polygon_id) %>%
            mutate(polygon_id = factor(.data$polygon_id, levels = hab_stdized$polygon_id))

        }

        suppressWarnings(st_crs(habitatmap) <- 31370)

        return(habitatmap)

    }










#' Return the data source \code{habitatmap_terr} as a list of two
#' objects
#'
#' \code{read_habitatmap_terr()} returns the data source \code{habitatmap_terr}
#' as a list of two objects: \code{habitatmap_terr_polygons}, having the Belgian
#' Lambert 72 CRS (EPSG-code \href{https://epsg.io/31370}{31370}), and
#' \code{habitatmap_terr_types}.
#' \code{habitatmap_terr} is the further interpreted, terrestrial part of
#' \code{habitatmap_stdized} (see \code{\link{read_habitatmap_stdized}}).
#' By default, occurrences of type \code{7220} are dropped because a more
#' reliable data source is available for this habitat type (see \code{drop_7220}
#' argument).
#'
#' \code{habitatmap_terr} was derived from \code{habitatmap_stdized} as
#' follows:
#' \itemize{
#' \item{it excludes all polygons
#' that are most probably aquatic habitat or RIB.
#' These are the polygons for which
#' \strong{all} habitat or RIB types are aquatic.
#' In the process, a distinction was also made between \code{2190_a} and
#' \code{2190_overig}.
#' There is no exclusion of aquatic types when these coexist with
#' terrestrial types in the same polygon.
#' The aquatic types are the types for which \code{hydr_class == "HC3"}
#' in the \code{\link{types}} data source (\code{hydr_class} is the hydrological
#' class; cf. the output of \code{\link[=read_types]{read_types()}});}
#' \item{it excludes types which most probably are \emph{no}
#' habitat or RIB at all.
#' Those are the types where \code{code_orig} contains \code{"bos"} or is
#' equal to \code{"6510,gh"} or \code{"9120,gh"};}
#' \item{it translates several main type codes into a corresponding
#' subtype which they almost always represent:
#' \code{6410} -> \code{6410_mo},
#' \code{6430} -> \code{6430_hf},
#' \code{6510} -> \code{6510_hu},
#' \code{7140} -> \code{7140_meso},
#' \code{9130} -> \code{9130_end};}
#' \item{it distinguishes types \code{rbbhfl} and \code{rbbhf}.}
#' }
#'
#' The data source \code{habitatmap_terr} is a GeoPackage, available at
#' \href{https://doi.org/10.5281/zenodo.3468948}{Zenodo}, that contains:
#' \itemize{
#'   \item{\code{habitatmap_terr_polygons}: a spatial polygon layer}
#'   \item{\code{habitatmap_terr_types}: a table with the types that occur
#'   in each polygon.}
#'   }
#'
#' The R-code for creating the \code{habitatmap_terr} data source
#' can be found in the
#' \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing}
#' repository.
#'
#' @param keep_aq_types Logical; \code{TRUE} by default.
#' The data source \code{habitatmap_terr} aims at delineating all
#' polygons with at least one (semi-)terrestrial type.
#' For those polygons, it returns all known habitat types and RIBs as types.
#' Hence, in several cases polygons do occur with a combination of terrestrial
#' and aquatic types (see \emph{Details} for a definition of 'aquatic').
#' Setting \code{keep_aq_types = FALSE} is convenient for use cases where one
#' only wants to look at the (semi-)terrestrial types: this setting will
#' discard all aquatic types in 'mixed' aquatic/terrestrial polygons.
#' As each polygon always has at least one (semi-)terrestrial type,
#' this will not affect the number of polygons returned,
#' only the number of types.
#' @param drop_7220 Logical; \code{TRUE} by default.
#' Should occurrences of type \code{7220} be dropped from the result?
#' To get more accurate information about type \code{7220}, notably its
#' occurrences, surface area and other characteristics, it is advised
#' to use the \code{habitatsprings} data source and not
#' \code{habitatmap_terr} - see
#' \code{\link[=read_habitatsprings]{read_habitatsprings()}}.
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A list of two objects:
#'   \itemize{
#'   \item \code{habitatmap_terr_polygons}: a Simple feature collection of
#'   geometry type \code{MULTIPOLYGON} with four attribute variables:
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{description_orig}: polygon description based on the
#'     original vegetation codes in the \code{habitatmap} data source
#'     \item \code{description}: based on \code{description_orig} but with the
#'     interpreted type codes
#'     \item \code{source}: states where \code{description} comes from: either
#'     \code{habitatmap_stdized} or \code{habitatmap_stdized + interpretation}
#'   }
#'   \item \code{habitatmap_terr_types}: a tibble with the following
#'   variables (the first 4 being identical to those in
#'   \code{habitatmap_stdized}):
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{code_orig}
#'     \item \code{phab}
#'     \item \code{certain}
#'     \item \code{type}: the interpreted habitat or RIB type
#'     \item \code{source}: states where \code{type} comes from: either
#'     \code{habitatmap_stdized} or \code{habitatmap_stdized + interpretation}
#'     }
#'     }
#'
#' @family functions involved in processing the 'habitatmap' data source
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'habitatmap_terr'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' habmap_terr <- read_habitatmap_terr()
#' habmap_terr$habitatmap_terr_polygons
#' habmap_terr$habitatmap_terr_types
#'
#' habmap_terr_noaq <- read_habitatmap_terr(keep_aq_types = FALSE)
#' habmap_terr_noaq$habitatmap_terr_polygons
#' habmap_terr_noaq$habitatmap_terr_types
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' is.string
#' @importFrom sf
#' read_sf
#' st_crs<-
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' mutate
#' filter
read_habitatmap_terr <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "20_processed/habitatmap_terr/habitatmap_terr.gpkg"),
             keep_aq_types = TRUE,
             drop_7220 = TRUE,
             version = "habitatmap_terr_2018_v2"){

        assert_that(is.flag(keep_aq_types), noNA(keep_aq_types))
        assert_that(is.flag(drop_7220), noNA(drop_7220))
        assert_that(is.string(version))

        habmap_terr_polygons <- read_sf(file,
                                   "habitatmap_terr_polygons")

        habmap_terr_polygons <- habmap_terr_polygons %>%
            mutate(polygon_id = factor(.data$polygon_id),
                   source = factor(.data$source))

        suppressWarnings(st_crs(habmap_terr_polygons) <- 31370)

        if (version == "habitatmap_terr_2018_v1") {
            habmap_terr_types <- suppressWarnings(
                read_sf(file,
                        "habitatmap_terr_patches")
            )
        } else {
            habmap_terr_types <- suppressWarnings(
                read_sf(file,
                        "habitatmap_terr_types")
            )
        }

        types <- read_types()

        habmap_terr_types <- habmap_terr_types %>%
             mutate(polygon_id = factor(.data$polygon_id,
                                        levels = levels(habmap_terr_polygons$polygon_id)),
                    certain = .data$certain == 1,
                    type = factor(.data$type,
                                  levels = levels(types$type)
                    ),
                    source = factor(.data$source)
            )

        if (!keep_aq_types) {
            habmap_terr_types <-
                habmap_terr_types %>%
                filter(!(.data$type %in% (types %>%
                                          filter(.data$hydr_class == "HC3") %>%
                                          .$type)
                         ))
            # The below step is unneeded (and takes several seconds),
            # because polygons with _no_ terrestrial types were already
            # excluded in the data source.
            #
            # habmap_terr_polygons %>%
            #     dplyr::semi_join(habmap_terr_types,
            #                      by = "polygon_id")
        }

        if (drop_7220) {
            habmap_terr_types <-
                habmap_terr_types %>%
                filter(.data$type != "7220")
            # note that no polygons need to be discarded: 7220 never occurred
            # alone
        }

        if (version == "habitatmap_terr_2018_v1") {
            result <- list(habitatmap_terr_polygons = habmap_terr_polygons,
                           habitatmap_terr_patches = habmap_terr_types)
        } else {
            result <- list(habitatmap_terr_polygons = habmap_terr_polygons,
                           habitatmap_terr_types = habmap_terr_types)
        }

        return(result)

    }











#' Return the data source \code{habitatstreams} as an \code{sf} linestring
#' layer or as a list
#'
#' Returns the raw data source \code{habitatstreams} (Leyssen et al., 2020)
#' as an \code{sf} linestring
#' layer or as a list of two objects: the \code{sf} object (CRS:
#' Belgian Lambert 72 (EPSG-code \href{https://epsg.io/31370}{31370}))
#' plus a data frame
#' with textual explanation of the values of the \code{source_id} variable.
#'
#' @param source_text Logical, defaults to \code{FALSE}.
#' If \code{TRUE}, a list is returned (see \emph{Value}).
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' With \code{source_text = FALSE} (default): a Simple feature collection of
#' type \code{LINESTRING}.
#'
#' With \code{source_text = TRUE}: a list of two objects:
#' \itemize{
#' \item \code{lines}: the same \code{sf} object as with \code{source_text = FALSE}.
#' \item \code{sources}: textual explanation on the values of the \code{source_id}
#' variable in the \code{sf} object.
#' }
#'
#' @references
#' Leyssen A., Smeekens V., Denys L. (2020). Indicatieve situering van het
#' Natura 2000 habitattype 3260. Submontane en laaglandrivieren met vegetaties
#' behorend tot het \emph{Ranunculion fluitantis} en het
#' \emph{Callitricho-Batrachion}.
#' Uitgave 2020 (versie 1.7). Rapporten van het Instituut voor Natuur- en
#' Bosonderzoek 2020 (34). Research Institute for Nature and Forest, Brussels.
#' \doi{10.21436/inbor.18903609}.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'habitatstreams'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' library(magrittr)
#' library(sf)
#' hs <- read_habitatstreams()
#' hs
#' hs2 <- read_habitatstreams(source_text = TRUE)
#' hs2
#' all.equal(hs %>% st_drop_geometry,
#'           hs2$lines %>% st_drop_geometry)
#' }
#'
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' @importFrom sf
#' read_sf
#' st_drop_geometry
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' mutate
#' select
#' distinct
#' @importFrom forcats
#' fct_reorder
#' @importFrom stringr
#' str_replace
#' str_squish
#' str_to_title
#' @export
read_habitatstreams <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "10_raw/habitatstreams"),
             source_text = FALSE){

        assert_that(file.exists(file))

        assert_that(is.flag(source_text), noNA(source_text))

        habitatstreams <-
            suppressWarnings(
                read_sf(file,
                        crs = 31370)
            )

        lines <-
            habitatstreams %>%
            select(river_name = .data$NAAM,
                   source_id = .data$BRON) %>%
            mutate(river_name = factor(
                       gsub(pattern = "(^|[[:punct:]])([[:alpha:]])",
                            replacement = "\\1\\U\\2",
                            str_replace(str_to_title(
                                str_squish(.data$river_name)),
                                pattern = "Ij",
                                replacement = "IJ"),
                            perl = TRUE)),
                   source_id = factor(.data$source_id),
                   type = "3260" %>%
                       factor(levels = read_types() %>%
                                            .$type %>%
                                            levels)) %>%
            select(.data$river_name,
                   .data$source_id,
                   .data$type)

        if (source_text) {

            sources <-
                habitatstreams %>%
                st_drop_geometry %>%
                distinct(source_id = .data$BRON,
                       source_text = .data$OMSCHR) %>%
                mutate(source_id = factor(.data$source_id,
                                          levels = lines %>% .$source_id %>%
                                              levels),
                       source_text = fct_reorder(.data$source_text,
                                                 as.numeric(.data$source_id)))

            result <- list(lines = lines,
                           sources = sources)

        } else {
            result <- lines
        }

        return(result)

        }















#' Return the data source \code{habitatsprings} as an \code{sf} point
#' layer
#'
#' Returns the raw data source \code{habitatsprings} as an \code{sf} point
#' layer in the Belgian Lambert 72 CRS (EPSG-code
#' \href{https://epsg.io/31370}{31370}).
#' Optionally, a derived `sf` object of type-`7220`-locations can be
#' returned at the population unit level, through aggregation by `unit_id`.
#'
#' The data source \code{habitatsprings} is a GeoJSON file (conforming to
#' the RFC7946 specifications), available at
#' \href{https://doi.org/10.5281/zenodo.3550994}{Zenodo}.
#' It represents sites that correspond with presence or absence
#' of the Natura 2000 habitat type \code{7220} (Petrifying springs with tufa
#' formation (\emph{Cratoneurion})) in springs and streaming water segments in
#' the Flemish Region, Belgium.
#'
#'
#' @param filter_hab If \code{TRUE}, only points with (potential) habitat
#' are returned. The default value is \code{FALSE}.
#' @param units_7220 If \code{TRUE}, an `sf` object of type-`7220`-locations is
#' returned at the population unit level.
#' To accomplish this, the data source is aggregated by `unit_id`.
#' Multiple points belonging to the same unit are replaced by their
#' centroid, their area attribute is summed (if all values are known)
#' and for other attributes the maximum value is returned.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of
#' type \code{POINT}, with attribute variables:
#'   \itemize{
#'     \item \code{point_id}
#'     \item \code{name}: site name.
#'     \item \code{system_type}: environmental typology of `7220`: `mire`,
#'     `rivulet` or `unknown` (non-`7220` types are `NA`)
#'     \item \code{code_orig}: original vegetation code in raw
#'     \code{habitatsprings}.
#'     \item \code{type}: habitat type listed in \code{\link{types}}.
#'     \item \code{certain}: \code{TRUE} when vegetation type is certain and
#'      \code{FALSE} when vegetation type is uncertain.
#'     \item \code{unit_id}: population unit id for large scale sampling
#'     events.
#'     Spatially close points have the same value.
#'     \item \code{area_m2}: area as square meters.
#'     \item \code{year}: year of field inventory.
#'     \item \code{in_sac}: logical.
#'     Is the site situated within a Special Area of Conservation?
#'     \item \code{source}: original data source of the record.
#'   }
#'
#' Note that the \code{type} and \code{system_type} variables have
#' implicit \code{NA} values
#' (i.e. there is
#' no factor level to represent the missing values).
#' If you want this category to appear in certain results, you can add
#' it as a level with
#' [forcats::fct_explicit_na()].
#'
#' With `units_7220 = TRUE`, variable `point_id` is dropped and
#' an extra attribute variable `nr_of_points` is
#' added.
#' It represents the number of points that belong to each unit.
#'
#' @md
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'habitatsprings'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' hs <- read_habitatsprings()
#' hs
#' hs2 <- read_habitatsprings(units_7220 = TRUE)
#' hs2
#' }
#'
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' is.string
#' @importFrom stringr
#' str_sub
#' @importFrom sf
#' read_sf
#' st_transform
#' st_centroid
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' mutate
#' select
#' filter
#' everything
#' group_by
#' summarise_if
#' mutate_at
#' n
#' vars
#' @export
read_habitatsprings <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "10_raw/habitatsprings/habitatsprings.geojson"),
             filter_hab = FALSE,
             units_7220 = FALSE,
             version = "habitatsprings_2020v2"){

        assert_that(file.exists(file))
        assert_that(is.flag(filter_hab), noNA(filter_hab))
        assert_that(is.flag(units_7220), noNA(units_7220))
        assert_that(is.string(version))

        typelevels <-
            read_types() %>%
            .$type %>%
            levels

        habitatsprings <-
            read_sf(file) %>%
            st_transform(31370) %>%
            mutate(
                area_m2 = ifelse(.data$area_m2 > 0,
                                 .data$area_m2,
                                 NA),
                year = ifelse(.data$year > 0,
                              .data$year,
                              NA),
                in_sac = (.data$sbz == 1),
                type = str_sub(.data$habitattype, end = 4) %>%
                        factor(levels = typelevels),
                certain = (.data$validity_status == "gecontroleerd")
            ) %>%
            {if (filter_hab) filter(., !is.na(.$type)) else .} %>%
            select(point_id = .data$id,
                   .data$name,
                   code_orig = .data$habitattype,
                   .data$type,
                   .data$certain,
                   .data$area_m2,
                   .data$year,
                   .data$in_sac,
                   everything(),
                   -.data$validity_status,
                   -.data$sbz)

        if (version != "habitatsprings_2019v1") {
            habitatsprings <-
                habitatsprings %>%
                mutate(system_type = factor(.data$system_type)) %>%
                select(1:2,
                       .data$system_type,
                       3:5,
                       .data$unit_id,
                       everything())
        }

        if (units_7220) {
            assert_that(version != "habitatsprings_2019v1",
                        msg = paste("'units_7220 = TRUE' is not supported for",
                                    "version habitatsprings_2019v1."))
            suppressWarnings(
            habitatsprings <-
                habitatsprings %>%
                filter(.data$type == "7220") %>%
                select(-.data$point_id) %>%
                group_by(.data$unit_id) %>%
                mutate(area_m2 = sum(.data$area_m2),
                       system_type = as.character(.data$system_type),
                       type = as.character(.data$type),
                       nr_of_points = n()) %>%
                summarise_if(function(x) {!inherits(x, "sfc")},
                             max) %>%
                mutate(type = .data$type %>% factor(levels = typelevels),
                       system_type = factor(.data$system_type)) %>%
                mutate_at(vars(.data$certain,
                               .data$in_sac),
                          as.logical) %>%
                st_centroid() %>%
                select(.data$unit_id,
                       .data$nr_of_points,
                       everything())
            )
        }

        return(habitatsprings)

    }







#' Return the data source \code{habitatquarries}
#'
#' Returns the raw data source \code{habitatquarries} as an \code{sf} polygon
#' layer in the Belgian Lambert 72 CRS (EPSG-code
#' \href{https://epsg.io/31370}{31370}).
#' Optionally, associated bibliographic references can be returned (arguments
#' `references` or `bibtex`).
#'
#' The data source \code{habitatquarries} is a GeoPackage, available at
#' \href{https://doi.org/10.5281/zenodo.4072967}{Zenodo}, that contains:
#' \itemize{
#'   \item{\code{habitatquarries}: a spatial polygon layer that corresponds
#'   with the presence or absence of the Natura 2000 Annex I habitat type `8310`
#'   (Caves not open to the public) in the Flemish Region (and border areas),
#'   Belgium;}
#'   \item{\code{extra_references}: a non-spatial table of site-specific
#'   bibliographic references.}
#'   }
#'
#' In general, different polygons represent different quarry units with their
#' own internal climatic environment.
#' Units that cross Flemish borders have been split into separate polygons.
#' Exceptionally they may overlap if such units are situated above each other.
#'
#' @param filter_hab If \code{TRUE}, only polygons with (known) habitat `8310`
#' are returned.
#' @param references If \code{TRUE}, a list is returned with both the `sf`
#' object (element `habitatquarries`) and a dataframe of bibliographic
#' references (element `extra_references`).
#' @param bibtex If \code{TRUE}, all that happens is bibliographic references
#' being printed to the console, formatted for usage in a BibTeX file (`*.bib`).
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' Depending on the arguments, one of:
#' \itemize{
#' \item a simple feature collection of
#' type \code{POLYGON}, with attribute variables:
#'   \itemize{
#'     \item \code{polygon_id}: a unique number per polygon.
#'     \item \code{unit_id}: a unique number for each quarry unit. Quarry units
#'     consisting of several polygons (= partly outside the Flemish region)
#'     have a number greater than 100.
#'     \item \code{name}: site name.
#'     \item \code{code_orig}: original 'habitattype' code in the raw data
#'     source \code{habitatquarries}.
#'     \item \code{type}: habitat type listed in \code{\link{types}} - in this
#'     case either `8310` or missing (`NA`).
#'     \item \code{extra_reference}: site-specific bibliographic reference(s).
#'     Values refer to rows in the non-spatial dataframe `extra_references`.
#'   }
#' \item \emph{if `references = TRUE`:} a list with both the `sf`
#' object (element `habitatquarries`) and a dataframe of bibliographic
#' references (element `extra_references`).
#' \item \emph{if `bibtex = TRUE`:} `NULL` (invisibly).
#' }
#'
#' @md
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the latest version of
#' # the 'habitatquarries'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' hq <- read_habitatquarries()
#' hq
#' hq2 <- read_habitatquarries(filter_hab = TRUE)
#' hq2
#' hq3 <- read_habitatquarries(references = TRUE)
#' hq3
#' read_habitatquarries(bibtex = TRUE)}
#'
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' is.string
#' @importFrom stringr
#' str_split
#' @importFrom sf
#' read_sf
#' @importFrom rlang .data
#' @importFrom magrittr
#' set_colnames
#' @importFrom dplyr
#' %>%
#' mutate
#' select
#' filter
#' @export
read_habitatquarries <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "10_raw/habitatquarries/habitatquarries.gpkg"),
             filter_hab = FALSE,
             references = FALSE,
             bibtex = FALSE,
             version = "habitatquarries_2020v1"){

        assert_that(file.exists(file))
        assert_that(is.flag(filter_hab), noNA(filter_hab))
        assert_that(is.flag(references), noNA(references))
        assert_that(is.flag(bibtex), noNA(bibtex))
        assert_that(is.string(version))

        if ((references | filter_hab) & bibtex) {
            warning("Will not read spatial layer when bibtex = TRUE. ",
                    "Ignoring other argument(s) that are set to TRUE.")
        }

        if (references | bibtex) {
            extra_references <-
                read_sf(file,
                        layer = "extra_references")
            if (bibtex)
            {
                if (!requireNamespace("bib2df", quietly = TRUE)) {
                    stop("Package \"bib2df\" is needed when bibtex = TRUE. ",
                         "Please install it from GitHub with: ",
                         "remotes::install_github(\"ropensci/bib2df\")",
                         call. = FALSE)
                }
                message("You can copy below output into a *.bib file ",
                        "for further use.\n")
                extra_references %>%
                    mutate(author = str_split(.data$author, " and ")) %>%
                    set_colnames(toupper(colnames(.))) %>%
                    bib2df::df2bib()
                return(invisible(NULL))
            }
        }

        typelevels <-
            read_types() %>%
            .$type %>%
            levels

        habitatquarries <-
            suppressWarnings(
                read_sf(file,
                        layer = "habitatquarries",
                        crs = 31370)
            ) %>%
            mutate(
                type = ifelse(.data$habitattype == "8310",
                              "8310",
                              NA_character_) %>%
                    factor(levels = typelevels),
                extra_reference = factor(.data$extra_reference)
            ) %>%
            {if (filter_hab) filter(., !is.na(.$type)) else .} %>%
            select(.data$polygon_id,
                   .data$unit_id,
                   .data$name,
                   code_orig = .data$habitattype,
                   .data$type,
                   .data$extra_reference)

if (references) {
    habitatquarries <- list(habitatquarries = habitatquarries,
                            extra_references = extra_references)
}

        return(habitatquarries)

    }


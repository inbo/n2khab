#' Return the data source \code{habitatmap_stdized} as a list of two
#' objects
#'
#' \code{read_habitatmap_stdized} returns the data source \code{habitatmap_stdized} as a list of two objects:
#' \itemize{
#'   \item \code{habitatmap_polygons}: an sf object with all polygons
#'   of the \code{habitatmap} that contain habitat or a regionally
#'   important biotope (RIB).
#'   \item \code{habitatmap_patches}: a tibble with information on the
#'   habitat and RIB patches (HAB1, HAB2,..., HAB5) that occur within
#'   the \code{habitatmap_polygons}, each row corresponding with one
#'   patch.
#'   }
#'
#' The data source \code{habitatmap_stdized} is the processed version
#' of the raw data source \code{habitatmap} (De Saeger et al., 2018).
#' Every polygon in the \code{habitatmap} can consist of maximum 5
#' different vegetation types. This information is stored in the
#' columns 'HAB1', HAB2',..., 'HAB5' of the attribute table. The
#' fraction of each vegetation type within the polygons is stored in
#' the columns 'PHAB1', 'PHAB2', ..., 'PHAB5'. The different parts of
#' the polygons are called 'patches'.
#'
#' The data source \code{habitatmap_stdized} is a GeoPackage that
#' contains:
#' \itemize{
#'   \item \code{habitatmap_polygons}: a spatial layer with every
#'   \code{habitatmap} polygon that contains a habitat or RIB type
#'   listed in \code{\link{types}}.
#'   \item \code{habitatmap_patches}: a table in which every row
#'   corresponds with one patch.
#'   }
#'
#' The processing of the \code{habitatmap_patches} tibble included
#' following steps:
#' \itemize{
#'   \item A new variable was created to provide the
#'   patch number (1 to 5): 'patch_id'.
#'   \item For some patches the vegetation type is uncertain, and the
#'   vegetation code in the raw \code{habitatmap} data source consists
#'   of 2 or 3 possible types, separated with a ','. The different
#'   possible vegetation types are split up and one row is created for
#'   each of them. The variable \code{certain} will be \code{FALSE} if
#'   a patch consists of 2 or 3 possible vegetation types, and \code{TRUE}
#'   if only one vegetation type is provided.
#'   \item For some patches the original vegetation code in the
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
#' @param path Location of the file.
#' Considering the default value of the \code{file} argument, this should be
#' the location of the folder '\strong{\code{n2khab_data}}'.
#' By default, the first \code{n2khab_data} folder is used that is found when
#' sequentially climbing up 0 to 10 levels in the file system hierarchy,
#' starting from the working directory.
#' @param file The filename of the data source.
#' May include a path prefix.
#' The default follows the data management advice in the
#' \href{../doc/v020_datastorage.html}{vignette} on data storage.
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
#'   \item \code{habitatmap_patches}: a tibble with following variables
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{patch_id}
#'     \item \code{code_orig}: original vegetation code in raw \code{habitatmap}.
#'     \item \code{phab}: proportion of polygon covered by patch, as a percentage.
#'     \item \code{certain}: \code{TRUE} when vegetation type is certain and
#'      \code{FALSE} when vegetation type is uncertain.
#'     \item \code{type}: habitat or RIB type listed in \code{\link{types}}.
#'     }
#'     }
#'
#' @family functions involved in processing the \code{habitatmap} data source
#'
#' @references
#'
#' De Saeger S., Guelinckx R., Oosterlynck P., De Bruyn A., Debusschere K., Dhaluin P.,
#' Erens R., Hendrickx P., Hendrix R., Hennebel D., et al. (2018). Biologische
#' Waarderingskaart en Natura 2000 Habitatkaart: Uitgave 2018. Rapporten van het
#' Instituut voor Natuur- en Bosonderzoek. Instituut voor Natuur- en Bosonderzoek (INBO).
#' DOI: https://doi.org/10.21436/inbor.15138099.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the 'habitatmap_stdized'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' r <- read_habitatmap_stdized()
#' r_polygons <- r$habitatmap_polygons
#' r_patches <- r$habitatmap_patches
#' }
#'
#' @export
#' @importFrom sf
#' st_read
#' st_crs<-
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#'
read_habitatmap_stdized <-
    function(path = fileman_up("n2khab_data"),
             file = "20_processed/habitatmap_stdized/habitatmap_stdized.gpkg"){

        habmap_polygons <- st_read(file.path(path, file),
                                   "habitatmap_polygons",
                                   quiet = TRUE,
                                   as_tibble = TRUE)

        habmap_polygons <- habmap_polygons %>%
            mutate( description_orig = as.character( .data$description_orig))

        suppressWarnings(st_crs(habmap_polygons) <- 31370)

        habmap_patches <- suppressWarnings(
            st_read(file.path(path, file),
                    "habitatmap_patches",
                    as_tibble = TRUE,
                    quiet = TRUE)
            )

        types <- suppressWarnings(read_types())

        habmap_patches <- habmap_patches %>%
            mutate( polygon_id = factor(.data$polygon_id,
                                        levels = levels(habmap_polygons$polygon_id)),
                    patch_id = as.numeric(.data$patch_id),
                    certain = .data$certain == 1,
                    type = factor(.data$type,
                                  levels = levels(types$type)
                                  )
                    )

        result <- list(habitatmap_polygons = habmap_polygons,
                       habitatmap_patches = habmap_patches)

        return(result)

    }





#' Return the data source \code{watersurfaces_hab} as a list of two
#' objects
#'
#' \code{read_watersurfaces_hab} returns the data source \code{watersurfaces_hab} as a list of two objects:
#' \itemize{
#'   \item \code{watersurfaces_hab_polygons}: an sf object with all polygons
#'   that contain aquatic habitat or regionally
#'   important biotope (RIB).
#'   \item \code{watersurfaces_hab_patches}: a tibble with information on the
#'   aquatic habitat and RIB patches (HAB1, HAB2,..., HAB5) that occur within
#'   the \code{watersurfaces_hab_polygons}, each row corresponding with one
#'   patch.
#'   }
#'
#' The data source \code{watersurfaces_hab} is a combination of \code{habitatmap_stdized} (see
#' \code{\link{read_habitatmap_stdized}}) and the \href{https://http://www.geopunt.be/catalogus/datasetfolder/10e87ad3-8235-40e0-8269-42c3c96a884d}{watersurface map of Flanders}.
#' It contains all aquatic habitat types and RIB in Flanders,
#' but excluding habitat 3260.
#'
#'
#' The data source \code{watersurfaces_hab} is a GeoPackage that
#' contains:
#' \itemize{
#'   \item \code{watersurfaces_hab_polygons}: a spatial layer with all polygons that contain aquatic habitat
#'   or RIB type listed in \code{\link{types}}, except type 3260.
#'   \item \code{watersurfaces_hab_patches}: a table in which every row corresponds with one patch.
#'   }
#'
#'The polygons with 2190_a habitat (dune slack ponds) are generated by selecting all watersurface polygons that
#'overlap with dune habitat polygons (21xx) of the standardized habitat map.
#'
#'For each of the other considered habitat types (31xx, 7220 and rbbah) we select the watersurface polygons that
#'overlap with the selected habitat type polygons of the standardized habitat map. We also select polygons of the
#'standardized habitat map that contain these aquatic habitat types but do not overlap with any watersurface polygon of the
#'watersurface map.
#'
#'The R-code for creating the \code{watersurfaces_hab_polygons} data source can be found in the \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing}
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
#'   \item \code{watersurfaces_hab_polygons}: an sf object of aquatic habitat and RIB polygons with four attribute variables:
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{polygon_id_ws}: id for the polygon in the \code{watersurface map}
#'     \item \code{polygon_id_habitatmap}: id's of all overlapping polygons of \code{habitatmap_stdized} that
#'     contain aquatic habitat. The different id's are separated by '+'.
#'     \item \code{description_orig}: descriptions of all overlapping polygons of \code{habitatmap_stdized} that
#'     contain standing water habitat. The different descriptions are separated by '+'.}
#'   }
#'   \itemize{
#'   \item \code{habitatmap_patches}: a tibble with following variables:
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{patch_id}
#'     \item \code{code_orig}: original vegetation code in raw \code{habitatmap}.
#'     \item \code{certain}: \code{TRUE} when vegetation type is certain and
#'      \code{FALSE} when vegetation type is uncertain.
#'     \item \code{type}: habitat or RIB type listed in \code{\link{types}}.
#'     }
#'     }
#'
#' @family functions involved in processing the \code{habitatmap} data source
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the 'watersurfaces_hab'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' r <- read_watersurfaces_hab()
#' r_polygons <- r$watersurfaces_hab_polygons
#' r_patches <- r$watersurfaces_hab_patches
#' }
#'
#' @export
#' @importFrom sf
#' st_read
#' st_crs<-
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#'
read_watersurfaces_hab <-
    function(path = fileman_up("n2khab_data"),
             file = "20_processed/watersurfaces_hab/watersurfaces_hab.gpkg",
             interpreted = FALSE){

        watersurfaces_polygons <- st_read(file.path(path, file),
                                   "watersurfaces_hab_polygons",
                                   quiet = TRUE,
                                   as_tibble = TRUE)

        watersurfaces_polygons <- watersurfaces_polygons %>%
            mutate( description_orig = as.character( .data$description_orig))

        suppressWarnings(st_crs(watersurfaces_polygons) <- 31370)

        watersurfaces_patches <- suppressWarnings(
            st_read(file.path(path, file),
                    "watersurfaces_hab_patches",
                    as_tibble = TRUE,
                    quiet = TRUE)
            )

        if (interpreted){
          watersurfaces_patches <- watersurfaces_patches %>%
              mutate(type = ifelse(.data$type == "3130", "3130_aom", .data$type))
        }

        types <- suppressWarnings(read_types())

        watersurfaces_patches <- watersurfaces_patches %>%
            mutate( polygon_id = factor(.data$polygon_id, levels = levels(watersurfaces_polygons$polygon_id)),
                    patch_id = as.numeric(.data$patch_id),
                    certain = .data$certain == 1,
                    type = factor(.data$type,
                                  levels = levels(types$type)
                                  )
                    )

        result <- list(watersurfaces_polygons = watersurfaces_polygons,
                       watersurfaces_patches = watersurfaces_patches)

        return(result)

    }



#' Return the data source \code{habitatmap}
#'
#' \code{read_habitatmap} returns the \code{habitatmap} (De Saeger et al., 2018).
#'
#' @param select_hab If \code{TRUE} only polygons that (partially) contain habitat or a regionally
#' important biotope (RIB) are returned. The default value is \code{FALSE}.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' An sf object
#'
#' @family functions involved in processing the \code{habitatmap} data source
#'
#' @references
#'
#' De Saeger S., Guelinckx R., Oosterlynck P., De Bruyn A., Debusschere K., Dhaluin P.,
#' Erens R., Hendrickx P., Hendrix R., Hennebel D., et al. (2018). Biologische
#' Waarderingskaart en Natura 2000 Habitatkaart: Uitgave 2018. Rapporten van het
#' Instituut voor Natuur- en Bosonderzoek. Instituut voor Natuur- en Bosonderzoek (INBO).
#' DOI: https://doi.org/10.21436/inbor.15138099.
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the 'habitatmapd'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' r <- read_habitatmap()
#' }
#'
#' @export
#' @importFrom sf
#' st_read
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
    function(path = fileman_up("n2khab_data"),
             file = "10_raw/habitatmap",
             select_hab = FALSE){

        habitatmap <- read_sf(file.path(path, file),
                                   "habitatmap",
                                   quiet = TRUE,
                              stringsAsFactors = FALSE)

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

        if(select_hab){

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


#' Return the data source \code{habitatmap_stdized} as a list of two
#' objects
#'
#' \code{read_habitatmap_stdized} returns the data source \code{habitatmap_stdized} as a list of two objects:
#' \itemize{
#'   \item \code{habitatmap_polygons}: an sf object with all polygons
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
#' r_types <- r$habitatmap_types
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
    function(path = fileman_up("n2khab_data"),
             file = "20_processed/habitatmap_stdized/habitatmap_stdized.gpkg",
             version = "habitatmap_stdized_2018_v2"){

        assert_that(is.string(version))

        habmap_polygons <- read_sf(file.path(path, file),
                                   "habitatmap_polygons")

        habmap_polygons <- habmap_polygons %>%
            mutate(polygon_id = factor(.data$polygon_id))

        suppressWarnings(st_crs(habmap_polygons) <- 31370)

        if (version == "habitatmap_stdized_2018_v1") {
            habmap_types <- suppressWarnings(
                read_sf(file.path(path, file),
                        "habitatmap_patches")
            )
        } else {
            habmap_types <- suppressWarnings(
                read_sf(file.path(path, file),
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
#'   \item \code{watersurfaces_hab_polygons}: an sf object with all polygons
#'   that contain standing water types (habitat or RIB).
#'   \item \code{watersurfaces_hab_types}: a tibble with information on the
#'   standing water types (HAB1, HAB2,..., HAB5) that occur within
#'   each polygon of \code{watersurfaces_hab_polygons}.
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
#'   \item \code{watersurfaces_hab_polygons}: an sf object of standing water polygons with four attribute variables:
#'   \itemize{
#'     \item \code{polygon_id}
#'     \item \code{polygon_id_ws}: id for the polygon in the \code{watersurface map}
#'     \item \code{polygon_id_habitatmap}: id's of all overlapping polygons of \code{habitatmap_stdized} that
#'     contain standing water habitat. The different id's are separated by '+'.
#'     \item \code{description_orig}: descriptions of all overlapping polygons of \code{habitatmap_stdized} that
#'     contain standing water habitat. The different descriptions are separated by '+'.}
#'   }
#'   \itemize{
#'   \item \code{watersurfaces_hab_types}: a tibble with following variables:
#'   \itemize{
#'     \item \code{polygon_id}
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
#' r_types <- r$watersurfaces_hab_types
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
    function(path = fileman_up("n2khab_data"),
             file = "20_processed/watersurfaces_hab/watersurfaces_hab.gpkg",
             interpreted = FALSE,
             version = "watersurfaces_hab_v3"){

        assert_that(is.string(version))

        watersurfaces_polygons <- read_sf(file.path(path, file),
                                   "watersurfaces_hab_polygons")

        watersurfaces_polygons <- watersurfaces_polygons %>%
            mutate_at(.vars = vars(starts_with("polygon_id")),
                      .funs = factor)

        suppressWarnings(st_crs(watersurfaces_polygons) <- 31370)

        if (version %in% c("watersurfaces_hab_v1", "watersurfaces_hab_v2")) {
            watersurfaces_types <- suppressWarnings(
                read_sf(file.path(path, file),
                        "watersurfaces_hab_patches")
            )
        } else {
            watersurfaces_types <- suppressWarnings(
                read_sf(file.path(path, file),
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
#' # levels above has the 'n2khab_data' folder AND that the 'habitatmap'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' r <- read_habitatmap()
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
#' select
#' filter
#' starts_with
#'
read_habitatmap <-
    function(path = fileman_up("n2khab_data"),
             file = "10_raw/habitatmap",
             select_hab = FALSE){

        habitatmap <- read_sf(file.path(path, file),
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










#' Return the data source \code{habitatmap_terr} as a list of two
#' objects
#'
#' \code{read_habitatmap_terr()} returns the data source \code{habitatmap_terr}
#' as a list of two objects: \code{habitatmap_terr_polygons} and
#' \code{habitatmap_terr_types}.
#' \code{habitatmap_terr} is the further interpreted, terrestrial part of
#' \code{habitatmap_stdized} (see \code{\link{read_habitatmap_stdized}}).
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
#' The aquatic types are the types for which \code{tag_2 == "HC3"}
#' in the \code{\link{types}} data source (\code{tag_2} is the hydrological
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
#'
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
#' @family functions involved in processing the \code{habitatmap} data source
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the 'habitatmap_terr'
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
    function(path = fileman_up("n2khab_data"),
             file = "20_processed/habitatmap_terr/habitatmap_terr.gpkg",
             keep_aq_types = TRUE,
             version = "habitatmap_terr_2018_v2"){

        assert_that(is.flag(keep_aq_types))
        assert_that(is.string(version))

        habmap_terr_polygons <- read_sf(file.path(path, file),
                                   "habitatmap_terr_polygons")

        habmap_terr_polygons <- habmap_terr_polygons %>%
            mutate(polygon_id = factor(.data$polygon_id),
                   source = factor(.data$source))

        suppressWarnings(st_crs(habmap_terr_polygons) <- 31370)

        if (version == "habitatmap_terr_2018_v1") {
            habmap_terr_types <- suppressWarnings(
                read_sf(file.path(path, file),
                        "habitatmap_terr_patches")
            )
        } else {
            habmap_terr_types <- suppressWarnings(
                read_sf(file.path(path, file),
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
                                          filter(.data$tag_2 == "HC3") %>%
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
#' Returns the raw data source \code{habitatstreams} as an \code{sf} linestring
#' layer or as a list of two objects: the \code{sf} object plus a data frame
#' with textual explanation of the values of the \code{source_id} variable.
#'
#' @param source_text Logical, defaults to \code{FALSE}.
#' If \code{TRUE}, the list version is returned (see \emph{Value}).
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
#' Leyssen A., Denys L. De Saeger S. (2018). Indicatieve situering van het
#' Natura 2000 habitattype 3260. Submontane en laaglandrivieren met vegetaties
#' behorend tot het \emph{Ranunculion fluitantis} en het
#' \emph{Callitricho-Batrachion}.
#' Uitgave 2018 (versie 1.6). Rapporten van het Instituut voor Natuur- en
#' Bosonderzoek 2018 (72). Research Institute for Nature and Forest, Brussels.
#' DOI: https://doi.org/10.21436/inbor.15138370
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the 'habitatstreams'
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
#' @export
read_habitatstreams <-
    function(path = fileman_up("n2khab_data"),
             file = "10_raw/habitatstreams",
             source_text = FALSE){

        filepath <- file.path(path, file)
        assert_that(file.exists(filepath))

        assert_that(is.flag(source_text))

        habitatstreams <-
            suppressWarnings(
                read_sf(filepath,
                        crs = 31370)
            )

        lines <-
            habitatstreams %>%
            select(river_name = .data$NAAM,
                   source_id = .data$BRON) %>%
            mutate(river_name = factor(.data$river_name),
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
#' layer, in the Belgian Lambert 72 CRS (EPSG-code
#' \href{https://epsg.io/31370}{31370}).
#'
#' The data source \code{habitatsprings} is a GeoJSON file (conforming to
#' the RFC7946 specifications), available at
#' \href{https://doi.org/10.5281/zenodo.3550994}{Zenodo}.
#' It represents sites that correspond with presence or absence
#' of the Natura 2000 habitat type \code{7220} (Petrifying springs with tufa
#' formation (\emph{Cratoneurion})) in springs and streaming water segments in
#' the Flemish Region, Belgium.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of
#' type \code{POINT}, with attribute variables:
#'   \itemize{
#'     \item \code{point_id}
#'     \item \code{name}: site name.
#'     \item \code{code_orig}: original vegetation code in raw
#'     \code{habitatsprings}.
#'     \item \code{type}: habitat type listed in \code{\link{types}}.
#'     \item \code{certain}: \code{TRUE} when vegetation type is certain and
#'      \code{FALSE} when vegetation type is uncertain.
#'     \item \code{area_m2}: area as square meters.
#'     \item \code{year}: year of field inventory.
#'     \item \code{in_sac}: logical.
#'     Is the site situated within a Special Area of Conservation?
#'     \item \code{source}: original data source of the record.

#'   }
#'
#'
#' @examples
#' \dontrun{
#' # This example supposes that your working directory or a directory up to 10
#' # levels above has the 'n2khab_data' folder AND that the 'habitatsprings'
#' # data source is present in the default subdirectory.
#' # In all other cases, this example won't work but at least you can
#' # consider what to do.
#'
#' library(magrittr)
#' library(sf)
#' hs <- read_habitatsprings()
#' hs
#' }
#'
#' @importFrom assertthat
#' assert_that
#' @importFrom stringr
#' str_sub
#' @importFrom sf
#' read_sf
#' st_transform
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' mutate
#' select
#' @export
read_habitatsprings <-
    function(path = fileman_up("n2khab_data"),
             file = "10_raw/habitatsprings/habitatsprings.geojson"){

        filepath <- file.path(path, file)
        assert_that(file.exists(filepath))

        habitatsprings <-
            read_sf(filepath) %>%
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
                        factor(levels = read_types() %>%
                                   .$type %>%
                                   levels),
                certain = (.data$validity_status == "gecontroleerd")
            ) %>%
            select(point_id = .data$id,
                   .data$name,
                   code_orig = .data$habitattype,
                   .data$type,
                   .data$certain,
                   .data$area_m2,
                   .data$year,
                   .data$in_sac,
                   .data$source)

        return(habitatsprings)

    }


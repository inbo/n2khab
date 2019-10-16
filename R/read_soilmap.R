#' Return the \code{soilmap} or \code{soilmap_simple} data source as an
#' \code{sf} object
#'
#' Returns either the raw data source \code{soilmap} or (by default) the
#' processed data source \code{soilmap_simple}.
#'
#' In case the raw data source \code{soilmap} is used, it is possible to
#' manually perform the standardization (for polders) and/or the simplification,
#' which were both applied in the \code{soilmap_simple} data source.
#'
#' See R-code in the \href{https://github.com/inbo/n2khab-preprocessing}{
#' n2khab-preprocessing} repository for the creation of the
#' \code{soilmap_simple} data source from
#' the \code{soilmap} data source.
#'
#' TO BE ADDED: explanation on the variables in the result.
#'
#'
#' @param use_processed Load and return the processed data source
#' \code{soilmap_simple}, instead of the raw data source \code{soilmap}.
#' @param standardize_polders Logical.
#' Only applied with \code{use_processed = FALSE}.
#' If \code{TRUE}, fill empty values of substrate,
#' texture and moisture in the 'Polders' area with translated values from
#' De Vos et al XXXX.
#' Also, use the value of \code{soiltype_unified} provided by this source.
#' @param simplify Logical.
#' Only applied with \code{use_processed = FALSE}.
#' If \code{TRUE}, only return a limited number of variables.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of geometry type \code{MULTIPOLYGON}.
#'
#' @importFrom assertthat
#' assert_that
#' is.flag
#' @importFrom sf
#' read_sf
#' @importFrom git2rdata
#' read_vc
#' @importFrom dplyr
#' %>%
#' select
#' mutate
#' mutate_at
#' left_join
#' vars
#' contains
#' @importFrom rlang .data
#' @export
read_soilmap <-
    function(path = fileman_up("n2khab_data"),
             file = "10_raw/soilmap",
             use_processed = TRUE,
             standardize_polders = FALSE,
             simplify = FALSE) {

        assert_that(is.flag(simplify))
        assert_that(is.flag(standardize_polders))
        assert_that(is.flag(use_processed))

        soilmap_path <- file.path(path, file)
        assert_that(file.exists(soilmap_path))

        suppressWarnings(
            soilmap <- read_sf(soilmap_path,
                               crs = 31370)
        )

        soilmap <-
            soilmap %>%
            select(polygon_id = .data$gid,
                   map_id = .data$Kaartbldnr,
                   region = .data$Streek,
                   in_polders = .data$Type_class,
                   soiltype_general = .data$Grove_leg,
                   soiltype_legend_title = .data$Uitleg_tit,
                   soiltype_legend_explan = .data$Uitleg,
                   soiltype_id = .data$codeid,
                   soiltype = .data$Bodemtype,
                   soiltype_unified = .data$Unitype,
                   soiltype_region = .data$Bodtypstr,
                   soil_series = .data$Bodemser_c,
                   soil_series_text = .data$Bodemserie,
                   substrate = .data$Substr_V_c,
                   substrate_text = .data$SubstraatV,
                   texture = .data$Textuur_c,
                   texture_text = .data$Textuur,
                   moisture = .data$Drainage_c,
                   moisture_text = .data$Drainage,
                   profile = .data$Profontw_c,
                   profile_text = .data$Profontw,
                   traits_code = .data$Fase_c,
                   mother_material = .data$Varimoma_c,
                   mother_material_text = .data$Varimoma,
                   profile_variant = .data$Variprof_c,
                   profile_variant_text = .data$Variprof,
                   polders_substrate = .data$Substr_p_c,
                   polders_substrate_text = .data$Substr_pol,
                   polders_series = .data$Serie_c,
                   polders_series_text = .data$Serie,
                   polders_subseries = .data$Subserie_c,
                   polders_subseries_text = .data$Subserie,
                   scanned_file_map = .data$Scan_kbl,
                   scanned_file_book = .data$Scan_boek,
                   scanned_file_detailedmap = .data$Scan_5000,
                   scanned_file_points = .data$Scan_stip
                   ) %>%
            mutate(in_polders = .data$in_polders == "Zeepolders") %>%
            mutate_at(.vars = vars(-.data$polygon_id,
                                   -.data$in_polders,
                                   -.data$soiltype_id,
                                   -.data$traits_code,
                                   -.data$geometry),
                      .funs = factor)

        if (standardize_polders) {
            transl <- read_vc(file = "soil_translation_polders",
                              root = pkgdatasource_path(
                                  "textdata/soil_translation_polders", ".tsv")) %>%
                mutate(soiltype_orig = factor(.data$soiltype_orig,
                                              levels = levels(soilmap$soiltype))
                       ) %>%
                filter(!is.na(texture_transl))

            soilmap <-
                soilmap %>%
                left_join(transl, by = c("soiltype" = "soiltype_orig")) %>%
                mutate(substrate = as.character(.data$substrate),
                       texture = as.character(.data$texture),
                       moisture = as.character(.data$moisture),
                       substrate = ifelse(is.na(.data$substrate) &
                                              !is.na(.data$polders_substrate),
                                          as.character(.data$polders_substrate),
                                          .data$substrate) %>%
                           factor(levels = levels(soilmap$substrate)),
                       texture = ifelse(is.na(.data$texture) &
                                            !is.na(.data$texture_transl),
                                        .data$texture_transl,
                                        .data$texture) %>%
                           factor(levels = levels(soilmap$texture)),
                       moisture = ifelse(is.na(.data$moisture) &
                                             !is.na(.data$moisture_transl),
                                         .data$moisture_transl,
                                         .data$moisture) %>%
                           factor(levels = levels(soilmap$moisture))
                       ) %>%
                select(-contains("transl"))

        }

        if (simplify) {
            soilmap <-
                soilmap %>%
                select(.data$polygon_id,
                       .data$region,
                       .data$in_polders,
                       .data$soiltype_unified,
                       .data$substrate,
                       .data$texture,
                       .data$moisture,
                       .data$profile,
                       .data$mother_material,
                       .data$profile_variant
                       )
        }

        return(soilmap)

    }

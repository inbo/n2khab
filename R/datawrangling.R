#' Expand a 'type' column in a dataframe
#'
#' Takes a dataframe with a column of type codes
#' (\emph{main type} or \emph{subtype }codes),
#' and, under certain conditions, adds new rows with codes of the associated
#' \emph{subtypes} and \emph{main types}, respectively.
#' It allows to do sensible selections and joins with interpreted forms of the
#' \code{habitatmap} data source at version \code{habitatmap_2018}:
#' \code{habitatmap_terr},
#' \code{watersurfaces_interpr},
#' \code{habitatmap_integr}
#' .
#' If the dataframe has one or more grouping variables, by default the
#' operation is done independently for each group in turn.
#'
#' The extra rows in the dataframe take the values for other variables
#' from the rows with which they are associated, based on the
#' subtype - main type relation.
#' Type codes in the dataframe are verified to comply with the codes from the
#' \code{\link{types}} data source.
#' A warning is given when they don't.
#'
#' Main type codes are always expanded with the subtype codes that belong to it.
#'
#' The applied approach to add main type codes only makes sense
#' assuming that the result is to be confronted
#' with one of the \emph{above listed} geospatial data sources.
#'
#' In order to add main type codes based on
#' subtype codes that are present in the type column, specific conditions have
#' to be met:
#' \itemize{
#' \item{for 2330: both subtype codes must be present}
#' \item{for 6230: 6230_ha, 6230_hmo and 6230_hnk must be present
#' (not the rare 6230_hnk)}
#' \item{for 91E0: 91E0_va, 91E0_vm and 91E0_vn must be present
#' (not the rarer 91E0_sf, 91E0_vc and 91E0_vo)}
#' }
#' However, it is possible to relax this requirement by setting
#' \code{strict = FALSE}.
#' This will add the main type code whenever \emph{one} corresponding subtype
#' code is present.
#' In all cases no other main type codes are added apart from
#' 2330, 6230 and 91E0.
#' This is because the data sources with which the result
#' is to be matched (see Description) don't contain certain main type codes,
#' and because it makes no sense in other cases
#' (rbbkam, rbbvos, rbbzil & 9120 in the \code{habitatmap} do not refer to a
#' main type but to an non-defined subtype with no specific code).
#'
#'
#' @param x An object of class \code{data.frame}.
#' @param type_var A string.
#' The name of the dataframe variable that holds the type codes.
#' Defaults to \code{type}.
#' @param use_grouping Logical.
#' If the dataframe has one or more grouping variables
#' (class \code{grouped_df}),
#' is the operation to be performed independently
#' for each group in turn?
#' @param strict Logical.
#' Apply conditions before expanding subtype codes to main type codes?
#'
#' @return
#' A dataframe, either identical or longer than the input dataframe.
#'
#' @seealso
#' \code{\link{read_scheme_types}},
#' \code{\link{read_types}},
#' \code{read_habitatmap_terr},
#' \code{read_watersurfaces_interpr},
#' \code{read_habitatmap_integr}
#'
#' @examples
#' library(dplyr)
#' x <-
#'     read_scheme_types() %>%
#'     filter(scheme == "GW_05.1_terr")
#' expand_types(x)
#' expand_types(x, strict = FALSE)
#'
#' x <-
#'     read_scheme_types() %>%
#'     filter(scheme == "GW_05.1_terr") %>%
#'     group_by(typegroup)
#' expand_types(x)
#' expand_types(x, use_grouping = FALSE) # equals above example
#'
#' x <-
#'     tribble(
#'         ~mycode, ~obs,
#'         "2130", 5,
#'         "2190", 45,
#'         "2330_bu", 8,
#'         "2330_dw", 8,
#'         "6410_mo", 78,
#'         "6410_ve", 4,
#'         "91E0_vn", 10
#'     )
#' expand_types(x, type_var = "mycode")
#' expand_types(x, type_var = "mycode", strict = FALSE)
#'
#' @importFrom assertthat
#' assert_that
#' is.string
#' is.flag
#' @importFrom tidyr
#' nest
#' unnest
#' @importFrom purrr
#' map
#' @importFrom dplyr
#' %>%
#' mutate
#' select
#' group_by_at
#' group_vars
#' @importFrom rlang .data
#' @export
expand_types <- function(x,
                         type_var = "type",
                         use_grouping = TRUE,
                         strict = TRUE) {

    assert_that(inherits(x, "data.frame"))
    assert_that(is.string(type_var))
    assert_that(type_var %in% colnames(x),
                msg = "type_var must be a variable name in x.")
    assert_that(is.flag(use_grouping))
    assert_that(is.flag(strict))

    if (!use_grouping) {

        expand_types_plain(x = x,
                           type_var = type_var,
                           strict = strict)

        } else {

    x %>%
        nest() %>%
        mutate(newdata = map(.data$data,
                             expand_types_plain,
                             type_var = type_var,
                             strict = strict)
        ) %>%
        select(-.data$data) %>%
        unnest %>%
        group_by_at(x %>% group_vars()) %>%
        select(colnames(x))

    }

}







#' Translates a factor with levels mapped according to the namelist
#'
#' Translates a factor, by default using the 'name' variable from the
#' 'namelist' object to substitute the factor levels. The join on 'namelist' is
#' by the variable 'code'.
#'
#' @inheritParams expand_types
#'
#' @return A dataframe.
#'
#' @importFrom dplyr
#' %>%
#' left_join
#' select
#' filter
#' rename
#' group_by
#' summarise
#' anti_join
#' pull
#' inner_join
#' bind_rows
#' mutate
#' distinct
#' @importFrom magrittr
#' set_colnames
#' @importFrom rlang .data
#' @keywords internal
expand_types_plain <- function(x,
                         type_var = "type",
                         strict = TRUE) {
    types <-
        read_types() %>%
        select(1:3)

    subtypes <-
        types %>%
        filter(.data$typelevel == "subtype") %>%
        select(1, 3)

    orig_types <-
        x[, type_var] %>%
        rename(orig_abcd = type_var)

    if (!all(unique(orig_types$orig_abcd) %in% types$type)) {
        warning("The dataframe contains type codes which are not standard.")
    }

    # main types to add:
    suppressWarnings(
    join_main_types <-
        subtypes %>%
            filter(.data$main_type == "2330" |
                       .data$type %in% c("6230_ha", "6230_hmo", "6230_hnk",
                                     "91E0_va", "91E0_vm", "91E0_vn")) %>%
        left_join(orig_types %>%
                      mutate(present = 1),
                    by = c("type" = "orig_abcd")) %>%
        group_by(.data$main_type) %>%
        summarise(add = if (strict) all(!is.na(.data$present)) else {
                                    any(!is.na(.data$present))
                                    }
                  ) %>%
        filter(.data$add) %>%
        # only adding codes absent from original dataframe:
        anti_join(orig_types, by = c("main_type" = "orig_abcd")) %>%
        pull(.data$main_type)
    )

    # expanding main types to their subtypes and adding the latter:
    suppressWarnings(
    x_expanded <-
        x %>%
        rename(orig_abcd = type_var) %>%
        inner_join(subtypes %>% rename(type_abcd = .data$type),
                   by = c("orig_abcd" = "main_type")) %>%
        mutate(orig_abcd = .data$type_abcd) %>%
        select(-.data$type_abcd) %>%
        anti_join(x %>%
                      rename(orig_abcd = type_var),
                  by = "orig_abcd") %>%
        set_colnames(gsub("orig_abcd", type_var, colnames(.))) %>%
        bind_rows(x, .)
    )

    # adding main_types:
    suppressWarnings(
    x_expanded <-
        x %>%
        rename(orig_abcd = type_var) %>%
        inner_join(subtypes %>%
                       rename(main_type_abcd = .data$main_type),
                   by = c("orig_abcd" = "type")) %>%
        filter(.data$main_type_abcd %in% join_main_types) %>%
        mutate(orig_abcd = if (is.factor(.data$orig_abcd)) {
                            factor(.data$main_type_abcd,
                                  levels = levels(.data$orig_abcd))
                        } else .data$main_type_abcd
            ) %>%
        select(-.data$main_type_abcd) %>%
        distinct %>%
        set_colnames(gsub("orig_abcd", type_var, colnames(.))) %>%
        bind_rows(x_expanded, .)
    )

    return(x_expanded)

}
#' Read favourable environmental ranges for 'types'
#'
#' A 'type' refers to either a (main) habitat type, a
#' habitat subtype or a regionally important biotope (RIB).
#' The function reads the data into the current R session if `file` exists.
#' If the `file` does not exist, it will attempt to download it and read it
#' after the download has completed.
#' The original source for the data is Van Calster et al. (2020).
#'
#' @inheritParams read_habitatmap_stdized
#' @inheritParams read_types
#'
#' @return A `tibble`.
#'
#' The first row is a column header and represents the variable name.
#' The following list provides a brief description for each variable:
#'
#' `type`: Natura 2000 (sub)type habitat code
#'
#' `subtype`: code/name of possible vegetation variants.
#' For aquatic habitats: this is the watertype as used in the Water Framework
#' Directive and thus also in the Flemish law texts (main use of this field).
#' In other cases: a subtype that is not officially recognized in the list of
#' Natura 2000 habitat subtypes in Flanders, for instance a phytosociological
#' association or a variant like acidophilous or calcarophilous, ...
#'
#' `compartment`: soil, groundwater, inundation water (flooding with
#' water of external origin), water column/surface water, air
#'
#' `variable`: name of the environmental variable.
#'
#' `abbreviation`: abbreviation used for the environmental variable
#'
#' `unit`: unit used for the environmental variable
#'
#' `summary_statistic`: for aquatic types, measurements are first summarized
#'  using certain statistics such as percentiles, mean, etc. (in accordance with
#'  Water Framework Directive-Integrated Water Policy Objectives)
#'
#' `range_type`: range type
#' - LL-UL: lower limit-upper limit
#' - 10-90 perc: 10-90 percentile values
#' - min-max: minimum - maximum
#' - < - >: less than - greater than
#'
#' `environmental_range`: overall measurement range of an environmental variable
#'  within which a habitat type can function sustainably:
#' - `environmental_range`: range as text field
#' - `value_1` - `value_2`: lower and upper limit as text field
#' - `value_num_1` - `value_num_2`: lower and upper limit as numerical value
#'  (where conversion was possible)
#'
#' `n_favourable`: number of test plots in favourable conservation status used
#' for the calculation. The definition of favourable conservation status is
#' based on Oosterlynck et al. (2020, version 3).
#'
#' `status`: source and method by which the range was derived:
#' - Dp: ranges calculated with INBO data, prediction intervals, local
#'   conservation status (LCS) known
#' - Dt: ranges determined using Titan method (Baker & King 2010), LCS unknown
#' - Dk: ranges calculated with INBO data, quantiles, insufficient data to
#'   calculate prediction intervals
#' - Lg: ranges derived from literature (LCS clearly indicated)
#' - Lk: ranges derived from literature (LCS derived, based on site
#'   characteristics)
#'
#' `reference`: references based on which the abiotic range was determined
#'
#' `remarks`: remarks
#'
#' `changes`: whether this record has been modified compared to Van Calster
#' et al. (2020) and how
#'
#' @export
#'
#' @references
#' - Van Calster H., Cools N., De Keersmaeker L., Denys L., Herr C., Leyssen A.,
#' Provoost S., Vanderhaeghe F., Vandevoorde B., Wouters J. en M. Raman (2020).
#' Gunstige abiotische bereiken voor vegetatietypes in Vlaanderen. Rapporten van
#' het Instituut voor Natuur- en Bosonderzoek 2020 (44). Instituut voor Natuur-
#' en Bosonderzoek, Brussel. DOI: doi.org/10.21436/inbor.19362510D
#' - Oosterlynck P., De Saeger S., Leyssen A., Provoost S., Thomaes A.,
#' Vandevoorde B., Wouters J., & Paelinckx D. (2020). Criteria voor de
#' beoordeling van de lokale staat van instandhouding van de Natura2000
#' habitattypen in Vlaanderen. Rapporten van het Instituut voor Natuur- en
#' Bosonderzoek 2020 (27). Instituut voor Natuur- en Bosonderzoek, Brussel.
#' DOI: doi.org/10.21436/inbor.14061248
#'
#' @examples
#' \dontrun{
#' favenv <- read_favenv()
#' }
#'
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr
#' as_tibble
#' filter
#' rename
#' if_all
#' everything
#' @importFrom curl
#' has_internet
read_favenv <- function(
    file = file.path(
      locate_n2khab_data(),
      "10_raw/favenv",
      "VanCalster_etal_2020_Gunstig_abiotisch_bereik_per_milieuvariabele_en_habitatsubtype_v01_00.txt"
    ),
    version = c("favenv_v1.0"),
    lang = c("nl")) {
  require_pkgs(c("fs"))

  version <- match.arg(version)
  lang <- match.arg(lang)

  # if file does not exist, download it from Zenodo
  if (!fs::file_exists(file)) {
    assert_that(
      has_internet(),
      msg =
        paste0(
          "The file does not exists and there is no internet connection.\n",
          "Aborting attempt to dowload the necessary file from Zenodo."
        )
    )
    doi <- switch(
      version,
      `favenv_v1.0` = "10.5281/zenodo.10533792",
      "10.5281/zenodo.10533791" # default resolves to latest
    )
    dir <- fs::path_dir(file)
    fs::dir_create(dir)
    download_zenodo(doi = doi, path = dir, quiet = TRUE)
  }

  if (version == "favenv_v1.0") {
    favenv <- read.delim(
      file = file,
      header = TRUE,
      sep = "\t",
      dec = ",",
      na.strings = c("NA", ""),
      strip.white = TRUE,
      blank.lines.skip = TRUE
    )

    favenv <- favenv |>
      convertdf_enc(from = "latin1", to = "UTF-8") |>
      as_tibble() |>
      rename(
        type = Habitatsubtype,
        subtype = Subtype,
        compartment = Milieucompartiment,
        variable = Variabele,
        abbreviation = Afkorting,
        unit = Eenheid,
        summary_statistic = `Toetswijze...bepaling`,
        range_type = Teken,
        environmental_range = Abiotisch.bereik,
        value_1 = Waarde.1,
        value_2 = Waarde.2,
        value_num_1 = WaardeNum1,
        value_num_2 = WaardeNum2,
        n_favourable = N.gunstig,
        status = Status,
        reference = Referentie,
        remarks = Opmerking,
        changes = Wijziging
      ) |>
      filter(
        !if_all(everything(), is.na)
      )

    # fix numerical values
    # contains mix of . and , as decimal mark
    favenv$value_num_1 <- as.numeric(gsub(",", ".", favenv$value_num_1))
    favenv$value_num_2 <- as.numeric(gsub(",", ".", favenv$value_num_2))

    # align with read_types()
    types <- read_types(lang = lang)
    favenv <- favenv |>
      mutate(
        type = factor(type, levels = levels(types$type))
      )

    # Elektrisch Geleidingsvermogen -> Geleidbaarheid
    favenv$variable[favenv$variable == "Elektrisch Geleidingsvermogen"] <-
      "Geleidbaarheid"
    favenv$abbreviation[favenv$abbreviation == "EGV"] <-
      "EC"


  } else {
    stop(paste0("Reading version ", version, " is not yet implemented."))
  }

  return(favenv)
}


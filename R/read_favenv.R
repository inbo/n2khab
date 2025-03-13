#' Read favourable environmental ranges for vegetation types of Flanders
#'
#' The function reads the data into the current R session if `file` exists.
#' If the `file` does not exist, it will attempt to download it and read it
#' after the download has completed.
#' The original source for the data is the report 10.21436/inbor.19362510.
#'
#' @inheritParams read_habitatmap_stdized
#' @param lang An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{
#'   IETF BCP 47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
#'   the language of names & shortnames to be returned in the tibble.
#'#'
#' @return A `tibble`.
#'
#' The table contains 950 rows and 19 columns.
#' The first row is a column header and represents the variable name.
#' The following list provides a brief description for each variable:
#'
#' `type`: Natura 2000 (sub)type habitat code
#'
#' `subtype`: code/name of possible vegetation variants.
#' For aquatic types, the Flemish water type is listed
#' (according to Water Framework Directive-Integrated Water Policy Objectives)
#'
#' `milieucompartiment`: soil, groundwater, inundation water (flooding with
#' water of external origin), water column/surface water, air
#'
#' `variabele`: name of the environmental variable.
#'
#' `afkorting`: abbreviation used for the environmental variable
#'
#' `eenheid`: unit used for the environmental variable
#'
#' `toetswijze_bepaling`: for aquatic types, measurements are first summarized
#'  using certain statistics such as percentiles, mean, etc. (in accordance with
#'  Water Framework Directive-Integrated Water Policy Objectives)
#'
#' `teken`: range type
#' - LL-UL: lower limit-upper limit
#' - 10-90 perc: 10-90 percentile values
#' - min-max: minimum - maximum
#' - < - >: less than - greater than
#'
#' `abiotisch_bereik`: overall measurement range of an environmental variable
#'  within which a habitat type can function sustainably:
#' - `abiotisch_bereik`: range as text field
#' - `waarde_1` - `waarde_2`: lower and upper limit as text field
#' - `waarde_num_1` - `waarde_num_2`: lower and upper limit as numerical value
#'  (where conversion was possible)
#'
#' `n_gunstig`: number of test plots in favorable conservation status used for
#' the calculation. The definition of favorable conservation status is based on
#' Oosterlynck et al. (2020, version 3).
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
#' `referentie`: references based on which the abiotic range was determined
#'
#' `opmerking`: remarks
#'
#' `wijziging`: whether this record has been modified compared to Van Calster
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
#' @importFrom n2khabmon
#' read_schemes
read_favenv <- function(
    file = file.path(
      locate_n2khab_data(),
      "10_raw/favenv",
      "VanCalster_etal_2020_Gunstig_abiotisch_bereik_per_milieuvariabele_en_habitatsubtype_v01_00.txt"
    ),
    version = c("01.00"),
    lang = c("nl")) {
  require_pkgs(c("fs"))

  version <- match.arg(version)
  lang <- match.arg(lang)

  # if file does not exist, download it from Zenodo
  if (!fs::file_exists(file)) {
    assert_that(
      curl::has_internet(),
      msg =
        paste0(
          "The file does not exists and there is no internet connection.\n",
          "Aborting attempt to dowload the necessary file from Zenodo."
        )
    )
    doi <- switch(
      version,
      `01.00` = "10.5281/zenodo.10533792",
      "10.5281/zenodo.10533791" # default resolves to latest
    )
    dir <- fs::path_dir(file)
    fs::dir_create(dir)
    download_zenodo(doi = doi, path = dir, quiet = TRUE)
  }

  favenv <- read.delim(
    file = file,
    header = TRUE,
    sep = "\t",
    dec = ",",
    na.strings = c("NA", ""),
    blank.lines.skip = TRUE
  )

  favenv <- favenv |>
    convertdf_enc(from = "latin1", to = "UTF-8") |>
    as_tibble() |>
    rename(
      type = Habitatsubtype,
      subtype = Subtype,
      milieucompartiment = Milieucompartiment,
      variabele = Variabele,
      afkorting = Afkorting,
      eenheid = Eenheid,
      toetswijze_bepaling = `Toetswijze...bepaling`,
      teken = Teken,
      abiotisch_bereik = Abiotisch.bereik,
      waarde_1 = Waarde.1,
      waarde_2 = Waarde.2,
      waarde_num_1 = WaardeNum1,
      waarde_num_2 = WaardeNum2,
      n_gunstig = N.gunstig,
      status = Status,
      referentie = Referentie,
      opmerking = Opmerking,
      wijziging = Wijziging
    ) |>
    filter(
      !if_all(everything(), is.na)
    )

  # align with read_types() and read_schemes()
  types <- read_types(lang = lang)
  schemes <- n2khabmon::read_schemes(lang = lang)
  favenv <- favenv |>
    mutate(
      type = factor(type, levels = levels(types$type))
    )

  return(favenv)
}


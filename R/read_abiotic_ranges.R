#' Read favourable abiotic ranges for vegetation types of Flanders
#'
#' The function will download the data from a Zenodo deposit to a temporary
#' folder and read the data back into the current R session.
#' The original source for the data is the report 10.21436/inbor.19362510.
#'
#' @param doi A doi string. The default DOI string will resolve to the latest
#' version of the data in the Zenodo deposit.
#' If you want a specific version, please consult the Zenodo deposits web page
#' to obtain the version-specific DOI.
#'
#' @return A `data.frame`.
#'
#' The table contains 950 rows and 19 columns.
#' The first row is a column header and represents the variable name.
#' The following list provides a brief description for each variable:
#'
#' `Habitatsubtype`: Natura 2000 (sub)type habitat code
#'
#' `Subtype`: code/name of possible vegetation variants.
#' For aquatic types, the Flemish water type is listed
#' (according to Water Framework Directive-Integrated Water Policy Objectives)
#'
#' `Milieucompartiment`: soil, groundwater, inundation water (flooding with
#' water of external origin), water column/surface water, air
#'
#' `Variabele`: name of the environmental variable.
#'
#' `Afkorting`: abbreviation used for the environmental variable
#'
#' `Eenheid`: unit used for the environmental variable
#'
#' `Toetswijze bepaling`: for aquatic types, measurements are first summarized
#'  using certain statistics such as percentiles, mean, etc. (in accordance with
#'  Water Framework Directive-Integrated Water Policy Objectives)
#'
#' `Teken`: range type
#' - LL-UL: lower limit-upper limit
#' - 10-90 perc: 10-90 percentile values
#' - min-max: minimum - maximum
#' - < - >: less than - greater than
#'
#' `Abiotisch bereik`: overall measurement range of an environmental variable
#'  within which a habitat type can function sustainably:
#' - `Abiotisch bereik`: range as text field
#' - `Waarde1` - `Waarde2`: lower and upper limit as text field
#' - `WaardeNum1` - `WaardeNum2`: lower and upper limit as numerical value
#'  (where conversion was possible)
#'
#' `N gunstig`: number of test plots in favorable conservation status used for
#' the calculation. The definition of favorable conservation status is based on
#' Oosterlynck et al. (2020, version 3).
#'
#' `Status`: source and method by which the range was derived:
#' - Dp: ranges calculated with INBO data, prediction intervals, local
#'   conservation status (LCS) known
#' - Dt: ranges determined using Titan method (Baker & King 2010), LCS unknown
#' - Dk: ranges calculated with INBO data, quantiles, insufficient data to
#'   calculate prediction intervals
#' - Lg: ranges derived from literature (LCS clearly indicated)
#' - Lk: ranges derived from literature (LCS derived, based on site
#'   characteristics)
#'
#' `Referentie`: references based on which the abiotic range was determined
#'
#' `Opmerking`: remarks
#'
#' `Wijziging`: whether this record has been modified compared to Van Calster
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
#' far <- read_abiotic_ranges()
#' }
#'
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr
#' as_tibble
#' @importFrom fs
#' is_file
#' file_exists
#' dir_create
read_abiotic_ranges <- function(
    file = file.path(
      locate_n2khab_data(),
      "10_raw/favenv",
      "VanCalster_etal_2020_Gunstig_abiotisch_bereik_per_milieuvariabele_en_habitatsubtype_v01_00.txt"
    ),
    version = c("01.00")) {

  version <- match.arg(version)

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
    file = file, header = TRUE, sep = "\t", dec = ",", na.strings = c("NA", "")
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
    )

  return(favenv)
}


library(git2rdata)
library(dplyr)
library(readxl)
library(stringr)
library(googledrive)

file_path <- file.path(tempdir(), "transl.xlsx")
drive_download(as_id("1QpVj2vGw_jPqqVFfDARQ-pQgUrlDqIxA"),
               path = file_path,
               overwrite = TRUE)

transl <-
    read_excel(file_path) %>%
    select(
        soiltype_orig = CODEIN,
        soiltype_unified_transl = CODEOUT,
        texture_transl = hoofdtextuur,
        moisture_transl = vochttrap
    ) %>%
    # solving a mistake:
    mutate(soiltype_orig = str_replace(soiltype_orig, "OV", "OU")) %>%
    # rejecting translations for OB and others for which texture &
    # moisture should remain NA:
    filter(str_length(texture_transl) < 2)

transl %>%
    write_vc("inst/textdata/soil_translation_polders",
             sorting = c("soiltype_orig"))


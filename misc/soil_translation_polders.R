## This script allows to reproduce the vc-formatted data source
## 'soil_translation_polders' which is delivered with the
## n2khab package and used internally by the read_soilmap() function,
## in case it has the arguments use_processed = FALSE and
## standardize_polders = TRUE

## The script is to be run from the root of the git repository, i.e. this must
## be the working directory.

library(git2rdata)
library(dplyr)
library(readxl)
library(stringr)
library(googledrive)

file_path <- file.path(tempdir(), "transl.xlsx")
drive_download(as_id("1safAsnwQnU_4Gvuf7K2czfrSDpPwE_k5"),
               path = file_path,
               overwrite = TRUE)

transl <-
    read_excel(file_path,
               sheet = 1) %>%
    select(
        soiltype_orig = Code,
        soiltype_unified_transl = Unibodemtype,
        texture_transl = hoofdtextuur,
        drainage_transl = vochttrap
    )

transl %>%
    write_vc("inst/textdata/soil_translation_polders",
             sorting = c("soiltype_orig"),
             strict = FALSE)


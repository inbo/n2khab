# This script allows the conversion of vc-formatted files because of specific
# upgrades of the git2rdata package that change the way files are written.
# Its repository is at https://github.com/inbo/git2rdata

# The used functions run from the root of the repo

getwd()


# 2019-05-16
# -------------------------------------------------------------------
# Previous state of git2rdata: commit 4b1f2fd
# Conversion to new state of git2rdata: commit df1ba32 (tag v0.0.4)

git2rdata::upgrade_data(path = "src/generate_textdata/rawraw_data")
    # rawraw_data/10_compmeetnet_types_milieudrukken.yml updated
    # rawraw_data/10_compmeetnet_types_milieudrukken.yml
    # "rawraw_data/10_compmeetnet_types_milieudrukken"

git2rdata::upgrade_data(path = "inst/textdata")
    # inst/textdata/env_pressures.yml updated
    # inst/textdata/namelist.yml updated
    # inst/textdata/schemes.yml updated
    # inst/textdata/scheme_types.yml updated
    # inst/textdata/types.yml updated
    # inst/textdata/env_pressures.yml
    # "inst/textdata/env_pressures"
    # inst/textdata/namelist.yml
    # "inst/textdata/namelist"
    # inst/textdata/schemes.yml
    # "inst/textdata/schemes"
    # inst/textdata/scheme_types.yml
    # "inst/textdata/scheme_types"
    # inst/textdata/types.yml
    # "inst/textdata/types"


# 2019-06-11
# -------------------------------------------------------------------
# Previous state of git2rdata: commit df1ba32
# Conversion to new state of git2rdata: commit 796ba98 (tag 0.0.5)

git2rdata::upgrade_data(path = "src/generate_textdata/rawraw_data")
git2rdata::upgrade_data(path = "inst/textdata")
git2rdata::upgrade_data(path = "src/generate_habitatmap_stdized/habmap_correction")


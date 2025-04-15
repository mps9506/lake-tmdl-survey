
library(rATTAINS)
library(arrow)
library(fs)
library(dplyr)
library(purrr)
library(tidyr)
actions <- read_feather(fs::path("data", "actions.feather"))

actions

## need to download the assessment unit info

au_ids <- actions |>
  distinct(asessment_unit_identifier)

au_ids <- au_ids |>
  split(au_ids$asessment_unit_identifier) |>
  map(\(au) assessment_units(assessment_unit_identifer = au$"asessment_unit_identifier"),
                    .progress= TRUE)

#au_id_info <- assessment_units(assessment_unit_identifer = au_ids)

flat_list <- purrr::list_rbind(au_ids)

write_feather(flat_list, fs::path("data", "assessment_units.feather"))

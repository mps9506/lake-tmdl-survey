
library(rATTAINS)
library(arrow)
library(fs)
library(dplyr)
library(purrr)
library(tidyr)

actions <- read_feather(fs::path("data", "actions.feather"))
au_data <- read_feather(fs::path("data", "assessment_units.feather"))
docs <-  read_feather(fs::path("data", "docs.feather"))
actions
au_data

## there are a few repeated assessment units in au data

actions <- actions |>
  left_join(au_data |>
              select(assessment_unit_identifier, water_type_code), by = c("asessment_unit_identifier" = "assessment_unit_identifier"),
            multiple = "first")

actions |>
  distinct(water_type_code)

## filter to lakes
actions <- actions |>
  filter(water_type_code %in% c("LAKE, FRESHWATER", "LAKE", "RESERVOIR", "LAKE/RESERVOIR/POND"))



lake_tmlds <- actions |>
  distinct(action_identifier)


tmdl_docs <- docs |>
  filter(document_type_code == "TMDL Report")


lake_tmdls <- lake_tmlds |>
  left_join(tmdl_docs, by = c("action_identifier" = "action_identifier"))


## downloads docs to folder structure- docs/action_id/document_file_name

lake_tmdls <- lake_tmdls |>
  mutate(row = dplyr::row_number())

lake_tmdls |>
  split(lake_tmdls$row) |>
  map(\(df) print(fs::path_abs(fs::path("docs", df$action_identifier, df$document_file_name)))
      )

lake_tmdls |>
  split(lake_tmdls$row) |>
  map(\(df) fs::dir_create(fs::path("docs", df$action_identifier)))

lake_tmdls |>
  split(lake_tmdls$row) |>
  map(\(df) download.file(url = df$document_url,
                          destfile = fs::path("docs", df$action_identifier, df$document_file_name),
                          method = "libcurl",
                          mode = "wb"),
      .progress = TRUE
      )


actions

docs

all_lake_tmdls <- actions |>
  left_join(tmdl_docs, by = c("action_identifier" = "action_identifier"),
            multiple = "first")


all_lake_tmdls <- all_lake_tmdls |>
  mutate(parameter_names = map(parameters,
                               \(x) select(x, parameters_name))) |>
  unnest(parameter_names)


write_feather(all_lake_tmdls, fs::path("data", "all_lake_tmdls_docs.feather"))


## write excel file to edit manually
readr::write_csv(all_lake_tmdls, fs::path("data", "all_lake_tmdls_docs.csv"))

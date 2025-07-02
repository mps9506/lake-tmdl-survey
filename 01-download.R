library(rATTAINS)
library(arrow)
library(fs)


## download data for lake tmdls
dvs <- domain_values()

action_type_codes <- domain_values("ActionType")
df <- actions(action_type_code = "TMDL", state_code = "NM,TX,OK,AR,LA,MS,AL,GA,FL,SC,NC,TN,KY",
              tmdl_date_later_than = "2020-01-01")


docs <- df$documents
actions <- df$actions

write_feather(docs, fs::path("data", "docs.feather"))
write_feather(actions, fs::path("data", "actions.feather"))



## download data for Texas impairements
st_dv <- domain_values(domain_name = "OrgName")
st_dv |>  dplyr::filter(context == "TX")
tceq <- state_summary(organization_id = "TCEQMAIN", reporting_cycle = "2024")

write_feather(tceq, fs::path("data", "tceq_summary_2024.feather"))

tceq |>
  dplyr::filter(water_type_code == "RESERVOIR") |>
  dplyr::glimpse()

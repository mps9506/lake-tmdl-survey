library(rATTAINS)
library(arrow)
library(fs)

dvs <- domain_values()

action_type_codes <- domain_values("ActionType")
df <- actions(action_type_code = "TMDL", state_code = "NM,TX,OK,AR,LA,MS,AL,GA,FL,SC,NC,TN,KY",
              tmdl_date_later_than = "2020-01-01")


docs <- df$documents
actions <- df$actions

write_feather(docs, fs::path("data", "docs.feather"))
write_feather(actions, fs::path("data", "actions.feather"))

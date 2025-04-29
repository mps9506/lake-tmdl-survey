library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(forcats)
library(mpsTemplates)
## load custom fonts
## https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/
font_hoist <- function(family, silent = FALSE) {
  font_specs <- systemfonts::system_fonts() |>
    dplyr::filter(family == {{family}}) |>
    dplyr::mutate(family = paste(family, style)) |>
    dplyr::select(plain = path, name = family)

  purrr::pwalk(as.list(font_specs), systemfonts::register_font)

  if (!silent)  message(paste0("Hoisted ", nrow(font_specs), " variants:\n",
                               paste(font_specs$name, collapse = "\n")))
}

## prep fancy figure font
font_hoist("Manrope")

df <- read_csv("data/all_lake_tmdls_docs_edited.csv")

df


### Count of distinct documents by state

df |>
  distinct(action_identifier, organization_name.x) |>
  mutate(organization_name.x = fct_recode(organization_name.x,
                                          "Georgia" = "Georgia Environmental Protection Division")) |>
  mutate(organization_name.x = fct_infreq(organization_name.x)) |>
  ggplot() +
  geom_bar(aes(y = organization_name.x),
            width = 0.01) +
  geom_point(aes(y = organization_name.x,
                 x = after_stat(count)),
             stat = "count") +
  geom_text(aes(y = organization_name.x,
                x = after_stat(count)+0.5,
                label = after_stat(count)),
            stat = "count") +
  theme_mps_noto(base_family = "Manrope Regular") +
  scale_y_discrete("State") +
  scale_x_continuous("Lake TMDL Documents", expand = expansion(mult = c(0,0.05))) +
  theme(panel.grid.major.y = element_blank())


## size of distinct assessment units

df |>
  distinct(asessment_unit_identifier, surface_area, watershed_area) |>
  ggplot() +
  geom_histogram(aes(surface_area),
                 bins = 13) +
  geom_vline(aes(xintercept = median(surface_area, na.rm = TRUE)),
             color = "#000000", size = 1.25) +
    stat_summary(fun = median,
               fun.args = list(na.rm = TRUE),
               geom = "text",
               orientation = "y",
               hjust = 0,
               family = "Manrope Regular",
               aes(x = stage(surface_area, after_stat = 700),
                   y = 10,
                   label = after_stat(paste0("Median = ", round(x, 2))))) +

  scale_x_log10("Lake Surface Area (acres)") +
  scale_y_continuous("Count", expand = expansion(mult = c(0,0.05))) +
  theme_mps_noto(base_family = "Manrope Regular")


## size of distinct watersheds

df |>
  distinct(asessment_unit_identifier, surface_area, watershed_area) |>
  ggplot() +
  geom_histogram(aes(watershed_area),
                 bins = 14) +
  geom_vline(aes(xintercept = median(watershed_area, na.rm = TRUE)),
             color = "#000000", size = 1.25) +
  stat_summary(fun = median,
               fun.args = list(na.rm = TRUE),
               geom = "text",
               orientation = "y",
               hjust = 0,
               family = "Manrope Regular",
               aes(x = stage(watershed_area, after_stat = 5000),
                   y = 10,
                   label = after_stat(paste0("Median = ", round(x, 2))))) +

  scale_x_log10("Watershed Area (acres)", labels = scales::label_comma()) +
  scale_y_continuous("Count", expand = expansion(mult = c(0,0.05))) +
  theme_mps_noto(base_family = "Manrope Regular")

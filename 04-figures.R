library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(forcats)
library(mpsTemplates)
library(patchwork)
library(arrow)
library(ggrepel)


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

## drop metals and temp tmdls
df <- df |>
  filter(pollutant_name != "ALUMINUM, TOTAL") |>
  filter(pollutant_name != "TEMPERATURE") |>
  filter(parameters_name != "TEMPERATURE")


### Count of distinct documents and assessment units by state

p1a <- df |>
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
                x = after_stat(count)+1.5,
                label = after_stat(count)),
            stat = "count") +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.8)),
        axis.title = element_text(rel(0.8))) +
  scale_y_discrete("State") +
  scale_x_continuous("Lake TMDL Documents", expand = expansion(mult = c(0,0.1))) +
  theme(panel.grid.major.y = element_blank())

p1b <- df |>
  distinct(action_identifier, asessment_unit_identifier, organization_name.x) |>
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
                x = after_stat(count)+2.5,
                label = after_stat(count)),
            stat = "count") +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.8)),
        axis.title = element_text(rel(0.8))) +
  scale_y_discrete("") +
  scale_x_continuous("Assessment Unit TMDLs", expand = expansion(mult = c(0,0.1))) +
  theme(panel.grid.major.y = element_blank())

p1 <- p1a + p1b + plot_annotation(tag_levels = "A")

p1
ggsave("fig1.png",
       p1,
       device = ragg::agg_png,
       path = "figures",
       width = 6,
       height = 3,
       units = "in",
       res = 300)

## size of distinct assessment units and watersheds

p2a <- df |>
  distinct(asessment_unit_identifier, surface_area, watershed_area) |>
  ggplot() +
  geom_histogram(aes(surface_area),
                 fill = "dodgerblue",
                 bins = 13,
                 color = "white",
                 linewidth = 0.05) +
  geom_vline(aes(xintercept = median(surface_area, na.rm = TRUE)),
             color = "#000000", size = 1.25) +
    stat_summary(fun = median,
               fun.args = list(na.rm = TRUE),
               geom = "text",
               orientation = "y",
               hjust = 0,
               family = "Manrope Regular",
               size = 2.5,
               aes(x = stage(surface_area, after_stat = 700),
                   y = 10,
                   label = after_stat(paste0("Median = ", round(x, 0))))) +

  scale_x_log10("Lake Surface Area (acres)",  labels = scales::label_comma()) +
  scale_y_continuous("Count", expand = expansion(mult = c(0,0.05))) +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(rel(0.8)))

p2b <- df |>
  distinct(asessment_unit_identifier, surface_area, watershed_area) |>
  ggplot() +
  geom_histogram(aes(watershed_area),
                 fill = "dodgerblue",
                 color = "white",
                 linewidth = 0.05,
                 bins = 14) +
  geom_vline(aes(xintercept = median(watershed_area, na.rm = TRUE)),
             color = "#000000", size = 1.25) +
  stat_summary(fun = median,
               fun.args = list(na.rm = TRUE),
               geom = "text",
               orientation = "y",
               hjust = 0,
               family = "Manrope Regular",
               size = 2.5,
               aes(x = stage(watershed_area, after_stat = 7000),
                   y = 10,
                   label = after_stat(paste0("Median = ",
                                             format(round(x, 0),
                                                    big.mark = ","))))) +

  scale_x_log10("Watershed Area (acres)", labels = scales::label_comma()) +
  scale_y_continuous("", expand = expansion(mult = c(0,0.05))) +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(rel(0.8)))


p2 <- p2a + p2b + plot_annotation(tag_levels = "A")
p2

ggsave("fig2.png",
       p2,
       device = ragg::agg_png,
       path = "figures",
       width = 6,
       height = 3,
       units = "in")

## pollutant loads by pollutant parameter

## seems parameters and pollutant are flip flopped for OK TMDLS.

p3b <- df |>
  mutate(tmdl_parameter = case_when(
    pollutant_name %in% c("CHLOROPHYLL-A", "DISSOLVED OXYGEN") ~ parameters_name,
    .default = pollutant_name
  )) |>
  distinct(asessment_unit_identifier, tmdl_parameter) |>
  mutate(tmdl_parameter = fct_infreq(tmdl_parameter)) |>
  ggplot() +
  geom_bar(aes(y = tmdl_parameter),
           width = 0.01) +
  geom_point(aes(y = tmdl_parameter,
                 x = after_stat(count)),
             stat = "count") +
  geom_text(aes(y = tmdl_parameter,
                x = after_stat(count)+5,
                label = after_stat(count)),
            stat = "count") +
  scale_x_continuous("Assessment Unit Count", expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete("Daily Load Allocation Parameters") +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.7)),
        panel.grid.major.y = element_blank())


## end point parameters
## TSI endpoints are actually site specific chlorophyll-a concentration targets
## NM, the target end point is pollutant name
## converts fecal and e.coli to indicator bacteria

p3a <- df |>
  mutate(endpoint_parameter = case_when(
    pollutant_name %in% c("CHLOROPHYLL-A", "DISSOLVED OXYGEN", "TURBIDITY") ~ pollutant_name,
    parameters_name == "TROPHIC STATE INDEX (TSI)" ~ "CHLOROPHYLL-A",
    organization_name.x == "New Mexico" ~ pollutant_name,
    .default = parameters_name
  )) |>
  mutate(endpoint_parameter = case_when(
    endpoint_parameter %in% c("FECAL COLIFORM", "ESCHERICHIA COLI (E. COLI)") ~ "INDICATOR BACTERIA",
    .default = endpoint_parameter
  )) |>
  distinct(asessment_unit_identifier, endpoint_parameter) |>
  mutate(endpoint_parameter = fct_infreq(endpoint_parameter)) |>
  ggplot() +

  geom_bar(aes(y = endpoint_parameter),
           width = 0.01) +
  geom_point(aes(y = endpoint_parameter,
                 x = after_stat(count)),
             stat = "count") +
  geom_text(aes(y = endpoint_parameter,
                x = after_stat(count)+5,
                label = after_stat(count)),
            stat = "count") +

  scale_x_continuous("Assessment Unit Count",
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete("Target Endpoint Parameter") +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.7)),
        panel.grid.major.y = element_blank())

p3 <- p3a + p3b + plot_annotation(tag_levels = "A")


ggsave("fig3.png",
       p3,
       device = ragg::agg_png,
       path = "figures",
       width = 6,
       height = 3,
       units = "in")


## model frequency

p4a <- df |>
  distinct(asessment_unit_identifier, empirical_model) |>
  filter(!is.na(empirical_model)) |>
  mutate(empirical_model = case_when(
    empirical_model == "Multiple Regression" ~ "Regression",
    .default = empirical_model
  )) |>
  mutate(empirical_model = fct_infreq(empirical_model)) |>
  ggplot() +
  geom_bar(aes(y = empirical_model),
           width = 0.01) +
  geom_point(aes(y = empirical_model,
                 x = after_stat(count)),
             stat = "count") +
  geom_text(aes(y = empirical_model,
                x = after_stat(count)+1,
                label = after_stat(count)),
            stat = "count") +
  scale_x_continuous("",
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete("") +
  labs(subtitle = "A: Empirical models") +
  coord_fixed(xlim = c(0,15)) +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.7)),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = rel(0.7)))


p4b <- df |>
  distinct(asessment_unit_identifier, watershed_model) |>
  filter(!is.na(watershed_model)) |>
  mutate(watershed_model = case_when(
    watershed_model == "Curve Number Method" ~ "CN",
    .default = watershed_model
  )) |>
  mutate(watershed_model = fct_infreq(watershed_model)) |>
  ggplot() +
  geom_bar(aes(y = watershed_model),
           width = 0.01) +
  geom_point(aes(y = watershed_model,
                 x = after_stat(count)),
             stat = "count") +
  geom_text(aes(y = watershed_model,
                x = after_stat(count)+1,
                label = after_stat(count)),
            stat = "count") +
  scale_x_continuous("",
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete("Models") +
  labs(subtitle = "B: Watershed models") +
  coord_fixed(xlim = c(0,15)) +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.7)),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = rel(0.7)))

p4c <- df |>
  distinct(asessment_unit_identifier, lake_model) |>
  filter(!is.na(lake_model)) |>
  mutate(lake_model = fct_infreq(lake_model)) |>
  ggplot() +
  geom_bar(aes(y = lake_model),
           width = 0.01) +
  geom_point(aes(y = lake_model,
                 x = after_stat(count)),
             stat = "count") +
  geom_text(aes(y = lake_model,
                x = after_stat(count)+1,
                label = after_stat(count)),
            stat = "count") +
  coord_fixed(xlim = c(0,15)) +
  scale_x_continuous("Assessment Unit Count",
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete("") +
  labs(subtitle = "C: Receiving body models") +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.7)),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = rel(0.7)))




### commonly linked watersehd and receiving models

p4d <- df |>
  mutate(watershed_model = case_when(
    watershed_model == "Curve Number Method" ~ "CN",
    .default = watershed_model
  )) |>
  distinct(asessment_unit_identifier, watershed_model, lake_model) |>
  filter(!is.na(watershed_model) | !is.na(lake_model)) |>
  ggplot() +
  geom_bin2d(aes(watershed_model, lake_model)) +

  geom_text(aes(x = watershed_model,
                y = lake_model,
                label = after_stat(count),
                color = after_stat(count) < 4),
            family = "Manrope Bold",
            stat = "bin_2d") +

  scale_fill_viridis_c() +
  scale_x_discrete("Watershed Model",
                   expand = expansion(mult = c(0,0))) +
  scale_y_discrete("Receiving Body Model",
                   expand = expansion(mult = c(0,0))) +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  coord_equal() +
  labs(subtitle = "D: Model combinations") +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.8),
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1),
        axis.title = element_text(size = rel(0.7)),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = rel(0.7)))

p4d
p4 <- (p4a / p4b / p4c) | p4d
p4
ggsave("fig4.png",
       p4,
       device = ragg::agg_png,
       path = "figures",
       width = 10,
       height = 5,
       units = "in")


### models and target combinations
## to do: shorten some of the model names, combine LSPC and HSPF to one model since underlying runoff processes are the same model.

p5 <- df |>
  mutate(endpoint_parameter = case_when(
    pollutant_name %in% c("CHLOROPHYLL-A", "DISSOLVED OXYGEN", "TURBIDITY") ~ pollutant_name,
    parameters_name == "TROPHIC STATE INDEX (TSI)" ~ "CHLOROPHYLL-A",
    organization_name.x == "New Mexico" ~ pollutant_name,
    .default = parameters_name
  )) |>
  mutate(endpoint_parameter = case_when(
    endpoint_parameter %in% c("FECAL COLIFORM", "ESCHERICHIA COLI (E. COLI)") ~ "INDICATOR BACTERIA",
    .default = endpoint_parameter
  )) |>
  mutate(model_set = case_when(
    !is.na(empirical_model) ~ empirical_model,
    is.na(watershed_model) ~ lake_model,
    is.na(lake_model) ~ watershed_model,
    .default = str_c(watershed_model, lake_model, sep = "+")
  )) |>
  distinct(asessment_unit_identifier, model_set, endpoint_parameter) |>
  ggplot() +
  geom_bin_2d(aes(endpoint_parameter, model_set), na.rm = TRUE) +
  geom_text(aes(x = endpoint_parameter,
                y = model_set,
                label = after_stat(count),
                color = after_stat(count) < 6),
            family = "Manrope Bold",
            stat = "bin_2d") +
  scale_fill_viridis_c() +
  scale_x_discrete("Endpoint Parameter",
                   expand = expansion(mult = c(0,0))) +
  scale_y_discrete("Model",
                   expand = expansion(mult = c(0,0))) +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  coord_equal() +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(axis.text.y = element_text(hjust = 1,
                                   size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.6),
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1),
        axis.title = element_text(size = rel(0.7)),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = rel(0.7)))

ggsave("fig5.png",
       p5,
       device = ragg::agg_png,
       path = "figures",
       width = 6,
       height = 4,
       units = "in")


## states and models
df

df |>
  mutate(organization_name.x = fct_recode(organization_name.x,
                                          "Georgia" = "Georgia Environmental Protection Division")) |>
  mutate(model_set = case_when(
    !is.na(empirical_model) ~ empirical_model,
    is.na(watershed_model) ~ lake_model,
    is.na(lake_model) ~ watershed_model,
    .default = str_c(watershed_model, lake_model, sep = "+")
  )) |>
  distinct(organization_name.x, model_set, action_identifier, asessment_unit_identifier) |>
  ggplot() +
  geom_bin_2d(aes(organization_name.x, model_set))



## state assessment summary

tceq <- read_feather(fs::path("data", "tceq_summary_2024.feather"))
tceq |>
  dplyr::filter(water_type_code == "RESERVOIR") |>
  dplyr::glimpse()

## add labels eg fully supporting = 75% over.

p6 <- tceq |>
  filter(water_type_code == "RESERVOIR", use_name == "General Use") |>
  select(use_name, fully_supporting, use_insufficient_information, not_assessed, not_supporting) |>
  tidyr::pivot_longer(fully_supporting:not_supporting, names_to = "classification", values_to = "acres") |>
  distinct() |>
  group_by(use_name) |>
  mutate(total = sum(acres, na.rm = TRUE)) |>
  ungroup() |>
  mutate(prop = acres/total) |>
  filter(use_name %in% c("Recreation Use", "General Use" , "Fish Consumption Use", "Aquatic Life Use")) |>
  mutate(classification = forcats::fct_reorder(classification, prop)) |>
  mutate(classification = forcats::fct_recode(classification,
                                              "not assessed" = "not_assessed",
                                              "insufficient information" = "use_insufficient_information",
                                              "not supporting" = "not_supporting",
                                              "fully supporting" = "fully_supporting")) |>
  arrange(prop) |>
  mutate(midpoint = 1 - (cumsum(prop) - prop/2)/sum(prop)) |>
  ggplot(aes(use_name, prop, fill = classification)) +
  geom_col(width = 0.4) +
  geom_text_repel(aes(y = midpoint, x = 1.2,
                      label = paste0(classification, "\n", round(prop*100, 1), "%"),
                      color = classification),
                  size = 3,
                  nudge_x = 0.4,
                  direction = "y",
                  hjust = "right",
                  segment.curvature = -1e-20,
                  family = "Manrope",
                  fontface = "bold") +
  theme_mps_noto(base_family = "Manrope Regular") +
  scale_y_continuous("Percent of Total Reservoir Acres",
                     expand = expansion(mult = 0),
                     labels = scales::label_percent()) +
  guides(fill = guide_legend(override.aes = aes(color = NA))) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = rel(0.7)),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length.x = rel(0))


ggsave("fig6.png",
       p6,
       device = ragg::agg_png,
       path = "figures",
       width = 7,
       height = 4,
       units = "in")


## should pare down to top reasons for general use impairment 13% algae, 6%acidity, 2% salts
## according to https://mywaterway.epa.gov/state/TX/water-quality-overview

## total acres cause == algal gorwth / total acres impaired = percent reason for impairment

## total acres insufficienet into == algal gorwth / total acres insuffient info = percent reason for insufficent info
p7 <- tceq |>
  filter(water_type_code == "RESERVOIR", use_name == "General Use") |>
  select(use_name, fully_supporting, use_insufficient_information, not_assessed, not_supporting, parameter_group, parameter_insufficient_information, cause, meeting_criteria, removed) |>
  tidyr::pivot_longer(fully_supporting:not_supporting, names_to = "classification", values_to = "acres") |>
  filter(classification == "not_supporting") |>
  mutate(percent_cause = cause/acres) |> ## this looks correct based
  filter(!is.na(percent_cause)) |>
  mutate(parameter_group = fct_reorder(parameter_group, percent_cause)) |>
  arrange(percent_cause) |>
  mutate(midpoint = 1 - (cumsum(percent_cause) - percent_cause/2)/sum(percent_cause)) |>
  ggplot() +
  geom_col(aes(use_name, y = percent_cause, fill = parameter_group),
           width = 0.1) +
  geom_text_repel(aes(y = midpoint, x = 1,
                        label = paste0(parameter_group, "\n", round(percent_cause*100, 1), "%"),
                        color = parameter_group),
                    size = 3,
                    nudge_x = 0.4,
                    direction = "y",
                    hjust = "right",
                    segment.curvature = -1e-20,
                    family = "Manrope",
                    fontface = "bold") +
  scale_y_continuous("Percent of Impaired Reservoir Acres",
                     expand = expansion(mult = 0),
                     labels = scales::label_percent()) +
  scale_x_discrete(expand = expansion(mult = (c(0.15,0.25)))) +
  guides(fill = guide_legend(override.aes = aes(color = NA))) +
  coord_cartesian(clip = "off") +
  theme_mps_noto(base_family = "Manrope Regular") +
  theme(panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = rel(0.7)),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length.x = rel(0))



ggsave("fig7.png",
       p7,
       device = ragg::agg_png,
       path = "figures",
       width = 7,
       height = 4,
       units = "in")


    tidyr::pivot_longer(parameter_insufficient_information:meeting_criteria, names_to = "classification", values_to = "acres") |>
    glimpse()
  group_by(parameter_group) |>
  mutate(total = sum(acres, na.rm = TRUE)) |>
  ungroup() |>
  mutate(prop = acres/total) |>
  ggplot() +
  geom_col(aes(prop, parameter_group, fill = classification)) +


tceq |>
  filter(water_type_code == "RESERVOIR", use_name == "Aquatic Life Use") |>
  select(use_name, fully_supporting, use_insufficient_information, not_assessed, not_supporting, parameter_group, parameter_insufficient_information, cause, meeting_criteria, removed) |>
  glimpse()
  tidyr::pivot_longer(parameter_insufficient_information:meeting_criteria, names_to = "classification", values_to = "acres") |>
  group_by(parameter_group) |>
  mutate(total = sum(acres, na.rm = TRUE)) |>
  ungroup() |>
  mutate(prop = acres/total) |>
  ggplot() +
  geom_col(aes(prop, parameter_group, fill = classification))

tceq |>
  filter(water_type_code == "RESERVOIR", use_name == "Fish Consumption Use") |>
  select(use_name, fully_supporting, use_insufficient_information, not_assessed, not_supporting, parameter_group, parameter_insufficient_information, cause, meeting_criteria, removed) |>
  tidyr::pivot_longer(parameter_insufficient_information:meeting_criteria, names_to = "classification", values_to = "acres") |>
  group_by(parameter_group) |>
  mutate(total = sum(acres, na.rm = TRUE)) |>
  ungroup() |>
  mutate(prop = acres/total) |>
  ggplot() +
  geom_col(aes(prop, parameter_group, fill = classification))

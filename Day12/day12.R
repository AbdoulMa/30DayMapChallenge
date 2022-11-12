
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------

# https://ourworldindata.org/co2-emissions # You can download here
global_emissions <- read_csv(global_emission_path) |> 
  filter(Year == 2021) |> 
  janitor::clean_names() |> 
  drop_na(code)

# Countries positions
world_grid <- geofacet::world_countries_grid1

# Countries names with codes mapping
countries_mapping <- read_csv("https://gist.githubusercontent.com/fogonwater/bc2b98baeb2aa16b5e6fbc1cf3d7d545/raw/6fd2951260d8f171181a45d2f09ee8b2c7767330/countries.csv") |> 
  select(country_code3, continent_name)

# Data joining
final_global_emissions <- global_emissions |> 
  inner_join(world_grid, by = c("code" = "code_alpha3")) |> 
  inner_join(countries_mapping, by = c("code" = "country_code3"))

# Graphic -----------------------------------------------------------------
final_global_emissions |> 
  ggplot() + 
  geom_point(aes(col, row, size = annual_co2_emissions, color = continent_name), alpha = .65) + 
  geom_text(aes(col, row, label = code), family = "UEFA Supercup", fontface = "bold",  size = 2.5) + 
  annotate(geom = "text", x = 4.5, y = 6, color = prismatic::clr_darken("#0072B2"), family = "UEFA Supercup", fontface = "bold",label = str_to_upper("North America"), size = 4) +
  annotate(geom = "text", x = 3, y = 12, color = prismatic::clr_darken("#D55E00"), family = "UEFA Supercup", fontface = "bold",label = str_to_upper("South America"), size = 4) +
  annotate(geom = "text", x = 15, y = 2, color = prismatic::clr_darken("#56B4E9"), family = "UEFA Supercup", fontface = "bold",label = str_to_upper("Europe"), size = 4) +
  annotate(geom = "text", x = 21, y = 4, color = prismatic::clr_darken("#E69F00"), family = "UEFA Supercup", fontface = "bold",label = str_to_upper("Asia"), size = 4) +
  annotate(geom = "text", x = 12, y = 18, color = prismatic::clr_darken("#009E73"), family = "UEFA Supercup", fontface = "bold",label = str_to_upper("Africa"), size = 4) +
  annotate(geom = "text", x = 23, y = 16, color = prismatic::clr_darken("#CC79A7"), family = "UEFA Supercup", fontface = "bold",label = str_to_upper("Oceania"), size = 4) +
  annotate(geom = "text", x = 15, y = 22, color = prismatic::clr_darken("#F0E442"), family = "UEFA Supercup", fontface = "bold",label = str_to_upper("Antarctica"), size = 4) +
  labs(
    title = "GLOBAL CO2 EMISSIONS", 
    caption = "#30DayMapChallenge<br>Data: **Our World in Data**<br>Day 12 : **Scale** · Abdoul ISSA BIDA."
  ) + 
  scale_y_reverse() + 
  scale_size_continuous(
    range = c(6, 65),
    guide = "none"
  ) + 
  scale_color_manual(
    values = c(
      "Asia"   =   "#E69F00" ,
      "Europe" = "#56B4E9" ,
      "Africa" = "#009E73",
      "Antarctica" = "#F0E442",
      "North America" ="#0072B2",
      "South America"= "#D55E00" ,
      "Oceania"  = "#CC79A7"
    ),
    guide = "none"
  ) + 
  coord_equal(clip = "off", expand = F) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "UEFA Supercup"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold", size = rel(2.25), margin = margin(b = 1.5, unit = "cm")),
    plot.caption = ggtext::element_markdown(hjust = 0, size = rel(1.25)),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin = margin(c(1,.25,.5,.25), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day12", "day12")
ggsave(glue::glue("{path}.png"), width = 10.5, height = 9, device = ragg::agg_png, dpi = 300)


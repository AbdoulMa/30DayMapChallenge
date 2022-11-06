
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
metropolitan_france <- read_sf(metropolitan_france_path)
metropolitan_france_roads <- read_sf(metropolitan_france_roads_path)

# Graphic -----------------------------------------------------------------
ggplot() + 
  geom_sf(data = metropolitan_france_roads, color = "#171717", size = .4, alpha = .8) +
  geom_sf(data = metropolitan_france, color = "#161616", fill = NA, size = .4, alpha = .8) + 
  labs(
    caption = '<span style="font-face: bold; font-size: 75px;">FRANCE</span><br><span style="font-face: bold; font-size: 60px;">Road Network</span><br><span style = "color: #444444;font-size: 20px;">#30DayMapChallenge Day 6 : Network ⋅ Abdoul ISSA BIDA</span>'
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.caption =ggtext::element_markdown(family = "Gotham Narrow", hjust = .5),
    plot.margin = margin(c(.25,.25,.35,.25), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day6", "day6")
ggsave(glue::glue("{path}.png"), width = 9, height = 9, device = ragg::agg_png, dpi = 300)


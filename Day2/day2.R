
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
teams_travels <- read_csv("Data/l1_teams_travels.csv")
regions <- read_sf("Data/regions-20180101-shp/regions-20180101.shp")
(regions <- regions %>% 
    filter(!code_insee %in% c("01","02","03","04","06")) # Exclusion des communes d'Outre-Mer
)
# Graphic -----------------------------------------------------------------
teams_travels %>% 
  mutate(team_summary = fct_reorder(team_summary, sum_distance)) %>% 
  ggplot() + 
  geom_sf(data = regions, size = 0.15, fill = "#C8C8C8", color = "#343434") +
  geom_curve(aes(x = longitude, y = latitude, xend = opponent_longitude, yend = opponent_latitude, color = distance_km),
             curvature = .1, 
             arrow = arrow(length = unit(0.015, "npc"))
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    title = "Travel of French League 1 clubs",
    caption = "#30DayMapChallenge - Day 2 : Lines · Abdoul ISSA BIDA inspired by Bob RUDIS"
  ) + 
  scale_color_viridis_c(
    name = "Distance", 
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = .5,
      barwidth = unit(7.5, "cm") 
    ),
    labels  = ~paste0(scales::comma(.x), 'km')
  ) + 
  facet_wrap(vars(team_summary), nrow = 4) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Gotham Medium"),
    plot.title = element_text(family = "Gotham Black", size = rel(2.5), hjust = .5, margin = margin(t=25, b=10)),
    plot.caption = element_text(size = rel(1.125), hjust = .5, margin = margin(b = 10)),
    plot.margin = margin(.75, unit = "cm"),
    plot.background = element_rect(fill = "#F7F7F7", color = NA),
    strip.text = element_markdown(), 
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    legend.position = "top"
  )


# Saving ------------------------------------------------------------------
path <- here::here("Day2", "day2")
ggsave(glue::glue("{path}.pdf"), width = 12.5, height = 14.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}_twitter.png"),
  dpi = 320
)

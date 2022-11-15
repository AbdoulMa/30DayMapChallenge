
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(geofacet)

# Data Reading and Wrangling ----------------------------------------------
# Inspiration
# https://www.nationalgeographic.com/magazine/graphics/where-to-find-the-good-life
happiness_df <- tibble(
  state_abbr = state.abb,
  state_name = state.name
)

happiness_df <- happiness_df |> 
  mutate( 
    index = case_when(
      state_abbr %in% c("IN", "OH", "KY", "WV", "AR", "OK", "LA", "MS", "AL","RI") ~ "😞", 
      state_abbr %in% c("NV", "KS","MO", "IL", "MI", "TN","NY", "MD", "CT", "DE") ~ "😕", 
      state_abbr %in% c("WA", "OR", "IA", "WI", "NC", "SC", "GA", "NJ", "PA", "NH") ~ "😐", 
      state_abbr %in% c("ID", "WY", "UT", "CA", "ND", "NE", "NM","VA", "MA", "FL") ~ "🙂", 
      TRUE ~ "😀"
    ), 
    index = fct_relevel(index, c("😞", "😕","😐", "🙂", "😀" ))
  )

# Graphic -----------------------------------------------------------------
grid_wo_dc <- us_state_grid1[us_state_grid1$code != "DC", ]
happiness_df |> 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = index)) + 
  geom_text(x = .5, y = .5, aes(label = index), color = "white",size = 10) + 
  facet_geo(vars(state_abbr),grid = grid_wo_dc,   strip.position = "bottom") +
  scale_fill_manual(
    values = c( "#E61D2D","#F57B33","#FFBC39","#C5C43D","#81C742"),
    guide = "none"
  ) +
  labs(
    title = "Well-being Map", 
    caption = "Data and inspiration from National Geographic\n#30DayMapChallenge - Day 16 : Minimal\nAbdoul ISSA BIDA"
  ) + 
  theme_minimal() + 
  coord_equal() + 
  theme( 
    panel.spacing.x = unit(-3, "pt"),
    panel.spacing.y = unit(-5, "pt"),
    panel.grid = element_blank(),
    axis.text = element_blank(), 
    plot.title  = element_text(size = rel(2.25), margin = margin(b = 1, unit = "cm")),
    plot.caption  = element_text(size = rel(1.25), margin = margin(t = .25, unit = "cm")),
    strip.text = element_text(size = rel(1.25)),
    plot.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# TODO Creer la légend

# Saving ------------------------------------------------------------------
path <- here::here("Day16", "day16")
ggsave(glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

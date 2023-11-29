# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
# https://www.statista.com/statistics/997040/world-population-by-continent-1950-2020/
africa <- read_sf("/path/shapefile.shp")

europe <- read_sf("/path/shapefile.shp") |>
  st_transform(crs = "ESRI:102013")

black_col <- "#111111"
grey_col <- "#B0B0B0"

# Example Africa grey  picto
africa |>
  ggplot() +
  geom_sf(fill = grey_col, color = grey_col) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023/Day29", "Continents")
ggsave(glue::glue("{path}/africa_grey.png"), width = 8, height = 8, device = ragg::agg_png, dpi = 300)


# Assembly with Illustrator ----

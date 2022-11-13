
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------

country_borders <- read_sf(world_shape_file_path)

# Retrieve Benin for World countries
benin_borders <- country_borders %>%
  filter(ISO_A3 == "BEN")

# https://data.humdata.org/dataset/hotosm_ben_waterways # You can download it here
benin_waters <- read_sf(benin_waters_path)
benin_waters <- st_intersection(benin_borders, benin_waters)

# Graphic -----------------------------------------------------------------
ggplot() +
  geom_sf(
    data = benin_borders,
    color = "white", size = 0.25, fill = "#111111"
  ) +
  geom_sf(data = benin_waters, size = 0.3, color = "grey40") +
  annotate(geom = "text", x = 2.433333, y = 6.4375, label = "★", color = "white", size = 10) +
  annotate(
    geom = "richtext", x = 2.433333, y = 5.8,
    label = '<span style="font-size: 45px;">Cotonou</span><br><span style="font-size: 55px;">BENIN</span><br><span style="font-size:25px;">WEST AFRICA</span>',
    color = "white",
    family = "Postoni Regular",
    fontface = "bold",
    label.size = unit(0, "pt"),
    fill = NA
  ) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#111111", color = NA),
    plot.margin = margin(b = .5, unit = "cm")
  )


# Saving ------------------------------------------------------------------
path <- here::here("Day13", "day13")
ggsave(glue::glue("{path}.png"), width = 6, height = 12, device = ragg::agg_png, dpi = 300)

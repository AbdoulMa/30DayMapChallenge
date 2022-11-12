
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------

country_borders <- read_sf(world_shape_file_path)
# Retrieve Benin for World countries 
benin_borders <- country_borders %>%
  filter(ISO_A3 == "BEN")

# https://data.humdata.org/dataset/hotosm_ben_waterways
benin_waters <- read_sf(benin_waters_path)
benin_waters <- st_intersection(benin_borders, benin_waters)


# Graphic -----------------------------------------------------------------
cities <- tribble(
  ~map, ~city, ~lat, ~long,
  "Benin", "Cotonou", 6.4375, 2.433333
) %>%
  st_as_sf(
    coords = c("long", "lat"), crs = 4326
  ) |> 
  mutate(
    long = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

(
  map_benin <- ggplot() +
    geom_sf(
      data = benin_borders,
      color = "white", size = 0.25, fill = "#111111"
    ) +
    geom_sf(data = benin_waters, size = 0.3, color = "grey40") +
    # geom_sf_text(data = cities, label = "★", color = "white", size = 10) +
    annotate(geom = "text", x=2.433333,  y = 6.4375,  label = "★", color = "white", size = 10) +
    annotate(geom = "richtext",  x=2.433333,  y = 5.75,  
             label = '<span style="font-size: 12.5px;">6° 22\' 0.001" N 2° 25\' 59.999" E</span><br><span style="font-size: 45px;">Cotonou</span><br><span style="font-size: 70px;">BENIN</span>',
             color = "white",
             label.size = unit(0,'pt'), 
             lineheight = unit(1, "cm"),
             fill = NA
             ) +
    # coord_sf(crs = 3355, datum = NA) +
    theme_void() +
    theme(
      plot.background =  element_rect(fill = "#111111", color = NA), 
      plot.margin = margin(b = .25, unit = "cm")
    )
)


# Saving ------------------------------------------------------------------
path <- here::here("Day13", "day13")
ggsave(glue::glue("{path}.png"), width = 6, height = 12, device = ragg::agg_png, dpi = 300)

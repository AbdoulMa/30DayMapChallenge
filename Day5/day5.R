
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(osmdata)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------

bbx <- getbb("Dijon, France")

# Available highway
available_tags("highway")

# motorway, trunk, primary, secondary, tertiary
highways <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary_link",
      "tertiary_link"
    )
  ) %>%
  osmdata_sf()


# Get small streets, pedestrian paths, living streets
streets <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "residential",
      "living_street",
      "service",
      "unclassified",
      "pedestrian",
      "footway",
      "track",
      "path"
    )
  ) %>%
  osmdata_sf()


water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>%
  unname_osmdata_sf()

river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()

water <- c(water_osm, river_osm) %>%
  .$osm_multipolygons %>%
  select(osm_id, name) %>%
  mutate(area = st_area(.)) %>%
  # this filter gets rid of tiny isolated lakes et cetera
  filter(area >= quantile(area, probs = 0.75))

# Center and Circle -------------------------------------------------------
crs2 <- 6384 # https://epsg.io/6384
center = c(long = 5.04148, lat = 47.322047)
center_proj <-
  tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

dist <-  3500
circle <- tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

streets_lines <- st_intersection(circle, streets$osm_lines)
highways_lines <- st_intersection(circle, highways$osm_lines)
water_lines <- st_intersection(circle, water)

# Graphic -----------------------------------------------------------------
# Plot
ggplot() +
  geom_sf(
    data = water_lines,
    fill = "steelblue",
    lwd = 0,
    alpha = .3
  ) +
  geom_sf(
    data = streets_lines,
    col = "grey40",
    size = .4,
    alpha = .65
  ) +
  geom_sf(
    data = highways_lines,
    col = "grey35",
    size = .6,
    alpha = .8
  ) +
  geom_sf(data = circle, color = "black", fill = NA) +
  labs(caption = "<span style='font-size:85px;'>Dijon</span><br><br><span style='font-size:35px;'>47° 19' 19.369\" N - 5° 2' 29.328\" E</span><br><br>
       <span style='font-size:20px;'>#30DayMapChallenge Day 5 : OpenStreetMap ⋅ Abdoul ISSA BIDA</span>") + 
  theme_void() + 
  theme(plot.caption = element_markdown(hjust = .5, family = "Operator SSm Book", face = "bold"),
        plot.background = element_rect(fill = "#F3F6F7", color = NA),
        plot.margin = margin(b = 25)) 

# Saving ------------------------------------------------------------------
path <- here::here("Day5", "day5")
ggsave(glue::glue("{path}.pdf"), width = 7.5, height =7.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

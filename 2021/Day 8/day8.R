
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

# Natural spaces
natural <- opq(bbx) |> 
  add_osm_feature(key = "natural") |> 
  osmdata_sf() |> 
  unname_osmdata_sf()

# Buildings
buildings <- opq(bbx) |> 
  add_osm_feature(key = "building") %>%
  osmdata_sf() |> 
  unname_osmdata_sf()

# Waters
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

# "EPSG:4326"
dist <-  3500
circle <- tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

streets_lines <- st_intersection(circle, streets$osm_lines)
highways_lines <- st_intersection(circle, highways$osm_lines)
# buildings_polygons <- st_intersection(circle, buildings$osm_polygons)
# natural_points <- st_intersection(circle, natural$osm_points)
# natural_lines <- st_intersection(circle, natural$osm_lines)
# natural_polygons <- st_intersection(circle, natural$osm_polygons)
water_lines <- st_intersection(circle, water)

# Graphic -----------------------------------------------------------------
ggplot() +
  # geom_sf(
  #   data = natural_points, 
  #   col = "#267300", 
  #   size = .125, 
  #   alpha = .65
  # ) + 
  # geom_sf(
  #   data = natural_lines,
  #   col = "#267300",
  #   size = .125,
  #   alpha = .65
  # ) +
  # geom_sf(
  #   data = natural_polygons,
  #   fill = "#267300",
  #   col = "#267300",
  #   size = .125,
  #   alpha = .35
  # ) +
  # geom_sf(
  #   data = buildings_polygons,
  #   size = .1,
  #   fill =  "#828282",
  #   color = "#828282",
  #   alpha = .125
  # ) +
  geom_sf(
    data = water_lines,
    fill = "steelblue",
    lwd = 0,
    alpha = .3
  ) +
  geom_sf(
    data = streets_lines,
    col = "#4e4e4e",
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
  labs(caption = "<span style='font-size: 30px;'>FRANCE</span><br><span style='font-size:85px;'>DIJON</span><br><br><span style='font-size:35px;'>47° 19' 19.369\" N - 5° 2' 29.328\" E</span><br><br>
       <span style='font-size:20px;'>#30DayMapChallenge Day 5 : OpenStreetMap ⋅ Abdoul ISSA BIDA</span>") + 
  theme_void() + 
  theme(plot.caption = element_markdown(hjust = .5, family = "Postoni Standard", face = "bold"),
        plot.background = element_rect(fill = "#F3F6F7", color = NA),
        plot.margin = margin(b = 25)) 

# Saving ------------------------------------------------------------------
path <- here::here("Day8", "day8")
ggsave(glue::glue("{path}.png"), width = 8.5, height = 8.5, device = ragg::agg_png, dpi = 300)

# Packages loading ---- 
library(osmdata)
library(sf)
library(tidyverse)

bbx <- getbb(paste0("Oujda", ",", "Morocco"))
# Get motorway, trunk, primary, secondary, tertiary ways ----
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

# Get small streets, pedestrian paths, living streets ----
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

# Get landuse ---- 
landuse <- opq(bbx) |> 
  add_osm_feature(key = "landuse") |> 
  osmdata_sf() |> 
  unname_osmdata_sf()

# Get buildings ---- 
buildings <- opq(bbx) |> 
  add_osm_feature(key = "building") %>%
  osmdata_sf() |> 
  unname_osmdata_sf()

# Center and circle ----
city_coords <- tibble(address = "Oujda, Morocco") |> 
  tidygeocoder::geocode(address, method = 'osm', long = long, lat = lat)

long <- city_coords$long[1]
lat <- city_coords$lat[1]

crs2 <- 6384 # https://epsg.io/6384
center_proj <-
  tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# circle to crop in ----
dist <-  3500
circle <- tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

streets_lines <- st_intersection(circle, streets$osm_lines)
highways_lines <- st_intersection(circle, highways$osm_lines)
buildings_polygons <- st_intersection(circle, buildings$osm_polygons)
landuse_polygons <- st_intersection(circle, landuse$osm_polygons)

# Graphic -----------------------------------------------------------------
ggplot() +
  geom_sf(
    data = landuse_polygons,
    fill = "#646464", 
    size = .05,
    alpha = .95
  ) + 
  geom_sf(
    data = buildings_polygons, 
    size = .05,
    fill = "#c8c8c8", 
    color = "#c8c8c8", 
    alpha = .75
  ) + 
  geom_sf(
    data = streets_lines,
    col = "grey40",
    size = .4,
    alpha = .65
  ) +
  geom_sf(
    data = highways_lines,
    col = "grey25",
    size = .6,
    alpha = .8
  ) +
  # geom_sf(data = circle, color = NA, fill =  NA) 
  labs(
    caption = "<span style='font-size: 30px;margin-bottom: 35px;'>MOROCCO</span><br><span style='font-size:85px;'>OUJDA</span><br><br><span style='font-size:35px;'>34° 41' 12.001\" N - 1° 54' 41\"W</span><br><br>
       <span style='font-size:25px;'>#30DayMapChallenge Day 8 : OpenStreetMap ⋅ Abdoul ISSA BIDA</span>"
  ) + 
  theme_void() + 
  theme(plot.caption = ggtext::element_markdown(hjust = .5, family = "Postoni Standard", face = "bold"), 
        plot.background = element_rect(fill = "grey98", color = NA),
        plot.margin = margin(b = 10)) 


# Saving ------------------------------------------------------------------
path <- here::here("Day8", "day8_2022")
ggsave(glue::glue("{path}_twitter.png"), width =9, height = 9, device = ragg::agg_png, dpi = 216)


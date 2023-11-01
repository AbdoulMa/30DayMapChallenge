
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Data Reading and Wrangling ----------------------------------------------
# Dat scraped from https://www.taylorswift.com/events/
taylor_events <- readRDS(taylor_events_path)

taylor_events_labels <- taylor_events |>
  group_by(city) |>
  mutate(
    nb_concerts = n()
  ) |>
  ungroup() |>
  distinct(city, .keep_all = T) |>
  mutate(
    city_num = row_number(),
    concerts_label = paste0(nb_concerts, ifelse(city_num == 1, " concerts", "c")),
    starting_on = paste0(ifelse(city_num == 1, "starting on ", "st. "), ifelse(city_num == 1, format(date_parsed, "%b %d, %Y"), format(date_parsed, "%b %d"))),
    fancy_label = paste0(city_num, "- ", city, " (", concerts_label, ")", "\n", starting_on)
  )

# Retrieve concerts places locations
place_sf <- function(city_name, city_fancy_label) {
  place <- mapboxapi::mb_geocode(
    city_name
  )
  place_sf <- tibble(
    city = city_name,
    city_label = city_fancy_label,
    long = place[1],
    lat = place[2]
  ) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)

  place_sf
}

taylor_places <- taylor_events_labels |>
  select(city, fancy_label) |>
  pmap_dfr(~ place_sf(..1, ..2))

taylor_travels <- taylor_events |>
  distinct(city, .keep_all = T) |>
  mutate(origin = lag(city)) |>
  drop_na(origin)

# Retrive driving roads from a concert place to another one
taylor_drivings <- taylor_travels |>
  select(origin, city) |>
  pmap_dfr(~ mapboxapi::mb_directions(
    origin = ..1,
    destination = ..2,
    profile = "driving",
    output = "sf",
    steps = TRUE
  ))

taylor_places <- taylor_places |>
  mutate(
    city_label = str_replace_all(city_label, "California", "CA")
  ) |>
  st_transform("ESRI:102005") |>
  mutate(
    long = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  mutate(
    face = case_when(
      city %in% c("Glendale, AZ", "Los Angeles, California") ~ "bold",
      TRUE ~ "italic"
    )
  )

# Transform to a better projecting
taylor_drivings <- taylor_drivings |>
  st_transform(crs = "ESRI:102005")

us_states <- albersusa::usa_sf() |>
  filter(name != "Alaska") |>
  st_transform("ESRI:102005")

# Graphic -----------------------------------------------------------------
# Image available at freepng.com
img <- png::readPNG(here::here(taylor_pic_path))
g_pic <- grid::rasterGrob(img, interpolate = T)

ggplot() +
  geom_sf(data = us_states, fill = "#E5E5E5", color = "#FFFFFF", size = .375) +
  geom_sf(data = taylor_drivings, color = "#4A69FA", size = rel(1.15)) +
  geom_point(data = taylor_places, aes(x = long, y = lat), fill = "white", color = "#4A69FA", pch = 21, size = 3, stroke = 1) +
  ggrepel::geom_text_repel(
    data = taylor_places, aes(x = long, y = lat, label = city_label, fontface = face),
    size = 2.75, lineheight = .85,
    hjust = 0,
    min.segment.length = Inf,
    family = "Ideal Sans Light"
  ) +
  annotation_custom(g_pic, xmin = -2257368, xmax = -1257368, ymin = 1500000, ymax = 2241485) +
  annotate(
    geom = "text", x = -1050000, y = 1800000, label = str_to_upper(" Taylor Swift · The Eras Tour"),
    family = "Postoni Standard",
    hjust = 0, size = 8.5
  ) +
  annotate(geom = "segment", x = -1050000, y = 1550000, xend = -1050000 + 3507500, yend = 1550000, size = 1) +
  annotate(geom = "segment", x = -1050000, y = 2050000, xend = -1050000 + 3507500, yend = 2050000, size = 1) +
  labs(
    caption = "#30DayMapChallenge<br>Data from **taylorswift.com**<br>Day 27 :**Music** · Abdoul ISSA BIDA."
  ) +
  coord_sf(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.caption = ggtext::element_markdown(
      family = "Ideal Sans Medium", hjust = .5, size = rel(1.125),
      margin = margin(t = -.5, unit = "cm")
    ),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    plot.margin = margin(c(.25, -.5, .25, -.5), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day27", "day27")
ggsave(glue::glue("{path}.png"), width = 8, height = 6, device = ragg::agg_png, dpi = 300)

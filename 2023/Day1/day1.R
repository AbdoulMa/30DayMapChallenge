# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(maps)
# Data Reading and Wrangling ----------------------------------------------
# Generating pixel grid
lat <- tibble(lat = seq(-90, 90, by = 1))
long <- tibble(long = seq(-180, 180, by = 1))

dots <- lat %>%
  merge(long, all = TRUE)

dots <- dots %>%
  mutate(
    country = map.where("world", long, lat),
    lakes = map.where("lakes", long, lat)
  ) %>%
  filter(!is.na(country) & is.na(lakes)) %>%
  dplyr::select(-lakes)

dots_sf <- dots |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)")) |>
  st_transform(crs = 4326)

sf_use_s2(FALSE)
dots_sf <- dots_sf |>
  st_join(world)

final_dots_sf <- dots_sf |>
  select(continent) |>
  filter(!is.na(continent)) |>
  mutate(
    long = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

final_dots_sf |>
  count(continent, name = "nb_points") |>
  mutate(proportion = round(prop.table(nb_points), 2))

# Graphic -----------------------------------------------------------------
final_dots_sf |>
  ggplot() +
  geom_point(aes(long, lat, color = continent), size = 0.8) +
  labs(
    title = "WORLD",
    caption = "#30DayMapChallenge - Day 1: Points <br/>Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 6 Brands\"'>&#xe61b;</span>**@issa_madjid** <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>**github.com/abdoulma**."
  ) +
  guides(
    color = "none"
  ) +
  scale_color_manual(
    values = c(
      "North America" = "#18378F",
      "South America" = "#6D3A8D",
      "Europe" = "#B70B79",
      "Africa" = "#E61F26",
      "Asia" = "#F39020",
      "Oceania" = "#FF7C88"
    )
  ) +
  coord_fixed(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = ggtext::element_markdown(family = "UEFA Supercup", face = "bold", color = "#FFFFFF", size = rel(6), hjust = 0.025, margin = margin(b = 0.5, unit = "cm")),
    plot.caption = ggtext::element_markdown(family = "UEFA Supercup", color = "#FFFFFF", hjust = 0.5, size = rel(1.5), margin = margin(t = 0.05, b = 0, unit = "cm")),
    plot.background = element_rect(fill = "#111111", color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023/Day1", "day1")
ggsave(glue::glue("{path}.pdf"), width = 20, height = 11, device = cairo_pdf)

# Additional annotations with Illustrator
pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"),
  filenames = glue::glue("{path}_polished.png"),
  dpi = 144
)

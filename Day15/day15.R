
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
#
fast_food_restaurants <- read_csv(fast_food_data_path,
  col_names = c("id", "restaurant_type", "street", "restaurant_county", "state", "code", "phone", "lat", "long", "other_num")
) |>
  mutate(
    restaurant_type = fct_recode(
      restaurant_type,
      "McDonald's" = "m",
      "Burger King" = "b",
      "Pizza Hut" = "p",
      "Taco Bell" = "t",
      "Wendy's" = "w",
      "KFC" = "k",
      "Jack's" = "j",
      "Hardee's" = "h",
      "Carl's Jr." = "c",
      "In-N-Out" = "i"
    )
  )

fast_foods_by_location <- fast_food_restaurants |>
  filter(!state %in% c("AK", "HI")) |>
  mutate(
    across(.cols = c("long", "lat"), .fns = ~ (plyr::round_any(., .5, floor)), .names = "{.col}_approxim")
  ) |>
  # Count nb restaurants by type and locations
  group_by(restaurant_type, long_approxim, lat_approxim) |>
  summarise(
    nb_restaurants = n()
  ) |>
  group_by(long_approxim, lat_approxim) |>
  slice_max(order_by = nb_restaurants, n = 1, with_ties = F) |>
  st_as_sf(coords = c("long_approxim", "lat_approxim")) |>
  st_set_crs(4326) |>
  st_transform(crs = "ESRI:102003")

us_map <- albersusa::usa_sf() |>
  filter(!iso_3166_2 %in% c("AK", "HI")) |>
  st_transform("ESRI:102003")

# Crop in the map
fast_foods_by_location <- st_intersection(us_map, fast_foods_by_location)

# Graphic -----------------------------------------------------------------s
ggplot() +
  geom_sf(data = us_map, fill = "#E6E7E8", color = "#FFFFFF", size = .25) +
  geom_sf(data = fast_foods_by_location, aes(color = restaurant_type)) +
  labs(
    title = str_to_upper("nearest fast food restaurant"),
    caption = "Data from fastfoodmaps.com\n#30DayMapChallenge - Day 15 : Food/Drink\nAbdoul ISSA BIDA inspired by Nathan YAU."
  ) +
  scale_color_manual(
    values = c(
      "McDonald's" = "#EDAD08",
      "Taco Bell" = "#5F4690",
      "Jack's" = "#CC503E",
      "Burger King" = "#38A645",
      "Wendy's" = "#1D6996",
      "KFC" = "#B50A37",
      "Hardee's" = "#111111",
      "Pizza Hut" = "#94346E",
      "Carl's Jr." = "#73AF48"
    ),
    guide = guide_legend(
      ncol = 2,
      title.theme = element_blank(),
      label.theme = element_text(family = "Inconsolata Semibold", size = rel(15), margin = margin(.125, .125, .125, .125, unit = "cm")),
      override.aes = list(size = 5)
    )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(.2, .125),
    plot.title = element_text(family = "Inconsolata Bold", hjust = .5, size = rel(3.5)),
    plot.caption = element_text(family = "Inconsolata Semibold", hjust = 0, size = rel(1.25), margin = margin(t = .5, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(.75, .5, .75, .5, "cm"),
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day15", "day15")
ggsave(glue::glue("{path}.png"), width = 10, height = 8.25, device = ragg::agg_png, dpi = 300)

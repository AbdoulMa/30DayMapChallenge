
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(biscale)
library(cowplot)
library(geojsonio)
library(broom)
library(rgeos)
library(grid)
library(cowplot)

# Data Reading and Wrangling ----------------------------------------------
# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
# Load this file. (Note: I stored in a folder called Data)
spdf <- geojson_read(here::here("Data/us_states_hexgrid.geojson"), what = "sp")

spdf@data <- spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), id = spdf@data$iso3166_2, State = spdf@data$google_name, label = spdf@data$label))

# States Populations
states_populations <- read_csv("Data/usa_states_population.csv") %>%
  select(State, Pop)

# COVID Data from NY Times Github
covid_deaths <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

states_deaths <- covid_deaths %>%
  group_by(state) %>%
  slice_max(order_by = date, n = 1) %>%
  ungroup() %>%
  select(state, deaths) %>%
  left_join(states_populations, by = c("state" = "State")) %>%
  drop_na() %>%
  mutate(
    deaths_per_million = deaths * 10^6 / Pop
  )

# States Obesity Pcts
states_obesity_df <- read_csv(here::here("Data/usa_states_obesity.csv"))

obesity_spdf <- spdf_fortified %>%
  left_join(states_obesity_df, by = c("id" = "State")) %>%
  left_join(states_deaths, by = c("id" = "state")) %>%
  bi_class(x = obesity_rate, y = deaths_per_million, style = "quantile", dim = 3)

# Hexagons Centers
centers <- centers %>%
  # Map with the bivariate class (important for the text color mapping)
  left_join(distinct(obesity_spdf, id, bi_class), by = c("State" = "id"))

# Graphic -----------------------------------------------------------------

custom_pal <- c(
  "1-1" = "#00441B",
  "2-1" = "#E7D4E8",
  "3-1" = "#C2A5CF",
  "1-2" = "#1B7837",
  "2-2" = "#F7F7F7",
  "3-2" = "#9970AB",
  "1-3" = "#A6DBA0",
  "2-3" = "#D9F0D3",
  "3-3" = "#40004B"
)

(legend <- bi_legend(
  pal = custom_pal,
  dim = 3
) +
  # See https://github.com/samiaab1990/30-day-map-challenge-2022/blob/f34fea97da89e39e87c487dfba97566d97108376/polygons/internet_pcp.R
  annotate(geom = "segment", x = .3, y = .3, xend = 1, yend = .3, color = "#111111", size = 1) +
  annotate(geom = "segment", x = .3, y = .3, xend = .3, yend = 1, color = "#111111", size = 1) +
  annotate(geom = "segment", x = .3, y = .3, xend = .2, yend = .2, color = "#111111", size = 1) +
  annotate(geom = "text", x = .2 - .6, y = .2 - .15, label = "Low Obesity rate\nLow Death rate", angle = 315, color = "#111111", size = 3.5, family = "Ideal Sans Bold", lineheight = .9, vjust = 1.15) +
  annotate(geom = "segment", x = 3, y = .3, xend = 3.7, yend = .3, color = "#111111", size = 1) +
  annotate(geom = "segment", x = 3.7, y = .3, xend = 3.7, yend = 1, color = "#111111", size = 1) +
  annotate(geom = "segment", x = 3.7, y = .3, xend = 3.8, yend = .2, color = "#111111", size = 1) +
  annotate(geom = "text", x = 3.8, y = .2, label = "Low Obesity rate\nHigh Death rate", color = "#111111", size = 3.5, family = "Ideal Sans Bold", lineheight = .9, angle = 315, hjust = -.15) +
  annotate(geom = "segment", x = .3, y = 3, xend = .3, yend = 3.7, color = "#111111", size = 1) +
  annotate(geom = "segment", x = .3, y = 3.7, xend = 1, yend = 3.7, color = "#111111", size = 1) +
  annotate(geom = "segment", x = .3, y = 3.7, xend = .2, yend = 3.8, color = "#111111", size = 1) +
  annotate(geom = "text", x = .2, y = 3.7, label = "High Obesity rate\nLow Death rate", color = "#111111", size = 3.5, family = "Ideal Sans Bold", lineheight = .9, angle = 315, hjust = 1.15) +
  annotate(geom = "segment", x = 3, y = 3.7, xend = 3.7, yend = 3.7, color = "#111111", size = 1) +
  annotate(geom = "segment", x = 3.7, y = 3.7, xend = 3.7, yend = 3, color = "#111111", size = 1) +
  annotate(geom = "segment", x = 3.7, y = 3.7, xend = 3.8, yend = 3.8, color = "#111111", size = 1) +
  annotate(geom = "text", x = 3.8 + .5, y = 3.8 + .2, label = "High Obesity rate\nHigh Death rate", color = "#111111", size = 3.5, family = "Ideal Sans Bold", lineheight = .9, angle = 315, vjust = -.15) +
  bi_theme(base_family = "Gotham Black") +
  theme(
    rect = element_rect(fill = "grey10"),
    panel.background = element_rect(fill = "#FAFAF2"),
    plot.background = element_rect(fill = "#FAFAF2"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
)

#  Main plot
subtitle <- "A study of cases suggests that risks of hospitalization, intensive care unit admission, invasive mechanical ventilation, and death are higher with increasing BMI.
Models estimate that 30.2% of hospitalizations were attributed to obesity."
(map <- ggplot() +
  geom_polygon(
    data = obesity_spdf, aes(fill = bi_class, x = long, y = lat, group = group),
    color = "#111111"
  ) +
  geom_richtext(
    data = centers, aes(
      x = x, y = y, label = glue::glue("{label}"),
      color = ifelse(str_detect(bi_class, "1-1|1-2|3-3"), "white", "black")
    ),
    size = 5,
    family = "Gotham Black",
    fill = NA, label.color = NA, show.legend = F
  ) +
  labs(
    title = str_to_upper("Obesity and COVID-19\n in USA"),
    subtitle = str_wrap(subtitle, 80),
    caption = "Data from stateofchildhoodobesity.org & cdc.gov.\n #30DayMapChallenge - Day 14 : Hexagons · Abdoul ISSA BIDA."
  ) +
  bi_scale_fill(pal = custom_pal, dim = 3, flip_axes = T, guide = "none") +
  scale_color_identity() +
  coord_map() +
  theme_void() +
  theme(
    plot.title = element_text(size = rel(3.5), hjust = .5, margin = margin(t = 10, b = 15), family = "Ideal Sans Bold"),
    plot.subtitle = element_text(size = rel(1.45), hjust = .5, margin = margin(b = 25), family = "Ideal Sans Medium"),
    plot.caption = element_text(color = "#111111", size = rel(1.75), family = "Ideal Sans Medium", margin = margin(t = 25, b = 10, r = 10), hjust = .5),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
    plot.background = element_rect(fill = "#FAFAF2", color = NA)
  )
)

# Rotate legend grob
# convert legend to grob and put in plot
legend_sub <- ggplotGrob(legend)

(legend_plot <- grobTree(legend_sub,
  vp = viewport(
    width = unit(1.5, "in"), x = 0.6, y = 0.6,
    height = unit(1.5, "in"),
    angle = 45
  )
)
)

# Combine Map with Legend
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend_plot, .7, .025, 0.25, .25) +
  theme(
    plot.background = element_rect(fill = "#FAFAF2", color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day14", "day14")
ggsave(glue::glue("{path}.png"), width = 12.5, height = 10.5, device = ragg::agg_png, dpi = 300)

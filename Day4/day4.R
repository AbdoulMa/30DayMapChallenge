
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(biscale)
library(cowplot)
library(geojsonio)
library(broom)
library(rgeos)

# Data Reading and Wrangling ----------------------------------------------
# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
# Load this file. (Note: I stored in a folder called Data)
spdf <- geojson_read(here::here("Data/us_states_hexgrid.geojson"),  what = "sp")

spdf@data <-  spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2, State = spdf@data$google_name, label = spdf@data$label))

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
custom_pal <- bi_pal_manual(val_1_1 = "#00441B", val_1_2 = "#1B7837", val_1_3 = "#A6DBA0",
                            val_2_1 = "#E7D4E8", val_2_2 = "#F7F7F7",val_2_3 = "#D9F0D3",
                            val_3_1 = "#C2A5CF", val_3_2 = "#9970AB",val_3_3 = "#40004B")

(legend <- bi_legend(pal = custom_pal,
                     dim = 3,
                     xlab = "Higher Obesity Rate",
                     ylab = "Higher Death Rate",
                     size = 8) + 
    bi_theme(base_family = "Gotham Black") + 
    theme(
      rect = element_rect(fill = "grey10"),
      panel.background = element_rect(fill = "#F7F7F7"),
      plot.background = element_rect(fill = "#F7F7F7"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_text(size = 10,
                                  color = "#C2A5CF"),
      axis.title.y = element_text(size = 10,
                                  color = "#1B7837"))
)

subtitle  <- "A study of cases suggests that risks of hospitalization, intensive care unit admission, invasive mechanical ventilation, and death are higher with increasing BMI.
Models estimate that 30.2% of hospitalizations were attributed to obesity."
(map <- ggplot() +
   geom_polygon(data = obesity_spdf, aes(fill =  bi_class, x = long, y = lat, group = group),
                color = "#111111") +
   geom_richtext(data=centers, aes(x=x, y=y, label=glue::glue("{label}"),
                                           color = ifelse(str_detect(bi_class, "1-1|1-2|3-3"), "white", "black")),
                 size = 5,
                 family = "Gotham Black",
                 fill = NA, label.color = NA, show.legend = F) +
   labs(
     title = "Obesity and COVID-19\n in USA",
     subtitle = str_wrap(subtitle, 80), 
     caption = "Data from stateofchildhoodobesity.org & cdc.gov.\n #30DayMapChallenge - Day 4 : Hexagons · Abdoul ISSA BIDA."
   ) + 
   bi_scale_fill(pal=custom_pal, dim =3, guide = "none") + 
   scale_color_identity() + 
   coord_map() + 
   theme_void() +
   theme(
     plot.title = element_text(size = rel(3.5),hjust = .5, margin = margin(t = 10, b = 15), family = "Gotham Black",  face = "bold"),
     plot.subtitle = element_text(size = rel(1.45),hjust = .5, margin = margin(b = 25), family = "Mercury Display"),
     plot.caption = element_text(color = "#111111", size = rel(1.25), face = "bold", family = "Mercury", margin = margin(t = 25, b = 10, r = 10), hjust = .5),
     plot.margin = margin(t = 20, r = 10, b =10, l = 10), 
     plot.background = element_rect(fill = "#F7F7F7", color = NA)
   )
)

# Combine Map with Legend
ggdraw() + 
  draw_plot(map, 0,0, 1,1) + 
  draw_plot(legend, .75, .05, 0.2,.2) + 
  theme(
    plot.background = element_rect(fill = "#F7F7F7")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day4", "day4")
ggsave(glue::glue("{path}.pdf"), width = 12, height = 12, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

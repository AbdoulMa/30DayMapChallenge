
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
us_states <- albersusa::usa_sf() |> 
  filter(!name %in% c("Hawaii", "Alaska")) |> 
  st_transform(crs = "ESRI:102003")

# Data wrangling was a little bit complex, feel free to join me if you really need the code
# Only keep towns with more than 15000 inhabitants
us_nuclear_data <- readRDS(us_nuclear_geodata_path)

# Graphic -----------------------------------------------------------------
us_nuclear_data |> 
select(matches("(plant|county)_")) |> 
  ggplot() + 
  geom_sf(data = us_states, size = .25) + 
  geom_segment(aes(x = county_long, y = county_lat, xend = plant_long, yend = plant_lat), size = .25) + 
  geom_point(aes(plant_long, plant_lat), size = 3.5, color = "black") + 
  geom_point(aes(plant_long, plant_lat), size = 2, color = "#FFD700")+ 
  labs(
    title = 'Which <span style="color: #FFD700;">nuclear plant</span> is closest to Americans?', 
    subtitle = "Each segment connects a city to its nearest nuclear power plant",
    caption = 'Cities with population > 15000<br>Inspired by **Robert Allison & Jack Merlin Bruce**<br>#30DayMapChallenge - Day 30 : **Remix** · Abdoul ISSA BIDA.'
  ) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Atlan"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = ggtext::element_markdown(face = "bold", size =rel(3)),
    plot.subtitle = element_text(color = "grey35", face ="bold", size = rel(1.625)),
    plot.caption = ggtext::element_markdown(size = rel(1.5)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(c(.75, .75,.75,.75), unit = "cm")
  )


# Saving ------------------------------------------------------------------
path <- here::here("Day30", "day30")
ggsave(glue::glue("{path}.png"), width = 9.5, height = 7.5, device = ragg::agg_png, dpi = 300)


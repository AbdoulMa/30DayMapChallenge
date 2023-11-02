
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(ggfx)
library(nngeo) # For st_connect

# Data Reading and Wrangling ----------------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)")) |> 
  st_transform(crs = 4326)


world_cities <- read_csv("https://gist.githubusercontent.com/curran/13d30e855d48cdd6f22acdf0afe27286/raw/0635f14817ec634833bb904a47594cc2f5f9dbf8/worldcities_clean.csv") |>  
  mutate(
    id = row_number(), .before = 1
  )

range(world_cities$population)

world_cities %>% 
  st_as_sf(coords = c("lng","lat"), crs = 4326) -> world_cities_sf

world_cities %>% 
  st_as_sf(coords = c("lng","lat")) -> world_cities_wo_sf

world <- world |>  
  select(continent)


sf_use_s2(FALSE)
world_cities_sf |> 
  st_join(world) |> 
  mutate(
    lng = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) -> world_cities_sf 

unique(world_cities_sf$continent)

(nn <- st_nn(world_cities_wo_sf, world_cities_wo_sf, k = 10, progress = F))

world_cities <- world_cities_sf |> 
  st_drop_geometry()

world_cities_nn <- world_cities |> 
  mutate(
    nn = nn, 
    .after = last_col()
  ) |> 
  unnest_longer(nn) |> 
  filter(id != nn)

closet_df <- world_cities_nn |> 
  left_join(world_cities, by = join_by(nn == id),  suffix = c("", "_closest")) |>
  filter(!is.na(continent), !is.na(continent_closest)) |> 
  mutate(
    diff_country = country != country_closest, 
    diff_continent = continent != continent_closest
  ) |> 
  rowwise() |> 
  mutate(
    cross_continents = case_match(
      diff_continent, 
      FALSE ~ continent, 
      TRUE ~ "different"
    )
  )

pts_cities <- world_cities |> 
  filter(city %in% c("Apia", "Cayenne", "Port-of-Spain", "Honolulu", "Perth", "Maina", "St.-Denis", "Papeete", "Santa Cruz de Tenerife")) 

# Graphic -----------------------------------------------------------------
closet_df |> 
  filter(!is.na(cross_continents)) |>
  ggplot() + 
  with_shadow(
    geom_segment(aes(x = lng, y = lat, xend = lng_closest, yend = lat_closest, color = cross_continents), linewidth = 0.25),
    colour = "white",
    x_offset = 0,
    y_offset = 0,
    sigma = 5  ) + 
  geom_point(data = pts_cities, aes(lng, lat, color = continent)) +
  labs(
    title  = "WORLD", 
    subtitle = "Cities with more than 50,000 inhabitants\nconnected to their 10 closest neighbors",
    caption = "#30DayMapChallenge - Day 2: Lines<br/> Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 6 Brands\"'>&#xe61b;</span>**@issa_madjid** <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>**github.com/abdoulma**." 
  ) + 
  scale_color_manual(
    values = c(
      "North America" = "#CC2929",
      "South America" = "#D7D700",
      "Europe" = "#01BF00",
      "Africa" = "#00A6A6",
      "Asia" = "#3193F2",
      "Oceania" = "#C361EB", 
      "different" = "#FFFFFF"
    ), 
    guide = "none"
    
  ) +
  coord_fixed(clip = "off") + 
  theme_minimal() + 
  theme(
    text = element_text(family = "UEFA Supercup", color = "#FFFFFF"),
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = rel(5.5), hjust = 0.1, margin = margin(t = 0.25, unit = "cm")),
    plot.subtitle = element_text(size = rel(1.25), hjust = 0.1),
    plot.caption = element_markdown( color = "#FFFFFF", hjust = 0.1, size = rel(1.25), margin = margin(t = 0.25, unit = "cm")),
    plot.background = element_rect(fill = "#111111", color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023/Day2", "day2")
ggsave(glue::glue("{path}.pdf"), width = 12, height = 12*9/16, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"), 
  filenames = glue::glue("{path}_polished.png"),
  dpi = 144
)

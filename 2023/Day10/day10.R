
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(nngeo)

# Data Reading and Wrangling ----------------------------------------------
counties_sf <- albersusa::counties_sf() |> 
  filter(!state %in% c("Alaska", "Hawaii")) |> 
  st_transform(crs = "ESRI:102005")

# Arena locations ----
teams_locations_df <- tibble::tribble(
  ~team, ~arena, ~latitude, ~longitude,
  "Atlanta Hawks", "State Farm Arena", 33.757222, -84.396389,
  "Boston Celtics", "TD Garden", 42.366303, -71.062228,
  "Brooklyn Nets", "Barclays Center", 40.682661, -73.975225,
  "Charlotte Hornets", "Spectrum Center", 35.225, -80.839167,
  "Chicago Bulls", "United Center", 41.880556, -87.674167,
  "Cleveland Cavaliers", "Rocket Mortgage FieldHouse", 41.496389, -81.688056,
  "Dallas Mavericks", "American Airlines Center", 32.790556, -96.810278,
  "Denver Nuggets", "Ball Arena", 39.748920, -105.008400,
  "Detroit Pistons", "Little Caesars Arena", 42.341111, -83.055,
  "Golden State Warriors", "Chase Center", 37.768056, -122.3875,
  "Houston Rockets", "Toyota Center", 29.750496998, -95.357331904,
  "Indiana Pacers", "Gainbridge Fieldhouse", 39.763889, -86.155556,
  "LA Clippers", "Crypto.com Arena", 34.043056, -118.267222,
  "Los Angeles Lakers", "Crypto.com Arena", 34.043056, -118.267222,
  "Memphis Grizzlies", "FedExForum", 35.138333, -90.050556,
  "Miami Heat", "FTX Arena", 25.781389, -80.188056,
  "Milwaukee Bucks", "Fiserv Forum", 43.045028, -87.918167,
  "Minnesota Timberwolves", "Target Center", 44.979595, -93.276566,
  "New Orleans Pelicans", "Smoothie King Center", 29.948889, -90.081944,
  "New York Knicks", "Madison Square Garden", 40.750556, -73.993611,
  "Orlando Magic", "Amway Center", 28.539167, -81.383611,
  "Oklahoma City Thunder", "Paycom Center", 35.463333, -97.515,
  "Philadelphia 76ers", "Wells Fargo Center", 39.901111, -75.171944,
  "Phoenix Suns", "Footprint Center", 33.445833, -112.071389,
  "Portland Trail Blazers", "Moda Center", 45.531667, -122.666667,
  "Sacramento Kings", "Golden 1 Center", 38.580361, -121.499611,
  "San Antonio Spurs", "AT&T Center", 29.426944, -98.4375,
  "Toronto Raptors", "Scotiabank Arena", 43.643333, -79.379167,
  "Utah Jazz", "Vivint Arena", 40.768333, -111.901111,
  "Washington Wizards", "Capital One Arena", 38.898129, -77.021172
) |>  
  mutate(
    id = row_number(), 
    .before = 1
  ) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(crs =  "ESRI:102005")

#  Teams Colors ----
teams_colors <- tibble(
  team = names(teamcolors::league_pal("nba")),
  primary_color = teamcolors::league_pal("nba"),
  secondary_color = teamcolors::league_pal("nba", 2)
)

teams_locations_df <- teams_locations_df |> 
  left_join(teams_colors) |> 
  mutate(
    primary_color = case_match(
      team, 
      "LA Clippers" ~ "#CC5365",
      "Oklahoma City Thunder" ~ "#EA3F27",
      "Denver Nuggets" ~ "#F9C225",
      "Detroit Pistons" ~ "#1D448C",
      "Toronto Raptors" ~ "#111111", 
      .default = primary_color
    )
  )

nn <- st_nn(counties_sf, teams_locations_df,  k = 1)

teams_locations_df <- teams_locations_df |> 
  mutate(
    long = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) |> 
  st_drop_geometry()


counties_nn <- counties_sf |>  
  mutate(
    nn = unlist(nn), 
    .after = last_col() 
  ) 
counties_nn |> 
  filter(nn == 1)

counties_teams <- counties_nn |>
  left_join(teams_locations_df, by = join_by("nn" == "id")) 

counties_teams <- counties_teams |> 
  rowwise() |>  
  mutate(location = map2(long, lat, ~ st_point(x = c(.x, .y))), 
         location = st_sfc(location, crs = "ESRI:102005")
  ) |>  
  mutate(
    distance = st_distance(st_centroid(geometry), location ),
    distance = str_remove(distance, "\\[m\\]"),
    distance = parse_number(distance)
  ) 

# Graphic -----------------------------------------------------------------
counties_teams |> 
  ggplot() + 
  geom_sf(aes(fill = I(primary_color), alpha = distance/1e5), color = NA, linewidth = 0.0125) +
  # geom_text(aes(long, lat, label = team ), stat = "unique") +
  # geom_text(data = teams_locations_df, aes(long, lat, label = team)) +
  labs(
    title = "NBA ARENAS", 
    subtitle = str_to_upper("Closest arena to U.S. Counties"),
    caption = "#30DayMapChallenge - Day 10: North America<br/> Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 6 Brands\"'>&#xe61b;</span>**@issa_madjid** <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>**github.com/abdoulma**." 
    
  ) + 
  scale_alpha_binned(
    range = c( 1, 0.1),
    breaks = seq(1, 10, 2), 
    guide = "none"
  ) +
  theme_void() + 
  theme( 
    text = element_text( color = "#111111", family = "UEFA Supercup"),
    plot.title = element_text(face = "bold", size = rel(3.5), hjust = 0, margin = margin(t = 0.5, unit = "cm")),
    plot.subtitle = element_text( color = "#444444", size = rel(1.5), hjust = 0),
    plot.caption = element_markdown(hjust = 0, size = rel(1), margin = margin(t = 0.25, unit = "cm")),
    plot.background =  element_rect(fill = "#F7F8F9", color = NA), 
    plot.margin = margin(c(0.25, 0,0.25,0.5), unit = "cm")
  )


# Saving ------------------------------------------------------------------
path <- here::here("2023/Day10", "day10")
ggsave(glue::glue("{path}.pdf"), width = 9, height = 6.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

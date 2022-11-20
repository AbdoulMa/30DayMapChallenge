
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(rvest)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
us_states <- albersusa::usa_sf() %>% 
  st_transform(albersusa::us_laea_proj) 

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
  "Houston Rockets", "Toyota Center", 41.880556, -87.674167,
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
  "Portland Trail Blazers", "Moda Center", 45.531667, -122.666667 ,
  "Sacramento Kings", "Golden 1 Center", 38.580361, -121.499611,
  "San Antonio Spurs", "AT&T Center", 29.426944, -98.4375,
  "Toronto Raptors", "Scotiabank Arena", 43.643333, -79.379167,
  "Utah Jazz", "Vivint Arena", 40.768333, -111.901111,
  "Washington Wizards", "Capital One Arena", 38.898129, -77.021172
)

#  Teams Colors ----
teams_colors <- tibble(
  team = names(teamcolors::league_pal("nba")), 
  primary_color = teamcolors::league_pal("nba"),
  secondary_color = teamcolors::league_pal("nba", 2)
)


# Scrape teams data ----
teams <- rjson::fromJSON(file = "http://site.api.espn.com/apis/site/v2/sports/basketball/nba/teams") 
teams_df <- pluck(teams, "sports", 1, "leagues", 1, "teams") |> 
  enframe() |> 
  select(value) |> 
  unnest_longer(value) |> 
  unnest_wider(value) |> 
  unnest_wider(logos, names_sep = "_") |> 
  hoist(logos_1, 
        logo_link = "href") |> 
  select(-where(is.list), -value_id) 

# Join with teams locations data
final_teams_df <- teams_df |> 
  left_join(teams_locations_df, by = c("displayName" = "team")) 


# Function to retrieve a team travels
team_travels <- function(team_id, team_short_name, team_abbr, team_slug) {

  retrieve_row_info <- function(row) {
    columns <- row |> 
      html_elements("td")
    
    date <- columns[1] |> 
      html_text()
    
    h_a <- columns[2] |> 
      html_element("span.pr2") |> 
      html_text2()
    
    opponent_link <- columns[2] |> 
      html_element("span a.AnchorLink") |> 
      html_attr("href")
    
    tibble(
      date, 
      h_a,
      opponent_link
    )
  }
  
  schedule_rows <- read_html(glue::glue('https://www.espn.com/nba/team/schedule/_/name/{team_abbr}/{team_slug}')) |> 
    html_element("table.Table") |> 
    html_elements("tbody tr") 
  
  map_dfr(schedule_rows, retrieve_row_info) |> 
    drop_na(h_a) |> 
    mutate(
      year  = ifelse(str_detect(date, "Oct|Nov|Dec"), 2022, 2023),
      date  = paste0(date, " ", year), 
      date = parse_date(date, format = "%a, %b %d %Y")) |> 
    mutate(
      team_id = team_id, 
      team_short_name = team_short_name,
      .before = "date"
    ) |> 
    select(-year)
}

# Retrieve teams travels ----
teams_travels_df <- teams_df |> 
  select(id, shortDisplayName, abbreviation, slug) |> 
  pmap_dfr(~team_travels(..1,..2,..3,..4)) |> 
  tidyr::extract(col = opponent_link, regex = ".*/(.*)/(.*)$", into = c("opp_abbreviation", "opp_slug"))


teams_selection_df <- final_teams_df |> 
  select(id, displayName, abbreviation,logo_link, longitude, latitude) |> 
  mutate(
    displayName = case_when(
      displayName == "LA Clippers" ~ "Los Angeles Clippers", 
      TRUE ~ displayName
    )
  ) 

# Add teams colors data ----
teams_selection_df <- teams_selection_df |> 
  left_join(teams_colors, by = c("displayName" = "team")) |> 
  replace_na(list(secondary_color = "#FFFFFF"))

opponent_selection_df <- final_teams_df |> 
  select(abbreviation, longitude, latitude)

teams_moves <- teams_travels_df |> 
  mutate(opp_abbreviation = str_to_upper(opp_abbreviation)) |> 
  left_join(teams_selection_df, by = c("team_id" = "id")) |> 
  left_join(opponent_selection_df, by = c("opp_abbreviation" = "abbreviation"), suffix = c("_main", "_opp"))

# Detect if team plays at home / away ----
teams_moves <- teams_moves |> 
  rowwise() |> 
  mutate(
    longitude = ifelse(h_a == "@", longitude_opp, longitude_main),
    latitude = ifelse(h_a == "@", latitude_opp, latitude_main)
  ) 

# Trasnform to a better projection for USA ----
teams_moves <- teams_moves |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(albersusa::us_laea_proj) |> 
  mutate(
    long = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  )
 
# For each team, define its next game location ----
teams_moves <- teams_moves |> 
  group_by(team_id) |> 
  mutate(
    next_long = lead(long),
    next_lat = lead(lat)
  ) 

# Compute moves distances and an approximate time ----
final_teams_moves <- teams_moves |> 
  group_by(team_id) |> 
  mutate(
    next_long = ifelse(is.na(next_long), long, next_long),
    next_lat = ifelse(is.na(next_lat), long, next_lat)
  ) |> 
  mutate(
    location = map2(long, lat, ~st_point(x = c(.x,.y))),
    location_sf = st_sfc(location, crs = albersusa::us_laea_proj),
    next_location = map2(next_long, next_lat, ~st_point(x = c(.x,.y))),
    next_location_sf = st_sfc(next_location,crs = albersusa::us_laea_proj)
  ) |> 
  rowwise() |> 
  mutate(
    distance = st_distance(location_sf, next_location_sf),
    distance = str_remove(distance, "\\[m\\]"), 
    distance = parse_number(distance), 
    distance_km = distance / 1000, 
    
  ) |>
  group_by(team_id) |> 
  mutate(
    sum_distance = sum(distance_km),
    sum_distance_miles = 0.621371 * sum_distance, # Convert in miles
    nb_hours = sum_distance_miles / 517.5, # Average flight speed 
    # Team summary 
    team_summary = glue::glue("<img src='{logo_link}' width='30'/><br> <span ><span style='font-size: 18px;line-height: 20px;'>{displayName}</span> <br> <span style='font-size: 15.5px;line-height: 17px;'>{scales::comma(round(sum_distance_miles, 2))}  Miles</span><br> <span style='font-size: 15px; color: #555555; line-height: 16px;'>{scales::comma(round(nb_hours , 0))}  H</span></span>")
  ) 

travels_summary <- final_teams_moves |> 
  ungroup() |> 
  distinct(team_id, sum_distance_miles, nb_hours) |> 
  summarise(
    mean_distance_miles = mean(sum_distance_miles),
    mean_nb_hours = mean(nb_hours)
  )

mean_distance <-  travels_summary$mean_distance_miles 
mean_nb_hours <-  travels_summary$mean_nb_hours

# Graphic -----------------------------------------------------------------
(plot <- ggplot() + 
  geom_sf(data = us_states, fill = "#D9D9D9", color = "grey35",  size = .1)  + 
  geom_path(data = final_teams_moves, aes(x = long, y = lat, color = I(primary_color), group = team_id), size = .85, alpha = .85) +
  geom_point(data = final_teams_moves, aes(long, lat, color = I(primary_color), fill = I(secondary_color)), shape = 21,  size = 1.75) + 
  labs(
    title = str_to_upper("NBA franchise trips"), 
    subtitle = glue::glue("A NBA player spends on average about {scales::comma(round(mean_distance))} miles for {round(mean_nb_hours)} H traveling over a season"), 
    caption = "Data from **espn.com**<br>**#30DayMapChallenge**<br>Day 20 : My favorite · Abdoul ISSA BIDA."
  ) +
  ggh4x::facet_wrap2(vars(team_summary), ncol = 6, strip = ggh4x::strip_vanilla(clip = "off")) + 
  theme_minimal() + 
  theme(
    text = element_text(family =  "UEFA Supercup"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = rel(3.5)),
    plot.subtitle = element_text(color = "#555555", face = "bold", hjust = 0, size = rel(1.75)),
    plot.caption = ggtext::element_markdown(size = rel(1.5)),
    strip.text = element_markdown(family = "UEFA Supercup", face = "bold"),
    panel.spacing.x = unit(1, "cm"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(c(1,.75,1,.75), unit = "cm")
  )
)

# Saving ------------------------------------------------------------------
path <- here::here("Day20", "day20")
ggsave(glue::glue("{path}.png"), width = 13.5, height = 13.5, device = ragg::agg_png, dpi = 640)

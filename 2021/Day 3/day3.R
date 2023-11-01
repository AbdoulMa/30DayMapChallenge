
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
tw_countries <- c("Canada", "United States", "United States of America","Costa Rica", "Brazil", "Argentina", "United Kingdom",
                  "Tunisia", "Spain", "Nigeria", "Ethiopia","South Africa", "Russia","Mongolia","Vietnam", "Malaysia","Australia")

tw_percentages <- tibble(
  country = tw_countries, 
  pct = c(39.2, 35.4, 35.4, 19.8, 10.7, 22.2,33.8,
          13.9, 16.2, 25.8, 15.9, 12.4, 23,11.5, 10.8, 36.1, 41)
  
)

#  Read countries centers coordinates 
countries_centers <- rjson::fromJSON(file = "https://gist.githubusercontent.com/rosszurowski/415eb3175249fff3714b14c1c51582cc/raw/97afcea125cb3ce31fa5fadf4e8aac102564f064/un-country-centroids.json") %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value) %>% 
  mutate(name = ifelse(name == "Russian Federation", "Russia", name)) %>% 
  filter(
    name %in% tw_countries
  ) %>% 
  left_join(tw_percentages, by = c("name" = "country")) %>% 
  rename( x = long, 
          y = lat) %>% 
  relocate(x, .before = y) %>% 
  mutate(
    x = case_when(name == "Spain" ~ x - 15,
                  name == "United Kingdom" ~ x -20,
                  name == "Costa Rica" ~ x +5,
                  name == "Tunisia" ~ x +5,
                  name == "Ethiopia" ~ x +10,
                  name == "Vietnam" ~ x +5,
                  TRUE ~ x),
    y = case_when(name %in% c("Nigeria","South Africa") ~ y - 9.5,
                  name == "Russia" ~ y + 5,
                  name == "Vietnam" ~ y + 7.5,
                  name == "Costa Rica" ~ y +2.5,
                  name == "United Kingdom" ~ y + 3,
                  TRUE ~ y)
  )


sf_world <- st_as_sf(rworldmap::getMap(resolution = "low")) 
sf_world <- sf_world %>% 
  filter(NAME != "Antarctica")


tw_countries_df <- sf_world %>% 
  filter(NAME %in% tw_countries) %>% 
  select(NAME, geometry) %>% 
  left_join(tw_percentages, by=c("NAME" = "country"))



# Graphic -----------------------------------------------------------------
ggplot() + 
  geom_sf(data = sf_world, fill = "#111111", color = "#686868",size = .075) + 
  geom_sf(data = tw_countries_df, fill = "#8C9175", color = NA ) + 
  geom_text(data = countries_centers, aes(x, y, label = name), hjust = 0, color = "white", size = 2, fontface = "bold", family = "Bahnschrift") + 
  geom_rect(data = countries_centers, aes(xmin = x, xmax = x +(50 * pct)/100,ymin = y - 3.5, ymax = y -6.5), fill = "#FDE14F") + 
  geom_segment(data = countries_centers, aes(x =  x +(50 * pct)/100, xend = x +(50 * pct)/100, y = y -8, yend = y -3.5),
               size = .025,
               color = "white",
               linejoin = "round",
               lineend = "round"
  ) +
  geom_text(data = countries_centers, aes(x = x +(50 * pct)/100,  y = y -8, label = glue::glue("{pct}%")),
            size = 1.75,
            family = "Mercury",
            fontface = "bold",
            color = "white",
            vjust = 1.5
  ) + 
  theme_minimal() + 
  labs(
    x = NULL, 
    y = NULL,
    caption = "Data from International Labor Organization and ABS. \n#30DayMapChallenge Day 3 : Polygons ⋅ Abdoul ISSA BIDA inspired by Lemonde"
  ) + 
  theme(
    plot.caption = element_text(color = "white", face = "bold", family = "Gotham Book", hjust = .5, size = 7.5,margin = margin(t = 20,b = 15)),
    plot.background = element_rect(fill = "#343434", color = NA),
    plot.margin = margin(95,-10,  0, -10),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )



# Saving ------------------------------------------------------------------
path <- here::here("Day3", "day3")
ggsave(glue::glue("{path}.pdf"), width = 6.5, height = 4.75, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"), 
  filenames = glue::glue("{path}_polished_twitter.png"),
  dpi = 320
)


# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(maps)

# Data Reading and Wrangling ----------------------------------------------
# Generating pixel grid 
lat <- tibble(lat = seq(-90, 90, by = .5))
long <- tibble(long = seq(-180, 180, by = .5))

dots <- lat %>% 
  merge(long, all = TRUE)

dots <- dots %>% 
  mutate(country = map.where('world', long, lat),
         lakes = map.where('lakes', long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  dplyr::select(-lakes)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(region_wb != "Antarctica")

dots <- dots %>% 
  mutate(
    country = case_when(
      country %in% c("Comoros:Grande Comore", "Comoros:Anjouan") ~ "Comoros",
      country %in% c("Cape Verde:Santo Antao","Cape Verde:Sao Tiago") ~ "Cape Verde",
      country == "Guinea-Bissau" ~ "Guinea Bissau", 
      country == "Equatorial Guinea:Rio Muni" ~ "Equatorial Guinea", 
      country ==  "Malawi:3" ~ "Malawi",
      country == "Somalia:Somaliland" ~ "Somaliland",
      country == "Tanzania" ~ "United Republic of Tanzania", 
      TRUE ~ country))

africa_dots <- world %>%
  filter(continent == "Africa") %>% 
  dplyr::select(admin, mapcolor7) %>% 
  left_join(dots, by = c("admin" = "country")) 


# Graphic -----------------------------------------------------------------
palette <- c("#8931EF", "#F2CA19", "#FF00BD" , "#0057E9", "#87E911", "#E11845", "#FF8A12") 
africa_dots %>% 
  ggplot() + 
  geom_point(aes(x = long, y = lat, color = factor(mapcolor7)), size = .95) + 
  guides(color ="none") +
  scale_color_manual(
    values = palette
  ) + 
  labs(
    title = "AFRICA", 
    subtitle = glue::glue("1 continent, {nrow(africa_dots)} points"), 
    caption = "#30DayMapChallenge - Day 1 : Points · Abdoul ISSA BIDA"
  ) + 
  annotate(geom = "text", x = -23.5, y = 11, label = "Cape Verde", size = 5, color= "white", family = "Mercury", fontface = "bold.italic") + 
  annotate(geom = "segment", x = -23.5, y = 12, xend = -23.5, yend = 14.5, size = .125, color = "white") + 
  coord_fixed(clip = "off") + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#111111",color = NA),
    plot.title = element_text(hjust = .5, size = rel(3), color = "white", family = "Gotham Black", margin = margin(t = 25,b = 15)),
    plot.subtitle = element_text(hjust = .5, size = rel(2.5), color = "white",family = "Mercury", face = "bold", margin = margin(b = 35)),
    plot.caption = element_text(size = rel(1.125), hjust = .5, color = "white", family = "Gotham Medium", margin = margin(b = 15))
  )



# Saving ------------------------------------------------------------------
path <- here::here("Day1", "day1")
ggsave(glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

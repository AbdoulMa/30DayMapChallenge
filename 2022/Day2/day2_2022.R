
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(nngeo)

# Data Reading and Wrangling ----------------------------------------------
airports <- read_csv("https://gist.githubusercontent.com/fletchjeff/b60f46ca3c1aa51e9aea9fd86d0ab433/raw/81ee446939110daf8b5c3a79425ccf1100070578/airports.csv") |> 
  mutate(row_num= row_number()) |>
  st_as_sf(coords =c("Longitude", "Latitude"))

# African airports
airports_africa <- airports |> 
  filter(str_detect(`Tz database time zone`,'Africa|Indian/Antananarivo'))

# Nearest neighbours
nn <- st_nn(airports_africa, airports_africa, k = 10, progress = F)
airports_connexions <- st_connect(airports_africa, airports_africa, ids = nn, progress = F)

# Graphic -----------------------------------------------------------------
airports_connexions |> 
  ggplot() + 
  geom_sf(size = .75) + 
  labs(
    title = "AIRPORTS",
    subtitle = "Every African airport connected to its 10 nearest neighbors", 
    caption = "Data from Jeff Fletcher\n 30DayMapChallenge Day 2 : Lines ⋅ Abdoul ISSA BIDA"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Gotham Black", size = rel(4)),
    plot.subtitle = element_text(family = "Gotham Narrow", size = rel(1.5), margin = margin(b = .25, unit = "cm")),
    plot.caption = element_text(family = "Gotham Narrow", size = rel(1.25), margin = margin(t = .5, unit = "cm")),
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    plot.background = element_rect(fill = "#E8E8E8", color = NA), 
    plot.margin = margin(c(.75,.5,.75,.5), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day2", "day2_2022")
ggsave(glue::glue("{path}.pdf"), width = 9.5, height = 10.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}_twitter.png"),
  dpi = 196
)

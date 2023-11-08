
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)")) |> 
  st_transform(crs = 4326)


african_countries <- world |> 
  filter(continent == "Africa") |> 
  select(admin) |> 
  rowwise() |> 
  mutate(
    w = st_bbox(geometry)[3] - st_bbox(geometry)[1], 
    h = st_bbox(geometry)[4] - st_bbox(geometry)[2])

dir_path <- path <- here::here("2023","Day8", "Countries")
if(!fs::dir_exists(here::here(dir_path))) { 
  fs::dir_create(here::here(dir_path))
}




# Graphic -----------------------------------------------------------------
width  <- 6
for (country_name in african_countries$admin) { 
  country <- african_countries |> 
    filter( admin == country_name)
  country_name <- case_match(
    country_name,
    "Central African Republic" ~ "C.R.A", 
    "Democratic Republic of the Congo" ~ "D.R.C.",
    "United Republic of Tanzania" ~ "Tanzania", 
    "Sao Tome and Principe" ~ "Sao Tome and P.",
    .default = country_name
  )
  ratio <- country$w / country$h
  
  height <- width / ratio
  
  plot <- country |> 
    ggplot() + 
    geom_sf(fill = "#111111", color = NA) + 
    theme_void() + 
    theme(
      plot.background = element_rect(fill = "#FFFFFF", color = NA) 
    )
  
  country_name <- str_to_lower(country_name)
  country_name <- str_replace_all(country_name, " ", "_")
  ggsave(here::here(dir_path, paste0(country_name, ".png")), plot = plot, width = width, height = height, dpi = 240, device = ragg::agg_png)
}

# Additional Assembly made with Illustrator
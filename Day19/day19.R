
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(rayrender)

# Data Reading and Wrangling ----------------------------------------------
# World countries borders
country_borders <- read_sf(world_shape_file_path)

# 2022 WC Countries with group
wc_groups <- tribble(
  ~group, ~country,
  "Group A", c("Qatar", "Ecuador", "Senegal", "Netherlands"),
  "Group B", c("England", "Iran", "USA", "Wales"),
  "Group C", c("Argentina", "Saudi Arabia", "Mexico", "Poland"),
  "Group D", c("France", "Australia", "Denmark", "Tunisia"),
  "Group E", c("Spain", "Costa Rica", "Germany", "Japan"),
  "Group F", c("Belgium", "Canada", "Morocco", "Croatia"),
  "Group G", c("Brazil", "Serbia", "Switzerland", "Cameroon"),
  "Group H", c("Portugal", "Ghana", "Uruguay", "South Korea")
) |> 
  rowwise() |> 
  unnest_longer(country) |> 
  mutate(
    name_en = case_when(
      country == "England" ~ "United Kingdom", 
      country == "Wales" ~ "United Kingdom", 
      country == "USA" ~ "United States of America", 
      TRUE ~ country
    )
  )

# Join to have countries with sf
wc_countries <- left_join(select(country_borders, NAME_EN), wc_groups, by = c("NAME_EN" = "name_en"))

# Graphic -----------------------------------------------------------------
# Groups colors palette
pal <- c("#C62828", "#D68400", "#FDFD00","#00A700","#00C2C2", "#297CCA", "#9048A4", "#E5BAE5")

wc_countries |> 
  ggplot() + 
  geom_sf(aes(fill = group), size = .005) +  
  coord_sf(expand = F) + 
  scale_fill_manual(
    values = pal,
    na.value = "#E9E5DC", 
    guide = "none"
  ) +
  theme_minimal() + 
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(), 
    plot.background = element_rect(fill = "#888888", color = NA),
    plot.margin = margin(0, unit = "pt")
  )

# ave the image texture for the globe
ggsave(here::here("Day19", "2k_countries.png"), width = 2048, height = 1024, units = "px",device = ragg::agg_png, dpi = 720)

# Initial idea was animation but my computer is not enough powerful
for(i in seq(1,720,by=270)) { 
  add_object(sphere(radius=0.99,material=diffuse(image_texture = here::here("Day19", "2k_countries.png")),angle=c(0,-90,0))) %>%
    group_objects(angle=c(0,-i/2,0)) %>% 
    add_object(sphere(y=10,z=5,radius=3,material=light(intensity = 20))) %>%
    add_object(sphere(y=0,z=20,radius=3,material=light(intensity = 20))) %>%
    render_scene(samples=64,width=1200,height=1200,fov=0,aperture=0, ortho_dimensions = c(2.3,2.3),
                 sample_method = "sobol_blue",filename=here::here("Day19", glue::glue("wc_teams_{i}.png")))
}

# Put groups labels with montage
system("./Day19/groups_labelling.sh")

# Additional annotations with Illustrators 
# Saving ------------------------------------------------------------------
path <- here::here("Day19", "day19")
pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"),
  filenames = glue::glue("{path}_polished.png"),
  dpi = 300
)

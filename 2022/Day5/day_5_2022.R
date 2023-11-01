
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
country_borders <- read_sf(world_shape_file_path) |> 
  filter(SOVEREIGNT != "Antarctica")
  
# Data cleaned 
famous_ukrainians <- readRDS(famous_ukrainians_data_path)

# Some columns selection
famous_ukrainians <- famous_ukrainians |> 
  select(Label,level1_main_occ, sum_visib_ln_5criteria, ranking_visib_5criteria, bplo1, bpla1)

ukraine_sf <- read_sf(ukraine_adm1_file_path) |> 
  mutate(
    region_center_long = st_coordinates(st_centroid(geometry))[,1],
    region_center_lat = st_coordinates(st_centroid(geometry))[,2]
  ) |> 
  select(ADM1_EN, starts_with("region_center"))

# Keep only those born in Ukraine 
famous_born_ukrainians <- st_join(famous_ukrainians, ukraine_sf)

# Most famous by region
famous_by_region <- famous_born_ukrainians |> 
  group_by(ADM1_EN) |> 
  slice_min(order_by = ranking_visib_5criteria, n = 1) |> 
  mutate(
    region_center_lat = case_when( ADM1_EN == "Kyivska" ~ region_center_lat - .35,
                                   ADM1_EN == "Kyiv" ~ region_center_lat + .4,
                                   TRUE ~ region_center_lat),
    region_center_long = case_when( ADM1_EN == "Volynska" ~ region_center_long - .35,
                                    ADM1_EN == "Odeska" ~ region_center_long + .15,
                                    TRUE ~ region_center_long)
  )

# Some adjustments
famous_by_region <- famous_by_region |> 
  mutate(
    ratio = case_when(
      ADM1_EN %in% c("Ternopilska", "Khmelnytska") ~ .8,
      ADM1_EN %in% c("Ivano-Frankivska") ~ .85,
      TRUE ~ .95
    ),
    fancy_ADM1_EN = str_wrap(ADM1_EN, width = 10),
    fancy_ADM1_EN = str_replace_all(fancy_ADM1_EN,"\n", "<br>"),
    fancy_ADM1_EN = str_to_upper(fancy_ADM1_EN),
    fancy_label = str_wrap(Label, width = 10), 
    fancy_label = str_replace_all(fancy_label,"\n", "<br>"),
    fancy_label = glue::glue('<span style="font-size:{9*ratio}pt;">**{fancy_label}**</span><br><span style="font-size:{7*ratio}pt;">*{level1_main_occ}*</span><br><span style="font-size:{8*ratio}pt;">**{fancy_ADM1_EN}**</span>')
  )


# Graphic -----------------------------------------------------------------
ggplot() + 
  geom_sf(data = country_borders, size = 1, color = "white", fill = "#FFDD00") +
  geom_sf(data = ukraine_sf, color = "#111111", fill = "#0057B7", size = .55) + 
  geom_richtext(data = famous_by_region, aes(
    x = region_center_long,
    y = region_center_lat,
    label = fancy_label),
    family = "Gotham Narrow",
    label.r = unit(0,'pt'),
    label.padding = unit(0,'pt'),
    label.size = unit(0,'pt'),
    label.color = NA,
    color = "white",
    fill = NA,
    lineheight = .5
  ) + 
  labs(
    title = "The most famous native of each Ukrainian region",
    caption = "Data from Wikipedia & Wikidata\n #30DayMapChallenge Day 5 : Ukraine · Abdoul ISSA BIDA."
  ) + 
  coord_sf(xlim = c(21.5,41), ylim = c(44,53), expand = F) + 
  theme_minimal()  + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    plot.title = element_text(family = "NY Bold", size = rel(2.5)),
    plot.caption = element_text(family = "Gotham Narrow", hjust = 0, size = rel(1.125)),
    plot.margin = margin(c(.5, .5, .5, .5), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day5", "day5_2022")
ggsave(glue::glue("{path}.png"), width = 9.5, height =7.5, device = ragg::agg_png, dpi = 640)

# Additional annotations with Illustrator 
pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"), 
  filenames = glue::glue("{path}_polished_twitter.png"),
  dpi = 216
)

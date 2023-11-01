
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
players_df <- rvest::read_html("https://www.lequipe.fr/Football/FootballFicheSelection165.html") |> 
  rvest::html_elements("section .data-content table") |>  
  purrr::pluck(2) |> 
  rvest::html_table() |> 
  select(-1) |> 
  as_tibble() |> 
  janitor::clean_names()

players_df$birth_town <- c("Paris", "Nice", "Kinshasa(RDC)", "Gonesse", "Marseille", 
                        "Marseille", "Paris", "Paris", "Maubeuge", "Bondy", "Évreux", "Lille", "Miconje(AGO)",
                        "Paris", "Paris", "Poissy", "Saint-Maurice", "Rouen", "Ancenis-Saint-Géréon",
                        "Lyon","Vernon", "Chambéry", "Mâcon", "Bondy", "Paris", "Parme(ITA)")

players_df <- players_df |> 
  mutate( 
    last_name = str_extract(nom, "(?<=([A-Z]\\.\\s))(.*)$")
    )
  
regions <- read_sf(regions_shp_path)
(regions <- regions %>% 
    filter(!code_insee %in% c("01","02","03","04","06")) # Exclusion des communes d'Outre-Mer
)


  

communes <- read_sf(communes_shp_path)

players_communes <- communes |>  
  inner_join(players_df, by = c("nom" = "birth_town"), suffix = c("_commune", "_player"))

players_regions <- st_join(regions, players_communes, suffix = c("_region", "_commune"))

players_regions <- players_regions |> 
  filter(case_when(
    str_detect(nom_player,"Rabiot") ~nom_region == "Île-de-France",
    str_detect(nom_player,"Dembélé") ~nom_region == "Normandie",
    str_detect(nom_player,"Griezmann") ~nom_region == "Bourgogne-Franche-Comté",
         TRUE ~  TRUE),
    !is.na(nom_player)
    ) 

players_group_regions <- players_regions |> 
  group_by(nom_region, geometry) |> 
  arrange(last_name) |> 
  summarise(
    natives = list(glue::glue("{nom_player} (**{nom_commune}**)"))
  ) |> 
  ungroup() |> 
  mutate(
    fancy_nom_region = str_replace_all(str_wrap(nom_region, width = 20), "\n", "<br>"),
    fancy_natives = map_chr(natives, ~  str_c(paste0("<span style='font-size: 13.5px;'>",.,"</span>"), collapse = "<br>")), 
    fancy_natives = paste(glue::glue("<span style='font-size: 16.5px; color:#002F79;'>**{str_to_upper(fancy_nom_region)}**</span>"), fancy_natives, sep = "<br>"),
    region_center_long = st_coordinates(st_centroid(geometry))[,1],
    region_center_lat = st_coordinates(st_centroid(geometry))[,2], 
    region_center_long = case_when( 
      nom_region %in% c("Normandie", "Pays de la Loire", "Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur") ~ region_center_long - 1.95,
      nom_region %in% c("Bourgogne-Franche-Comté") ~ region_center_long + 1.6,
      nom_region %in% c("Île-de-France") ~ region_center_long -.25,
      TRUE ~ region_center_long
      ),
    region_center_lat = case_when( 
      nom_region %in% c("Hauts-de-France") ~ region_center_lat + .5,
      nom_region %in% c("Île-de-France") ~ region_center_lat -1.15,
      nom_region %in% c("Auvergne-Rhône-Alpes") ~ region_center_lat -.35,
      TRUE ~ region_center_lat
      )
  )


# Graphic -----------------------------------------------------------------
born_abroad_players <- players_df |> 
  filter(str_detect(birth_town,"\\(.*\\)$")) |> 
  mutate(birth_town = str_replace(birth_town, "\\((.*)\\)$", " - \\1")) |> 
  mutate(fancy_birth_town = glue::glue("{nom} (**{birth_town}**)")) |> 
  pull(fancy_birth_town)

born_abroad_players <- paste0(born_abroad_players, collapse = "<br>")


born_abroad_players_label <- paste0("<span style='font-size: 17.5px; color:#002F79;'>**BORN ABROAD**</span><br>", born_abroad_players)

ggplot() + 
  geom_sf(data = regions, fill = "#E0E0E0", size = .125) + 
  geom_sf(data = players_regions, fill = "#002F79", color = "grey80", size =0.25) + 
  ggtext::geom_richtext(data = players_group_regions, aes(
    x = region_center_long, 
    y = region_center_lat, 
    label = fancy_natives
      ),
    family = "Gotham Narrow",
    hjust = 0,
    label.padding = unit(3.5, "pt"),
    label.r = unit(0, "pt"),
    label.size = unit(0, "pt"),
    label.color = NA,
    lineheight = 1.25
    ) + 
  annotate(geom = "richtext", x = -7, y = 46, label = born_abroad_players_label,
           hjust = 0,
           family = "Gotham Narrow",
           label.padding = unit(3.5, "pt"),
           label.r = unit(0, "pt"),
           label.size = unit(0, "pt"),
           label.color = NA,
           lineheight = 1.25
           ) + 
  labs(
    title = "Birthplaces of FRANCE FIFA WC 2022 players", 
    caption = "Data from **lequipe.fr**.<br> #30DayMapChallenge - Day 18 : Blue · Abdoul ISSA BIDA."
  ) + 
  coord_sf(clip = "off") + 
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Gotham Narrow Bold", hjust = .45,  size = rel(2.25)),
    plot.caption = ggtext::element_markdown(family = "Gotham Narrow", size = rel(1.25)),
    plot.background = element_rect(fill = "#ECF0F1", color = NA), 
    plot.margin = margin(c(1, .5, 1, .5), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day18", "day18")
ggsave(glue::glue("{path}.png"), width = 9.5, height = 9.5, device = ragg::agg_png, dpi = 640)


pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"), 
  filenames = glue::glue("{path}_polished.png"),
  dpi = 300
)

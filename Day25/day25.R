
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
country_borders <- read_sf(world_shape_file_path) |>
  select(NAME_EN) |>
  filter(NAME_EN != "Antarctica") |>
  st_transform(crs = st_crs("ESRI:54030")) # Robinson Projection

(world_masculinity_df <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_countries_by_sex_ratio") |>
  rvest::html_element("table.wikitable") |>
  rvest::html_table() |>
  as_tibble() |>
  janitor::clean_names() |>
  select(1, last_col()) |>
  mutate(
    country_region = str_remove_all(country_region, "\\(.*\\)"),
    country_region = str_trim(country_region),
    country_region = case_when(
      country_region == "Congo, Democratic Republic of the" ~ "Democratic Republic of the Congo",
      country_region == "Congo, Republic of the" ~ "Republic of the Congo",
      country_region == "Bahamas, The" ~ "The Bahamas",
      country_region == "China" ~ "People's Republic of China",
      country_region == "Curacao" ~ "Curaçao",
      country_region == "Czechia" ~ "Czech Republic",
      country_region == "Eswatini" ~ "eSwatini",
      country_region == "Gambia, The" ~ "The Gambia",
      country_region == "Korea, North" ~ "North Korea",
      country_region == "Korea, South" ~ "South Korea",
      country_region == "Micronesia, Federated States of" ~ "Federated States of Micronesia",
      country_region == "Saint Barthelemy" ~ "Saint-Barthélemy",
      country_region == "Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena",
      country_region == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
      country_region == "Timor-Leste" ~ "East Timor",
      country_region == "United States" ~ "United States of America",
      country_region == "Virgin Islands" ~ "United States Virgin Islands",
      country_region == "West Bank/Palestine" ~ "Palestine",
      country_region == "Gaza Strip/Palestine" ~ "Palestine",
      country_region == "North Macedonia" ~ "Republic of Macedonia",
      TRUE ~ country_region
    )
  )
)


countries_sex_ratio <- country_borders |>
  left_join(world_masculinity_df, by = c("NAME_EN" = "country_region"))

# Graphic -----------------------------------------------------------------
countries_sex_ratio |>
  ggplot() +
  geom_sf(aes(fill = total > 1), color = "grey25", size = .1) +
  labs(
    title = 'Countries where there are more <span style="color: #0085ca;">Men</span> or more <span style="color:#f24a4a;">Women</span>',
    caption = "Data from Wikipedia\n#30DayMapChallenge\nDay 25 : 2 colours Map · Abdoul ISSA BIDA."
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#0085ca",
      "FALSE" = "#f24a4a"
    ),
    guide = "none",
    na.value = "grey55"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    plot.title = ggtext::element_markdown(color = "#FFFFFF", hjust = .5, family = "Ideal Sans Bold", margin = margin(c(.25, 0, .25, 0), unit = "cm"), size = rel(3)),
    plot.caption = element_text(color = "#FFFFFF", family = "Ideal Sans Bold", hjust = .5, size = rel(1.15)),
    plot.background = element_rect(fill = "#292929", color = NA),
    plot.margin = margin(c(0, -1.25, 0, -1.25), unit = "cm"),
    panel.grid = element_blank()
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day25", "day25")
ggsave(glue::glue("{path}.png"), width = 12.5, height = 7.5, device = ragg::agg_png, dpi = 300)


# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(patchwork)

# Data Reading and Wrangling ----------------------------------------------
# Scrape data
timeline_df <- rvest::read_html("https://en.wikipedia.org/wiki/Timeline_of_space_travel_by_nationality") |> 
  rvest::html_element(".wikitable") |> 
  rvest::html_table()

# Clean data
expeditions_df <- timeline_df |>  
  janitor::clean_names() |> 
  filter(!str_detect(no, "\\d{4}s")) |> 
  mutate(across(.cols = c("country", "name"), ~ str_remove_all(., "(\\[.\\])+$"), .names = "{.col}")) |> 
  mutate(date = parse_date(date_utc, format = "%d %B %Y"), 
         decade = (lubridate::year(date) %/% 10) * 10, 
         decade = as.factor(decade))

# Clean countries names to join
expeditions_df <- expeditions_df |> 
  mutate(
    country = case_when(
      country == "Soviet Union" ~ "Russia", 
      country == "United States" ~ "United States of America", 
      country == "Czechoslovakia" ~ "Czechia", 
      str_detect(country, "Germany") ~ "Germany", 
      TRUE ~ country
    )
  ) |> 
  distinct(country, .keep_all =  T)

world_countries <- select(country_borders, ADMIN, ISO_A3)  |> 
  st_transform(crs = "ESRI:54030")

expeditions_sf <- world_countries |> 
  right_join(expeditions_df, by = c( "ADMIN" = "country"))

# Graphic -----------------------------------------------------------------

colors_pal <- c("#DB2C2C", "#D07D00", "#A1A100", "#009500", "#00B0B0", "#2061A0", "#834198")
countries_plot <- ggplot() + 
  geom_sf(data = world_countries, size = 0) +
  geom_sf(data = expeditions_sf, aes(fill = decade), size = .05) + 
  scale_fill_manual(
    values = colors_pal,
    guide = "none"
  ) + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#111111", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(b = 0 )
  )

expeditions_sf |> 
  group_by(decade) |> 
  arrange(date) |> 
  mutate(
    num = row_number(),
    .after = "no"
  ) |> 
  select(-geometry) |> 
  mutate(
    ISO_A3 = case_when(ADMIN == "France" ~ "FRA", 
                       TRUE ~ ISO_A3)
  )-> travelers_df

travelers_plot <- ggplot()  + 
  ggtext::geom_textbox(
    data = travelers_df, 
    aes(y = num,label = paste0(name," (**",ISO_A3,"**)")), 
    color = "white",
    family = "UEFA Supercup",
    fill = NA,
    hjust = 0,
    vjust = .5,
    lineheight = 1,
    x = 0,
    size = 4.25, 
    box.colour = NA, 
    width = unit(0.9, "npc")
    
  ) + 
  geom_label(
    data = travelers_df,
    aes(label  = paste0(decade,"s"), fill = decade), 
    color = "white",
    family = "UEFA Supercup",
    fontface = "bold",
    y = 0, 
    x = 0.5, 
    hjust = 0.7, 
    vjust = 0,
    label.size = unit(0, 'pt'), 
    label.r = unit(0, 'pt'),
    label.padding =  unit(7.5, 'pt'),
    stat = "unique", 
    size = 9
  ) +
  coord_cartesian(expand =  F, clip = "off",
                  xlim = c(0, 1), ylim =c(14,-1)) + 
  scale_fill_manual(
    values = colors_pal,
    guide = "none"
  ) +
  scale_y_reverse() +
  facet_wrap(vars(decade), nrow = 1)+ 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    strip.text = element_blank()
  )

# Combine plots
countries_plot / travelers_plot  + 
  plot_annotation(
    caption = "Data from **Wikipedia** <br> #30DayMapChallenge Day 9 : **Space** · Abdoul ISSA BIDA."
  ) + 
  plot_layout(heights = c(.45, .55)) & 
  theme_minimal() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#151515", color =NA), 
    plot.caption = ggtext::element_markdown(color = "white", family = "UEFA Supercup", hjust = .5, size = rel(1.25), margin = margin(b = .25, unit = "cm")),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(t = 0, l = 5, unit = "pt")
  )


# Saving ------------------------------------------------------------------
path <- here::here("Day9", "day9")
ggsave(glue::glue("{path}.png"), width = 11.75, height = 14.5, device = ragg::agg_png, dpi = 720)

# Additional annotations with illustrator 
pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"), 
  filenames = glue::glue("{path}_polished_twitter.png"),
  dpi = 216
)

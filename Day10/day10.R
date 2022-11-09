

# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)

# Data Reading and Wrangling ----------------------------------------------
regions <- read_sf(old_french_regions_path) %>%
  mutate(
    # Convert between encodings
    nom = iconv(nom, "latin1", "UTF-8")
  )

# Remove DROM-COM
(regions <- regions %>%
  filter(!code_insee %in% c("01", "02", "03", "04", "06")) 
)

# Categories viewed
regions <- regions %>%
  mutate(
    search = case_when(
      nom == "Île-de-France" ~ "Ebony",
      nom == "Midi-Pyrénées" ~ "Big Tits",
      nom == "Alsace" ~ "Bondage",
      nom %in% c("Poitou-Charentes", "Aquitaine", "Lorraine") ~ "Mature",
      nom %in% c("Languedoc-Roussillon", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes", "Corse") ~ "Anal",
      TRUE ~ "French"
    )
  )

# Graphic -----------------------------------------------------------------
palette <- c(
  "French" = "#3A86FF", "Ebony" = "#FF006E", "Mature" = "#FFBE0B",
  "Big Tits" = "#FB5607", "Bondage" = "#8338EC", "Anal" = "#ff595e"
)
regions %>%
  ggplot() +
  geom_sf(aes(fill = search), color = "white") +
  labs(
    title = "Pornhub Most Viewed Relative Categories in 2016",
    subtitle = "Viewed more when compared to all others regions",
    caption = "*Subdivision of regions in 2015<br> Data from **Pornhub**<br> #30DayMapChallenge Day 10 : **Bad Map**"
  ) +
  scale_fill_manual(
    values = palette,
    guide = "none"
  ) +
  annotate("text", x = 3, y = 47.5, family = "S One", fontface = "bold", label = "French", size = 17.5) +
  annotate("text", x = -.5, y = 45, family = "S One", fontface = "bold", label = "Mature", angle = 80, size = 12.5) +
  annotate("text", x = 1.75, y = 44, family = "S One", fontface = "bold", label = "Big\n Tits", size = 8.5) +
  annotate("text", x = 2.35, y = 48.75, family = "S One", fontface ="bold",  label = "Ebony", size = 6) +
  annotate("text", x = 6, y = 48.75, family = "S One", fontface ="bold", label = "Mature", size = 7.5) +
  annotate("text", x = 5, y = 44.5, family = "S One", fontface ="bold", label = "Anal", size = 10) +
  annotate("text", x = 8.85, y = 49, family = "S One", fontface ="bold", label = "Bondage", size = 6) +
  annotate("text", x = 8, y = 42, family = "S One", fontface ="bold", label = "Anal", size = 6) +
  theme_void() +
  theme(
    text = element_text(family = "S One"),
    plot.title = element_text(face = "bold", size = rel(2.5)),
    plot.subtitle = element_text(color = "#666666", face = "bold", size = rel(1.5)),
    plot.caption = ggtext::element_markdown(size = rel(1.25)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(c(.5, .5, .5, .55), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day10", "day10")
ggsave(glue::glue("{path}.png"), width = 10, height = 10, device = ragg::agg_png, dpi = 300)


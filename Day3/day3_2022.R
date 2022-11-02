
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(geofacet)
library(ggtext)

# Data Reading and Wrangling ---------------------------------------------
# link to orginal map
# https://www.bloomberg.com/graphics/2022-google-search-abortion-clinic-crisis-pregnancy-center/?leadSource=uverify%20wall
crisis_centers <- tribble(
  ~state_abbr, ~state, ~abortion_status, ~nb_pregnancy_centers,
  "AL", "Alabama", "banned", 5,
  "AK", "Alaska", "authorized", 2,
  "AZ", "Arizona", "authorized", 2,
  "AR", "Arkansas", "banned", 7,
  "CA", "California", "authorized", 0,
  "CO", "Colorado", "authorized", 0,
  "CT", "Connecticut", "authorized", 0,
  "DE", "Delaware", "authorized", 2,
  "FL", "Florida", "limited", 0,
  "GA", "Georgia", "limited", 0,
  "HI", "Hawaii", "authorized", 4,
  "ID", "Idaho", "expected", 6,
  "IL", "Illinois", "authorized", 0,
  "IN", "Indiana", "expected", 1,
  "IA", "Iowa", "authorized", 4,
  "KS", "Kansas", "authorized", 6,
  "KY", "Kentucky", "banned", 6,
  "LA", "Louisiana", "banned", 4,
  "ME", "Maine", "authorized", 1,
  "MD", "Maryland", "authorized", 0,
  "MA", "Massachusetts", "authorized", 1,
  "MI", "Michigan", "authorized", 0,
  "MN", "Minnesota", "authorized", 3,
  "MS", "Mississippi", "banned", 9,
  "MO", "Missouri", "banned", 4,
  "MT", "Montana", "authorized", 3,
  "NE", "Nebraska", "authorized", 5,
  "NV", "Nevada", "authorized", 1,
  "NH", "New Hampshire", "authorized", 3,
  "NJ", "New Jersey", "authorized", 0,
  "NM", "New Mexico", "authorized", 3,
  "NY", "New York", "authorized", 0,
  "NC", "North Carolina", "authorized", 0,
  "ND", "North Dakota", "authorized", 5,
  "OH", "Ohio", "limited", 0,
  "OK", "Oklahoma", "banned", 5,
  "OR", "Oregon", "authorized", 3,
  "PA", "Pennsylvania", "authorized", 0,
  "RI", "Rhode Island", "authorized", 2,
  "SC", "South Carolina", "limited", 7,
  "SD", "South Dakota", "banned", 5,
  "TN", "Tennessee", "expected", 3,
  "TX", "Texas", "banned", 0,
  "UT", "Utah", "authorized", 2,
  "VT", "Vermont", "authorized", 3,
  "VA", "Virginia", "authorized", 0,
  "WA", "Washington", "authorized", 0,
  "WV", "West Virginia", "authorized", 5,
  "WI", "Wisconsin", "banned", 1,
  "WY", "Wyoming", "authorized", 6
)


# Graphic -----------------------------------------------------------------
crisis_centers |> 
  ggplot(aes(fill = nb_pregnancy_centers)) + 
  geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(color = abortion_status, linetype = abortion_status),  size = 1) +
  geom_text(x = .5, y = .5, aes(color = after_scale(prismatic::best_contrast(fill)), label = glue::glue("{state_abbr}\n{nb_pregnancy_centers}")), family = "Gotham Narrow", size = 6.5) + 
  labs(
    title = "Number of Crisis Pregnancy Centers in Top 10 Search Results", 
    caption ="Data from bloomberg, as of August 11th, 2022\n#30DayMapChallenge Day 3 : Polygons \n Abdoul ISSA BIDA inspired by  Davey Alba and Jack Gillu"
  ) +
  coord_equal() +
  geofacet::facet_geo(vars(state), grid = us_state_grid3)  +
  scale_linetype_manual(
    values = c(
      "authorized" = "blank",
      "banned" = "solid",
      "expected" = "dotted",
      "limited" = "solid"
    ),
    guide = "none"
  ) + 
  scale_color_manual(
    values = c(
      "authorized" = NA,
      "banned" = "black",
      "expected" = "black",
      "limited" = "grey25"
    ),
    guide = "none"
  ) + 
  colorspace::scale_fill_continuous_sequential(
    palette = "Rocket", 
    begin = .05, 
    end = .9, 
    rev = T,
    guide = "none"
  )  +
  theme_minimal() + 
  theme(
    text = element_text(color = "#111111", family = "Gotham Narrow"),
    plot.background = element_rect(fill = "white", color = NA), 
    plot.margin = margin(c(.5,.5,.5,.5), unit = "cm"),
    plot.title = element_text(face = "bold", size = rel(2.5), margin = margin(b = 1.75, unit = "cm")), 
    plot.caption = element_text(hjust = 0, size = rel(1.25), margin = margin(t = .75, unit = "cm")), 
    panel.spacing = unit(.01, "cm"),
    strip.text = element_blank()
  )


# Saving ------------------------------------------------------------------
path <- here::here("Day3", "day3_2022")
ggsave(glue::glue("{path}.pdf"), width = 10.5, height = 9.5, device = cairo_pdf)

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

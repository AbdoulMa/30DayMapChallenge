
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------

# States sf
us_states <- albersusa::usa_sf() |> 
  st_transform(crs = "ESRI:102003")

# Coaches df
coaches_df <- tibble(
  state_abbr = state.abb,
  state_name = state.name
)

coaches_df <- coaches_df |> 
  mutate(
    highest_paid = case_when(
      state_abbr == "NH" ~ "Hockey Coach",
      state_abbr == "WI" ~ "Basketball Coaches,Football Coaches",
      state_abbr %in% c("AK", "NV", "MT", "ND", "SD", "NY", "VT", "ME", "MA") ~ "Not Coaches",
      state_abbr %in%  c("CA", "AZ", "KS", "WI", "MI", "IN", "KY", "NC", "FL", "NJ", "CT", "RI") ~ "Basketball Coaches", 
      TRUE ~ "Football Coaches"
    )
  )

coaches_df <- coaches_df |> 
  separate_rows(highest_paid, sep = ",") |> 
  mutate(
    highest_paid = str_to_upper(highest_paid),
    highest_paid = fct_infreq(highest_paid), 
         highest_paid = fct_relevel(highest_paid, str_to_upper("Not Coaches"),after = Inf))

final_coaches_df <- us_states |> 
  right_join(coaches_df, by = c('name' = 'state_name'))

final_coaches_df <- final_coaches_df |> 
  mutate(
    long = st_coordinates(st_centroid(geometry))[,1],
    lat = st_coordinates(st_centroid(geometry))[,2]
  ) 

# Graphic -----------------------------------------------------------------
final_coaches_df |> 
  ggplot() + 
  geom_sf(data = us_states, fill = NA, color = "white",  size = .075) + 
  geom_sf(fill = "#DC0437", color = "white",  size = .075) + 
  ggrepel::geom_text_repel(aes(x = long, y = lat, label = state_abbr),
                           direction = "x",
                           color = "white", 
                           family = "UEFA Supercup",
                           fontface = "bold",
                           bg.color = "black",
                           nudge_x = .15,
                           nudge_y = .5,
                           size = 4.5
                           ) + 
  labs(
    title = str_to_upper("Highest-paid\nPublic Employees"),
    caption = "#30DayMapChallenge<br>Day 11 : **Red** · Abdoul ISSA BIDA."
  ) +
  facet_wrap(vars(highest_paid), nrow = 2) + 
  theme_minimal() + 
  theme(
    text = element_text(color = "#FFFFFF", family = "UEFA Supercup"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = rel(2), margin = margin(b = .75, unit = "cm")), 
    plot.caption = ggtext::element_markdown(hjust = 0, size = rel(1.75)),
    strip.text = element_text(color = "#FFFFFF", hjust = 0, face = "bold", size = rel(2), margin = margin(b = .375, unit = "cm")),
    panel.grid = element_blank(),
    panel.spacing = unit(.25, unit = "cm"),
    plot.background = element_rect(fill = "#40356D", color = NA), 
    plot.margin = margin(c(.5,.5,.5,.5), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day11", "day11")
ggsave(glue::glue("{path}.png"), width = 11, height = 9.75, device = ragg::agg_png, dpi = 720)

# Additional annotations with illustrator 
pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"), 
  filenames = glue::glue("{path}_polished.png"),
  dpi = 300
)

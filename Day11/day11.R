
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------

# Coaches attempt ---- 
# /home/abdoul-ma/Images/Illustrations/05kreider-image-threeByTwoSmallAt2X.jpg red colors
# file:///home/abdoul-ma/Images/Illustrations/merlin_204744780_3050db32-1002-4554-848c-8f6f8faeafb8-videoLarge.jpg bg

us_states <- albersusa::usa_sf() |> 
  # st_transform(albersusa::us_laea_proj)
  st_transform(crs = "ESRI:102003")

glimpse(us_states)
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
  mutate(highest_paid = fct_infreq(highest_paid), 
         highest_paid = fct_relevel(highest_paid, "Not Coaches",after = Inf))
levels(coaches_df$highest_paid)
final_coaches_df <- us_states |> 
  right_join(coaches_df, by = c('name' = 'state_name'))
final_coaches_df |> 
  mutate(
    long = st_coordinates(st_centroid(geometry))[,1],
    lat = st_coordinates(st_centroid(geometry))[,2]
  ) -> final_coaches_df




# Graphic -----------------------------------------------------------------
final_coaches_df |> 
  ggplot() + 
  geom_sf(data = us_states, fill = NA,  size = .125) + 
  geom_sf(fill = "red", size = .125) +
  ggrepel::geom_text_repel(aes(x = long, y = lat, label = state_abbr),
                           direction = "x",
                           color = "white", 
                           bg.color = "black",
                           nudge_x = .15,
                           nudge_y = .5,
                           # force = .5, 
                           size = 2
                           ) + 
  # geom_sf_text(data = final_coaches_df, aes(label = name)) + 
  facet_wrap(vars(highest_paid), nrow = 1) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Saving ------------------------------------------------------------------
path <- here::here("Day11", "day11")
ggsave(glue::glue("{path}.png"), width = 12, height = 4.5, device = ragg::agg_png, dpi = 300)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

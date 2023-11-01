
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(geofacet)

# Data Reading and Wrangling ----------------------------------------------
# Inspiration
# https://www.nationalgeographic.com/magazine/graphics/where-to-find-the-good-life

happiness_df <- tibble(
  state_abbr = state.abb,
  state_name = state.name
)

happiness_df <- happiness_df |>
  mutate(
    index = case_when(
      state_abbr %in% c("IN", "OH", "KY", "WV", "AR", "OK", "LA", "MS", "AL", "RI") ~ "😞",
      state_abbr %in% c("NV", "KS", "MO", "IL", "MI", "TN", "NY", "MD", "CT", "DE") ~ "😕",
      state_abbr %in% c("WA", "OR", "IA", "WI", "NC", "SC", "GA", "NJ", "PA", "NH") ~ "😐",
      state_abbr %in% c("ID", "WY", "UT", "CA", "ND", "NE", "NM", "VA", "MA", "FL") ~ "🙂",
      TRUE ~ "😀"
    ),
    index = fct_relevel(index, c("😞", "😕", "😐", "🙂", "😀"))
  )

# Graphic -----------------------------------------------------------------
grid_wo_dc <- us_state_grid1[us_state_grid1$code != "DC", ]

# Main plot
main_plot <- happiness_df |>
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = index)) +
  geom_text(x = .5, y = .5, aes(label = index), color = "white", size = 10) +
  facet_geo(vars(state_abbr), grid = grid_wo_dc, strip.position = "bottom") +
  scale_fill_manual(
    values = c("#E61D2D", "#F57B33", "#FFBC39", "#C5C43D", "#81C742"),
    guide = "none"
  ) +
  labs(
    title = str_to_upper("Map of Happiness"),
    caption = "Data and inspiration from National Geographic\n#30DayMapChallenge - Day 16 : Minimal · Abdoul ISSA BIDA"
  ) +
  theme_minimal() +
  coord_equal() +
  theme(
    text = element_text(family = "Ideal Sans Medium", color = "#111111"),
    panel.spacing.x = unit(-3, "pt"),
    panel.spacing.y = unit(5, "pt"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0, family = "Ideal Sans Bold", size = rel(3.25), margin = margin(b = 1, unit = "cm")),
    plot.caption = element_text(size = rel(1), margin = margin(t = .25, unit = "cm")),
    strip.text = element_text(size = rel(1.25)),
  )

# Legend plot
legend_plot <- tibble(
  index = 1:5,
  emoji = fct_inorder(levels(happiness_df$index))
) |>
  ggplot() +
  geom_rect(aes(xmin = index - 1, xmax = index, ymin = 0, ymax = 1, fill = emoji)) +
  geom_text(aes(x = index - 1 / 2, label = emoji), hjust = .5, size = 7.5, y = .5) +
  annotate(geom = "text", x = 0, y = 1.25, label = "Well-being score", color = "grey20", family = "Ideal Sans Bold", hjust = 0, vjust = 0, size = 4.5) +
  annotate(geom = "text", x = 5, y = -.25, label = "Highest quintile →", color = "grey20", family = "Ideal Sans Medium", hjust = 1, vjust = 1, size = 3.5) +
  scale_fill_manual(
    values = c("#E61D2D", "#F57B33", "#FFBC39", "#C5C43D", "#81C742"),
    guide = "none"
  ) +
  coord_equal(clip = "off", expand = F) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Combine plot
cowplot::ggdraw(main_plot) +
  cowplot::draw_plot(legend_plot, x = 0.075, y = 0.35, width = .25, halign = 0) +
  theme(
    text = element_text(family = "Ideal Sans Meidum"),
    plot.background = element_rect(fill = "#EDF1E0", color = NA),
    plot.margin = margin(c(.5, -1, .25, -1), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day16", "day16")
ggsave(glue::glue("{path}.png"), width = 9, height = 9, device = ragg::agg_png, dpi = 300)

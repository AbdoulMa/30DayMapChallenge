
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(patchwork)

# Data Reading and Wrangling ----------------------------------------------

# You have to declare world_shape_file_path & wps_data_path
country_borders <- read_sf(world_shape_file_path)

# Remove Antartica
country_borders <- country_borders |> 
  filter(SOVEREIGNT != "Antarctica") |> 
  st_transform(crs = st_crs('ESRI:54030')) # Robinson Projection

# You can download the data here: https://giwps.georgetown.edu/the-index/ 
(women_index <- read_csv(wps_data_path) |>
    janitor::row_to_names(row_number = 1) |>
    select(1:4,6,8,10,12,14,16,18, 20, 22, 24)
)

women_index <- women_index |>
  rename_with(~ tolower(str_replace_all(., " ", "_"))) |>
  drop_na()

# Country name cleaning t
women_index_final <- women_index |>
  mutate(
    wps_index_score = as.numeric(wps_index_score),
    country = case_when(
      country == "Congo" ~ "Republic of the Congo",
      country == "Czech Republic" ~ "Czechia",
      country == "Cote d'Ivoire" ~ "Ivory Coast",
      country == "Eswatini" ~ "eSwatini",
      country == "Lao PDR" ~ "Laos",
      country == "Kosovo*" ~ "Kosovo",
      country == "North Macedonia" ~ "Macedonia",
      country == "Russian Federation" ~ "Russia",
      country == "Sao Tome and Principe" ~ "São Tomé and Principe",
      country == "Serbia" ~ "Republic of Serbia",
      country == "Tanzania" ~ "United Republic of Tanzania",
      country == "Timor-Leste" ~ "East Timor",
      country == "United States" ~ "United States of America",
      country == "Viet Nam" ~ "Vietnam",
      country == "Western Sahara" ~ "Maroc",
      TRUE ~ country
    )
  )


# Graphic -----------------------------------------------------------------

pal <- scales::div_gradient_pal(low = "#971A60", mid = "#FFFFFE", high = "#7EBC42")(seq(0,1, length.out = 7))
# Function to build specific domain plot
caracteristic_plot <- function(caracteristic, desc_order = T, subtitle = "") {
  caracteristic_df <- women_index_final |>
    mutate(
      desc_order = desc_order,
      rk =  case_when(
       desc_order ~ rank(desc({{caracteristic}}), ties.method = "first"), 
       T ~ rank({{caracteristic}}, ties.method = "first")
      )
     ,
      rk_group = cut_interval(rk, 7) # label = paste0(c("top", "second", "third", "fourth", "bottom"), " quintile")
    ) 
  
  caracteristic_df <- select(country_borders, SOVEREIGNT) |>
    left_join(caracteristic_df, by = c("SOVEREIGNT" = "country"))
  
  caracteristic_df |> 
    ggplot() +
    geom_sf(aes(fill = rk_group), size = 0.05, color = "#efefef") + 
    labs(
      subtitle = str_to_upper(subtitle)
    ) + 
    scale_fill_manual(
      values = rev(pal),
      guide = "none"
    ) + 
    
    theme_minimal() + 
    theme(
      
    )
}

# Define specific domain plots 
main_plot <- caracteristic_plot(`wps_index_score`, subtitle = "Global Index")
community_safety_plot <- caracteristic_plot(`perception_of_community_safety_(%)_^m`, subtitle = "Perception of Community Safety")
discriminatry_plot <- caracteristic_plot(`discriminatry_norms_(%)`, desc_order = F, subtitle = "Discriminatory standards")
education_plot <- caracteristic_plot(`education_(years)`, subtitle = "Education Years")
employment_plot <- caracteristic_plot(`employment_(%)`, subtitle = "Employment")
partner_violence_plot <- caracteristic_plot(`intimate_partner_violence_(%)`, desc_order = F, subtitle = "Intimate Partner Violence")
parliamentary_plot <- caracteristic_plot(`parliamentary_representation_(%)`, subtitle = "Parliamentary Representation")

# Combine plot
other_plots <- (community_safety_plot + discriminatry_plot + education_plot) / 
  (employment_plot + partner_violence_plot + parliamentary_plot)  + 
  plot_layout(nrow = 2)


final_plot <- main_plot / other_plots + 
  plot_annotation(title = "Women, Peace and Security Index", 
                  caption = "Data from GIWPS for 2021. \n #30DayMapChallenge \nDay 4 : Colour Friday: Green · Abdoul ISSA BIDA.") +
  plot_layout(nrow = 2, heights = c(.55, .45)) & 
  theme(
    text = element_text(color = "#FFFFFF", family = "Gotham Narrow"), 
    # panel.grid.major = element_line(color = "grey75",size = .025),
    panel.grid.major = element_blank(),
    plot.title = element_text(face = "bold", hjust = .5,  size = rel(3.5), margin =  margin(b = 3.5, unit = "cm")),
    plot.subtitle = element_text(hjust = .5),
    plot.caption = element_text(hjust= .5,  size = rel(1)),
    plot.margin = margin(c(.5,.125,.5,.125), unit = "cm"),
    plot.background = element_rect(fill = "#111111", color = NA),
    axis.text = element_blank()
    # axis.text = element_text(color = "white", family = "Inconsolata", size =  rel(.95))
  )

# Saving ------------------------------------------------------------------
path <- here::here("Day4", "day4_2022")
ggsave(glue::glue("{path}.pdf"), plot = final_plot, width = 14, height = 14, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 144
)

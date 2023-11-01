
# Libraries Loading -------------------------------------------------------
library(dplyr)
library(tidyr)
library(arrow)
library(ambient)

# Data Reading and Wrangling ----------------------------------------------
# Inspiration: https://github.com/djnavarro/arrow-nyc-taxi-scatter
# To download the csv Data file: https://data.humdata.org/dataset/worldpop-population-counts-for-france 

france_densities <- read_csv_arrow(france_population_path)

# Case general population
france_densities <- france_densities |>
  transmute(
    longitude = Lon,
    latitude = Lat,
    population = Population
  )

gc(reset = T, full = T)

long_range <- range(france_densities$longitude)
lat_range <- range(france_densities$latitude)

x0 <- long_range[1] - .5
x1 <- long_range[2] + .5
y0 <- lat_range[1] - .5
y1 <- lat_range[2] + .5
pixels <- 4000

france_densities_scaled <- france_densities |> 
  mutate(
    x = as.integer(round(pixels * (longitude - x0) / (x1-x0))),
    y = as.integer(round(pixels * (latitude - y0) / (y1-y0)))
  ) |> 
  count(x, y, wt = population, name = "population") |> 
  compute()

# Convert to grid ----
x_coord <- 0:pixels
y_coord <- 0:pixels

grid <- expand_grid(
  x = x_coord,
  y = y_coord
) |> 
  as_arrow_table() |> 
  left_join(france_densities_scaled, by = c("x", "y")) |> 
  mutate(
    population = case_when(is.na(population) ~ 0,
                           TRUE ~ population)
  ) |> 
  arrange(x,y) |> 
  collect()

gc(reset = T, full = T)

population_grid <- long_grid(
  x = x_coord, 
  y = y_coord, 
  z = 0
)

gc(reset = T, full = T)
population_grid$z <- grid$population

population_grid <- as.matrix(population_grid, value = z)

gc(reset = T, full = T)

# Graphic -----------------------------------------------------------------
path <- here::here("Day21", "day21")
op <- par(mar = c(0,0,0,0))
# ragg::agg_png(
#   filename = glue::glue("{path}.png"), 
#   width = 4000, #pixels, 
#   height = 4000,# pixels, 
#   bg = "black"
# )

cairo_pdf(
  filename = glue::glue("{path}.pdf"), 
  width = 9, #pixels, 
  height = 9,# pixels, 
  bg = "black"
)

image(
  z = log10(t(population_grid)),
  axes = FALSE, 
  asp = 1,
  useRaster = TRUE,
  col = scales::colour_ramp(c("black", "white"))(seq(0,1, length.out = 1024))
)

dev.off()

# Addition annotation with Illustrator 
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 720
)

pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"), 
  filenames = glue::glue("{path}_polished.png"),
  dpi = 640
)


# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------



# Graphic -----------------------------------------------------------------



# Saving ------------------------------------------------------------------
path <- here::here("Day1", "day1")
ggsave(glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 640
)

#  Map made with QGIS + Illutrator
# Just use R to convert the pdf 

# Saving ------------------------------------------------------------------
path <- here::here("Day7", "day7")

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}_twitter.png"),
  dpi = 216
)


# Libraries Loading -------------------------------------------------------
library(tidyverse)

# Data Reading and Wrangling ----------------------------------------------
# Data page: https://fr.wikipedia.org/wiki/Liste_des_contributions_des_%C3%89tats_membres_au_budget_de_l%27Union_europ%C3%A9enne_par_%C3%89tat
countries_contributions <- tribble(
  ~"country", ~"contribution",
  "Allemagne",  	434.85,
  "Autriche", 	50.47 ,
  "Belgique",  	66.39 ,
  "Bulgarie",  	5.45 ,
  "Chypre",  	2.61 ,
  "Croatie",  	3.14 ,
  "Danemark",  	42.57,
  "Espagne",  	183.09,
  "Estonie",  	2.66 ,
  "Finlande",  	34.12,
  "France",  	368.32 ,
  "Grèce",  	34.64 ,
  "Hongrie",  	14.83,
  "Irlande",  	30.08,
  "Italie",  	278.13 ,
  "Lettonie",3.25,
  "Lituanie",  	4.79,
  "Luxembourg", 5.61,
  "Malte",  	1.16 ,
  "Pays-Bas", 	88.98,
  "Pologne",  	54.77,
  "Portugal", 30.81,
  "République tchèque", 	22.08 ,
  "Roumanie",  	18.75,
  "Royaume-Uni", 235.95,
  "Slovaquie",  	9.79 ,
  "Slovénie",  	5.53 	,
  "Suède",  	57.82 
)

countries_contributions <- countries_contributions |> 
  mutate(
    square = scales::rescale(contribution, to = c(10, 100) , from = range(countries_contibutions$contribution)), 
    rk = rank(desc(contribution))
  ) |> 
  arrange(rk)


# Graphic -----------------------------------------------------------------
# Chart made with illustrator


# Saving ------------------------------------------------------------------
path <- here::here("2023/Day3", "day3")
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"), 
  filenames = glue::glue("{path}.png"),
  dpi = 300
)

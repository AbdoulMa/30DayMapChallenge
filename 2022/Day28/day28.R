
# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
country_borders <- read_sf(world_shape_file_path)

africa <- country_borders |> 
  filter(CONTINENT == "Africa")


# Graphic -----------------------------------------------------------------
j <-1
for (i in 270:-90) {
  generate_ground(depth=-0.05,
                  material = diffuse(color="#111111",checkercolor="#111111")) |>
    add_object(extruded_polygon(africa, center = TRUE,
                                material=diffuse(color="#ff2222",sigma=90),
                                scale = c(1, 2, 1)
    )) |> 
    add_object(sphere(y=60,x=-30,radius=10,
                      material=light(color="lightblue",intensity=60))) |> 
    render_scene(parallel=TRUE,lookfrom = c(-10 + 55*cospi(i/180), 20, 0 + 65*sinpi(i/180)), width = 640, height = 640, samples=32,fov=60, aperture = 0,
                 filename = glue::glue("Graphics/Africa_3D/{j}.png"))
  j <-  j + 1 
}

# Use convert for image annotation & ffmpeg for video 
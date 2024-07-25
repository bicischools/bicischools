library(sf)

home = readRDS("../internal/Bicischools_home_sample.Rds")
school = readRDS("../internal/Bicischools_school_sample.Rds")
st_crs(home) = 4326
st_crs(school) = 4326

tm_shape(home) + tm_lines()

tm_shape(school) + tm_dots()

home_dots = st_as_sf(home$home_coords)
tm_shape(home_dots) + tm_dots()

dim(home_dots)
# [1] 169   3


# Distance frequency curves -----------------------------------------------
# For routes and desire lines
home = home |> 
  mutate(
    linestring_route_length = st_length(geometry),
    desire_line_length = st_distance(home_coords, school)[,1]
    )


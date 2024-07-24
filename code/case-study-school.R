home = readRDS("../internal/Bicischools_home_sample.Rds")
school = readRDS("../internal/Bicischools_school_sample.Rds")

tm_shape(home) + tm_lines()

tm_shape(school) + tm_dots()

home_dots = st_as_sf(home$home_coords)
tm_shape(home_dots) + tm_dots()

dim(home_dots)
# [1] 169   3


# Distance frequency curves -----------------------------------------------
# For routes and desire lines

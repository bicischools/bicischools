library(sf)
library(ggplot2)
library(units)

home = readRDS("../internal/Bicischools_home_sample.Rds")
school = readRDS("../internal/Bicischools_school_sample.Rds")
st_crs(home) = 4326
st_crs(school) = 4326

# tm_shape(home) + tm_lines()

# tm_shape(school) + tm_dots()

home_dots = st_as_sf(home$home_coords)
# tm_shape(home_dots) + tm_dots()

dim(home_dots)
# [1] 169   1

# the 169 students have 100 unique home locations (so it includes kids within the same family)
length(unique(home$home_coords))
# [1] 100 

# Distance frequency curves -----------------------------------------------
# For routes and desire lines
home = home |> 
  mutate(
    linestring_route_length = st_length(geometry),
    desire_line_length = st_distance(home_coords, school)[,1]
    )

ggplot(home, aes(desire_line_length)) +
  geom_histogram()
ggplot(home, aes(linestring_route_length)) +
  geom_histogram()

summary(home$linestring_route_length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 321.6   592.3  1243.2  3202.8  2822.0 38486.3

below_5km = home |> 
  drop_units() |> 
  filter(linestring_route_length < 5000)
dim(below_5km)
# [1] 143   5
dim(below_5km)[1]/dim(home)[1] # 85% of students live within 5km route length of school
# [1] 0.8461538 

below_3km = home |> 
  drop_units() |> 
  filter(linestring_route_length < 3000,
         linestring_route_length > 500)
dim(below_3km)
# [1] 94   5
dim(below_3km)[1]/dim(home)[1] # 56% of students live between 500m and 3km route length from school
# [1] 0.556213 


# Assign home locations to enumeration districts --------------------------

bgri = read_sf("../internal/BGRI21_170/BGRI21_170.gpkg")
bgri_4326 = st_transform(bgri, 4326)
bgri_4326 = st_make_valid(bgri_4326)
home_assigned = st_join(bgri_4326, home_dots, left = F)
home_assigned = home_assigned |> 
  select(OBJECTID, DTMN21, N_INDIVIDUOS_0_14)

tm_shape(home_assigned) + tm_polygons()

# The 169 students live in 87 different enumeration zones
length(unique(home_assigned$OBJECTID))
# [1] 87

# Now make table showing the number of students in each zone (list each zone once only)
zone_counts = home_assigned |> 
  group_by(OBJECTID, DTMN21, N_INDIVIDUOS_0_14) |> 
  summarise(n_students = n())

# Then assign centroids for each zone to generate OD dataset at the zone-school level with counts
zone_centroids = st_centroid(zone_counts)

tm_shape(zone_centroids) + tm_dots()

# Filter out centroids >3km euclidean distance from the school 
zone_centroids = zone_centroids |> 
  mutate(
    desire_line_length = st_distance(geom, school)[,1],
    desire_line_length = drop_units(desire_line_length)
  )

centroids_3km = zone_centroids |> 
  filter(desire_line_length < 3000)


# Route trips from zone centroids to school -------------------------------



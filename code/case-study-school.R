library(sf)
library(ggplot2)
library(units)
library(tidyverse)
library(tmap)
tmap_mode("view")

# Explore existing data for Escola Básica Adriano Correia de Oliveira

schools_year = readRDS("data/SCHOOLS_year.Rds")
existing = schools_year |> filter(DGEEC_id == 1106908)
# View(existing)
sum(existing$STUDENTS)
# [1] 110 # this is different from the case study dataset because it's from a different year

# Case study dataset

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

summary(home$desire_line_length)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 80.92   248.09   713.56  2167.34  1973.25 28564.06

summary(home$linestring_route_length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 321.6   592.3  1243.2  3202.8  2822.0 38486.3

below_5km_desire = home |> 
  drop_units() |> 
  filter(desire_line_length < 5000)
dim(below_5km_desire)
# [1] 153   5

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
# So we can route from zone centroids instead of home postcodes, for anonymity

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
  summarise(n_students = n()) |> 
  ungroup()

saveRDS(zone_counts, "data/zone_counts.Rds")

# Then assign centroids for each zone to generate OD dataset at the zone-school level with counts
zone_centroids = st_centroid(zone_counts)

tm_shape(zone_centroids) + tm_dots()

# Filter out centroids >5km euclidean distance from the school 
zone_centroids = zone_centroids |> 
  mutate(
    desire_line_length = st_distance(geom, school)[,1],
    desire_line_length = drop_units(desire_line_length)
  )

centroids_5km = zone_centroids |> 
  filter(desire_line_length < 5000)

tm_shape(centroids_5km) + tm_dots()

od_5km = centroids_5km |>
  mutate(d = school$geometry)

od_5km$geometry = 
  Map(st_union, od_5km$geom, od_5km$d) |> 
  st_as_sfc(crs = st_crs(od_5km)) |> 
  st_cast("LINESTRING")
st_geometry(od_5km) = "geometry"
od_5km = od_5km |> 
  select(-c(geom, d))

# Route trips from zone centroids to school -------------------------------

library(stplanr)

routes_quiet = route(l = od_5km, route_fun = cyclestreets::journey, plan = "quietest")

tm_shape(routes_quiet) + tm_lines()

routes_fast = route(l = od_5km, route_fun = cyclestreets::journey, plan = "fastest")

tm_shape(routes_fast) + tm_lines()


# PCT cycle uptake --------------------------------------------------------

library(pct)
routes_quiet_pct = routes_quiet |> 
  mutate(pcycle_go_dutch = uptake_pct_godutch_school2(distance = distance, gradient = gradient_smooth),
         bicycle_go_dutch = pcycle_go_dutch * n_students
         )
rnet_raw = routes_quiet_pct |> 
  overline(attrib = c("bicycle_go_dutch", "quietness", "gradient_smooth"), 
           fun = list(sum = sum, mean = mean))
rnet = rnet_raw |> 
  transmute(bicycle_go_dutch = bicycle_go_dutch_sum,
            quietness = round(quietness_mean),
            gradient = round(gradient_smooth_mean*100))
tm_shape(rnet) + tm_lines("bicycle_go_dutch", palette = "viridis", lwd = 2)

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
plan = "quiet"
plans = c("quiet", "fast")

for(plan in plans) {
  location = paste0("../internal/routes-", plan, "-casestudy.Rds")
  routes_plan_location = location
  if (file.exists(routes_plan_location)) {
    routes_plan = readRDS(routes_plan_location)
  } else {
    plan_name = paste0(plan, "est")
    routes_plan = route(l = od_5km, route_fun = cyclestreets::journey, plan = plan_name)
    routes_plan = routes_plan |> 
      group_by(route_number) |> 
      mutate(route_hilliness = weighted.mean(gradient_smooth, distances)) |> 
      ungroup()
    saveRDS(routes_plan, paste0("../internal/routes-", plan, "-casestudy.Rds"))
  }
  class(routes_plan$route_number) = "character"
  class(routes_plan$length) = "numeric"
  class(routes_plan$quietness) = "numeric"
  routes_plan = routes_plan |> 
    filter(length < 5000) 
  assign(x = paste0("routes_", plan), value = routes_plan)
}

# tm_shape(routes_quiet) + tm_lines()
# tm_shape(routes_fast) + tm_lines()


# PCT cycle uptake --------------------------------------------------------

library(pct)

# Quiet routes
for(plan in plans) {
  routes_plan = get(x = paste0("routes_", plan))
  routes_plan_pct = routes_plan |> 
    group_by(route_number) |> 
    mutate(
      pcycle_godutch = pct::uptake_pct_godutch_school2(
        case_when(length > 30000 ~ 30000, TRUE ~ length),
        route_hilliness),
      bicycle_godutch = pcycle_godutch * n_students
    )
  rnet_plan_raw = routes_plan_pct |> 
    overline(attrib = c("n_students", "bicycle_godutch", "quietness", "gradient_smooth"), 
             fun = list(sum = sum, mean = mean))
  rnet_plan = rnet_plan_raw |> 
    transmute(n_students = n_students_sum, 
              bicycle_godutch = bicycle_godutch_sum,
              quietness = round(quietness_mean),
              gradient = round(gradient_smooth_mean*100))
  assign(x = paste0("rnet_", plan), value = rnet_plan)
}

tm_shape(rnet_quiet) +
  tm_lines("bicycle_godutch", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100)) +
  tm_shape(centroids_5km) + tm_bubbles("n_students")
tm_shape(rnet_fast) +
  tm_lines("bicycle_godutch", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100)) +
  tm_shape(centroids_5km) + tm_bubbles("n_students")

# Explore results ---------------------------------------------------------

summary(routes_quiet$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 144    1890    3024    2909    3856    4967 
summary(routes_fast$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 144    1496    2525    2680    3637    4918 



# Total number of students cycling to school under Go Dutch
# Strangely, there are more cyclists under the quiet routes, even though the routes are longer
route_summaries = routes_quiet_pct |> 
  group_by(route_number) |> 
  summarise(n_students = mean(n_students),
            bicycle_godutch = mean(bicycle_godutch),
            length = mean(length)
            )
# n students within 5km route distance of school
sum(route_summaries$n_students)
# [1] 140
# n_cyclists under go dutch
sum(route_summaries$bicycle_godutch)
# [1] 40.38902

# median route length
library(matrixStats)
weightedMedian(route_summaries$length, route_summaries$n_students)
# [1] 928.2857

# route_summaries = routes_fast_pct |> 
#   group_by(route_number) |> 
#   summarise(n_students = mean(n_students),
#             bicycle_godutch = mean(bicycle_godutch),
#             length = mean(length)
#             )
# sum(route_summaries$bicycle_godutch)
# # [1] 43.49924

# median route length
weightedMedian(route_summaries$length, route_summaries$n_students)
# [1] 1064

# Mean quietness of routes - not working yet
rnet_quiet = rnet_quiet |> 
  mutate(quietness_cycled = quietness*bicycle_godutch)



# Bundling function -------------------------------------------------------

# set of routes + distance weighted number of cyclists on adjoining routes + minimum threshold 
# need to identify when route segments have the same geometry

# tm_shape(routes_quiet_buffer) + tm_polygons()
rnet = rnet_quiet
routes = route_summaries
attribute_trips = "bicycle_godutch"
cycle_bus_routes = function(
  routes,
  rnet,
  min_trips = 3,
  attribute_trips = "bicycle_godutch",
  buffer = 10
) {
  i = 1 # for testing
  rnet_subset = rnet[rnet[[attribute_trips]] > min_trips,]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  rnet_union = sf::st_union(rnet_subset)
  rnet_buffer = geo_buffer(rnet_union, dist = buffer)
  routes_subset = sf::st_intersection(routes, rnet_buffer)
  routes_subset = routes_subset |> 
    distinct(geometry)
  routes_subset$length = sf::st_length(routes_subset) |> 
    as.numeric()
  attribute_trips_mean = paste0(attribute_trips, "_mean")
  routes_subset[[attribute_trips_mean]] = NA
  class(routes_subset[[attribute_trips_mean]]) = "numeric"
  for (i in 1:nrow(routes_subset)) {
    route_i = routes_subset[i,]
    route_i_buffer = geo_buffer(route_i, dist = buffer)
    rnet_in_route = rnet_subset[route_i_buffer, , op = sf::st_within]
    routes_subset[i, attribute_trips_mean] = weighted.mean(rnet_in_route[[attribute_trips]], rnet_in_route$length)*routes_subset[i,]$length
  }
  routes_subset = routes_subset[order(routes_subset[[attribute_trips_mean]]),]
  routes_subset
}

# Get all routes within min_trips rnet, ordered by length*mean_godutch
ordered_routes = cycle_bus_routes(routes, rnet, min_trips = 3, attribute_trips = "bicycle_godutch", buffer = 10)

tm_shape(ordered_routes) + tm_lines()

# Remove routes with start points too close to other higher ranked routes

routes = ordered_routes
selected_routes = nrow(routes)
filter_routes = function(
    routes,
    buffer,
    top_n
) {
  i = 1
  z = nrow(routes)-1
  for(i in 1:z) {
    route_i = routes[i, ]
    p = st_cast(route_i$geometry, "POINT")
    p1 = p[1]
    better_routes = routes |> 
      slice_tail(n = nrow(routes)-i)
    better_routes_union = sf::st_union(better_routes)
    distance = units::drop_units(sf::st_distance(p1, better_routes_union))
    if(distance > buffer) {
      selected_routes = c(selected_routes, i)
    }
    selected_routes
  }
  routes_subset = routes[selected_routes, ]
  routes_subset = routes_subset[order(-routes_subset[["bicycle_godutch_mean"]]),]
  routes_subset = routes_subset |> 
    slice_head(n = top_n)
  routes_subset
}

top_routes = filter_routes(routes, buffer = 300, top_n = 3)

tm_shape(top_routes) + tm_lines() +
  tm_shape(centroids_5km) + tm_bubbles("n_students")


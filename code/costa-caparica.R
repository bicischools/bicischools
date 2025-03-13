library(sf)
library(tmap)
tmap_mode("view")
library(dplyr)
library(stplanr)

# Input SIM model results
res_output = read_sf("res_output_almada.geojson")

schools_c1 = readRDS("data/c1.Rds")
schools_c1 = schools_c1 |>
  filter(MUNICIPIO == "Almada")

schools_c1$ciclo_school = grepl("Escola Básica n.º 2 da Costa da Caparica, Almada", schools_c1$ESCOLA)
ciclo_school = schools_c1[which(schools_c1$ciclo_school),]

bgri = read_sf("../internal/BGRI21_170/BGRI21_170.gpkg")
bgri_4326 = st_transform(bgri, 4326)
bgri_4326 = st_make_valid(bgri_4326)

bgri_almada = bgri_4326 |> 
  filter(DTMN21 == 1503)

zone_counts = bgri_almada |> 
  select(OBJECTID, DTMN21, N_INDIVIDUOS_0_14)
zone_centroids = st_centroid(zone_counts)

# Zone centroids within 5km of school
zone_centroids = zone_centroids |> 
  mutate(
    desire_line_length = st_distance(geom, ciclo_school)[,1],
    desire_line_length = units::drop_units(desire_line_length)
  )
centroids_5km = zone_centroids |> 
  filter(desire_line_length < 5000)

# Get OD desire lines for Costa da Caparica school
res_cc = res_output |> 
  filter(
    D == ciclo_school$DGEEC_id
    # , !is.na(trips_modelled)
    )

tm_shape(res_cc) + tm_lines()

res_cc = res_cc |> 
  mutate(
    desire_line_length = st_length(res_cc),
    desire_line_length = units::drop_units(desire_line_length)
  )

# Convert desire lines to routes
plan = "quiet"
plans = c("quiet", "fast")

for(plan in plans) {
  location = paste0("../internal/routes-", plan, "-costa-da-caparica.Rds")
  routes_plan_location = location
  if (file.exists(routes_plan_location)) {
    routes_plan = readRDS(routes_plan_location)
  } else {
    plan_name = paste0(plan, "est")
    routes_plan = route(l = res_cc, route_fun = cyclestreets::journey, plan = plan_name)
    routes_plan = routes_plan |> 
      group_by(route_number) |> 
      mutate(route_hilliness = weighted.mean(gradient_smooth, distances)) |> 
      ungroup()
    saveRDS(routes_plan, paste0("../internal/routes-", plan, "-costa-da-caparica.Rds"))
  }
  class(routes_plan$route_number) = "character"
  class(routes_plan$length) = "numeric"
  class(routes_plan$quietness) = "numeric"
  assign(paste0("routes_", plan, "_all"), routes_plan)
  routes_plan = routes_plan |> 
    filter(length < 5000) 
  assign(x = paste0("routes_", plan), value = routes_plan)
}

for(plan in plans) {
  routes_plan_all = get(paste0("routes_", plan, "_all"))
  route_summaries = routes_plan_all |> 
    group_by(route_number) |> 
    summarise(trips_modelled = mean(trips_modelled),
              length = mean(length),
              desire_line_length = mean(desire_line_length)
    )
  assign(paste0("route_summaries_all_", plan), route_summaries)
}

# median route length for all students living within 5km euclidean distance of the school
library(matrixStats)
weightedMedian(route_summaries_all_quiet$length, route_summaries_all_quiet$trips_modelled)
# [1] 814.0006
weightedMedian(route_summaries_all_fast$length, route_summaries_all_fast$trips_modelled)
# [1] 785.9941

# median desire line length for all students living within 5km euclidean distance of the school
weightedMedian(res_cc$desire_line_length, res_cc$trips_modelled)
# [1] 564.7516

# trips_modelled within 5km euclidean distance
under_5 = route_summaries_all_quiet |> 
  filter(desire_line_length < 5000)
sum(under_5$trips_modelled)
# [1] 238

# PCT cycle uptake --------------------------------------------------------

library(pct)

for(plan in plans) {
  routes_plan = get(x = paste0("routes_", plan))
  routes_plan_pct = routes_plan |> 
    group_by(route_number) |> 
    mutate(
      pcycle_godutch = pct::uptake_pct_godutch_school2(
        case_when(length > 30000 ~ 30000, TRUE ~ length),
        route_hilliness),
      bicycle_godutch = pcycle_godutch * trips_modelled
    )
  assign(paste0("routes_", plan, "_pct"), routes_plan_pct)
  rnet_plan_raw = routes_plan_pct |> 
    overline(attrib = c("trips_modelled", "bicycle_godutch", "quietness", "gradient_smooth"), 
             fun = list(sum = sum, mean = mean))
  rnet_plan = rnet_plan_raw |> 
    transmute(trips_modelled = trips_modelled_sum, 
              bicycle_godutch = bicycle_godutch_sum,
              quietness = round(quietness_mean),
              gradient = round(gradient_smooth_mean*100))
  assign(x = paste0("rnet_", plan), value = rnet_plan)
}

tm_shape(rnet_quiet) +
  tm_lines("bicycle_godutch", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100)) 
tm_shape(rnet_fast) +
  tm_lines("bicycle_godutch", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100))

m2 = tm_shape(rnet_quiet |> rename(`Potential cyclists` = bicycle_godutch)) +
  tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100))
m6 = tm_shape(rnet_fast |> rename(`Potential cyclists` = bicycle_godutch)) +
  tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100))

quietness_breaks = c(0, 25, 50, 75, 100)
pal = c('#882255','#CC6677', '#44AA99', '#117733')

m9 = tm_shape(rnet_quiet |> rename(`Cycle friendliness` = quietness)) +
  tm_lines("Cycle friendliness", palette = pal, lwd = 3, breaks = quietness_breaks)
m10 = tm_shape(rnet_fast |> rename(`Cycle friendliness` = quietness)) +
  tm_lines("Cycle friendliness", palette = pal, lwd = 3, breaks = quietness_breaks)

# Explore results ---------------------------------------------------------

summary(routes_quiet$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 102    1127    2062    2292    3418    4829  
summary(routes_fast$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 102     965    1824    2217    3704    4982 


# Total number of students cycling to school under Go Dutch
for(plan in plans) {
  routes_plan_pct = get(paste0("routes_", plan, "_pct"))
  route_summaries = routes_plan_pct |> 
    rename(OBJECTID = O) |> 
    group_by(OBJECTID,
             route_number, 
             trips_modelled, 
             bicycle_godutch, 
             length
    ) |> 
    summarise() |> 
    ungroup()
  assign(paste0("route_summaries_", plan), route_summaries)
}

quiet_join = route_summaries_quiet |>
  sf::st_drop_geometry() |>
  select(OBJECTID, route_number, bicycle_godutch)
centroids_quiet_5km = inner_join(centroids_5km, quiet_join, by = "OBJECTID")

fast_join = route_summaries_fast |>
  sf::st_drop_geometry() |>
  select(OBJECTID, route_number, bicycle_godutch)
centroids_fast_5km = inner_join(centroids_5km, fast_join, by = "OBJECTID")


tm_shape(zone_centroids) + tm_bubbles("N_INDIVIDUOS_0_14", alpha = 0.3) +
  tm_shape(ciclo_school) + tm_bubbles(col = "green")
tm_shape(centroids_quiet_5km |> rename(`'Go Dutch' cycling potential` = bicycle_godutch)) + tm_bubbles("'Go Dutch' cycling potential", alpha = 0.3) +
  tm_shape(ciclo_school) + tm_bubbles(col = "green")
tm_shape(centroids_fast_5km |> rename(`'Go Dutch' cycling potential` = bicycle_godutch)) + tm_bubbles("'Go Dutch' cycling potential", alpha = 0.3) +
  tm_shape(ciclo_school) + tm_bubbles(col = "green")

# n students within 5km route distance of school
sum(route_summaries_quiet$trips_modelled)
# [1] 213.9925
sum(route_summaries_fast$trips_modelled)
# [1] 215.2971

# n_cyclists under go dutch
sum(route_summaries_quiet$bicycle_godutch)
# [1] 75.2093
sum(route_summaries_fast$bicycle_godutch)
# [1] 75.48766


# Bundling function -------------------------------------------------------

# set of routes + (distance weighted number of cyclists on adjoining routes * route length within cropped zone) + minimum threshold 
# buffer identifies when route segments have the same geometry

rnet = rnet_quiet
routes = route_summaries_quiet
attribute_trips = "bicycle_godutch"
min_trips = 3
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
  routes_crop = sf::st_intersection(routes, rnet_buffer)
  
  routes_subset = routes_crop |> 
    distinct(geometry)
  routes_subset$id = seq.int(nrow(routes_subset))
  routes_subset$length = sf::st_length(routes_subset) |> 
    as.numeric()
  attribute_trips_x_distance = paste0(attribute_trips, "_x_distance")
  routes_subset[[attribute_trips_x_distance]] = NA
  class(routes_subset[[attribute_trips_x_distance]]) = "numeric"
  for (i in 1:nrow(routes_subset)) {
    route_i = routes_subset[i,]
    route_i_buffer = geo_buffer(route_i, dist = buffer)
    rnet_in_route = rnet_subset[route_i_buffer, , op = sf::st_within]
    routes_subset[i, attribute_trips_x_distance] = weighted.mean(rnet_in_route[[attribute_trips]], rnet_in_route$length)*routes_subset[i,]$length
  }
  routes_subset = routes_subset[order(routes_subset[[attribute_trips_x_distance]]),]
  routes_subset
}


# Get all routes within min_trips rnet, ordered by length*mean_godutch
ordered_routes_quiet = cycle_bus_routes(route_summaries_quiet, rnet_quiet, min_trips = 3, attribute_trips = "bicycle_godutch", buffer = 10)
ordered_routes_fast = cycle_bus_routes(route_summaries_fast, rnet_fast, min_trips = 3, attribute_trips = "bicycle_godutch", buffer = 10)

tm_shape(ordered_routes_quiet) + tm_lines()
tm_shape(ordered_routes_fast) + tm_lines()

# now identify which centroid OBJECTIDs are associated with which selected routes
buffer = 10
for(plan in plans) {
  rnet_plan = get(paste0("rnet_", plan))
  routes = get(paste0("route_summaries_", plan))
  
  rnet_subset = rnet_plan[rnet_plan[[attribute_trips]] > min_trips,]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  rnet_union = sf::st_union(rnet_subset)
  rnet_buffer = geo_buffer(rnet_union, dist = buffer)
  routes_crop = sf::st_intersection(routes, rnet_buffer)
  
  ordered_routes = get(paste0("ordered_routes_", plan))
  join = sf::st_join(routes_crop, ordered_routes, join = st_equals)
  route_stats = join |> 
    mutate(full_length = length.x,
           bike_bus_length = length.y,
           dist_to_bike_bus = length.x - length.y) |> 
    select(-length.x, -length.y)
  assign(paste0("route_stats_", plan), route_stats)
}


rnet_quiet_subset = rnet_quiet[rnet_quiet[[attribute_trips]] > min_trips,]
m3 = tm_shape(rnet_quiet_subset |> rename(`Potential cyclists` = bicycle_godutch)) +
  tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(3, 5, 10, 100))

rnet_fast_subset = rnet_fast[rnet_fast[[attribute_trips]] > min_trips,]
m7 = tm_shape(rnet_fast_subset |> rename(`Potential cyclists` = bicycle_godutch)) +
  tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(3, 5, 10, 100))

# Remove routes with start points too close to other higher ranked routes

routes = ordered_routes_quiet
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
  routes_subset = routes_subset[order(-routes_subset[["bicycle_godutch_x_distance"]]),]
  routes_subset = routes_subset |> 
    slice_head(n = top_n)
  routes_subset
}

top_routes_quiet = filter_routes(routes = ordered_routes_quiet, buffer = 300, top_n = 3)
top_routes_fast = filter_routes(routes = ordered_routes_fast, buffer = 300, top_n = 3)

# Bubbles by number of students
tm_shape(top_routes_quiet) + tm_lines() +
  tm_shape(centroids_quiet_5km) + tm_bubbles("N_INDIVIDUOS_0_14")
tm_shape(top_routes_fast) + tm_lines() +
  tm_shape(centroids_fast_5km) + tm_bubbles("N_INDIVIDUOS_0_14")

# Bubbles by Go Dutch uptake
# m4 = tm_shape(centroids_quiet_5km |> rename(`Potential cyclists` = bicycle_godutch)) + tm_bubbles("Potential cyclists", alpha = 0.3) + 
#   tm_shape(ciclo_school) + tm_bubbles(col = "green") +
#   tm_shape(top_routes_quiet) + tm_lines(lwd = 2)
# m8 = tm_shape(centroids_fast_5km |> rename(`Potential cyclists` = bicycle_godutch)) + tm_bubbles("Potential cyclists", alpha = 0.3) + 
#   tm_shape(ciclo_school) + tm_bubbles(col = "green") +
#   tm_shape(top_routes_fast) + tm_lines(lwd = 2)

# Matching centroids to routes --------------------------------------------

# Top routes centroids
# Adds in all (centroids for) routes with origins within ~10m of the top routes
# finds which top route the discarded routes were closest to
routes_cents = ordered_routes_quiet
top_routes = top_routes_quiet

match_centroids = function(
    routes_cents,
    top_routes,
    route_stats,
    centroids_5km
) {
  routes_cents = routes_cents |> filter(!id %in% top_routes$id) # removed the top routes from this object
  routes_cents$pick = NA
  z = nrow(routes_cents)
  for(i in 1:z) {
    route_i = routes_cents[i, ]
    p = st_cast(route_i$geometry, "POINT")
    p1 = p[1]
    top_route_one = top_routes[1,]
    top_route_two = top_routes[2,]
    top_route_three = top_routes[3,]
    distance_one = units::drop_units(sf::st_distance(p1, top_route_one))
    distance_two = units::drop_units(sf::st_distance(p1, top_route_two))
    distance_three = units::drop_units(sf::st_distance(p1, top_route_three))
    a = c(distance_one, distance_two, distance_three)
    closest = which.min(a) # if two distances are tied, this picks the higher ranked route
    dist = a[closest]
    pick = if(dist <= (buffer + 5)) closest else 0 # have to allow distances a bit greater than buffer (10m) to account for rounding errors
    routes_cents$pick[i] = pick
  }
  
  routes_both = routes_cents |> 
    filter(pick != 0)
  routes_both = rbind(routes_both, top_routes |> mutate(pick = row_number()))
  routes_both = routes_both |> 
    mutate(pick = as.character(pick))
  
  top_cents = inner_join(route_stats, routes_both |> sf::st_drop_geometry(), by = "id")
  
  # remove routes <500m or where less than half of the distance is on the bike bus
  top_cents = top_cents |> 
    filter(
      full_length > 500,
      bike_bus_length > dist_to_bike_bus
    )
  
  cents = inner_join(centroids_5km, top_cents |> sf::st_drop_geometry(), by = "OBJECTID")
  cents
}

cents_quiet = match_centroids(ordered_routes_quiet, top_routes_quiet, route_stats_quiet, centroids_5km)
cents_fast = match_centroids(ordered_routes_fast, top_routes_fast, route_stats_fast, centroids_5km)

to_map_quiet = top_routes_quiet |> 
  mutate(`Candidate route` = as.character(row_number())) |> 
  arrange(desc(`Candidate route`))
to_map_fast = top_routes_fast |> 
  mutate(`Candidate route` = as.character(row_number())) |> 
  arrange(desc(`Candidate route`))

m4 = tm_shape(cents_quiet |> rename(`Potential cyclists` = bicycle_godutch)) + 
  tm_bubbles("Potential cyclists", col = "pick") +
  tm_shape(school) + tm_bubbles(col = "green") +
  tm_shape(to_map_quiet) + 
  tm_lines(lwd = 3, col = "Candidate route")
m8 = tm_shape(cents_fast |> rename(`Potential cyclists` = bicycle_godutch)) + 
  tm_bubbles("Potential cyclists", col = "pick") +
  tm_shape(school) + tm_bubbles(col = "green") +
  tm_shape(to_map_fast) + 
  tm_lines(lwd = 3, col = "Candidate route")




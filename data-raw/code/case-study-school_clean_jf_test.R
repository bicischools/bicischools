library(sf)
library(ggplot2)
library(units)
library(tidyverse)
library(tmap)
tmap_mode("view")

# Explore existing data for Escola Básica Adriano Correia de Oliveira

schools_year = readRDS("data-raw/SCHOOLS_year.Rds")
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

# Distance frequency curves -----------------------------------------------
# For routes and desire lines
home = home |>
  mutate(
    linestring_route_length = st_length(geometry),
    desire_line_length = st_distance(home_coords, school)[, 1]
  )

# Assign home locations to enumeration districts --------------------------
# So we can route from zone centroids instead of home postcodes, for anonymity

bgri = read_sf("../internal/BGRI21_170/BGRI21_170.gpkg")
bgri_4326 = st_transform(bgri, 4326)
bgri_4326 = st_make_valid(bgri_4326)
home_assigned = st_join(bgri_4326, home_dots, left = F)
home_assigned = home_assigned |>
  select(OBJECTID, DTMN21, N_INDIVIDUOS_0_14)

# Now make table showing the number of students in each zone (list each zone once only)
zone_counts = home_assigned |>
  group_by(OBJECTID, DTMN21, N_INDIVIDUOS_0_14) |>
  summarise(n_students = n()) |>
  ungroup()

saveRDS(zone_counts, "data-raw/zone_counts.Rds")

# Then assign centroids for each zone to generate OD dataset at the zone-school level with counts
zone_centroids = st_centroid(zone_counts)

# Filter out centroids >5km euclidean distance from the school
zone_centroids = zone_centroids |>
  mutate(
    desire_line_length = st_distance(geom, school)[, 1],
    desire_line_length = drop_units(desire_line_length)
  )

centroids_5km = zone_centroids |>
  filter(desire_line_length < 5000)


saveRDS(centroids_5km, "data-raw/centroids_5km.Rds")
centroids_5km = readRDS("data-raw/centroids_5km.Rds")

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
# plan = "quiet"
plans = c("quiet")


## 0 Routing -----------------------------------------------------------------

for (plan in plans) {
  location = paste0("../internal/routes-", plan, "-casestudy.Rds")
  routes_plan_location = location
  if (file.exists(routes_plan_location)) {
    routes_plan = readRDS(routes_plan_location)
  } else {
    plan_name = paste0(plan, "est")
    routes_plan = route(
      l = od_5km,
      route_fun = cyclestreets::journey,
      plan = plan_name
    )
    browser()
    routes_plan = routes_plan |>
      group_by(route_number) |>
      mutate(route_hilliness = weighted.mean(gradient_smooth, distances)) |>
      ungroup()
    saveRDS(routes_plan, paste0("../internal/routes-", plan, "-casestudy.Rds"))
  }
  class(routes_plan$route_number) = "character"
  class(routes_plan$length) = "numeric"
  class(routes_plan$quietness) = "numeric"
  # assign(paste0("routes_", plan, "_all"), routes_plan)
  routes_plan = routes_plan |>
    filter(length < 5000)
  assign(x = paste0("routes_", plan), value = routes_plan)
  rm(routes_plan, location, routes_plan_location)
}


# routes_quiet_pkg <- bici_routes(od_5km,trips.col = "n_students",distance.threshold = 5e3)
#
# saveRDS(routes_quiet_pkg,"data-raw/routes_quiet_pkg.rds")
routes_quiet_pkg <- readRDS("data-raw/routes_quiet_pkg.rds")


plot(routes_quiet$geometry, lwd = 3, reset = T)

plot(routes_quiet_pkg$geometry, col = "red", alpha = 0.3, add = T)


# median desire line length for all students living within 5km euclidean distance of the school
library(matrixStats)

# PCT cycle uptake --------------------------------------------------------

library(pct)

## 1 uptake -----------------------------------------------------------------

for (plan in plans) {
  routes_plan = get(x = paste0("routes_", plan))
  routes_plan_pct = routes_plan |>
    group_by(route_number) |>
    mutate(
      pcycle_godutch = pct::uptake_pct_godutch_school2(
        case_when(length > 30000 ~ 30000, TRUE ~ length),
        route_hilliness
      ),
      bicycle_godutch = pcycle_godutch * n_students
    )
  assign(paste0("routes_", plan, "_pct"), routes_plan_pct)
  rnet_plan_raw = routes_plan_pct |>
    stplanr::overline(
      attrib = c(
        "n_students",
        "bicycle_godutch",
        "quietness",
        "gradient_smooth"
      ),
      fun = list(sum = sum, mean = mean)
    )
  rnet_plan = rnet_plan_raw |>
    transmute(
      n_students = n_students_sum,
      bicycle_godutch = bicycle_godutch_sum,
      quietness = round(quietness_mean),
      gradient = round(gradient_smooth_mean * 100)
    )
  assign(x = paste0("rnet_", plan), value = rnet_plan)
  rm(routes_plan, rnet_plan, rnet_plan_raw, routes_plan_pct)
}

routes_quiet_pct_pkg <- routes_pct_uptake(
  routes_quiet_pkg,
  trips.col = "n_students"
)

rnet_quiet_pkg <- routes_network(routes_quiet_pct_pkg, trips.col = "n_students")


plot(rnet_quiet$geometry)

plot(rnet_quiet_pkg$geometry, col = "red", alpha = 0.3, add = T)

# Explore results ---------------------------------------------------------

## 2 summaries -----------------------------------------------------------------

# Total number of students cycling to school under Go Dutch
for (plan in plans) {
  routes_plan_pct = get(paste0("routes_", plan, "_pct"))
  route_summaries = routes_plan_pct |>
    group_by(OBJECTID, route_number, n_students, bicycle_godutch, length) |>
    summarise() |>
    ungroup()
  assign(paste0("route_summaries_", plan), route_summaries)
  rm(routes_plan_pct, route_summaries)
}

route_summaries_quiet_pkg <- summarise_routes(
  routes_quiet_pct_pkg,
  origin.col = "OBJECTID",
  trips.col = "n_students",
  destination.col = "DTMN21"
)

plot(route_summaries_quiet$geometry)

plot(route_summaries_quiet_pkg$geometry, col = "red", alpha = 0.3, add = T)


# Bundling function -------------------------------------------------------

# set of routes + (distance weighted number of cyclists on adjoining routes * route length within cropped zone) + minimum threshold
# buffer identifies when route segments have the same geometry

cycle_bus_routes = function(
  routes,
  rnet,
  min_trips = 3,
  attribute_trips = "bicycle_godutch",
  buffer = 10
) {
  rnet_subset = rnet[rnet[[attribute_trips]] > min_trips, ]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  rnet_union = sf::st_union(rnet_subset)
  rnet_buffer = stplanr::geo_buffer(rnet_union, dist = buffer)
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
    route_i = routes_subset[i, ]
    route_i_buffer = stplanr::geo_buffer(route_i, dist = buffer)
    rnet_in_route = rnet_subset[route_i_buffer, , op = sf::st_within]
    routes_subset[i, attribute_trips_x_distance] = weighted.mean(
      rnet_in_route[[attribute_trips]],
      rnet_in_route$length
    ) *
      routes_subset[i, ]$length
  }
  routes_subset = routes_subset[
    order(routes_subset[[attribute_trips_x_distance]]),
  ]
  routes_subset
}

## 3 ordered routes -----------------------------------------------------------------

# Get all routes within min_trips rnet, ordered by length*mean_godutch
ordered_routes_quiet = cycle_bus_routes(
  routes = route_summaries_quiet,
  rnet = rnet_quiet,
  min_trips = 3,
  attribute_trips = "bicycle_godutch",
  buffer = 10
)

ordered_routes_quiet_test <- bicischools::cycle_bus_routes(
  routes = route_summaries_quiet,
  rnet = rnet_quiet,
  min_trips = 3,
  trips.col = "n_students",
  buffer = 10,
  attribute_trips = "bicycle_godutch"
)


ordered_routes_quiet_pkg <- bicischools::cycle_bus_routes(
  routes = route_summaries_quiet_pkg,
  rnet = rnet_quiet_pkg,
  min_trips = 3,
  trips.col = "n_students",
  buffer = 10,
  attribute_trips = "bicycle_godutch"
)


plot(ordered_routes_quiet$geometry, lwd = 3)
# plot(ordered_routes_quiet_pkg_test$geometry,col = "blue",alpha = 0.3,add = T)
plot(ordered_routes_quiet_test$geometry, col = "yellow", add = T, lwd = 1.5)

plot(ordered_routes_quiet_pkg$geometry, col = "red", add = T, lwd = 0.7)


## 4 Stats -----------------------------------------------------------------
# now identify which centroid OBJECTIDs are associated with which selected routes
attribute_trips = "bicycle_godutch"
min_trips = 3
buffer = 10

for (plan in plans) {
  rnet_plan = get(paste0("rnet_", plan))
  routes = get(paste0("route_summaries_", plan))

  rnet_subset = rnet_plan[rnet_plan[[attribute_trips]] > min_trips, ]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  rnet_union = sf::st_union(rnet_subset)
  rnet_buffer = stplanr::geo_buffer(rnet_union, dist = buffer)
  routes_crop = sf::st_intersection(routes, rnet_buffer)

  ordered_routes = get(paste0("ordered_routes_", plan))
  join = sf::st_join(routes_crop, ordered_routes, join = st_equals)
  route_stats = join |>
    mutate(
      full_length = length.x,
      bike_bus_length = length.y,
      dist_to_bike_bus = length.x - length.y
    ) |>
    select(-length.x, -length.y)
  assign(paste0("route_stats_", plan), route_stats)
  rm(
    route_stats,
    ordered_routes,
    rnet_buffer,
    rnet_union,
    rnet_subset,
    routes,
    rnet_plan,
    routes_crop,
    join
  )
}

route_stats_quiet_pkg <- calc_stats(
  routes = route_summaries_quiet_pkg,
  rnet_plan = rnet_quiet_pkg,
  ordered_routes = ordered_routes_quiet_pkg,
  min_trips = 3,
  buffer = 10
)

route_stats_quiet_test <- calc_stats(
  routes = route_summaries_quiet,
  rnet_plan = rnet_quiet,
  ordered_routes = ordered_routes_quiet,
  min_trips = 3,
  buffer = 10
)


plot(route_stats_quiet$geometry, lwd = 3)
# plot(ordered_routes_quiet_pkg_test$geometry,col = "blue",alpha = 0.3,add = T)
plot(route_stats_quiet_test$geometry, col = "yellow", add = T, lwd = 1.5)

plot(route_stats_quiet_pkg$geometry, col = "red", add = T, lwd = 0.7)

## 5 Filtering  -----------------------------------------------------------------

# routes = ordered_routes_quiet

filter_routes = function(
  routes,
  buffer,
  top_n,
  selected_routes = nrow(routes)
) {
  z = nrow(routes) - 1
  for (i in 1:z) {
    route_i = routes[i, ]
    p = st_cast(route_i$geometry, "POINT")
    p1 = p[1]
    better_routes = routes |>
      slice_tail(n = nrow(routes) - i)
    better_routes_union = sf::st_union(better_routes)
    distance = units::drop_units(sf::st_distance(p1, better_routes_union))
    if (distance > buffer) {
      selected_routes = c(selected_routes, i)
    }
    selected_routes
  }
  routes_subset = routes[selected_routes, ]
  routes_subset = routes_subset[
    order(-routes_subset[["bicycle_godutch_x_distance"]]),
  ]
  routes_subset = routes_subset |>
    slice_head(n = top_n)
  routes_subset
}

top_routes_quiet = filter_routes(
  routes = ordered_routes_quiet,
  buffer = 300,
  top_n = 3
)

top_routes_quiet_pkg <- bicischools::filter_routes(
  routes = ordered_routes_quiet_pkg,
  buffer = 300,
  top_n = 3
)

top_routes_quiet_test <- bicischools::filter_routes(
  routes = ordered_routes_quiet,
  buffer = 300,
  top_n = 3
)

plot(top_routes_quiet$geometry, lwd = 3)
# plot(ordered_routes_quiet_pkg_test$geometry,col = "blue",alpha = 0.3,add = T)
plot(top_routes_quiet_test$geometry, col = "yellow", add = T, lwd = 1.5)

plot(top_routes_quiet_pkg$geometry, col = "red", add = T, lwd = 0.7)


# Matching centroids to routes --------------------------------------------

# Top routes centroids
# Adds in all (centroids for) routes with origins within ~10m of the top routes
# finds which top route the discarded routes were closest to

## 6 Matching centroids ---------------------------------------------------

match_centroids = function(
  routes_cents,
  top_routes,
  route_stats,
  centroids_5km
) {
  routes_cents = routes_cents |> filter(!id %in% top_routes$id) # removed the top routes from this object
  routes_cents$pick = NA
  z = nrow(routes_cents)
  for (i in 1:z) {
    route_i = routes_cents[i, ]
    p = st_cast(route_i$geometry, "POINT")
    p1 = p[1]
    top_route_one = top_routes[1, ]
    top_route_two = top_routes[2, ]
    top_route_three = top_routes[3, ]
    distance_one = units::drop_units(sf::st_distance(p1, top_route_one))
    distance_two = units::drop_units(sf::st_distance(p1, top_route_two))
    distance_three = units::drop_units(sf::st_distance(p1, top_route_three))
    a = c(distance_one, distance_two, distance_three)
    closest = which.min(a) # if two distances are tied, this picks the higher ranked route
    dist = a[closest]
    pick = if (dist <= (buffer + 5)) closest else 0 # have to allow distances a bit greater than buffer (10m) to account for rounding errors
    routes_cents$pick[i] = pick
  }

  routes_both = routes_cents |>
    filter(pick != 0)
  routes_both = rbind(routes_both, top_routes |> mutate(pick = row_number()))
  routes_both = routes_both |>
    mutate(pick = as.character(pick))

  top_cents = inner_join(
    route_stats,
    routes_both |> sf::st_drop_geometry(),
    by = "id"
  )

  # remove routes <500m or where less than half of the distance is on the bike bus
  top_cents = top_cents |>
    filter(
      full_length > 500,
      bike_bus_length > dist_to_bike_bus
    )

  cents = inner_join(
    centroids_5km,
    top_cents |> sf::st_drop_geometry(),
    by = "OBJECTID"
  )
  cents
}

cents_quiet = match_centroids(
  routes_cents = ordered_routes_quiet,
  top_routes = top_routes_quiet,
  route_stats = route_stats_quiet,
  centroids_5km
)

cents_quiet_pkg <- bicischools::match_centroids(
  routes_cents = ordered_routes_quiet_pkg,
  top_routes = top_routes_quiet_pkg,
  route_stats = route_stats_quiet_pkg,
  origins = centroids_5km,
  max_dist_to_bikebus = 30,
  min_dist_threshold = 400,
  id.col = "id",
  origin.id = "OBJECTID",
  origin_route.id = "OBJECTID",
  attribute_trips = "bicycle_godutch"
)

cents_quiet_test <- bicischools::match_centroids(
  routes_cents = ordered_routes_quiet,
  top_routes = top_routes_quiet,
  route_stats = route_stats_quiet,
  origins = centroids_5km,
  max_dist_to_bikebus = 15,
  min_dist_threshold = 500,
  id.col = "id",
  origin.id = "OBJECTID",
  origin_route.id = "OBJECTID",
  attribute_trips = "bicycle_godutch"
)

tm_shape(cents_quiet) +
  tm_dots("pick") +
  tm_shape(top_routes_quiet |> mutate(id = factor(id))) +
  tm_lines("id")

tm_shape(cents_quiet_test) +
  tm_dots("pick") +
  tm_shape(
    top_routes_quiet_test |>
      mutate(id = factor(id))
  ) +
  tm_lines("id")

tm_shape(cents_quiet_pkg) +
  tm_dots("pick") +
  tm_shape(
    top_routes_quiet_pkg |>
      mutate(id = factor(id))
  ) +
  tm_lines("id")

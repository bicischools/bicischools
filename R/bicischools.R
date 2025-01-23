#' Generate candidate routes
#' 
#' See the code/case-study-school.R script for starting point
#'
#' @param routes 
#' @param rnet 
#' @param min_trips 
#' @param attribute_trips 
#' @param buffer 
#'
#' @return
#' @export
#'
#' @examples
#' # rnet = rnet_quiet
#' # routes = route_summaries_quiet
#' # attribute_trips = "bicycle_godutch"
#' # min_trips = 3
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
rnet_quiet_subset = rnet_quiet[rnet_quiet[[attribute_trips]] > min_trips,]
rnet_quiet_subset$length = sf::st_length(rnet_quiet_subset) |>
  as.numeric()
rnet_quiet_union = sf::st_union(rnet_quiet_subset)
rnet_quiet_buffer = geo_buffer(rnet_quiet_union, dist = buffer)
routes_crop = sf::st_intersection(routes, rnet_quiet_buffer)

join = sf::st_join(routes_crop, ordered_routes_quiet, join = st_equals)
route_stats_quiet = join |> 
  mutate(full_length = length.x,
         bike_bus_length = length.y,
         dist_to_bike_bus = length.x - length.y) |> 
  select(-length.x, -length.y)



m3 = tm_shape(rnet_quiet_subset |> rename(`Potential cyclists` = bicycle_godutch)) + tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(3, 5, 10, 100))
rnet_fast_subset = rnet_fast[rnet_fast[[attribute_trips]] > min_trips,]
m7 = tm_shape(rnet_fast_subset |> rename(`Potential cyclists` = bicycle_godutch)) + tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(3, 5, 10, 100))

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
  tm_shape(centroids_quiet_5km) + tm_bubbles("n_students")
tm_shape(top_routes_fast) + tm_lines() +
  tm_shape(centroids_fast_5km) + tm_bubbles("n_students")

# Bubbles by Go Dutch uptake
m4 = tm_shape(centroids_quiet_5km |> rename(`Potential cyclists` = bicycle_godutch)) + tm_bubbles("Potential cyclists", alpha = 0.3) + 
  tm_shape(school) + tm_bubbles(fill = "green") +
  tm_shape(top_routes_quiet) + tm_lines(lwd = 2)
m8 = tm_shape(centroids_fast_5km |> rename(`Potential cyclists` = bicycle_godutch)) + tm_bubbles("Potential cyclists", alpha = 0.3) + 
  tm_shape(school) + tm_bubbles(fill = "green") +
  tm_shape(top_routes_fast) + tm_lines(lwd = 2)

# Could also add feature to function so routes are penalised if students live far away from the route origin?

# For panel figure in paper
tmap_arrange(m1, m2, m3, m4, m5, m6, m7, m8, nrow = 2)
# tmap_arrange(m1, m5, m2, m6, m3, m7, m4, m8, nrow = 4)

tmap_arrange(m9, m10)

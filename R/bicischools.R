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

cycle_bus_routes = function(routes,
                            rnet,
                            min_trips = 3,
                            attribute_trips = "bicycle_godutch",
                            buffer = 10) {

  rnet_subset = rnet[rnet[[attribute_trips]] > min_trips, ]
  
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
    route_i = routes_subset[i, ]
    route_i_buffer = geo_buffer(route_i, dist = buffer)
    rnet_in_route = rnet_subset[route_i_buffer, , op = sf::st_within]
    routes_subset[i, attribute_trips_x_distance] = weighted.mean(rnet_in_route[[attribute_trips]], rnet_in_route$length) *
      routes_subset[i, ]$length
  }
  routes_subset = routes_subset[order(routes_subset[[attribute_trips_x_distance]]), ]
  routes_subset
}



#' Title
#'
#' @param routes 
#' @param buffer 
#' @param top_n 
#'
#' @returns
#' @export
#'
#' @examples

filter_routes = function(routes,
                         buffer,
                         top_n) {
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
  routes_subset = routes_subset[order(-routes_subset[["bicycle_godutch_x_distance"]]), ]
  routes_subset = routes_subset |>
    slice_head(n = top_n)
  routes_subset
}



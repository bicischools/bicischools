#' Title
#'
#' @param routes 
#' @param min_trips 
#' @param attribute_trips 
#' @param trips.col 
#' @param buffer 
#'
#' @returns a dataset with cycle bus routes
#' @export
#' 
#' @examples
#' \dontrun{
#' cycle_bus_routes(routes_almada)
#' }
cycle_bus_routes = function(
    routes,
    min_trips = 0,
    attribute_trips = c("bicycle_godutch",
                        "quietness",
                        "gradient_smooth"),
    trips.col = names(routes)[grep("trip", names(routes))],
    buffer = 10
) {
  
  
  attribute_trips <- match.arg(attribute_trips)
  
  rnet <- routes |>
    stplanr::overline(
      attrib = c(
        trips.col,
        attribute_trips
      ),
      fun = sum
    )
  
  rnet_subset = rnet[rnet[[attribute_trips]] > min_trips, ]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  
  rnet_union = sf::st_union(rnet_subset)
  
  rnet_buffer = stplanr::geo_buffer(rnet_union, dist = buffer)
  
  routes_crop = sf::st_intersection(routes, rnet_buffer)

  routes_subset = routes_crop |>
    dplyr::distinct(geometry)
  
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
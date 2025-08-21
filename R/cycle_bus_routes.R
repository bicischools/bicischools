#' Create bicycle bus route network from individual routes
#'
#' Aggregates overlapping bicycle routes to identify high-demand corridors
#' suitable for bike bus implementation. Routes are scored by distance-weighted
#' cycling potential for prioritization.
#'
#' @param routes An sf object with route geometries and cycling attributes.
#' @param min_trips Minimum threshold for attribute filtering. Default: 0.
#' @param attribute_trips Attribute for scoring: "bicycle_godutch", "quietness", or "gradient_smooth".
#' @param trips.col Column name(s) for trip counts. Auto-detected by default.
#' @param buffer Buffer distance (meters) for spatial operations. Default: 10.
#'
#' @return An sf object with optimized routes, sorted by distance-weighted scores.
#'   Lower scores indicate higher priority routes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' bike_network <- cycle_bus_routes(routes_almada)
#' 
#' # High-potential routes only
#' priority_routes <- cycle_bus_routes(routes_almada, min_trips = 5, 
#'                                   attribute_trips = "bicycle_godutch")
#' }
#'
#' @seealso \code{\link{filter_routes}}
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
    dplyr::distinct(.data$geometry)
  
  routes_subset$id = seq.int(nrow(routes_subset))
  
  routes_subset$length = sf::st_length(routes_subset) |>
    as.numeric()
  
  attribute_trips_x_distance = paste0(attribute_trips, "_x_distance")
  
  routes_subset[[attribute_trips_x_distance]] = NA
  
  class(routes_subset[[attribute_trips_x_distance]]) = "numeric"
  
  route_buffer = stplanr::geo_buffer(routes_subset, dist = buffer)
  
  routes_within <- sf::st_contains(route_buffer,rnet_subset)
  
  routes_subset[, attribute_trips_x_distance] <- vapply(routes_within,function(x){
    stats::weighted.mean(
      rnet_subset[[attribute_trips]][x],
      rnet_subset$length[x]
    )  
  },
  FUN.VALUE = numeric(1))
  
  routes_subset[, attribute_trips_x_distance] <- routes_subset[[attribute_trips_x_distance]]*routes_subset$length
  
  routes_subset = routes_subset[order(routes_subset[[attribute_trips_x_distance]],decreasing = FALSE),  ]
  
  routes_subset[!is.nan(routes_subset[[attribute_trips_x_distance]]),]
}

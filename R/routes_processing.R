#' Title
#'
#' @param routes 
#' @param attribute_trips 
#' @param buffer 
#' @param top_n 
#'
#' @returns a dataset with the filtered routes
#' @export
#'
#' @examples
#' \dontrun{
#' cycle_bus_routes(routes_almada)
#' }
filter_routes = function(routes,
                         attribute_trips = c("bicycle_godutch",
                                             "quietness",
                                             "gradient_smooth"),
                         buffer = 300,
                         top_n = 3
                         ) {
  
  
  attribute_trips <- match.arg(attribute_trips)
  
  z = nrow(routes) - 1
  
  attribute_trips_x_distance = paste0(attribute_trips, "_x_distance")
  
  routes_sorted <- routes[order(routes[[attribute_trips_x_distance]],decreasing = FALSE), ]
  
  selected_routes = numeric(0)
  
  for (i in 1:(nrow(routes) - 1)) {
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
    
  }
  routes_subset = routes[selected_routes, ]
  routes_subset = routes_subset[order(-routes_subset[["bicycle_godutch_x_distance"]]), ]
  routes_subset = routes_subset |>
    slice_head(n = top_n)
  routes_subset
}




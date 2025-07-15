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
  
  attribute_trips_x_distance = paste0(attribute_trips, "_x_distance")
  
  routes_sorted <- routes[order(routes[[attribute_trips_x_distance]],decreasing = FALSE), ]
  
  route_names <- names(routes_sorted)[grep("geom",x = names(routes_sorted),invert = T)]
  
  selected_routes = numeric(0)
  
  routes_sorted_initial_point <- routes_sorted |>
    sf::st_cast( "POINT") |> 
    dplyr::slice_head(n = 1,by = dplyr::all_of(route_names))
  
  
  
  for (i in 1:(nrow(routes_sorted) - 1)) {
    p1 = routes_sorted_initial_point[i,]
    better_routes = routes_sorted |> 
      dplyr::slice_tail(n = nrow(routes_sorted) - i)
    better_routes_union = sf::st_union(better_routes)
    distance = units::drop_units(sf::st_distance(p1, better_routes_union))
    if (distance > buffer) {
      selected_routes = c(selected_routes, i)
    }
    
  }
  routes_subset = routes[selected_routes, ]
  routes_subset = routes_subset[order(-routes_subset[[attribute_trips_x_distance]]), ]
  routes_subset = routes_subset |>
    dplyr::slice_head(n = top_n)
  routes_subset
}




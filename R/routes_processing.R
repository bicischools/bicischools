#' Filter bicycle routes based on spatial proximity and attribute ranking
#'
#' This function filters a set of bicycle routes by selecting routes that are 
#' spatially separated (based on start point distances) and ranked by a specified 
#' attribute. It's designed to identify optimal bike bus routes that don't overlap 
#' at their origins and maximize cycling potential.
#'
#' @param routes An sf object containing route geometries with associated attributes.
#'   Must contain a geometry column and the specified attribute ranking column.
#' @param attribute_trips Character string specifying which attribute to use for 
#'   ranking routes. One of "bicycle_godutch", "quietness", or "gradient_smooth".
#'   The function will look for a column named "{attribute_trips}_x_distance".
#' @param buffer Numeric value specifying the minimum distance (in meters) between 
#'   route start points. Routes with start points closer than this distance will 
#'   be filtered out to avoid spatial overlap. Default is 300 meters.
#' @param top_n Integer specifying the maximum number of routes to return after 
#'   filtering and ranking. Default is 3.
#'
#' @returns a dataset with the filtered routes
#' @export
#'
#' @examples
#' \dontrun{
#' cycle_bus_routes <- cycle_bus_routes(routes_almada)
#' filter_routes(cycle_bus_routes)
#' }
#'
#' @seealso 
#' \code{\link{cycle_bus_routes}} for creating route networks

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
  
  
  
  if (nrow(routes_sorted) < 2) {
    routes_subset = routes_sorted |>
      dplyr::slice_head(n = top_n)
  } else {
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
  }
  routes_subset = routes_subset[order(routes_subset[[attribute_trips_x_distance]], decreasing = TRUE), ]
  routes_subset = routes_subset |>
    dplyr::slice_head(n = top_n)
  routes_subset
}




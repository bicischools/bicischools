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



calc_route_stats <- function(rnet_plan,
                             routes,
                             attribute_trips = c("bicycle_godutch",
                                                 "quietness",
                                                 "gradient_smooth"),
                             min_trips = 0)
                             {
  
  attribute_trips <- match.arg(attribute_trips)
                               
  
  group_by(route_number) |>
    summarise(
      n_students = mean(n_students),
      length = mean(length),
      desire_line_length = mean(desire_line_length)
    )
  
  
  rnet_subset = rnet_plan[rnet_plan[[attribute_trips]] > min_trips, ]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  rnet_union = sf::st_union(rnet_subset)
  rnet_buffer = geo_buffer(rnet_union, dist = buffer)
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
}




#' Title
#'
#' @param routes_cents 
#' @param top_routes 
#' @param route_stats 
#' @param centroids_5km 
#'
#' @returns
#' @export
#'
#' @examples
match_centroids = function(
    routes_cents,
    top_routes,
    route_stats,
    centroids,
    id.col = names(routes_cents)[1],
    dist_threshold = 15,
    attribute_trips = c("bicycle_godutch",
                        "quietness",
                        "gradient_smooth")
    ) {
  
  attribute_trips <- match.arg(attribute_trips)
  
  routes_cents_clean = routes_cents[!(routes_cents[[id.col]] %in% top_routes[[id.col]]),] # removed the top routes from this object
  
  routes_cents_clean$pick = 0
  
  routes_cents_initial_point <- routes_cents_clean |>
    sf::st_cast( "POINT") |> 
    dplyr::slice_head(n = 1,by = dplyr::all_of(id.col))
  
  dist_matrix <- routes_cents_clean |> 
    sf::st_distance(top_routes)  
    
  index_closest <- apply(dist_matrix,1,which.min)
  
  mindist_closest <- apply(dist_matrix,1,min)
  
  index_closest[mindist_closest>dist_threshold] <- 0
  
  routes_cents_clean$pick <- index_closest
  
  
  
  routes_both = routes_cents_clean |>
    filter(pick != 0)
  routes_both = dplyr::bind_rows(routes_both,
                      top_routes |>
                        dplyr::mutate(pick = dplyr::row_number()))
  routes_both = routes_both |>
    dplyr::mutate(pick = as.character(pick))
  
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




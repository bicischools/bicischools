routes_pct_uptake <- function(routes,
                              trips.col = names(routes)[grep("trip", names(routes))],
                              length.col = "length",
                              max.length = 30000,
                              hilliness.col = "route_hilliness"){
   routes |> 
    dplyr::mutate(
      pcycle_godutch = pct::uptake_pct_godutch_school2(
        dplyr::case_when(
          !!dplyr::sym(length.col) > max.length ~ max.length,
          TRUE ~ !!dplyr::sym(length.col)),
        .data$route_hilliness
      ),
      bicycle_godutch = .data$pcycle_godutch * !!dplyr::sym(trips.col)
    )
}


routes_network <- function(routes,
                           attribute_trips = c("bicycle_godutch",
                                               "quietness",
                                               "gradient_smooth"),
                           trips.col = names(routes)[grep("trip", names(routes))]){
  
  attribute_trips <- match.arg(attribute_trips)
  
  if (attribute_trips == "bicycle_godutch"){
  rnet <- routes |>
    stplanr::overline(
      attrib = c(
        trips.col,
        attribute_trips
      ),
      fun = sum
    )
    
  }else{
    summary.trips.col <- paste0(trips.col,"_sum")
    summary.attribute_trips.col <- paste0(attribute_trips,"_mean")
    rnet <- routes |>
      stplanr::overline(
        attrib = c(
          trips.col,
          attribute_trips
        ),
        fun = list(sum = sum, mean = mean)
      ) |> 
      dplyr::select(dplyr::any_of(c(summary.trips.col,summary.attribute_trips.col)))
    
    names(rnet)[names(rnet) == summary.trips.col] <- trips.col
    names(rnet)[names(rnet) == summary.attribute_trips.col] <- attribute_trips
    
    }
  
  rnet
  }


summarise_routes <- function(routes,
                             id_route.col = "route_number",
                             origin.col = "O",
                             destination.col = "D",
                             trips.col = names(routes)[grep("trip", names(routes))],
                             attribute_trips = c("bicycle_godutch",
                                                 "quietness",
                                                 "gradient_smooth"),
                             length.col = "length"){
  
  attribute_trips <- match.arg(attribute_trips)

  routes |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(id_route.col,
                      origin.col,
                      destination.col,
                      trips.col,
                      attribute_trips,
                      length.col)))) |>
    dplyr::summarise() |>
    dplyr::ungroup()
  
}


cycle_bus_routes <- function(routes,
                              rnet,
                              min_trips = 0,
                              attribute_trips = c("bicycle_godutch",
                                                  "quietness",
                                                  "gradient_smooth"),
                              trips.col = names(routes)[grep("trip", names(routes))],
                              buffer = 10){
  
  attribute_trips <- match.arg(attribute_trips)
  
  rnet_subset <- rnet[rnet[[attribute_trips]] > min_trips, ]
  rnet_subset$length  <- sf::st_length(rnet_subset) |>
    as.numeric()
  
  rnet_union <- sf::st_union(rnet_subset)
  
  rnet_buffer <- sf::st_buffer(rnet_union, dist = buffer)
  
  routes_crop <- sf::st_intersection(routes, rnet_buffer)
  
  routes_subset <- routes_crop |>
    dplyr::distinct(.data$geometry)
  
  routes_subset$id  <-  seq.int(nrow(routes_subset))
  
  routes_subset$length <-  sf::st_length(routes_subset) |>
    as.numeric()
  
  attribute_trips_x_distance  <- paste0(attribute_trips, "_x_distance")
  
  routes_subset[[attribute_trips_x_distance]] <- NA
  
  class(routes_subset[[attribute_trips_x_distance]])  <-  "numeric"
  
  route_buffer <- sf::st_buffer(routes_subset, dist = buffer)
  
  routes_within <- sf::st_contains(route_buffer,rnet_subset)
  
  routes_subset[, attribute_trips_x_distance] <- vapply(routes_within,function(x){
    stats::weighted.mean(
      rnet_subset[[attribute_trips]][x],
      rnet_subset$length[x]
    )  
  },
  FUN.VALUE = numeric(1))
  
  routes_subset[, attribute_trips_x_distance] <- routes_subset[[attribute_trips_x_distance]]*routes_subset$length
  
  routes_subset[order(routes_subset[[attribute_trips_x_distance]],decreasing = FALSE),  ]
  
  # routes_subset[!is.nan(routes_subset[[attribute_trips_x_distance]]),]
  
}

calc_stats <- function(routes,
                       rnet_plan,
                       ordered_routes,
                       attribute_trips = c("bicycle_godutch",
                                           "quietness",
                                           "gradient_smooth"),
                       min_trips = 0,
                       buffer = 10
                       ){
  
  attribute_trips <- match.arg(attribute_trips)
  
  rnet_subset = rnet_plan[rnet_plan[[attribute_trips]] > min_trips, ]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  rnet_union = sf::st_union(rnet_subset)
  rnet_buffer = sf::st_buffer(rnet_union, dist = buffer)
  routes_crop = sf::st_intersection(routes, rnet_buffer)
  
  
  join = sf::st_join(routes_crop, ordered_routes, join = st_equals)
  
  route_stats = join |>
    mutate(
      full_length = length.x,
      bike_bus_length = length.y,
      dist_to_bike_bus = length.x - length.y
    ) |>
    select(-length.x, -length.y)
  
  route_stats
}
  

filter_routes <- function(routes,
                           attribute_trips = c("bicycle_godutch",
                                               "quietness",
                                               "gradient_smooth"),
                           buffer = 300,
                           top_n = 3){
  
  attribute_trips <- match.arg(attribute_trips)
  
  attribute_trips_x_distance = paste0(attribute_trips, "_x_distance")
  
  routes_sorted <- routes[order(routes[[attribute_trips_x_distance]],decreasing = FALSE), ]
  
  route_names <- names(routes_sorted)[grep("geom",x = names(routes_sorted),invert = T)]
  
  selected_routes = numeric(0)
  
  routes_sorted_initial_point <- routes_sorted |>
    sf::st_cast("POINT") |> 
    dplyr::slice_head(n = 1,by = dplyr::all_of(route_names))
  
  
  if (nrow(routes_sorted) < 2) {
    routes_subset = routes_sorted 
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

match_centroids = function(
    routes_cents,
    top_routes,
    route_stats,
    origins,
    origin.id = names(origins)[1],
    oringin_route.id = "O",
    id.col = names(routes_cents)[1],
    dist_threshold = 500,
    attribute_trips = c("bicycle_godutch",
                        "quietness",
                        "gradient_smooth")) {
  
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
    by = id.col
  )
  
  # remove routes <500m or where less than half of the distance is on the bike bus
  top_cents = top_cents |>
    filter(
      full_length > dist_threshold,
      bike_bus_length > dist_to_bike_bus
    )
  
  key_vector = oringin_route.id
  names(key_vector) <- origin.id
  
  cents = inner_join(
    centroids,
    top_cents |> sf::st_drop_geometry(),
    by = key_vector
  )
  cents
}




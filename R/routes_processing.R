#' Calculate cycling uptake using PCT Go Dutch scenario
#'
#' Estimates cycling potential based on route distance and hilliness using
#' the Propensity to Cycle Tool (PCT) Go Dutch model for school trips.
#'
#' @param routes An sf object with route data
#' @param trips.col Column name for trip counts. Auto-detected by default.
#' @param length.col Column name for route length. Default: "length"
#' @param max.length Maximum route length for uptake calculation. Default: 30000m
#' @param hilliness.col Column name for route hilliness. Default: "route_hilliness"
#'
#' @return An sf object with added columns: pcycle_godutch (proportion) and bicycle_godutch (absolute numbers)
#' @export
#'
#' @examples
#' \dontrun{
#' routes_with_uptake <- routes_pct_uptake(routes_almada)
#' }
routes_pct_uptake <- function(
  routes,
  trips.col = names(routes)[grep("trip", names(routes))],
  length.col = "length",
  max.length = 30000,
  hilliness.col = "route_hilliness"
) {
  routes |>
    dplyr::mutate(
      pcycle_godutch = pct::uptake_pct_godutch_school2(
        dplyr::case_when(
          !!dplyr::sym(length.col) > max.length ~ max.length,
          TRUE ~ !!dplyr::sym(length.col)
        ),
        !!dplyr::sym(hilliness.col)
      ),
      bicycle_godutch = .data$pcycle_godutch * !!dplyr::sym(trips.col)
    )
}


#' Create route network by overlaying individual routes
#'
#' Aggregates overlapping routes using stplanr::overline to create a network.
#' Sums bicycle_godutch values, averages other attributes.
#'
#' @param routes An sf object with route geometries
#' @param trips.col Column name for trip counts. Auto-detected by default.
#'
#' @return An sf object with aggregated route network
#' @export
#'
#' @examples
#' \dontrun{
#' network <- routes_network(routes_almada)
#' }

routes_network <- function(
  routes,
  trips.col = names(routes)[grep("trip", names(routes))]
) {
  attribute_trips <- c("quietness", "gradient_smooth")

  trips.col <- c(trips.col, "bicycle_godutch")

  summary.trips.col <- paste0(trips.col, "_sum")
  summary.attribute_trips.col <- paste0(attribute_trips, "_mean")

  rnet <- routes |>
    stplanr::overline(
      attrib = c(
        trips.col,
        attribute_trips
      ),
      fun = list(sum = sum, mean = mean)
    ) |>
    dplyr::select(dplyr::any_of(c(
      summary.trips.col,
      summary.attribute_trips.col
    )))

  names(rnet)[names(rnet) %in% summary.trips.col] <- trips.col
  names(rnet)[names(rnet) %in% summary.attribute_trips.col] <- attribute_trips

  if (any(names(rnet) == "gradient_smooth")) {
    rnet$gradient_smooth <- round(rnet$gradient_smooth * 100)
  }

  rnet
}


#' Summarise routes by grouping variables
#'
#' Groups routes by specified columns and creates route summaries,
#' typically used for aggregating routes by origin-destination pairs.
#'
#' @param routes An sf object with route data
#' @param id_route.col Column name for route ID. Default: "route_number"
#' @param origin.col Column name for origin ID. Default: "O"
#' @param destination.col Column name for destination ID. Default: "D"
#' @param trips.col Column name for trip counts. Auto-detected by default.
#' @param attribute_trips Attribute to include: "bicycle_godutch", "quietness", or "gradient_smooth"
#' @param length.col Column name for route length. Default: "length"
#'
#' @return An sf object with summarized routes grouped by specified variables
#' @export
#'
#' @examples
#' \dontrun{
#' route_summaries <- summarise_routes(routes_almada)
#' }
summarise_routes <- function(
  routes,
  id_route.col = "route_number",
  origin.col = "O",
  destination.col = "D",
  trips.col = names(routes)[grep("trip", names(routes))],
  attribute_trips = c("bicycle_godutch", "quietness", "gradient_smooth"),
  length.col = "length"
) {
  attribute_trips <- match.arg(attribute_trips)

  routes |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(
          origin.col,
          destination.col,
          id_route.col,
          trips.col,
          attribute_trips,
          length.col
        ))
      )
    ) |>
    dplyr::summarise() |>
    dplyr::ungroup()
}


#' Create bicycle bus routes from route network
#'
#' Identifies optimal bike bus routes by filtering high-demand network segments
#' and calculating distance-weighted scores for route prioritization.
#'
#' @param routes An sf object with individual route geometries
#' @param rnet Route network created by routes_network()
#' @param min_trips Minimum threshold for attribute filtering. Default: 0
#' @param attribute_trips Attribute for scoring: "bicycle_godutch", "quietness", or "gradient_smooth"
#' @param trips.col Column name(s) for trip counts. Auto-detected by default.
#' @param buffer Buffer distance (meters) for spatial operations. Default: 10
#'
#' @return An sf object with bike bus routes sorted by distance-weighted scores
#' @export
#'
#' @examples
#' \dontrun{
#' rnet <- routes_network(routes_almada, "bicycle_godutch")
#' bike_routes <- cycle_bus_routes(routes_almada, rnet, min_trips = 3)
#' }
cycle_bus_routes <- function(
  routes,
  rnet,
  min_trips = 0,
  attribute_trips = c("bicycle_godutch", "quietness", "gradient_smooth"),
  trips.col = names(routes)[grep("trip", names(routes))],
  buffer = 10
) {
  attribute_trips <- match.arg(attribute_trips)

  rnet_subset <- rnet[rnet[[attribute_trips]] > min_trips, ]
  rnet_subset$length <- sf::st_length(rnet_subset) |>
    as.numeric()

  rnet_union <- sf::st_union(rnet_subset)

  rnet_buffer <- sf::st_buffer(rnet_union, dist = buffer)

  routes_crop <- sf::st_intersection(routes, rnet_buffer)

  routes_subset <- routes_crop |>
    dplyr::distinct(.data$geometry)

  routes_subset$id <- seq.int(nrow(routes_subset))

  routes_subset$length <- sf::st_length(routes_subset) |>
    as.numeric()

  attribute_trips_x_distance <- paste0(attribute_trips, "_x_distance")

  routes_subset[[attribute_trips_x_distance]] <- NA

  class(routes_subset[[attribute_trips_x_distance]]) <- "numeric"

  route_buffer <- sf::st_buffer(routes_subset, dist = buffer)

  routes_within <- sf::st_contains(route_buffer, rnet_subset)

  routes_subset[, attribute_trips_x_distance] <- vapply(
    routes_within,
    function(x) {
      stats::weighted.mean(
        rnet_subset[[attribute_trips]][x],
        rnet_subset$length[x]
      )
    },
    FUN.VALUE = numeric(1)
  )

  routes_subset[, attribute_trips_x_distance] <- routes_subset[[
    attribute_trips_x_distance
  ]] *
    routes_subset$length

  routes_subset[
    order(routes_subset[[attribute_trips_x_distance]], decreasing = FALSE),
  ]

  # routes_subset[!is.nan(routes_subset[[attribute_trips_x_distance]]),]
}

#' Calculate route statistics for bike bus planning
#'
#' Computes statistics for routes intersecting with bike bus corridors,
#' including full route length, bike bus segment length, and distance metrics.
#'
#' @param routes An sf object with individual routes
#' @param rnet_plan Route network from routes_network()
#' @param ordered_routes Prioritized routes from cycle_bus_routes()
#' @param attribute_trips Attribute for filtering: "bicycle_godutch", "quietness", or "gradient_smooth"
#' @param min_trips Minimum threshold for network filtering. Default: 0
#' @param buffer Buffer distance (meters) for spatial operations. Default: 10
#'
#' @return An sf object with route statistics including full_length, bike_bus_length, dist_to_bike_bus
#' @export
#'
#' @examples
#' \dontrun{
#' rnet <- routes_network(routes_almada, "bicycle_godutch")
#' bike_routes <- cycle_bus_routes(routes_almada, rnet)
#' stats <- calc_stats(routes_almada, rnet, bike_routes)
#' }
calc_stats <- function(
  routes,
  rnet_plan,
  ordered_routes,
  attribute_trips = c("bicycle_godutch", "quietness", "gradient_smooth"),
  min_trips = 0,
  buffer = 10
) {
  attribute_trips <- match.arg(attribute_trips)

  rnet_subset = rnet_plan[rnet_plan[[attribute_trips]] > min_trips, ]
  rnet_subset$length = sf::st_length(rnet_subset) |>
    as.numeric()
  rnet_union = sf::st_union(rnet_subset)
  rnet_buffer = sf::st_buffer(rnet_union, dist = buffer)
  routes_crop = sf::st_intersection(routes, rnet_buffer)

  join = sf::st_join(routes_crop, ordered_routes, join = sf::st_equals)

  route_stats = join |>
    dplyr::mutate(
      full_length = .data$length.x,
      bike_bus_length = .data$length.y,
      dist_to_bike_bus = .data$length.x - .data$length.y
    ) |>
    dplyr::select(-dplyr::any_of(c("length.x", "length.y")))

  route_stats
}


#' Filter routes by spatial separation and ranking
#'
#' Selects spatially separated routes based on start point distances and
#' ranks them by specified attributes for bike bus implementation.
#'
#' @param routes An sf object with route geometries and attributes
#' @param attribute_trips Attribute for ranking: "bicycle_godutch", "quietness", or "gradient_smooth"
#' @param buffer Minimum distance (meters) between route start points. Default: 300
#' @param top_n Maximum number of routes to return. Default: 3
#'
#' @return An sf object with filtered and ranked routes
#' @export
#'
#' @examples
#' \dontrun{
#' rnet <- routes_network(routes_almada, "bicycle_godutch")
#' bike_routes <- cycle_bus_routes(routes_almada, rnet)
#' final_routes <- filter_routes(bike_routes, buffer = 500, top_n = 3)
#' }
filter_routes <- function(
  routes,
  attribute_trips = c("bicycle_godutch", "quietness", "gradient_smooth"),
  buffer = 300,
  top_n = 3
) {
  attribute_trips <- match.arg(attribute_trips)

  attribute_trips_x_distance = paste0(attribute_trips, "_x_distance")

  routes_sorted <- routes[
    order(routes[[attribute_trips_x_distance]], decreasing = FALSE),
  ]

  route_names <- names(routes_sorted)[grep(
    "geom",
    x = names(routes_sorted),
    invert = T
  )]

  selected_routes = numeric(0)

  routes_sorted_initial_point <- routes_sorted |>
    sf::st_cast("POINT") |>
    dplyr::slice_head(n = 1, by = dplyr::all_of(route_names))

  if (nrow(routes_sorted) < 2) {
    routes_subset = routes_sorted
  } else {
    for (i in 1:(nrow(routes_sorted) - 1)) {
      p1 = routes_sorted_initial_point[i, ]
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
  routes_subset = routes_subset[
    order(routes_subset[[attribute_trips_x_distance]], decreasing = TRUE),
  ]
  routes_subset = routes_subset |>
    dplyr::slice_head(n = top_n)

  routes_subset
}

#' Match origin centroids to bike bus routes
#'
#' Assigns origin points to their nearest bike bus routes within a distance
#' threshold, creating a mapping between residential areas and route services.
#'
#' @param routes_cents An sf object with candidate routes
#' @param top_routes An sf object with selected top routes
#' @param route_stats Route statistics from calc_stats()
#' @param origins An sf object with origin points/zones
#' @param origin.id Column name for origin ID in origins data. Auto-detected by default.
#' @param origin_route.id Column name for origin ID in route data. Default: "O"
#' @param id.col Column name for route ID. Default: "id"
#' @param max_dist_to_bikebus a buffer distance to assign the route to a bike bus
#' @param min_dist_threshold the minimum total trip distance to be included in a bike bus
#' @param attribute_trips Attribute for processing: "bicycle_godutch", "quietness", or "gradient_smooth"
#'
#' @return An sf object with origins matched to their assigned bike bus routes
#' @export
#'
#' @examples
#' \dontrun{
#' # Complete workflow
#' rnet <- routes_network(routes_almada, "bicycle_godutch")
#' bike_routes <- cycle_bus_routes(routes_almada, rnet)
#' top_routes <- filter_routes(bike_routes, top_n = 3)
#' stats <- calc_stats(routes_almada, rnet, bike_routes)
#' matched <- match_centroids(bike_routes, top_routes, stats, origins_lisbon)
#' }

match_centroids = function(
  routes_cents,
  top_routes,
  route_stats,
  origins,
  origin.id = names(origins)[1],
  origin_route.id = "O",
  id.col = "id",
  max_dist_to_bikebus = 30,
  min_dist_threshold = 500,
  attribute_trips = c("bicycle_godutch", "quietness", "gradient_smooth")
) {
  attribute_trips <- match.arg(attribute_trips)

  attribute_trips_x_distance = paste0(attribute_trips, "_x_distance")

  # Remove top routes to avoid duplicate matching in subsequent processing
  routes_cents_clean = routes_cents[
    !(routes_cents[[id.col]] %in% top_routes[[id.col]]),
  ]

  routes_cents_clean$pick = 0

  routes_cents_initial_point <- routes_cents_clean |>
    sf::st_cast("POINT") |>
    dplyr::slice_head(n = 1, by = dplyr::all_of(id.col)) |>
    tidyr::drop_na()

  dist_matrix <- routes_cents_initial_point |>
    sf::st_distance(top_routes)

  index_closest <- apply(dist_matrix, 1, which.min)

  mindist_closest <- apply(dist_matrix, 1, min)

  index_closest[mindist_closest > max_dist_to_bikebus] <- 0

  routes_cents_clean$distance_bb <- mindist_closest

  routes_cents_clean$pick <- index_closest

  routes_both = routes_cents_clean |>
    dplyr::filter(.data$pick != 0)

  routes_both = dplyr::bind_rows(
    routes_both,
    top_routes |>
      dplyr::mutate(pick = dplyr::row_number())
  )
  routes_both = routes_both |>
    dplyr::mutate(pick = as.character(.data$pick))

  top_cents = dplyr::inner_join(
    route_stats,
    routes_both |> sf::st_drop_geometry(),
    by = c(id.col, attribute_trips_x_distance)
  )

  # remove routes shorter than dist_threshold or where less than half of the distance is on the bike bus
  top_cents = top_cents |>
    dplyr::filter(
      .data$full_length > min_dist_threshold,
      .data$bike_bus_length > .data$dist_to_bike_bus
    ) |>
    dplyr::select(-dplyr::any_of(id.col))

  key_vector = origin_route.id
  names(key_vector) <- origin.id

  cents = dplyr::inner_join(
    origins |> dplyr::select(dplyr::any_of(origin.id)),
    top_cents |> sf::st_drop_geometry(),
    by = key_vector
  )
  cents
}

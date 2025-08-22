#' Extract bike/bici routes for origins and destinations
#'
#' @param od.data A sf object with OD data
#' @param osrm.profile for the osrmRoute function
#' @param origin.col string with name of the column with origin id
#' @param destination.col string with name of the column with destination id
#' @param trips.col string with name of the column with number of trips for each OD pair
#' @param distance.threshold distance threshold in metres to create a subset of the routes
#'
#' @returns An sf object with the routes to school
#' @export
#'
#' @examples
#' \dontrun{
#' bici_routes(od.data)
#' }
bici_routes_osrm <- function(
  od.data,
  distance.threshold = Inf,
  osrm.profile = "foot",
  origin.col = names(od.data)[1],
  destination.col = names(od.data)[2],
  trips.col = names(od.data)[grep("trip", names(od.data))]
) {
  od_clean <- prepare_od(
    od.data,
    trips.col = trips.col,
    destination.col = destination.col,
    distance.threshold = distance.threshold
  )

  od_clean |>
    # Map over each group to calculate routes
    dplyr::mutate(
      route = purrr::map(
        .x = .data$data,
        function(.x) {
          # Extract origins from OD data, keeping only relevant columns
          origins <- .x |>
            tidyr::drop_na(dplyr::any_of(trips.col)) |>
            sf::st_cast("POINT", warn = FALSE) |>
            dplyr::select(dplyr::any_of(c(origin.col, trips.col))) |>
            dplyr::slice_head(n = 1, by = dplyr::any_of(origin.col))

          # Extract destination from OD data, ensuring it is a single point
          destination <- .x[1, ] |>
            sf::st_cast("POINT", warn = FALSE) |>
            dplyr::mutate(id = "destination") |>
            dplyr::select(dplyr::any_of("id")) |>
            dplyr::slice_tail(n = 1, by = dplyr::any_of("id"))

          # Query routes using OSRM
          routes <- batch_osrmRoutes(origins, destination, osrm.profile)

          # Combine origins with route details to produce a clean sf object
          clean_routes <- origins |>
            sf::st_drop_geometry() |>
            dplyr::bind_cols(routes[, c("duration", "distance")])

          clean_routes
        }
      )
    ) |>
    dplyr::select(dplyr::any_of(c(destination.col, "route"))) |>
    tidyr::unnest(cols = c("route")) |>
    sf::st_as_sf() |>
    dplyr::relocate(dplyr::any_of(origin.col))
}

#' A function to query routes for several destinations to a single origin
#'
#' @param origins sf object with origins (homes)
#' @param destination sf object with destination (school)
#' @param osrm.profile string with the osrm profile to be used
#'
#' @returns an sf object with all the routes

batch_osrmRoutes <- function(origins, destination, osrm.profile) {
  lapply(
    1:nrow(origins),
    function(i) {
      tryCatch(
        osrm::osrmRoute(
          src = origins[i, ],
          dst = destination,
          osrm.profile = osrm.profile
        ),
        error = function(e) {
          message(sprintf(
            "Error querying route for origin %d: %s",
            i,
            e$message
          ))
          NULL
        }
      )
    }
  ) |>
    dplyr::bind_rows()
}

#' Extract bike/bici routes for origins and destinations using CycleStreets
#'
#' @inheritParams bici_routes_osrm
#' @param plan one of "quietest" (default), "fastest", or "balanced" for cycle streets
#'
#' @returns An sf object with the routes to school
#' @export
#'
#' @examples
#' \dontrun{
#' bici_routes(od.data)
#' }
bici_routes_cyclestreets <- function(
  od.data,
  distance.threshold = Inf,
  plan = c("quietest", "fastest", "balanced"),
  origin.col = names(od.data)[1],
  destination.col = names(od.data)[2],
  trips.col = names(od.data)[grep("trip", names(od.data))]
) {
  # check argument
  plan = match.arg(plan)

  od_clean <- prepare_od(
    od.data,
    trips.col = trips.col,
    destination.col = destination.col,
    distance.threshold = distance.threshold
  )

  od_clean |>
    # Map over each group to calculate routes
    dplyr::mutate(
      route = purrr::map(
        .x = .data$data,
        function(.x) {
          # Extract origins from OD data, keeping only relevant columns
          origins <- .x |>
            tidyr::drop_na(dplyr::any_of(trips.col)) |>
            sf::st_cast("POINT", warn = FALSE) |>
            dplyr::select(dplyr::any_of(c(origin.col, trips.col))) |>
            dplyr::slice_head(n = 1, by = dplyr::any_of(origin.col))

          # Extract destination from OD data, ensuring it is a single point
          destination <- .x[1, ] |>
            sf::st_cast("POINT", warn = FALSE) |>
            dplyr::mutate(id = "destination") |>
            dplyr::select(dplyr::any_of("id")) |>
            dplyr::slice_tail(n = 1, by = dplyr::any_of("id"))

          # Query routes using OSRM
          routes <- batch_CSRoutes(origins, destination, plan = plan)

          # Renaming origin column
          names(routes)[names(routes) == "id"] <- origin.col

          # Matching the classes for the original origin id
          routes[[origin.col]] <- methods::as(
            routes[[origin.col]],
            Class = class(origins[[origin.col]])
          )

          clean_routes <- origins |>
            sf::st_drop_geometry() |>
            dplyr::left_join(routes, by = origin.col)

          clean_routes
        }
      )
    ) |>
    dplyr::select(dplyr::any_of(c(destination.col, "route"))) |>
    tidyr::unnest(cols = c("route")) |>
    sf::st_as_sf() |>
    dplyr::relocate(dplyr::any_of(origin.col))
}

batch_CSRoutes <- function(origins, destination, plan) {
  lapply(
    1:nrow(origins),
    function(i) {
      tryCatch(
        cyclestreets::journey2(
          id = origins[[i, 1]],
          fromPlace = origins[i, ],
          toPlace = destination,
          plan = plan,
          segments = TRUE
        ),
        error = function(e) {
          message(sprintf(
            "Error querying route for origin %d: %s",
            i,
            e$message
          ))
          NULL
        }
      )
    }
  ) |>
    dplyr::bind_rows()
}

#' Extract bike/bici routes for origins and destinations using CycleStreets via stplanr
#'
#' @inheritParams bici_routes_osrm
#' @param plan one of "quietest" (default), "fastest", or "balanced" for cycle streets
#'
#' @returns An sf object with the routes to school
#' @export
#'
#' @examples
#' \dontrun{
#' bici_routes(od_data_almada)
#' }
bici_routes <- function(
  od.data,
  distance.threshold = Inf,
  plan = c("quietest", "fastest", "balanced"),
  origin.col = names(od.data)[1],
  destination.col = names(od.data)[2],
  trips.col = names(od.data)[grep("trip", names(od.data))]
) {
  # od.data <- od_data_almada
  # check argument
  plan = match.arg(plan)

  od_clean <- prepare_od(
    od.data,
    trips.col = trips.col,
    destination.col = destination.col,
    distance.threshold = distance.threshold
  )

  od_clean |>
    # Map over each group to calculate routes
    dplyr::mutate(
      route = purrr::map(
        .x = .data$data,
        function(.x) {
          # Query routes using stplanr
          routes <- stplanr::route(
            l = .x,
            route_fun = cyclestreets::journey,
            plan = plan
          )

          # Renaming origin column
          names(routes)[names(routes) == "origin"] <- origin.col

          routes |>
            dplyr::mutate(
              route_hilliness = stats::weighted.mean(
                .data$gradient_smooth,
                .data$distances
              )
            )
        }
      )
    ) |>
    dplyr::select(dplyr::any_of(c(destination.col, "route"))) |>
    tidyr::unnest(cols = c("route")) |>
    sf::st_as_sf() |>
    dplyr::relocate(dplyr::any_of(origin.col)) |>
    dplyr::filter(.data$length <= distance.threshold)
}

#' A function to get a nested OD data subset by school
#'
#' @param od_data original od dataset
#' @inheritParams bici_routes_osrm
#'
#' @returns a nested tibble
#'
prepare_od <- function(
  od_data,
  destination.col,
  trips.col,
  distance.threshold
) {
  # Filter desirelines with trips
  od_data[!is.nan(od_data[[trips.col]]), ] |>
    # filters Od paris within the distance threshold
    dplyr::filter(
      as.numeric(sf::st_length(.data$geometry)) <= distance.threshold
    ) |>
    # Nest dataset by school
    tidyr::nest(.by = dplyr::any_of(destination.col))
}

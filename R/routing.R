#' Extract bike/bici routes for origins and destinations
#'
#' @param od.data A sf object with OD data
#' @param osrm.profile for the osrmRoute function
#' @param origin.col string with name of the column with origin id
#' @param destination.col string with name of the column with destination id
#' @param trips.col string with name of the column with number of trips for each OD pair
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
    osrm.profile = "foot",
    origin.col = names(od.data)[1],
    destination.col = names(od.data)[2],
    trips.col = names(od.data)[grep("trip", names(od.data))]) {
  # Nest data by destination column to group OD pairs by destination
  od.data |>
    tidyr::nest(.by = dplyr::any_of(destination.col)) |>
    # Map over each group to calculate routes
    dplyr::mutate(route = purrr::map(
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
    )) |>
    dplyr::select(dplyr::any_of(c(destination.col, "route"))) |>
    tidyr::unnest(cols = c("route")) |>
    sf::st_as_sf() |>
    dplyr::relocate(any_of(origin.col))
}

#' A function to query routes for several destinations to a single origin
#'
#' @param origins sf object with origins (homes)
#' @param destination sf object with destination (school)
#' @param osrm.profile string with the osrm profile to be used
#'
#' @returns an sf object with all the routes

batch_osrmRoutes <- function(origins,
                             destination,
                             osrm.profile) {
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
          message(sprintf("Error querying route for origin %d: %s", i, e$message))
          NULL
        }
      )
    }
  ) |>
    dplyr::bind_rows()
}

#' Extract bike/bici routes for origins and destinations
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
    plan = c("quietest","fastest", "balanced"),
    origin.col = names(od.data)[1],
    destination.col = names(od.data)[2],
    trips.col = names(od.data)[grep("trip", names(od.data))]) {
  
  # check argument
  plan = match.arg(plan)
  
  # Nest data by destination column to group OD pairs by destination
  od.data |>
    tidyr::nest(.by = dplyr::any_of(destination.col)) |>
    # Map over each group to calculate routes
    dplyr::mutate(route = purrr::map(
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
        routes[[origin.col]] <- as(routes[[origin.col]],Class = class(origins[[origin.col]]))
        
        clean_routes <- origins |>
          sf::st_drop_geometry() |>
          dplyr::left_join(routes,by = origin.col)
        
        clean_routes
        
      }
    )) |>
    dplyr::select(dplyr::any_of(c(destination.col, "route"))) |>
    tidyr::unnest(cols = c("route")) |>
    sf::st_as_sf() |>
    dplyr::relocate(any_of(origin.col))
}

batch_CSRoutes <- function(origins,
                           destination,
                           plan) {
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
          message(sprintf("Error querying route for origin %d: %s", i, e$message))
          NULL
        }
      )
    }
  ) |> dplyr::bind_rows() 
}

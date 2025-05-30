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
bici_routes <- function(
    od.data,
    osrm.profile = "foot",
    origin.col = names(od.data)[1],
    destination.col = names(od.data)[2],
    trips.col = names(od.data)[grep("trip", names(od.data))]) {
  od.data |>
    tidyr::nest(.by = dplyr::any_of(destination.col)) |>
    dplyr::mutate(route = purrr::map(
      .x = .data$data,
      function(.x) {
        # .x = nested_d$data[[1]]
        # .y = nested_d$D[[1]]

        # Extracting origins from OD data

        origins <- .x |>
          tidyr::drop_na(dplyr::any_of(trips.col)) |>
          sf::st_cast("POINT", warn = F) |>
          dplyr::select(dplyr::any_of(c(origin.col, trips.col))) |>
          dplyr::slice_head(n = 1, by = dplyr::any_of(origin.col))

        # Extracting destination from OD data

        destination <- .x[1, ] |>
          sf::st_cast("POINT", warn = F) |>
          dplyr::mutate(id = "destination") |>
          dplyr::select(dplyr::any_of("id")) |>
          dplyr::slice_tail(n = 1, by = dplyr::any_of("id"))

        # Querying routes using osrm

        routes <- batch_osrmRoutes(origins, destination, osrm.profile)

        # Producing a clean sf object with the routes

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
      osrm::osrmRoute(
        src = origins[i, ],
        dst = destination,
        osrm.profile = osrm.profile
      )
    }
  ) |>
    dplyr::bind_rows()
}

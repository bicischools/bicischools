#' Title
#'
#' @param origins An sf object representing the origins.
#' @param destinations An sf object representing the destinations.
#' @param model A character string specifying the model to use.
#' @param balancing A character string specifying the balancing method. One of "origins", "destinations", or "both".
#' @param max_dist A numeric value specifying the maximum distance for interaction.
#' @param origins.id A character string specifying the ID column in the origins sf object.
#' @param origins.size A character string specifying the size column in the origins sf object.
#' @param destinations.id A character string specifying the ID column in the destinations sf object.
#' @param destinations.size A character string specifying the size column in the destinations sf object.
#' @param keep_cols A logical value. If TRUE, all columns from the input data are kept. If FALSE (default), only O, D and the output_col are kept.
#' @param output_col A character string specifying the name of the column for the modelled trips (default is "modelled_trips").
#' @param max_iter An integer, the maximum number of iterations for the balancing algorithm (default is 25).
#' @param ... Additional arguments to be passed to the model function.
#'
#' @returns An sf object with the modelled trips.
#' @export
#' @importFrom dplyr select all_of
#' @importFrom rlang .data
#'
#' @examples
#' # sim_schools(origins, destinations, model = "gravity")
#'

#'
sim_schools <- function(
    origins,
    destinations,
    model,
    ...,
    balancing = c("origins", "destinations", "both"),
    max_dist = Inf,
    origins.id = names(origins)[1],
    origins.size = names(origins)[2],
    destinations.id = names(destinations)[1],
    destinations.size = names(destinations)[2],
    keep_cols = FALSE,
    output_col = "modelled_trips",
    max_iter = 25) {
  # Checks

  ## crs check
  if (!check_same_crs(origins, destinations)) {
    stop("st_crs(origins) != st_crs(destinations)")
  }

  ## logical values
  if (!check_logical(keep_cols)) {
    stop("keep_cols must be an atomic logical")
  }

  ## Arguments
  balancing <- match.arg(balancing, several.ok = F)

  if (!is.numeric(max_dist)) {
    stop("max_dist must be numerical")
  }

  # Balancing OD

  total_origins <- sum(origins[[origins.size]], na.rm = T)
  total_destinations <- sum(destinations[[destinations.size]], na.rm = T)
  total_avg <- mean(c(total_origins, total_destinations))

  ## Calculating balancing factors
  if (balancing == "origins") {
    o_bal_factor <- 1
    d_bal_factor <- total_origins / total_destinations
  } else if (balancing == "destinations") {
    o_bal_factor <- total_destinations / total_origins
    d_bal_factor <- 1
  } else if (balancing == "both") {
    o_bal_factor <- total_avg / total_origins
    d_bal_factor <- total_avg / total_destinations
  }

  ## Applying factors
  origins[[origins.size]] <- origins[[origins.size]] * o_bal_factor
  destinations[[destinations.size]] <- destinations[[destinations.size]] *
    d_bal_factor

  # Dummy OD creation
  dummy_si <- simodels::si_to_od(
    origins = origins,
    destinations = destinations,
    max_dist = max_dist,
    intrazonal = F
  )

  # Fitting SIM
  fitted_sim <- simodels::si_calculate(
    dummy_si,
    fun = get_models(model),
    output_col = output_col,
    ...
  )

  # Constraining

  const_sim <- fitted_sim

  if (!keep_cols) {
    const_sim <- const_sim |>
      dplyr::select(dplyr::all_of(c("O", "D", output_col)))
  }

  return(const_sim)
}

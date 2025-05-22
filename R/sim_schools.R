#' Title
#'
#' @param origins
#' @param destinations
#' @param model
#' @param balancing
#' @param constraining
#' @param max_dist
#' @param origins.id
#' @param origins.size
#' @param destinations.id
#' @param destinations.size
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
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
  max_iter = 25
) {
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
  balancing = match.arg(balancing, several.ok = F)

  if (!is.numeric(max_dist)) {
    stop("max_dist must be numerical")
  }

  # Balancing OD

  total_origins <- sum(origins[[origins.size]], na.rm = T)
  total_destinations <- sum(destinations[[destinations.size]], na.rm = T)
  total_avg <- mean(c(total_origins, total_destinations))

  ## Calculating balancing factors
  if (balancing == "origins") {
    o_bal_factor = 1
    d_bal_factor = total_origins / total_destinations
  } else if (balancing == "destinations") {
    o_bal_factor = total_destinations / total_origins
    d_bal_factor = 1
  } else if (balancing == "both") {
    o_bal_factor = total_avg / total_origins
    d_bal_factor = total_avg / total_destinations
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
      select(O, D, {{ output_col }})
  }

  return(const_sim)
}

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
sim_schools <- function(origins,
                        destinations,
                        model,
                        balancing = c("origins", "destinations", "both"),
                        constraining = c("origins", "destinations", "both"),
                        max_dist = Inf,
                        origins.id = names(origins)[1],
                        origins.size = names(origins)[2],
                        destinations.id = names(destinations)[1],
                        destinations.size = names(destinations)[2],
                        ...) {
  # Checks
  
  ## crs check
  if (!check_same_crs(origins, destinations)) {
    stop("st_crs(origins) != st_crs(destinations)")
  }
  
  ## Arguments
  balancing = match.arg(balancing, several.ok = F)
  constraining = match.arg(constraining, several.ok = F)
  
  if (!is.numeric(max_dist)) {
    stop("max_dist must be numerical")
  }
  
  # Balancing OD

  total_origins <- sum(origins[[origins.size]],na.rm = T)
  total_destinations <- sum(destinations[[destinations.size]],na.rm = T)
  total_avg <- mean(c(total_origins,total_destinations))
  
  ## Calculating balancing factors
  if(balancing == "origins"){
    o_bal_factor = 1
    d_bal_factor = total_destinations/total_origins
  } else if (balancing == "destinations"){
    o_bal_factor = total_origins/total_destinations
    d_bal_factor = 1
  } else if(balancing == "both"){
    o_bal_factor = total_origins/total_avg
    d_bal_factor = total_destinations/total_avg
  }
  
  ## Applying factors
  origins[[origins.size]] <- origins[[origins.size]]*o_bal_factor
  destinations[[destinations.size]] <- destinations[[destinations.size]]*d_bal_factor
  
  
  
  # Dummy OD creation
  dummy_si <- simodels::si_to_od(origins = origins,
                                 destinations = destinations,
                                 max_dist = max_dist)


  # # TODO formula parameters to be included as an input of main function
  fitted_sim <- simodels::si_calculate(
    dummy_si,
    fun = get_models(model),
    d = distance_euclidean,
    m = origin_N_INDIVIDUOS_0_14,
    n = destination_n_pupils,
    beta = 0.8
  )
  
  # Constraining
  # TODO 
  
  
  # Return results
  return(fitted_sim)
}

#' Check CRS of two sf objects
#'
#' @param sf1 An sf object.
#' @param sf2 An sf object.
#'
#' @returns TRUE if CRSs are the same, FALSE otherwise.

check_same_crs <- function(sf1, sf2) {
  # Check if inputs are sf objects
  if (!inherits(sf1, "sf") || !inherits(sf2, "sf")) {
    stop("Both inputs must be sf objects.")
  }

  # Get the CRS of each sf object
  crs1 <- sf::st_crs(sf1)
  crs2 <- sf::st_crs(sf2)

  # Check if either CRS is missing
  if (is.na(crs1) || is.na(crs2)) {
    stop("One or both of the sf objects do not have a CRS.")
  }

  # Check if the CRSs are the same
  if (crs1 == crs2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Get a model for SIM calculation
#'
#' @param model A character string specifying the model name.
#'
#' @returns A function representing the specified model.
#' @export
#'
#' @examples
#' get_models("gravity")
#'
get_models <- function(model) {
  models <- list(
    "gravity" = function(beta, d, m, n) {
      m * n * exp(-beta * d / 1000)
    }
  )

  # Check if model exists
  model <- match.arg(model, names(models), several.ok = F)

  return(models[[model]])
}


check_logical <- function(value) {
  return(is.logical(value) & length(value) == 1)
}


#' petrol_co2_to_fuel
#'
#' @param .co2 The co2 intensity (g/km) to be converted
#'
#' @return A value of the fuel consumed
#' @export
#'
#' @examples
#'
#'
petrol_co2_to_fuel <- function(.co2) {
  .petrol_consumption <- 0.006957 + 0.043820 * .co2
  return(.petrol_consumption)
}





#' diesel_co2_to_fuel
#'
#' @param .co2 The co2 intensity (g/km) to be converted
#'
#' @return A value of the fuel consumed
#' @export
#'
#' @examples
#'
#'
diesel_co2_to_fuel <- function(.co2) {
  .diesel_consumption <- -0.001201 + 0.037436 * .co2
  return(.diesel_consumption)
}


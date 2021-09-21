
#' petrol_co2_to_fuel
#'
#' @param .co2 The co2 intensity (g/km) to be converted
#'
#' @return A value of the fuel consumed
#' @export
#'
#'
#'

petrol_co2_to_fuel <- function(.co2) {
  #this relationship (and the one below) is determined from daa provided by the NTC:
  #https://www.ntc.gov.au/sites/default/files/assets/files/Carbon-dioxide-emissions-intensity-for-new-Australian-light-vehicles-2019.pdf
  #page 9
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
#'
#'

diesel_co2_to_fuel <- function(.co2) {
  .diesel_consumption <- -0.001201 + 0.037436 * .co2
  return(.diesel_consumption)
}


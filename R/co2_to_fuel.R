
#' petrol_co2_to_fuel
#'
#' @description A function that converts vehicle carbon dioxide emissions (in grams/kilometre) to estimated fuel consumption (in L/100km) for petrol vehicles.
#' This is based on data provided by the National Transport Commission (NTC): https://www.ntc.gov.au/sites/default/files/assets/files/Carbon-dioxide-emissions-intensity-for-new-Australian-light-vehicles-2019.pdf
#'
#' @param .co2 The co2 intensity (g/km) to be converted
#'
#' @return The esimated fuel consumption of the vehicle (L/100km)
#' @export
#'
#'
#'

petrol_co2_to_fuel <- function(.co2) {
  #page 9
  .petrol_consumption <- 0.006957 + 0.043820 * .co2
  return(.petrol_consumption)
}



#' diesel_co2_to_fuel
#'
#' @description A function that converts vehicle carbon dioxide emissions (in grams/kilometre) to estimated fuel consumption (in L/100km) for diesel vehicles.
#' This is based on data provided by the National Transport Commission (NTC): https://www.ntc.gov.au/sites/default/files/assets/files/Carbon-dioxide-emissions-intensity-for-new-Australian-light-vehicles-2019.pdf
#'
#' @param .co2 The co2 intensity (g/km) to be converted
#'
#' @return The esimated fuel consumption of the vehicle (L/100km)
#' @export
#'
#'
#'

diesel_co2_to_fuel <- function(.co2) {
  .diesel_consumption <- -0.001201 + 0.037436 * .co2
  return(.diesel_consumption)
}


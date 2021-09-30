#' Fleet creator
#'
#' @name fleet_creator
#'
#' @description Creates a fleet of simulated vehicle sales until 2050, characterised by vehicle
#' segment and emissions.
#'
#'
#'
#' @param .i_cars The number of cars sold in the base year (2021) in the simulated fleet. Each car represents 10\% of new vehicle sales. Shares from [NTC (p. 23)](https://www.ntc.gov.au/sites/default/files/assets/files/Carbon-dioxide-emissions-intensity-for-new-Australian-light-vehicles-2019.pdf) and scaled to make 100\% by adding in the 5\% to passenger vehicles (the sports/people movers and where they fall) these shares are the 'initial shares' of sales. As the years progress, we get growth across the fleet segments differently according to the growth rates.
#' @param .i_passenger_share The share of passenger vehicles in the base year. From these figures, comparing the 2011 and 2019 numbers, we are also going to assume the following about the growth of share of each vehicle type out to 2025 (this gives an overall growth of the fleet around ~1.4\% a year, which is about the long term average (see Australian data [here](https://tradingeconomics.com/australia/total-vehicle-sales).)
#' @param .i_suv_share The share of SUVs in the base year
#' @param .i_lcv_share The share of Light Commercial Vehicles in the base year
#' @param .passenger_growth The growth rate of passenger vehicle sales per year
#' @param .suv_growth The growth rate of SUV sales per year
#' @param .lcv_growth The growth rate of LCV sales per year
#' @param .passenger_co2 The assumed average co2 emissions of passenger vehicles in base year -- the current co2 intensity in each segment these values are calculated from FCAI data - sales weighted averages of emissions in each segment.
#' @param .suv_co2 The assumed average co2 emissions of SUV vehicles in base year
#' @param .lcv_co2 The assumed average co2 emissions of LCV vehicles in base year
#'
#' @return a \code{tibble} with each row representing a vehicle.
#'
#' @export
#'

globalVariables(c("passenger", "lcv", "suv", "total", "share", "base_emissions"))

fleet_creator <- function(.i_cars = 100,
                          .i_passenger_share = 0.33,
                          .i_suv_share = 0.47,
                          .i_lcv_share = 0.20,
                          .passenger_growth = 1,
                          .suv_growth = 1.02,
                          .lcv_growth = 1.015,
                          .passenger_co2 = 158.4,
                          .suv_co2 = 177.7,
                          .lcv_co2 = 215) {

  # checks
  if (round(.i_passenger_share + .i_suv_share + .i_lcv_share, 2) != 1) {
    stop("shares must sum to 1. Currently:",
         "\n\t.i_passenger_share = ", .i_passenger_share,
         "\n\t.i_suv_share = ", .i_suv_share,
         "\n\t.i_lcv_share = ", .i_lcv_share,
         "\n\tTotal: ", .i_passenger_share + .i_suv_share + .i_lcv_share
         )
  }

  if (any(.passenger_growth > 1.5, .suv_growth > 1.5, .lcv_growth > 1.5)) stop("annual growth (_growth) inputs must be 1.5 (50%) or less.")

  start_year <- 2021
  end_year <- 2050

  # a dataset with the changing shares/cars over time
  fleet_out <- tibble(year = start_year:end_year) %>%
    mutate(passenger = (.i_passenger_share * .i_cars) * (.passenger_growth)^(year - start_year),
           suv = (.i_suv_share * .i_cars) * (.suv_growth)^(year - start_year),
           lcv = (.i_lcv_share * .i_cars) * (.lcv_growth)^(year - start_year),
           across(c(passenger, suv, lcv), as.integer)) %>%
    pivot_longer(-year, names_to = "vehicle_group", values_to = "count") %>%
    uncount(count) %>%
    group_by(year) %>%
    mutate(id = row_number(),
           base_emissions = fcase(
             vehicle_group == "passenger", .passenger_co2,
             vehicle_group == "suv", .suv_co2,
             vehicle_group == "lcv", .lcv_co2),
           current_emissions = base_emissions,
           electric_applied = FALSE,
           tech_pkg_applied = 0,
           cost = 0) %>%
    arrange(vehicle_group, id) %>%
    ungroup()

  return(fleet_out)

}

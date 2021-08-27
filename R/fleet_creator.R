#' Fleet creator
#'
#' @name fleet_creator
#'
#' @description Creates a fleet of simulated vehicle sales until 2050, characterised by vehicle
#' segment and emissions.
#'
#'
#'
#' @param .i_cars The number of cars sold in the base year (2021) in the simulated fleet
#' @param .i_passenger_share The share of passenger vehicles in the base year
#' @param .i_suv_share The share of SUVs in the base year
#' @param .i_lcv_share The share of Light Commercial Vehicles in the base year
#' @param .passenger_growth The growth rate of passenger vehicle sales per year
#' @param .suv_growth The growth rate of SUV sales per year
#' @param .lcv_growth The growth rate of LCV sales per year
#' @param .passenger_co2 The assumed average co2 emissions of passenger vehicles in base year
#' @param .suv_co2 The assumed average co2 emissions of SUV vehicles in base year
#' @param .lcv_co2 The assumed average co2 emissions of LCV vehicles in base year
#' @param .digits Used for rounding
#'
#' @return a \code{tibble} with each row representing a vehicle.
#'
#' @export
#'

globalVariables(c("passenger", "lcv", "suv", "total", "share", "base_emissions"))

fleet_creator <- function(.i_cars = 100, # (each car represents 10% of new vehicle sales)
                          #shares from: https://www.ntc.gov.au/sites/default/files/assets/files/Carbon-dioxide-emissions-intensity-for-new-Australian-light-vehicles-2019.pdf
                          #page 23 and scaled to make 100% by adding in the 5% to passenger vehicles (as that's the sports/people movers and where they fall)

                          #these shares are the 'initial shares' of sales. As the years progress, we get growth
                          #across the fleet segments differently according to the growth rates.
                          .i_passenger_share = 0.33,
                          .i_suv_share = 0.47,
                          .i_lcv_share = 0.20,
                          #from these figures comparing the 2011 and 2019 numbers, we are also going to assume the following
                          #about the growth of share of each vehicle type out to 2025 (this gives an overall
                          #growth of the fleet aroun ~1.4% a year, which is about the long term average
                          #(https://tradingeconomics.com/australia/total-vehicle-sales#:~:text=Looking%20forward%2C%20we%20estimate%20Total,according%20to%20our%20econometric%20models.)
                          .passenger_growth = 1,
                          .suv_growth = 1.02,
                          .lcv_growth = 1.015,
                          #and we're going to put in the current co2 intensity in each segment
                          #these values are calculated from FCAI data - sales weighted averages
                          #of emissions in each segment

                          .passenger_co2 = 158.4,
                          .suv_co2 = 177.7,
                          .lcv_co2 = 215,
                          #for rounding, make 2 if using 100 cars)
                          .digits = 2) {

  #first making a dataset with the changing shares/cars over time
  vehicle_trends <- tibble(year = 2021,
                           passenger = .i_passenger_share * .i_cars,
                           suv = .i_suv_share * .i_cars,
                           lcv = .i_lcv_share * .i_cars,
                           total = .i_cars) %>%
    complete(year = (2021:2050))

  #looping over time
  i <- 2
  while (i <= nrow(vehicle_trends)) {

    vehicle_trends$passenger[i] = vehicle_trends$passenger[i-1] * .passenger_growth
    vehicle_trends$suv[i] = vehicle_trends$suv[i-1] * .suv_growth
    vehicle_trends$lcv[i] = vehicle_trends$lcv[i-1] * .lcv_growth
    vehicle_trends$total[i] = vehicle_trends$passenger[i] + vehicle_trends$suv[i] + vehicle_trends$lcv[i]

    i <- i + 1
  }


  #now converting these back into shares
  vehicle_trends <- vehicle_trends %>%
    mutate(passenger = passenger / 100,
           suv = suv / 100,
           lcv = lcv / 100)


  #creating a loop which will add one year at a time
  .year <- 2021
  fleet_out <- tibble()

  while (.year <= 2050 ) {

    .this_year_shares <- vehicle_trends %>%
      filter(year == .year )

    .passenger_share <- .this_year_shares$passenger[1]
    .suv_share <- .this_year_shares$suv[1]
    .lcv_share <- .this_year_shares$lcv[1]
    .cars <- .this_year_shares$total[1]


    #creating our fleet for the first year
    .year_fleet <- tibble(vehicle_group = c("passenger", "suv", "lcv"),
                          share = c(.passenger_share,
                                    .suv_share,
                                    .lcv_share)) %>%
      mutate(share = share * .i_cars,
             #this is a bit odd but we're going to round the share values to the log10 - 1 of
             #how many cars - i.e. to 1 digit for 10 cars, 2 digits for 100 cars (as
             #we don't have adequate precision otherwise)
             share = as.integer(share)) %>%
      uncount(share) %>%
      mutate(year = .year,
             id = row_number(),
             base_emissions = case_when(
               vehicle_group == "passenger" ~ .passenger_co2,
               vehicle_group == "suv" ~ .suv_co2,
               vehicle_group == "lcv" ~ .lcv_co2),
             current_emissions = base_emissions,
             electric_applied = FALSE,
             tech_pkg_applied = 0,
             cost = 0)

    fleet_out <- bind_rows(fleet_out, .year_fleet)

    .year <- .year + 1

  }

  return(fleet_out)

}


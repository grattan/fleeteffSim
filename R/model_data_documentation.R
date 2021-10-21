

#' Cost curves
#'
#' The costs and efficiency improvements associated with technology upgrades available to vehicles. Derived from US EPA cost curves used in the OMEGA model.
#'
#' @format A \code{tibble} object with 8 variables:
#' \describe{
#' \item{\code{vehicle_group}}{The type of vehicle the upgrade applies to (passenger, suv, lcv)}
#' \item{\code{year}}{The year the upgrade applies to}
#' \item{\code{estimate}}{The upgrade estimate type - a central scenario (central), price parity I year ahead of of central (early_pp),
#' or one year behind central (late_pp)}
#' \item{\code{incr_cost}}{The incremental cost of applying the given upgrade (compared to the previous technology
#' package) for the given vehicle type, in a given year}
#' \item{\code{incr_reduction}}{The incremental emissions reduction (as a proportion of emissions stated between
#' 0 and 1. 1 refers to a 100\% reduction in emissions.)}
#' \item{\code{type}}{The upgrade type - defined as either an upgrade to electric (ev) or
#' as an improvement to an ICE engine (ice)}
#' \item{\code{tech_pkg_no}}{The technology package to be applied. tech_pkg_no 100 refers to an electric vehicle.
#' Technology packages (excluding no 100) must be applied sequentially. Package 100 contains cost relative to base package
#' 0.}
#' \item{\code{weighted_emissions}}{Vehicle emissions (historical and unused)}
#' }
"cost_curves"



#' Electricity prices
#'
#' The assumed future electricity prices under different scenarios.
#'
#' @format A \code{tibble} object with 3 variables:
#' \describe{
#' \item{\code{scenario}}{The energy price scenario to be assumed. Options are "central",
#' "low_price", "high_price", and "off_peak"}
#' \item{\code{year}}{The year the cost applies to}
#' }
"electricity_prices"


#' Energy consumption
#'
#' The assumed future energy consumption, in kW consumed per kilometer, of electric vehicles by vehicle type. Data is adapted from ICCT data described here:
#' (https://theicct.org/sites/default/files/publications/EV_cost_2020_2030_20190401.pdf).
#'
#' @format A \code{tibble} object with 3 variables:
#' \describe{
#' \item{\code{vehicle_type}}{The energy price scenario to be assumed. Options are "central",
#'  "low_price", "high_price", and "off_peak". The central and off-peak scenarios assume a constant electricity price. The low price option assume electricity prices decrease over time, while the high price estimate assumes an increase in prices over time.}
#' \item{\code{year}}{The year the value applies to}
#' \item{\code{energy_consumption}}{The assumed energy consumption (kW per kilometre)}
#' }
"energy_consumption"

#' Energy intensity
#'
#' The assumed future energy intensity of the electricity grid. The data provided is the 'step change' scenario forecast by the Australian Energy Market Operator (AEMO) between 2020 and 2040. It is assumed that after 2040, a linear trajectory to net 0 by 2050 continues.
#'
#' @format A \code{tibble} object with 2 variables:
#' \describe{
#' \item{\code{year}}{The year the value applies to}
#' \item{\code{ei_g_wh}}{The energy intensity of the grid, in grams carbon dioxide per watt hour from 2021
#' to 2060. }
#' }
"energy_intensity"


#' Fleet
#'
#' A simulated fleet of new vehicle sales from 2021 to 2035, as produced by 'fleet_creator()'. Assumes a default of 100 cars sold in the base year. Each car therefore represents 1\% of new Australian car sales.
#'
#' @format A \code{tibble} object with 8 variables:
#' \describe{
#' \item{\code{vehicle_group}}{The type of vehicle}
#' \item{\code{year}}{The year vehicle is 'sold' in}
#' \item{\code{id}}{The ID for the vehicle in the given sales year}
#' \item{\code{base_emissions}}{The assumed emissions of the vehicle if it is a 'base' vehicle (
#' considered to be a 4-cylinder petrol engine of similar spec to 2008 vehicle sales)}
#' \item{\code{current_emissions}}{The assumed emissions of the vehicle after upgrades are
#' applied}
#' \item{\code{electric_applied}}{A logical indicating if the vehicle is electric or not}
#' \item{\code{tech_pkg_applied}}{A value representing the highest level technology package number the vehicle
#' has applied. This is it's position on the cost curve}
#' \item{\code{cost}}{The total cost of upgrades applied to the vehicle}
#' }
"fleet"

#' Fuel prices without taxes
#'
#' The assumed future prices of petrol and diesel fuels, excluding taxes.
#'
#' @format A \code{tibble} object containing forecast fuel prices for use in the fleetEffSim model. Fuel prices are base prices only - they do not include fuel excise tax or GST. There are 4 variables:
#' \describe{
#' \item{\code{scenario}}{The fuel price scenario to be assumed. Options are "central",
#' "low_price", "high_price"}
#' \item{\code{fuel_type}}{The type of fuel the cost applies to. Includes "diesel", "petrol_91",
#' "petrol_95" and "petrol_98"}
#' \item{\code{price}}{The assumed future price, excluding all taxes}
#' }
"fuel_prices_no_tax"

#' Fuel prices with taxes
#'
#' The assumed future prices of petrol and diesel fuels, including taxes.
#'
#' @format  A \code{tibble} object containing forecast fuel prices for use in the fleetEffSim model. Fuel prices are retail prices - they include fuel excise tax and GST. There are 4 variables:
#' \describe{
#' \item{\code{scenario}}{The fuel price scenario to be assumed. Options are "central",
#' "low_price", "high_price"}
#' \item{\code{fuel_type}}{The type of fuel the cost applies to. Includes "diesel", "petrol_91",
#' "petrol_95" and "petrol_98"}
#' \item{\code{price}}{The assumed future price, excluding all taxes}
#' }
"fuel_prices_tax"


#' KM travelled
#'
#' The assumed distance driven in a given year, determined by vehicle type and age. Calculated from the Australian Bureau of Statistics (ABS) Motor Vehicle Use Survey (2016)
#'
#' @format A \code{tibble} object with 3 variables:
#' \describe{
#' \item{\code{age}}{The age of the vehicle}
#' \item{\code{vehicle_type}}{The type of the vehicle (passenger, lcv, suv)}
#' \item{\code{km_travelled}}{The assumed dstance (km) travelled by the vehicle in a given year}
#' }
"km_travelled"

#' Targets and Business as usual
#'
#' The business as usual emissions trajectory of new vehicle sales, and potential fleetwide
#' emissions standards that may be implemented
#'
#' @format A \code{tibble} object with 3 variables:
#' \describe{
#' \item{\code{year}}{The year the emissions applies to}
#' \item{\code{target_type}}{The type of target or scenario. bau refers to the business as usual scenario. Other
#' scenarios ("central", "ambitious", "linear" refer to different targets that may be tested).}
#' }
"targets_and_bau"



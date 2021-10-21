
#script to import all required data for model
library(readr)
library(readxl)


#data for the cost model
cost_curves <- read_rds("data-raw/model_data/final-data/cost_curves.rds")
targets_and_bau <- read_rds("data-raw/model_data/final-data/targets_and_bau.rds")
fuel_prices_no_tax <- read_rds("data-raw/model_data/final-data/fuel-forecasts-no-tax.rds")
fuel_prices_tax <- read_rds("data-raw/model_data/final-data/fuel-forecasts-tax.rds")
electricity_prices <- read_rds("data-raw/model_data/final-data/energy_price_forecast.rds")
energy_consumption <- read_rds("data-raw/model_data/final-data/ev_energy_consumption.rds")
energy_intensity <- read_rds("data-raw/model_data/final-data/energy_intensity.rds")
km_travelled <- read_rds("data-raw/model_data/final-data/km_traveled.rds")
fleet <- fleet_creator(.i_cars = 100)


usethis::use_data(
  cost_curves,
  targets_and_bau,
  fuel_prices_tax,
  fuel_prices_no_tax,
  electricity_prices,
  energy_consumption,
  energy_intensity,
  km_travelled,
  fleet,
  internal = FALSE,
  overwrite = TRUE)

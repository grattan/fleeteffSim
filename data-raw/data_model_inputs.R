
#script to import all required data for model
library(readr)
library(readxl)

#data for the cost model
cost_curves <- read_rds("data-raw/cost-curves/cost_curves.rds")

#the targets and bau file
bau <- read_rds("data-raw/bau_scenarios/bau_emissions_trajectory.rds") %>%
  filter(scenario == "slow") %>%
  select(year, total_emissions) %>%
  rename("bau" = total_emissions)

targets <- read_xlsx("data-raw/model-inputs/targets.xlsx")


targets_and_bau <- inner_join(bau, targets) %>%
  pivot_longer(cols = (2:5),
               names_to = "target_type",
               values_to = "value")

#now data for the benefit model
#fuel and electricity prices
fuel_prices <- read_rds("data-raw/model-inputs/fuel-forecasts.rds")
electricity_prices <- read_rds("data-raw/model-inputs/energy_price_forecast.rds")

#energ intensity of ev's and the grid
energy_consumption <- read_rds("data-raw/model-inputs/ev_energy_consumption.rds")
energy_intensity <- read_xlsx("data-raw/AEMO/emissions-intensity-grid.xlsx",
                              sheet = "step_change") %>%
                              clean_names()

#km travelled
km_travelled <- read_rds("data-raw/model-inputs/km_traveled.rds")



#FOR THE MOMENT, TO CHANGE
fleet <- read_rds("data-raw/model-inputs/projected_fleet_in-100.rds")


usethis::use_data(
  cost_curves,
  targets_and_bau,
  fuel_prices,
  electricity_prices,
  energy_consumption,
  energy_intensity,
  km_travelled,
  fleet,
  internal = FALSE,
  overwrite = TRUE)

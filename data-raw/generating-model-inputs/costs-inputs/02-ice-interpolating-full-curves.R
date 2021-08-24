# 02-ice-interpolating-full-curves
# by Lachlan Fox, Grattan Institute

#This script takes the aggregated base year cost curves (for 2021 and 2025) as detailed
#in script 01_ice_cost_curve_creation and extrapolates the results to give a range of years
#from 2021 to 2050.

# Setup ------------------------------------------------------------------------

source("data-raw/generating-model-inputs/00-setup.R")


# Read data  -------------------------------------------------------------------

#Data from the previous script 01_ice_cost_curve_creation

ice_cost_curves_base <- read_rds("data-raw/temp/ice_costs_base_years.rds")

# Interpolating between base years  --------------------------------------------

#we're going to linearly interpolate between the years 2021-2025, and then beyond
#2025 we're going to assume that costs freeze, because manufacturers are focusing on ev's
#this is evidently a conservative estimate, and it's likely costs would continue to fall post 2025

#for the total costs
ice_costs <- ice_cost_curves_base %>%
  #we're first jus going to do the costs, then we'll repeat with the
  #incremental improvement
  select(-weighted_emissions, -aie, -incr_cost) %>%
  #we're going to make the data wide to deal with
  pivot_wider(names_from = year,
              values_from = weighted_cost) %>%
  #now simply interpolating it
  mutate(`2022` = `2021` + (`2025`-`2021`)/4 * 1,
         `2023` = `2021` + (`2025`-`2021`)/4 * 2,
         `2024` = `2021` + (`2025`-`2021`)/4 * 3) %>%
  #and back to long form
  pivot_longer((3:7),
               names_to = "year",
               values_to = "cost_aus")


#and the incremental costs
ice_incr_costs <- ice_cost_curves_base %>%
  #we're first jus going to do the costs, then we'll repeat with the
  #incremental improvement
  select(-weighted_emissions, -aie, -weighted_cost) %>%
  #we're going to make the data wide to deal with
  pivot_wider(names_from = year,
              values_from = incr_cost) %>%
  #now simply interpolating it
  mutate(`2022` = `2021` + (`2025`-`2021`)/4 * 1,
         `2023` = `2021` + (`2025`-`2021`)/4 * 2,
         `2024` = `2021` + (`2025`-`2021`)/4 * 3) %>%
  #and back to long form
  pivot_longer((3:7),
               names_to = "year",
               values_to = "incr_cost")



#now repeating this for the emissions

ice_emissions <- ice_cost_curves_base %>%
  #we're first jus going to do the costs, then we'll repeat with the
  #incremental improvement
  select(-weighted_cost, -weighted_emissions, -incr_cost) %>%
  #we're going to make the data wide to deal with
  pivot_wider(names_from = year,
              values_from = aie) %>%
  #now simply interpolating it
  mutate(`2022` = `2021` + (`2025`-`2021`)/4 * 1,
         `2023` = `2021` + (`2025`-`2021`)/4 * 2,
         `2024` = `2021` + (`2025`-`2021`)/4 * 3) %>%
  #and back to long form
  pivot_longer((3:7),
               names_to = "year",
               values_to = "aie")


#and we're going to repeat a third time to get our rolling improvement

ice_emissions_rolling <- ice_cost_curves_base %>%
  #we're first jus going to do the costs, then we'll repeat with the
  #incremental improvement
  select(-weighted_cost, -aie, -incr_cost) %>%
  #we're going to make the data wide to deal with
  pivot_wider(names_from = year,
              values_from = weighted_emissions) %>%
  #now simply interpolating it
  mutate(`2022` = `2021` + (`2025`-`2021`)/4 * 1,
         `2023` = `2021` + (`2025`-`2021`)/4 * 2,
         `2024` = `2021` + (`2025`-`2021`)/4 * 3) %>%
  #and back to long form
  pivot_longer((3:7),
               names_to = "year",
               values_to = "weighted_emissions")



#combining the two and projecting to 2035 ==================================

ice_cost_curves <- inner_join(ice_costs, ice_emissions)
ice_cost_curves <- inner_join(ice_cost_curves, ice_incr_costs)
ice_cost_curves <- inner_join(ice_cost_curves, ice_emissions_rolling)


#and now we can hold the 2025 values constant and project them to 2035
ice_cost_curves <- ice_cost_curves %>%
  mutate(year = as.integer(year)) %>%
  group_by(vehicle_group, tech_pkg_no) %>%
  complete(year = (2026:2050)) %>%
  arrange(vehicle_group, tech_pkg_no, year) %>%
  mutate(incr_cost = case_when(
    tech_pkg_no == 0 ~ 0,
    tech_pkg_no != 0 ~ incr_cost)) %>%
  na.locf()


#and now we can save our completed data
write_rds(ice_cost_curves, "data/temp/ice_cost_curves.rds")


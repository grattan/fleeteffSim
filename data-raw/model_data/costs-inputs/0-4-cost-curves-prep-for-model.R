# 04-cost-curves-prep-for-model
# by Lachlan Fox, Grattan Institute

#This file takes the ICE and EV cost curves prepared in scripts `01-ice-cost-curve-creation.R`,
#`03-ev-battery-costs.R` and combines and prepares them for use in the fleeteffSim model.

# Setup -----------------------------------------------------------------------

source("data-raw/model_data/00-setup.R")

#for the actual model, we need to have standardised cost curves for everything that
#we can work with easily and that can take the arguments we want to use

# the structure we want for the data is:
  # vechicle_group
  # year
  # estimate type (central/early_pp/late_pp)
  # cost (as an incremental cost)
  # type (ICE/EV)
  # tech_pkg_no
  # perc reduction of technology (icnr_reduction)


#Read data: cost curves --------------------------------------------------------

ev_cost_curves <- read_rds("data-raw/model_data/temp/ev_cost_curve.rds") %>%
  select(-conventional, -electric) %>%
  rename("incr_cost" = incremental_cost) %>%
  mutate(incr_reduction = 1,
         type = "ev",
         #we arbitrarily use tech_pkg_no = 100 for EV's.
         tech_pkg_no = 100,
         base_emissions = case_when(
           #the following values are those used in the `fleet_creator` function
           vehicle_group == "lcv" ~ 215,
           vehicle_group == "suv" ~ 177.7,
           vehicle_group == "passenger" ~ 158.4),
         weighted_emissions = 0)


ice_cost_curves <- read_rds("data-raw/model_data/temp/ice_cost_curves.rds") %>%
  mutate(type = "ice")


#Price parity scenarios ------------------------------------------------------

#' To create different scenarios we can sensitivity test, we're going to create three scenarios total:
#' A "central" scenario where EV costs are as predicted; a "late_pp" scenario where
#' all ev costs are shifted forwards one year; and an "early_pp" scenario where the costs
#' are shifted backward one year (so price parity comes one year earlier)

last_year <- ev_cost_curves %>%
  filter(year == 2050)

#price parity early (one year early)
early_pp <- ev_cost_curves %>%
  mutate(year = year - 1)
#pp one year late
late_pp <- ev_cost_curves %>%
  mutate(year = year + 1) %>%
  filter(year != 2051) %>%
  mutate(estimate = "late_pp")

#and now binding these to account fo the year we "lost" by adding/subtracting a year
#don't have to do this for late scenario as data starts at 2018 normally
early_pp <- bind_rows(early_pp, last_year) %>%
  mutate(estimate = "early_pp")


#and the central scenario we're going to call "central"
ev_cost_curves <- ev_cost_curves %>%
  mutate(estimate = "central")
ice_cost_curves_cen <- ice_cost_curves %>%
  mutate(estimate = "central")
ice_cost_curves_early <- ice_cost_curves %>%
  mutate(estimate = "early_pp")
ice_cost_curves_late <- ice_cost_curves %>%
  mutate(estimate = "late_pp")

ice_cost_curves <- bind_rows(ice_cost_curves_cen, ice_cost_curves_early,
                             ice_cost_curves_late)

#binding together the EV curves
ev_cost_curves <- bind_rows(early_pp, late_pp, ev_cost_curves) %>%
  arrange(estimate, year)


#binding these together
cost_curves <- rbind(ev_cost_curves, ice_cost_curves)


#' The final adjustement we're going to make is to assume that for all years where the EV
#' costs are negative, that manufacturers will use these savings to increase vehicle
#' specifications or generate more profit. So the ev price will simply fall to 0,
#' not negative.

cost_curves <- cost_curves %>%
  mutate(incr_cost = case_when(
    incr_cost < 0 ~ 0,
    incr_cost >= 0 ~ incr_cost))


#and saving the file
write_rds(cost_curves, "data-raw/model_data/final-data/cost_curves.rds")



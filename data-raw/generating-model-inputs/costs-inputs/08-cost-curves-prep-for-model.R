# 08-cost-curves-prep-for-model
# by Lachlan Fox, Grattan Institute
# SET UP =======================================================================
# Packages ---------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(purrr)
library(glue)
library(fst)
library(janitor)
library(grattantheme)
library(spatstat)
library(ggtext)
library(zoo)
library(readxl)
# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)
# READ DATA  ===================================================================

#for the actual model, we need to have standardised cost curves for everything that
#we can work with easily and that can take the arguments we want to use
#currently, the curves aren't exactly standardised, so this script will deal with that
#and prepare them for the model runs

# the structure we want for the data is:
  # vech_group
  # year
  # estimate type (i.e. icct/epa)
  # cost (incremental)
  # type (hybrid/ev etc.)
  # tech_pkg_no
  # perc reduction of technology (AIE)


ev_cost_curves <- read_rds("data/temp/ev_cost_curve.rds") %>%
  select(-conventional, -electric) %>%
  rename("incr_cost" = incremental_cost) %>%
  mutate(incr_reduction = 1,
         type = "ev",
         #this tech_pkg_no we've got for ev's is arbitrary and used for us to sort them
         tech_pkg_no = 100,
         #and now adding a column for the weighted emissions (reduction from 100, % increase)
         weighted_emissions = 100)



ice_cost_curves <- read_rds("data/temp/ice_cost_curves.rds") %>%
  select(-cost_aus) %>%
  mutate(type = "ice",
         incr_reduction = aie) %>%
    select(-aie)

#because we've only got one set of data (epa_estimate) for the moment we're jsut going
#to duplicate it and set a different label to each duplicate so we can run the code
#under each scenario
ice_cost_curves <- bind_rows(ice_cost_curves %>%
  mutate(estimate = "epa_estimate"),
  ice_cost_curves %>%
    mutate(estimate = "icct_estimate"))


#HYBRIDS CURRENTLY EXCLUDED
#I'm begining to think we won't actually use the hybrid data because I think
#it's baked ito the EPA curves already
#hybrid_cost_curve <- read_rds("data/temp/hybrid_cost_curve.rds") %>%
#  rename("incr_cost" = cost_aus,
#         "incr_reduction" = effectiveness) %>%
#  mutate(tech_pkg_no = NA,
#         estimate = "epa_estimate",
#         )



#CHANGING SCENARIOS TO PP INSTEAD OF ICCT ETC.
#because the "epa_estimate" ended up being rather useless, we're going to change the scenarios
#so that there are three scenarios - a central estimate (the icct estimate), a PP coming 1 year
#earlier estimate, and a PP coming one year later estimate


#so first we're going to filter to get rid of the epa estimate

ice_cost_curves <- ice_cost_curves %>%
  filter(estimate == "icct_estimate")

ev_cost_curves <- ev_cost_curves %>%
  filter(estimate == "icct_estimate")

#and now we're going to make our extra two estimates
#first we want to grab the data at the first and last years only
#the next bit will work with ev curves only

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


#one last adjustment we're going to make is to assume that beyond the point of price
#parity, consumers don't make savings (i.e. ev's don't become cheaper than ice's.) Instead
#we're assuming manufacturers take the difference in terms profit and adding new features/
#improving the specs of the cars.

cost_curves <- cost_curves %>%
  mutate(incr_cost = case_when(
    incr_cost < 0 ~ 0,
    incr_cost >= 0 ~ incr_cost))


#and saving the file
write_rds(cost_curves, "data/cost-curves/cost_curves.rds")



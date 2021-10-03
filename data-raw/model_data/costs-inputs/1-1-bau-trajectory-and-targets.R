# 09-BAU-emissions-trajectories
# by Lachlan Fox, Grattan Institute


#this script uses BITRE data and other BAU estimates to produce a forecast of the
#BAU emissions trajectory for new vehicles sold. It then joins this dataset with our 'targets'
#for a fleetwide standard

# Setup ------------------------------------------------------------------------

source("data-raw/model_data/00-setup.R")


#Reading data: ARENA -----------------------------------------------------------


#this data comes from ARENA estimates: https://arena.gov.au/assets/2018/06/australian-ev-market-study-report.pdf

#the `no intervention` scenario is used as our BAU

uptake_curves <- read_xlsx("data-raw/external_data/arena/ev-upake-forecasts.xlsx",
                           sheet = "arena-data") %>%
  rename(central = "ev_uptake_no_intervention",
         fast = "ev_uptake_moderate_intervention") %>%
  pivot_longer(cols = (2:3),
               names_to = "scenario",
               values_to = "ev_uptake") %>%
  filter(scenario == "central")

#first we're going to use this data to add column for the expected non-ev % of the fleet

uptake_curves <- uptake_curves %>%
  mutate(ice_proportion = 100 - ev_uptake)

#(e.g. arena: https://arena.gov.au/assets/2018/06/australian-ev-market-study-report.pdf assumed we hit
# 100% penetration in no intervention scenario at 2048).


# Estimating non-ev BAU ---------------------------------------------------------

#now we're going to make out BAU estimates for how ICE vehicles
#improve. We are going to assume a 1.5% improvement of ICE vehicles per year

bau_ice_emissions <- tibble(year = c(2021:2055))

#making a function to create the estimates based on a starting emissions and a rate
bau_ice_estimator <- function(.emissions_start = 181,
                              .years = bau_ice_emissions,
                              .rate = 0.02) {
  .years <- .years %>%
    mutate(emissions = 181,
           id = row_number())
  i <- 1

  while (i <= nrow(.years)) {

    .id <- .years$id[i]


    if (.id == 1) {
      .years <- .years %>%
        mutate(emissions = case_when(
          id == .id ~ .emissions_start,
          id != .id ~ emissions))
    }


    if (.id != 1) {
      .years <- .years %>%
        mutate(emissions = case_when(
          id == .id ~ .years$emissions[.id-1] * (1 - .rate),
          id != .id ~ emissions))

    }

    i <- i + 1

  }
  return(.years)
}


bau_ice <- bau_ice_estimator(.emissions_start = 181,
                              .years = bau_ice_emissions,
                              .rate = 0.015) %>%
  select(-id) %>%
  mutate(scenario = "central")


#joining ev and non-ev estimates for overall emissions trajectory
#-------------------------------------------------------------------------------

bau_all <- inner_join(uptake_curves, bau_ice) %>%
  rename("ice_emissions" = emissions) %>%
  mutate(ev_emissions = 0,
         ev_uptake = ev_uptake * 0.01,
         ice_proportion = ice_proportion * 0.01) %>%

  #now just calculating the total emissions from the data we have to get our
  #emissions trajectories.

  mutate(total_emissions = ev_uptake * ev_emissions + ice_proportion * ice_emissions) %>%
  select(year, total_emissions) %>%
  mutate(scenario = "bau") %>%
  rename("value" = total_emissions,
         "target_type" = scenario)



#Creating a non-saturated BAU curve --------------------------------------------

#Based on feedback from external reviewers, we're also going to create a non-saturated BAU
#curve that assumes that EV's do not reach 100% sales before 2050. The curve is also slower in
#emissions reductions over the mid 2030s period

#To do this, we're first going to use the same data from the original BAU scenario up to 2030.

bau_slow_1 <- bau_all %>%
  filter(year <= 2030) %>%
  mutate(target_type = "bau_slow")

#This gets the emissions to ~120/km in 2030.
#beyond this point, we're going to assume the rate follows to an annual improvement of
# 8\% per year to 2050. This assumption leads to a 2050 emissions value of ~22-23g/km

bau_slow_start <- bau_slow_1 %>%
  filter(year == 2030) %>%
  pull(value)

bau_slow_2 <- bau_ice_estimator(.emissions_start = bau_slow_start,
                  .years = tibble(year = c(2030:2050)),
                  .rate = 0.08) %>%
  select(-id) %>%
  rename("value" = emissions) %>%
  mutate(target_type = "bau_slow")

bau_slow <- bind_rows(bau_slow_1, bau_slow_2)

#and adding this to the bau all dataset

bau_all <- bind_rows(bau_all,
                     bau_slow)

#Plotting  ----------------------------------------------------------------------
bau_all %>%
  ggplot(aes(x = year, y = value, colour = target_type)) +
  #geom_point() +
  scale_y_continuous_grattan(limits = c(0,200)) +
  theme_grattan(legend = "top") +
  grattan_colour_manual() +
  geom_smooth(aes(colour = target_type), span = 0.2, se = FALSE) +
  labs(title = "In both tested BAU scenario, vehicle emissions fall rapidly to 2035",
       subtitle = "Average emissions (g/km) of new vehicle sales",
       caption = "BAU scenario assumes an ICE emissions improvement of 1.5% per year, and
       an electric vehicle uptake following the ARENA no-intervention scenario. The BAU-slow scenario
       assumes the same parameters as the BAU scenario until 2030. Beyond 2030, it assumes
       emissions improve at a rate of 8% annually. ")
#grattan_save_all("atlas/bau.pdf")






#Joining with target estimates --------------------------------------------------

targets <- read_xlsx("data-raw/model_data/targets.xlsx") %>%
  pivot_longer(cols = (2:4),
               names_to = "target_type",
               values_to = "value")


targets_and_bau <- bind_rows(targets, bau_all)

#saving data

write_rds(targets_and_bau, "data-raw/model_data/final-data/targets_and_bau.rds")












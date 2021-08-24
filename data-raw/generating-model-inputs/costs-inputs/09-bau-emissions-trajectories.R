# 09-BAU-emissions-trajectories

#this script uses BITRE data and other BAU estimates to produce a forecast of the
#BAU emissions trajectory for new vehicles sold.
# by Lachlan Fox, Grattan Institute
# SET UP =======================================================================
source("R/00-setup.R")

#first reading in our data
#this data comes from ARENA estimates: https://arena.gov.au/assets/2018/06/australian-ev-market-study-report.pdf

#the two estimates we use as our slow_BAU and fast_BAU scenario are the no intervention scenario and
#the moderate intervention scenario

#reading data
#-----------------------------------------------

uptake_curves <- read_xlsx("data/arena/ev-upake-forecasts.xlsx",
                           sheet = "arena-data") %>%
  rename(slow = "ev_uptake_no_intervention",
         fast = "ev_uptake_moderate_intervention") %>%
  pivot_longer(cols = (2:3),
               names_to = "scenario",
               values_to = "ev_uptake")

#first we're going to use this data to add column for the expected non-ev % of the fleet

uptake_curves <- uptake_curves %>%
  mutate(ice_proportion = 100 - ev_uptake)

#(e.g. arena: https://arena.gov.au/assets/2018/06/australian-ev-market-study-report.pdf assumed we hit
# 100% penetration in no intervention scenario at 2048).



#estimating non-ev BAU
#---------------------------------------------

#now we're going to make out BAU estimates for how ICE vehicles (including hybrids)
#improves. For a 'fast scenario' we're going to assume 3% (i.e. decent hybrid takup),
#and we're going to assume 1.5% in the slow scenario

bau_ice_emissions <- tibble(year = c(2021:2055))

#making a function to create the estimates based on a strating emissions and a rate
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



#and doing this for fast and slow scenarios
bau_ice_fast <- bau_ice_estimator(.emissions_start = 181,
                              .years = bau_ice_emissions,
                              .rate = 0.03) %>%
  select(-id) %>%
  mutate(scenario = "fast")

bau_ice_slow <- bau_ice_estimator(.emissions_start = 181,
                              .years = bau_ice_emissions,
                              .rate = 0.015) %>%
  select(-id) %>%
  mutate(scenario = "slow")



bau_ice <- rbind(bau_ice_slow, bau_ice_fast)


#joining ev and non-ev estimates for overall emissions trajectory
#--------------------------------------------

bau_all <- inner_join(uptake_curves, bau_ice) %>%
  rename("ice_emissions" = emissions) %>%
  mutate(ev_emissions = 0,
         ev_uptake = ev_uptake * 0.01,
         ice_proportion = ice_proportion * 0.01) %>%

  #now just calculating the total emissions from the data we have to get our
  #emissions trajectories.

  mutate(total_emissions = ev_uptake * ev_emissions + ice_proportion * ice_emissions)


#and saving our BAU estimates
write_rds(bau_all, "data/bau_scenarios/bau_emissions_trajectory.rds")


bau_all %>%
  ggplot(aes(x = year, y = total_emissions, colour = scenario)) +
  #geom_point() +
  scale_y_continuous_grattan(limits = c(0,200)) +
  theme_grattan(legend = "top") +
  grattan_colour_manual() +
  geom_smooth(aes(colour = scenario), span = 0.2, se = FALSE) +
  labs(title = "In our BAU scenario, new vehicles are zero emissions by 2048",
       subtitle = "Average emissions (g/km) of new vehicle sales",
       caption = "'Slow' assumed arena no intervention scenario, 1.5% ICE imprvoement/year.
       'Fast' assumed ARENA's 'moderate intervention' EV scenario and 3% ICE improvement/year")
grattan_save_all("atlas/bau.pdf")







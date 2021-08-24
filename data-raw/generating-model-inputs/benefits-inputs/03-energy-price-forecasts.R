#energy price forecast

source("R/00-setup.R")


#there's a lack of great information
#https://www.aemc.gov.au/sites/default/files/2020-12/2020%20Residential%20Electricity%20Price%20Trends%20report%20-%2015122020.pdf

#but in our central scenario, we're going to assume for the moment that we have a 1% a year
#increase in electricity prices (this is the same as BITRE's assumption)

#the base electricity cost (
#from https://www.aemc.gov.au/sites/default/files/2020-12/2020%20Residential%20Electricity%20Price%20Trends%20report%20-%2015122020.pdf
#page 4 )
#is assumed to be $0.27/kW in 2021 (base year)

energy_cost <- tibble() %>%
  mutate(year = 2021) %>%
  complete(year = (2021:2030)) %>%
  mutate(energy_price = 0.27)

#no we're going to add our change in price through a little function

energy_rate <- function(.rate = 1.015,
                        .energy_cost = energy_cost) {

    i <- 2
    while (i <= nrow(.energy_cost)) {
      .energy_cost$energy_price[i] = .energy_cost$energy_price[i-1] * .rate
      i <- i + 1
    }

    return(.energy_cost)
}

#using this for the central scenario (assuming no increase)
central_energy <- energy_rate(.rate = 1.0, .energy_cost = energy_cost) %>%
  mutate(scenario = "central")

#a 'cheap energy' scenario where prices decrease by 1% a year until 2030, then flat
cheap_energy <- energy_rate(.rate = 0.99, .energy_cost = energy_cost) %>%
  mutate(scenario = "low_price")

#and a high price energy scenario, increases by 2% a year until 2030, then flat
exp_energy  <- energy_rate(.rate = 1.02, .energy_cost = energy_cost) %>%
  mutate(scenario = "high_price")

#and a night charging scenario with a steady but lower price
night <- energy_cost %>%
  select(-energy_price) %>%
  mutate(scenario = "night",
         energy_price = 0.20)

#now binding all three together

energy_forecasts <- bind_rows(cheap_energy, exp_energy, central_energy, night) %>%
  #to get our residual values, we're just going to extend these forecasts to 2060.
  #This allows us to get benefits after 2050 into the model, and all prices are frozen at 2030
  #anyway
  group_by(scenario) %>%
  complete(year = (2031:2060)) %>%
  arrange(scenario, year) %>%
  na.locf()

#and saving our dataset
write_rds(energy_forecasts, "data/model-inputs/energy_price_forecast.rds")




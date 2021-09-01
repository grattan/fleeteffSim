#04-ev-energy-consumption
# by Lachlan Fox, Grattan Institute

#This script produces estimates of the electricity consumption of electric vehicles by type
#and year for the fleetEffSim model

# Setup ------------------------------------------------------------------------

source("data-raw/model_data/00-setup.R")

# Assumptions : ----------------------------------------------------------------

#from the ICCT paper they have efficiencies of:
#' (https://theicct.org/sites/default/files/publications/EV_cost_2020_2030_20190401.pdf)

# type        :   2018 kWh/mile    :  2030 kWh/mile
#-----            ------------        -------------
# passenger   :   0.30             :  0.28
# crossover   :   0.36             :  0.33
# suv         :   0.51             :  0.47


#if we take the weighted average between crossovers and SUV's (using our sales
#weighted proportions of crossover = 0.206 (small suv) and SUV = 0.153 (large suv)),
#we get the following:

#from the ICCT paper they have efficiencies of
# type        :   2018 kWh/mile    :  2030 kWh/mile
#-----            ------------        -------------
# passenger   :   0.30             :  0.28
# suv         :   0.42             :  0.39

#converting these to kWh/km (/ 1.60934)

#from the ICCT paper they have efficiencies of
# type        :   2018 kWh/mile    :  2030 kWh/mile
#-----            ------------        -------------
# passenger   :   0.186            :  0.17
# suv         :   0.26             :  0.24


#From these figures we get that the 2017 base rate for each is as above, and the rate
#of improvement for each is:
    #passenger = (0.186-0.17)/(2030-2018) = 0.00133
    #suv = (0.26-0.24)/(2030-2018) = 0.00166

#however, we want to be conservative in our estimate to account for the fact that
#vehicle are likely o have added featues that need more elctricity etc. So we will assume
#a rate of 0.001 for passenger vehicles and 0.0015 for suv's. We'll also assume the same rate as SUVs
#for LCV's.

#Using all the above to work out our energy consumption from 2018 onwards assuming this rate
#stays constant, until 2030. Beyond 2030 we will freeze the rate to be conservative.

energy_consumption <- tibble(vehicle_type = c("passenger", "lcv", "suv"),
                             energy_consumption = c(0.186, 0.26, 0.26),
                             year = c(2018, 2018, 2018)) %>%
  group_by(vehicle_type) %>%
  complete(year = (2019:2030)) %>%
  arrange(vehicle_type, year)


#now applying our rates to this data

i <- 1
while ( i <= nrow(energy_consumption)) {

  if (!is.na(energy_consumption$energy_consumption[i]) == TRUE) {
    i <- i + 1

  } else {
        if (energy_consumption$vehicle_type[i] == "passenger") {
          #applying our rate of -0.001/year for passenger vehicles
          energy_consumption$energy_consumption[i] = energy_consumption$energy_consumption[i-1] - 0.001
        } else {
          #and applying our rate of -0.002/year for non-passenger vehicles
         energy_consumption$energy_consumption[i] = energy_consumption$energy_consumption[i-1] - 0.0015
        }
    i <- i + 1
  }
}

#now we've got our improvements in efficiency out to 2030, we will assume they freeze beyond that
#until 2060 (we're including to 2060 not just 2050 to get our residual values of benefits accrued past
#this pint)
energy_consumption <- energy_consumption %>%
  group_by(vehicle_type) %>%
  complete(year = (2031:2060)) %>%
  arrange(vehicle_type, year) %>%
  na.locf()


write_rds(energy_consumption, "data-raw/model_data/final-data/ev_energy_consumption.rds")




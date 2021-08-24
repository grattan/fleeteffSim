# 02-ev-interpolating-base-years

#this script takes the base year cost estimates and interpolates between the base years
#to get estimates of non-battery costs for ev's
#between 2017-2025. This allows us to then scale the costs for different car classes (next script)
# and then factor in battery costs later to get our curves.

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
library(readxl)
# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)
# READ DATA  ===================================================================

#first we're going to import UBS data (https://neo.ubs.com/shared/d1wkuDlEbYPjF/ , pages 26 and 43)
#this data gives the breakdown of manufacturing costs for ev's (2017 and 2020) and for a
#conventional vehicle, seperated by component.

ubs_vehicle_costs <- read_xlsx("data/ubs/ubs-2017-teardown-costs.xlsx",
                               sheet = "r-input-costs")



#first we're going to simplify the UBS vehicle cost estimates into 4 components
#powertrain costs - the cost of components that make up the powertrain
#other direct costs - i.e. vehicle assembly
#indirect costs - i.e. depreciation, r&d, admin expenses
#battery costs - the costs (direct/indirect) of battery manufacturing

ubs_vehicle_costs <- ubs_vehicle_costs %>%
  #first just going to convert to aus 2021 dollars
  mutate(cost_aus = cost_us_2017 * 1.10 * 1.33) %>%
  select(-cost_us_2017) %>%
  #and now to add the costs within each broad group
  group_by(cost_type_1, vehicle_type, year) %>%
  summarise(cost_aus = sum(cost_aus)) %>%
  arrange(vehicle_type, year) %>%
  #and for the moment we're going to strip out the battery costs,
  #because these will be dealt with seperately.
  filter(cost_type_1 != "battery_costs")





# INTERPOLATNG ==================================================================

#so firstly, we're going to subset our data to remove the conventional vehicles.
  # the conventional vehicle cost estimates we are going to assume remain unchanged
  # over the forward years. This is based on the assumption that the base model we
  # are considering is mature technology and is unlikely to significantly change over time

ice_ubs_costs <- ubs_vehicle_costs %>%
  subset(vehicle_type ==  "conventional")

el_ubs_costs <- ubs_vehicle_costs %>%
  subset(vehicle_type ==  "electric") %>%
  select(1:4) %>%
  #we're going to make the data wide to deal with
  pivot_wider(names_from = year,
              values_from = cost_aus) %>%
  #now simply interpolating it
  mutate(`2018` = `2017` + (`2025`-`2017`)/8 * 1,
         `2019` = `2017` + (`2025`-`2017`)/8 * 2,
         `2020` = `2017` + (`2025`-`2017`)/8 * 3,
         `2021` = `2017` + (`2025`-`2017`)/8 * 4,
         `2022` = `2017` + (`2025`-`2017`)/8 * 5,
         `2023` = `2017` + (`2025`-`2017`)/8 * 6,
         `2024` = `2017` + (`2025`-`2017`)/8 * 7) %>%
  #and back to long form
  pivot_longer((3:11),
               names_to = "year",
               values_to = "cost_aus")


#binding the conventional and the electric datasets together

ubs_vehicle_costs <- rbind(el_ubs_costs %>%
                        mutate(year = as.double(year)),
                      ice_ubs_costs) %>% view()


#Saving data ----------------------------------------------------------------

write_rds(ubs_vehicle_costs, "data/temp/interpolated_ubs_costs.rds")






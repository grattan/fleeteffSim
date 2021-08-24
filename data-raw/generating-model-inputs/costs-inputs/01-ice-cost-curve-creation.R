# xx-cost-curve-creation
# by Lachlan Fox, Grattan Institute

#This script creates cost curves for the broad categories (small_passenger, passenger,
#small_suv, suv, and lcv) that were created in the 01-vehicle-classification script.

#It does this by averging the cost curves (and caps) between the different EPA classes
#contained within each broad class, based on weights that represent the characteristics
#of the Australian fleet.

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
library(scales)
# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)
# READ DATA  ===================================================================

#data on the australia fleet vehicle classes and breakdown into epa
vehicle_classes <- read_xlsx("data/collected/ntc-vehicle-collected-type.xlsx",
                             sheet = "epa_type_breakdown") %>%
  rename(vech_type_no = epa_type,
         vehicle_group = type) %>%
  filter(vech_type_no != "total") %>%
  mutate(vech_type_no = as.double(vech_type_no))

write_rds(vehicle_classes, "data/temp/vehicle_classes.rds")

#------------

#EPA cost curve data for both 2021 and 2025. This data was obtained from:
#     NEED TO PUT IN SOURCE HERE!!!

packages_2021 <- read_xls("data/epa/2021-technology-packages.xls") %>%
  select(1:8, -4) %>%
  clean_names() %>%
  mutate(year = "2021") %>%
  #removing all ev/hybrid data from the curves. This will be dealt with seperately
  filter(primary_fuel == "G")


packages_2025 <- read_xls("data/epa/2025-technology-packages.xls") %>%
  select(1:8, -4) %>%
  clean_names() %>%
  mutate(year = "2025") %>%
  #removing all ev/hybrid data from the curves. This will be dealt with seperately
  filter(primary_fuel == "G")








# COST CURVE FUNCTION ALL TYPES =================================================

#should probably turn this into a function

cost_curves <- function(.packages, .vehicle_type = (1:19),
                        .passenger = 149,
                        .suv = 184,
                        .lcv = 226) {

  #just deling with 2021 for the moment
  .packages <- .packages %>%
    filter(vech_type_no %in% (1:19)) %>%
    filter(vech_type_no %in% .vehicle_type) %>%
    mutate(vehicle_group = case_when(
          vech_type_no %in% c(1,2,4,6) ~ "passenger",
          vech_type_no %in% c(7,8) ~ "suv",
          vech_type_no %in% c(11,13) ~ "lcv",
          vech_type_no %in% c(3,5,9,10,12,14,15,16,17,18,19) ~ "unused")) %>%
    mutate(baseline_emissions = case_when(
          vehicle_group == "passenger" ~ .passenger,
          vehicle_group == "suv" ~ .suv,
          vehicle_group == "lcv" ~ .lcv)) %>%


    #just initialising these columns
    mutate(total_cost = 0,
           total_emissions = 0)

    #starting from vech type 1, this rolls through each tech package number.
    i <- 1
    while (i <= nrow(.packages)) {

      #at tech pakage = 1, we make the incremental cost the cap x the cost.
      if (.packages$tech_pkg_no[i] == 1) {
        .packages$total_cost[i] <- .packages$incr_cost[i] * .packages$cap[i]
          #print(.packages$tech_pkg_no[i])
          i <- i + 1
      }


      #beyond tech package 1, we add the increment cost x cap to the previous total cost to
      #give what is basically a running total
      else {
        .packages$total_cost[i] <- .packages$incr_cost[i] * .packages$cap[i] + .packages$total_cost[i-1]
          #print(.packages$tech_pkg_no[i])
          i <- i + 1
      }
    }


    #doing the same pattern for the incremental improvement
    i <- 1
    while (i <= nrow(.packages)) {
      if (.packages$tech_pkg_no[i] == 1) {
        .packages$total_emissions[i] <- .packages$baseline_emissions[i] * (1 - (.packages$cap[i] * .packages$aie[i]))
        #print(.packages$tech_pkg_no[i])
        i <- i + 1
      }

      else {
        .packages$total_emissions[i] <- .packages$total_emissions[i-1] * (1 - (.packages$cap[i] * .packages$aie[i]))
        #print(.packages$tech_pkg_no[i])
        i <- i + 1
      }
    }

   .packages <- .packages %>%
     #just adjusting the costs first for inflation (1.12) and converting to
     #aus dollars (1.34)
      mutate(total_cost_aus = total_cost * 1.12 * 1.34,
             incr_cost = incr_cost * 1.12*1.34)

  return(.packages)
}







#WEIGHTED AVERAGE CURVES BY CLASS (PASSENGER, SUV, LCV) ========================

#this function is designed to be used on the output of the cost_curves function
#it takes that output, and generates cost curves that simplify the vehicle classes
#into those we have defined (suv/lcv/passenger).

#It does this by using weighted averages of the different epa classes, using this
#to average the incremental improvements and costs. However, because we are averaging across
#packages now they no longer represent specific technologies - simply different curves
#for efficiency improvements.

simplify_curves <- function(.cost_curves,
                            .vehicle_classes = vehicle_classes,
                            #this `simplified` argument just strips some of the
                            #columns we don't need any more from the outpu - if all columns are wanted
                            #change it to false
                            .simplified = TRUE) {


    #let's join the datasets so that we get our weights into the data
    .cost_curves <- left_join(.cost_curves, .vehicle_classes)

    #now we're just quickly going to simplfy the dataset back to the fundamental things
    #that we really need

    if (.simplified == TRUE) {
      .cost_curves <- .cost_curves %>%
        select(vech_type_no, tech_pkg_no, cap, aie, incr_cost, year, vehicle_group,
               baseline_emissions, total_cost_aus, weight, total_emissions)
    }

    .cost_curves <- .cost_curves %>%
      filter(vehicle_group != "unused") %>%
      group_by(vehicle_group, tech_pkg_no, year) %>%
      summarise(weighted_cost =
               weighted.mean(x = total_cost_aus, w = weight),
             weighted_emissions =
               weighted.mean(x = total_emissions, w = weight),
             aie =  weighted.mean(x = aie, w = weight),
             incr_cost =  weighted.mean(x = incr_cost, w = weight))


    #adding the point 0

    initial <- .vehicle_classes %>%
      select(vehicle_group, type_emissions_average) %>%
      group_by(vehicle_group) %>%
      summarise(weighted_emissions = mean(type_emissions_average)) %>%
      mutate(weighted_cost = 0,
             tech_pkg_no = 0,
             year21 = 2021,
             year25 = 2025,
             aie = 0) %>%
      pivot_longer(c("year21", "year25"),
                   values_to = "year") %>%
      select(-name) %>%
      mutate(year = as.character(year))

    .cost_curves <- rbind(.cost_curves, initial)

}






#USING FUNCTION TO GENERATE OUTPUT-----------------------------------------------

#using the above functions on the epa data for 2021 and 2025 (putting into one df)
#we're setting al the vehicle to 100 so we can calculate the data as a % increase.

#cost_curves_all <- rbind(cost_curves(packages_2021), cost_curves(packages_2025))
#cost_curves_all <- simplify_curves(cost_curves_all)


cost_curves_all <- rbind(cost_curves(packages_2021,
                         .vehicle_type = (1:19),
                         .passenger = 100,
                         .suv = 100,
                         .lcv = 100),
                   cost_curves(packages_2025,
                               .vehicle_type = (1:19),
                               .passenger = 100,
                               .suv = 100,
                               .lcv = 100))

cost_curves_all <- simplify_curves(cost_curves_all) %>%
  group_by(vehicle_group) %>%
    mutate(perc_reduction = 100 - weighted_emissions) %>%
  mutate(perc_reduction = case_when(
    perc_reduction < 0 ~ 0,
    perc_reduction >= 0 ~ perc_reduction)) %>%
  select(-weighted_emissions) %>%
  rename("weighted_emissions" = perc_reduction)


write_rds(cost_curves_all, "data/temp/ice_costs_base_years.rds")


#plot 1 - faceted by year
cost_curves_all %>%
  ggplot(aes(x = weighted_emissions, y = weighted_cost)) +
  geom_point(aes(colour = vehicle_group)) +
  facet_wrap(~year) +
  theme_grattan(legend = "top",
                chart_type = "scatter") +
  grattan_y_continuous(limits = c(0,20000)) +
  grattan_x_continuous() +
  grattan_colour_manual(3) +
  labs(title = "Vehicles could reduce emissions cheaply in the short term, wihout EV's",
       subtitle = "Cost of reducing emissions (CO2) across vehicle types, g/km",
       y = "Total cost ($2021)",
       x = "Test cycle emissions (gCO2/km)",
       caption = "EV's and hybrids are excluded for all improvements. ")


#plot 2 - faceted by vehicle class
cost_curves_all %>%
  ggplot(aes(x = weighted_emissions, y = weighted_cost)) +
  geom_point(aes(colour = year)) +
  facet_wrap(~vehicle_group) +
  theme_grattan(legend = "top",
                chart_type = "scatter") +
  grattan_y_continuous(limits = c(0,15000),
                       breaks = c(0, 5000, 10000, 15000),
                       labels = dollar) +
  grattan_x_continuous(limits = c(0, 250)) +
  grattan_colour_manual(3) +
  coord_flip() +
  labs(title = "Vehicles could reduce emissions cheaply in the short term, without EV's",
       subtitle = "Cost of reducing emissions (CO2) across vehicle types, g/km",
       y = "Total cost ($2021)",
       x = "Test cycle emissions (gCO2/km)",
       capion = "EV's and hybrids are excluded for all improvements.")







#generating % improvement/cost charts for each type (just for 2021 numbers)
#-----------------------------------------------------

perc_2021 <- cost_curves(packages_2021,
                         .vehicle_type = (1:19),
                         .passenger = 100,
                         .suv = 100,
                         .lcv = 100)

perc_2021 <- simplify_curves(perc_2021) %>%
  group_by(vehicle_group) %>%
  mutate(perc_reduction = 100 - weighted_emissions) %>%
  filter(year == 2021) %>%
  mutate(perc_reduction = case_when(
    perc_reduction < 0 ~ 0,
    perc_reduction >= 0 ~ perc_reduction
  ))

perc_2021 %>%
  ggplot(aes(x = perc_reduction, y = weighted_cost, colour = vehicle_group)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_grattan(legend = "top", chart_type = "scatter") +
  grattan_colour_manual(3) +
  scale_y_continuous_grattan(limits = c(0, 10000)) +
  scale_x_continuous_grattan(limits = c(0, 100)) +
  labs(title = "There are many options to improve ICE efficiency relatively cheaply",
       subtitle = "Percentage reduction of emisions and cost, 2021",
       x = "Percentage co2 emissions reduction",
       y = "Cost ($)",
       caption = "These cost curves assume a base model vehicle. Current selling vehicles are likely
       to already incorerate some technology listed on the curve to some extent. It is likely
       a passenger vehicle has ~20% reduction already incorperated, while due to higher rates
       of diesel engines LCV's have up to 30% depending on the model.")


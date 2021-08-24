# 03-ev-costs-base-years

#this script takes input data from the previous script (which produces interpolated ubs data)
#and from various other sources (UBS/EPA/ICCT) to create the base costs for each vehicle type
#for ev's (excluding battery costs - these are incorperated later).
#Subsequent scripts incorporate the battery estimates to make full ev cost curves
#all costs in these files aren't incremental, they're total costs.

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
library(zoo)
# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)
# READ DATA  ===================================================================

#from 02 script
#reading in the data from the previous script, with the interpolated UBS costs
ad_ubs_costs <- read_rds("data/temp/interpolated_ubs_costs.rds")


# UBS - vehicle cost estimates ==================================================
#creating vehicle group costs -------------------------------------------------

#now we're going to create costs by vehicle type, using the current cost values as
#a 'base' cost. This is based on methods used in icct (2019) -
#(https://theicct.org/sites/default/files/publications/EV_cost_2020_2030_20190401.pdf)

#the assumptions are that:
    # - electric powertrain costs scaled 47% for large suvs (no scaling for passenger or crossover). This is based on power.
        # given we have crossover/suv in one category (and aus sales are approx 20% for crossover,
        # 15% for suv, this gives a weighted average increase of 19%)
        # we're going to make the same assumption for lcv's (as this is a lot of power already, around 178kW)
    # - conventional powertrain costs scaled by 18% for passenger cars/crossovers, and 74% for suvs.
        # (to reflect higher power/size of us market). Again weighting crossover/suv, our
        # suv category gets 42% increase
        # going to make the same assumption for lcv's
    # - other direct (assembly) costs scaled 6% for passenger, 21% for suvs (based on increased footprint)
        # Again weighting crossover/suv, our suv category gets 12.5% increase
    # - indirect costs are kept at 20.5% of other costs for conventional car production, and done
        # seperately for ev's based on UBS estimates (takes into account the changing scale of ev's). Th
        # is done in script 5 (as it relies on having battery costs already done). But the essence is
        # - in 2017% it's assumed that the indirect costs are 38.5%, which falls to
        # - 14% in 2025. Beyond that we assume that indirect costs are stable (not at 14% but
        # - at the actual figure this represents in 2025.
        # - this is calculated from ubs/icct as the indirect cost/all direct costs in each year

#the weightings between categories is in the data here:
vehicle_classes <- read_rds("data/temp/vehicle_classes.RDS")

vehicle_type_costs <- function(
  .ubs_costs = ad_ubs_costs,
  .passenger_electric_pt = 1,
  .suv_electric_pt = 1.19,
  .lcv_electric_pt = 1.19,
  .passenger_conv_pt = 1.18,
  .suv_conv_pt = 1.42,
  .lcv_conv_pt = 1.42,
  .passenger_dir = 1.06,
  .suv_dir = 1.125,
  .lcv_dir = 1.125,
  .conv_indirect = 0.205
)
  {
  .ubs_costs <- .ubs_costs %>%
    rename(base_cost_aus = cost_aus) %>%
    mutate(

    #PASSENGER  SCALING
      #--------
      passenger_cost = case_when(
        #first conventional costs
        cost_type_1 == "powertrain_costs" & vehicle_type == "conventional" ~ base_cost_aus * .passenger_conv_pt,
        #now scaling other direct costs for electric
        cost_type_1 == "other_direct_costs" & vehicle_type == "conventional" ~ base_cost_aus * .passenger_dir,
        #the indirect costs we'll deal with seperately for conventional vehicles

        #and now ev costs
        #no scaling on powertrain, in here for completeness
        cost_type_1 == "powertrain_costs" & vehicle_type == "electric" ~ base_cost_aus * .passenger_electric_pt,
        cost_type_1 == "other_direct_costs" & vehicle_type == "electric" ~ base_cost_aus * .passenger_dir,
      ),

     #SUV SCALING
     #------
      #now for suvs - first for conventional cars
      suv_cost = case_when(
        #conventional powertain scaling
        cost_type_1 == "powertrain_costs" & vehicle_type == "conventional" ~ base_cost_aus * .suv_conv_pt,
        #now scaling other direct costs electric
        cost_type_1 == "other_direct_costs" & vehicle_type == "conventional" ~ base_cost_aus * .suv_dir,
        #the indirect costs we'll deal with seperately for conventional vehicles



      #ev powertrain scaling
        cost_type_1 == "powertrain_costs" & vehicle_type == "electric" ~ base_cost_aus * .suv_electric_pt,
        #now scaling other direct costs for electric
        cost_type_1 == "other_direct_costs" & vehicle_type == "electric" ~ base_cost_aus * .suv_dir,
      ),

      #we're assuming lcv have same costs as suv's (but we'll later assume require more range for towing)
     lcv_cost = case_when(
       #conventional powertain scaling
       cost_type_1 == "powertrain_costs" & vehicle_type == "conventional" ~ base_cost_aus * .lcv_conv_pt,
       #now scaling other direct costs electric
       cost_type_1 == "other_direct_costs" & vehicle_type == "conventional" ~ base_cost_aus * .lcv_dir,
       #the indirect costs we'll deal with seperately for conventional vehicles



       #ev powertrain scaling
       cost_type_1 == "powertrain_costs" & vehicle_type == "electric" ~ base_cost_aus * .lcv_electric_pt,
       #now scaling other direct costs for electric
       cost_type_1 == "other_direct_costs" & vehicle_type == "electric" ~ base_cost_aus * .lcv_dir
     )


    )


    #INDIRECT COSTS
  #now we're got our other costs, we can work out the indirect costs (as 20.5% of
  #other total costs for conventional vehicles. EV indirect = base cost indirect)
        #the current method is long and poorly coded but works

  #first just calculating the indirect costs for conventional
  #-------------
  indirect_costs <- .ubs_costs %>%
    filter(vehicle_type == "conventional")
  indirect_costs_passenger <- sum(indirect_costs$passenger_cost, na.rm = TRUE) * .conv_indirect
  indirect_costs_suv <- sum(indirect_costs$suv_cost, na.rm = TRUE) * .conv_indirect
  indirect_costs_lcv <- sum(indirect_costs$lcv_cost, na.rm = TRUE) * .conv_indirect

  #now subsetting data
  indirect_costs <- .ubs_costs %>%
    filter(vehicle_type == "conventional",
           cost_type_1 == "indirect_costs")
  #and creating the new row with correct indirect costs
  indirect_costs <- indirect_costs %>%
    mutate(passenger_cost = indirect_costs_passenger,
           suv_cost = indirect_costs_suv,
           lcv_cost = indirect_costs_lcv)

  #and adding this back into the overall dataframe
  .ubs_costs <- rbind(.ubs_costs %>%
                        subset(cost_type_1 != "indirect_costs" | vehicle_type != "conventional"),
                      indirect_costs)



  #indirect costs for ev's will be dealt with later, as this relies on knowing the battery
  #cost

  #returning our dataset
  return(.ubs_costs)

}


#-----------------------------

#and now calling our function to store in a variable, adjusted ubs costs
ad_ubs_costs <- vehicle_type_costs() %>%
  arrange(vehicle_type, year) %>%
  unique()


#-----------------------
#adding columns through to 2035
#so that we can make use this data later, we're going to basically stretch the data
#out to 2035 (from 2017) for conventional vehicles. Given we're assuming that the
#costs are remaining constant over this period, this is simply cut and pasting the
#2017 results into all years out until 2035

conventional_all_years <- ad_ubs_costs %>%
  filter(vehicle_type == "conventional") %>%
  #just removing the lcv_cost column for the moment as we have no data here
  complete(year = 2018:2050)  %>%
  arrange(cost_type_1, year) %>%
  map_dfr(na.locf)

ad_ubs_costs <- rbind(ad_ubs_costs %>%
                        filter(vehicle_type != "conventional"),
                      conventional_all_years)


#now we're going to do a similar thing for the electric data
#but in this case we assume that the costs freeze at 2025 out to 2035. The difference
#between the ice and ev assumption here is that ev indirect costs (like r&d) etc. will
#initially be concentrated aond relatively few vehicles, but over time the market
#will mature and costs will stop falling as much. It's a conservative estimate to assume
#they don't fall beyond 2025 however, as they almost certainly will (but it's dificult)
#to predict how far.

electric_all_years <- ad_ubs_costs %>%
  filter(vehicle_type == "electric",
         #here we're just removing the indirect costs as we'll deal with these
         #later on
         cost_type_1 != "indirect_costs") %>%
  #just removing the lcv_cost column for the moment as we have no data here
  complete(year = 2025:2050)  %>%
  filter(year >= 2025) %>%
  arrange(cost_type_1, year) %>%
  map_dfr(na.locf) %>%
  filter(year != 2025)

ad_ubs_costs <- rbind(ad_ubs_costs,
                      electric_all_years)



#saving data
write_rds(ad_ubs_costs, "data/temp/adjusted_ubs_costs.rds")











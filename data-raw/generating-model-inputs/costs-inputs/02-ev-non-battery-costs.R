# 02-ev-interpolating-base-years
# by Lachlan Fox, Grattan Institute

#this script uses UBS vehicle teardown study costs to estimate the cost of non-battery
#electric vehicle components and assembly from 2017-2025.

# Setup ------------------------------------------------------------------------

source("data-raw/generating-model-inputs/00-setup.R")

# Read data: ubs vehicle costs -------------------------------------------------

#first we're going to import UBS data (https://neo.ubs.com/shared/d1wkuDlEbYPjF/ , pages 26 and 43)
#this data gives the breakdown of manufacturing costs for ev's (2017 and 2020) and for a
#conventional vehicle.

ubs_vehicle_costs <- read_xlsx("data-raw/ubs/ubs-2017-teardown-costs.xlsx",
                               sheet = "r-input-costs")


#simplfying the cost data into indirect, powertrain and other direct costs:
  #powertrain costs     -   the cost of components that make up the powertrain
  #other direct costs   -   i.e. vehicle assembly
  #indirect costs       -   i.e. depreciation, r&d, admin expenses
  #battery costs        -   the costs (direct/indirect) of battery manufacturing

ubs_vehicle_costs <- ubs_vehicle_costs %>%
  #Converting to aus 2021 dollars (as defined in setup script)
  mutate(cost_aus = cost_us_2017 * us_inflation_2017 * us_aus_exchange) %>%
  select(-cost_us_2017) %>%
  group_by(cost_type_1, vehicle_type, year) %>%
  summarise(cost_aus = sum(cost_aus)) %>%
  arrange(vehicle_type, year) %>%
  #removing battery costs, as these will be considered independently
  filter(cost_type_1 != "battery_costs")


# Interpolating costs between years --------------------------------------------

#so firstly, we're going to subset our data to remove the conventional vehicles.
  # the conventional vehicle cost estimates we are going to assume remain unchanged
  # over the forward years. This is based on the assumption that the base model we
  # are considering is mature technology and is unlikely to significantly change over time

ice_ubs_costs <- ubs_vehicle_costs %>%
  subset(vehicle_type ==  "conventional")

#linearly interpolating between 2017 and 2025 costs provided for electric vehicles
el_ubs_costs <- ubs_vehicle_costs %>%
  subset(vehicle_type ==  "electric") %>%
  select(1:4) %>%
  pivot_wider(names_from = year,
              values_from = cost_aus) %>%
  mutate(`2018` = `2017` + (`2025`-`2017`)/8 * 1,
         `2019` = `2017` + (`2025`-`2017`)/8 * 2,
         `2020` = `2017` + (`2025`-`2017`)/8 * 3,
         `2021` = `2017` + (`2025`-`2017`)/8 * 4,
         `2022` = `2017` + (`2025`-`2017`)/8 * 5,
         `2023` = `2017` + (`2025`-`2017`)/8 * 6,
         `2024` = `2017` + (`2025`-`2017`)/8 * 7) %>%
  pivot_longer((3:11),
               names_to = "year",
               values_to = "cost_aus")


#combining datasets
ad_ubs_costs <- rbind(el_ubs_costs %>%
                        mutate(year = as.double(year)),
                      ice_ubs_costs) %>% view()


#the previous code established costs for between 2017 and 2025 for conventional vehicles
#and non-battery components for an electric vehicle. However, this was based on a vehicle teardown
#study of a relatively small passenger vehicle.
#the following section uses these estimated costs to derive estimated costs for
#larger vehicles such as LCV's and SUV's.
#again, battery costs are excluded.

#creating vehicle group costs -------------------------------------------------

#to extrapolate costs to different vehicle types, we follow ICCT methodology (2019):
#(https://theicct.org/sites/default/files/publications/EV_cost_2020_2030_20190401.pdf)
#this approach uses vehicle characteristic to scale the base costs to get estimates for other vehicle types.
#the figures used are those provided by the ICCT, which use details of the US vehicle fleet
#to estimate scaling values. These values are likely to overstate the scaling required in the
#Australian fleet, give that Australian vehicle are on average less powerful and smaller.

#as a result, the resulting estimates are conservative estimates that are likely to
#to overstate electric vehicle costs

#Further, the ICCT uses different vehicle classes in their analysis. Where we use passenger,
#suv and lcv, they use passenger, crossover, and large suv (lcv's are excluded). As a result,
#our scaling parameters for the SUV category are calculated as a sales weighted average between
#the crossover and large suv figures used by the ICCT. The sales fractions of crossovers and suv's
#are approximately 20% (crossover) and 15% (suv) (as in `vehicle_classes`).

#because no information is provided for LCV's, we're going to make the very conservative assumption that
#costs are 20% greater than the SUV category. This adjustment is made at the end after the
#data is prepared, so the figures used in the function below are simply the same as the SUV
#figures.

#To scale the costs, we assume that: (see ICCT paper for more detail)
    #' Electric powertrain costs are scaled by the ratio of vehicle power in the US fleet:
        #' ICCT scaling: 47\% for large SUV's, no scaling for crossover or passenger vehicles
        #' Once SUV/Crossover combined, this gives ( 0.15 x 47 + 0.2 x 0 ) = 19% scaling for all suv's
        #' This gives passenger (0% scaling), SUV (19% scaling) for out estimate
        #'
      #' Conventional powertrain costs are similarly scaled by power ratios:
        #' ICCT scaling: 18% for passenger vehicles and crossovers, 74% for large SUV's
        #' This gives 42% increase for our SUV category, 18% for passenger
        #'
      #' Direct costs (assembly) are scaled based on the ratio of vehicle footprint size
        #'  ICCT costs:  21% large SUVs, 6% passenger and crossover
        #'  This gives 12.5% for our SUV category, and 6% for passenger
        #'
      #' Indirect costs:
        #' Conventional vehicle: held constant at 20.5% of other total costs
        #' Electric vehicles: assumed as 38.5% in 2017, falling to 14% in 2025. Beyond
        #' 2025 they are held constant at the absolute $2025 figure. However these indirect costs
        #' for electric vehicles are dealt with in a seperate script after battery costs are added


#the weightings between categories is in the `vehicle_classes` data used previously.
vehicle_classes <- read_rds("data-raw/temp/vehicle_classes.RDS")

#the below function scales the costs according to the assumptions outlined above
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
  .conv_indirect = 0.205){

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
  #other total costs for conventional vehicles.

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


#Calling vehicle costs function ------------------------------------------------

#and now calling our function to store in a variable, adjusted ubs costs
ad_ubs_costs <- vehicle_type_costs() %>%
  arrange(vehicle_type, year) %>%
  unique()


#-----------------------
#We're assuming the conventional costs for a base vehicle remain steady from
#2017 to 2035. This is as manufacturers are likely to prioritise investment in
#electric and other vehicle types.

conventional_all_years <- ad_ubs_costs %>%
  filter(vehicle_type == "conventional") %>%
  complete(year = 2018:2050)  %>%
  arrange(cost_type_1, year) %>%
  map_dfr(na.locf)

ad_ubs_costs <- rbind(ad_ubs_costs %>%
                        filter(vehicle_type != "conventional"),
                      conventional_all_years)

#we're similarly going to assume that electric vehicle costs freeze at 2025 levels
#for non-battery components (battery components, dealt with later, will continue to fall).
#in part this is due to uncertainty. However, it is also unlikely to affect the estimates
#significantly as price parity is likely to be reached relatively soon after 2025.

#Indirect (as a % of total) and total costs will continue to fall beyond 2035.

electric_all_years <- ad_ubs_costs %>%
  filter(vehicle_type == "electric",
         cost_type_1 != "indirect_costs") %>%
  complete(year = 2025:2050)  %>%
  filter(year >= 2025) %>%
  arrange(cost_type_1, year) %>%
  map_dfr(na.locf) %>%
  filter(year != 2025)

ad_ubs_costs <- rbind(ad_ubs_costs,
                      electric_all_years) %>%
  #adding the assumption that LCV costs are 20% greater than SUVs
  mutate(lcv_cost = suv_cost * 1.2)


write_rds(ad_ubs_costs, "data-raw/temp/adjusted_ubs_costs.rds")






















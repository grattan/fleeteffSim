# 04-ev-battery-costs
# by Lachlan Fox, Grattan Institute

#this script prepares the battery costs from us EPA and ICCT estimates
#for each car type out to 2035. These are later integrated with the other ev cost
#data to create overall cost curves for ev's out to 2035.

# Setup -----------------------------------------------------------------------

source("data-raw/model_data/00-setup.R")

# Read data: UBS costs ---------------------------------------------------------

#data prepared in the previous script `ev_interpolating_base_years`
ad_ubs_costs <- read_rds("data-raw/model_data/temp/adjusted_ubs_costs.rds")



# Read data: EPA battery costs -------------------------------------------------

#we're also now going to read in the US epa battery data costs
#this data has the epa estimates for battery costs from 2017-2025 (in US $ 2015)
#it comes from https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100Q3L4.pdf , pages 478-487
#some explanatory notes are also contained here: https://www.mdpi.com/2032-6653/9/3/42

battery_costs <- read_xlsx("data-raw/external_data/epa/ev-costs-epa.xlsx",
                           sheet = "batterycosts-ev") %>%
  clean_names() %>%
  filter(cost_type == "TC") %>%
  select(-(15:16))

#We're going to make the assumption that all electric vehicles are 200 mile capable vehicles
#(although some scaling for vehicle classes will occur later).

battery_costs <-  battery_costs %>%
  filter(type == "bev-200") %>%
  select(-(3:5)) %>%
  pivot_longer(cols = (3:11),
               names_to = "year",
               values_to = "base_cost") %>%
  mutate(year = str_sub(year, start = 2)) %>%

  #Converting costs to $AUD 2021
  mutate(aus_base_cost = base_cost * us_inflation_2015 * 1.30) %>%
  select(-base_cost)


#although the current EPA dataset only provides values for vehicles in weight classes 1-5,
#we are going to add a class 6 weight class (which the EPA uses and generally corresponds to
#LCV type vehicles)
#the values will be adjusted later to take account for the increased need for range of lcv's.

lcv_battery_costs <- battery_costs %>%
  subset(curb_weight_class == 5) %>%
  mutate(curb_weight_class = 6)

battery_costs <- rbind(battery_costs, lcv_battery_costs)



#Battery costs by vehicle type : -----------------------------------------------
#now that our data is nice and clean, we can get on to creating battery costs for
#our vehicle types (suv/lcv/passenger) based on weighted averages of the epa weight classes.
#the epa vehicle classes are discussed in more detail in the draft TAR and here:
#https://www.mdpi.com/2032-6653/9/3/42 page 8
#the 6 classes correspond approximately to the following :
    # 1 - small car
    # 2 - standard car
    # 3 - large car
    # 4 - small MPV
    # 5 - large MPV
    # 6 - truck
#however, in the draft TAR no costs are put forward for type 6 battery classes.
#we're going to categorise type 1,2,3 as passenger, 4,5 as SUV and 6 as LCV
#this is consistent with how we classified the EPA classes into our vehicle types
#we're also going to weight the classes based on the same weightings we used in
#script `01-ice-cost-curve-creation`, which is based on the Australian fleet.
#the `vehicle_classes` dataframe in script one has the following

vehicle_classes <- read_rds("data-raw/model_data/temp/vehicle_classes.RDS")

#from this, we get the following:
    # 1 - small car       :     corresponds to EPA type 1 -         :    weight = 0.0793 (passenger)
    # 2 - standard car    :     corresponds to EPA type 2 -         :    weight = 0.188  (passenger)
    # 3 - large car       :     corresponds to EPA type 4 and 6 -   :    weight = 0.023  (passenger)
    # 4 - small MPV       :     corresponds to EPA type 7 -         :    weight = 0.206 (suv)
    # 5 - large MPV       :     corresponds to EPA type 8 -         :    weight = 0.153 (suv)
    # 6 - truck           :     corresponds to EPA type 11 and 13 - :    weight = 0.351 (lcv)


#adding these weights to the data (but through the vehicle class file not the
# numbers above)

battery_costs <- battery_costs %>%
  mutate(weight = case_when(
    curb_weight_class == 1 ~ vehicle_classes$weight[1],
    curb_weight_class == 2 ~ vehicle_classes$weight[2],
    #note that the indexes DON'T correspond to the class - it's the row of the table
    #so row (index) 3 == type 4 and row (index) 4 == type 6 etc.
    curb_weight_class == 3 ~ vehicle_classes$weight[3] + vehicle_classes$weight[4],
    curb_weight_class == 4 ~ vehicle_classes$weight[5],
    curb_weight_class == 5 ~ vehicle_classes$weight[6],
    curb_weight_class == 6 ~ vehicle_classes$weight[7] + vehicle_classes$weight[8]
  ),
  #and also adding the vehicle categories to the data
  vehicle_group = case_when(
    curb_weight_class %in% c(1, 2, 3) ~ "passenger",
    curb_weight_class %in% c(4, 5) ~ "suv",
    curb_weight_class == 6 ~ "lcv"
  )) %>%

#and now we're going to use these weights to summarise the battery costs into
#our vehicle types for each year

  group_by(type, vehicle_group, year) %>%
  summarise(aus_base_cost =
              weighted.mean(x = aus_base_cost, w = weight)) %>%
  #and just changing some naming to make consistent with other files
  rename(battery_type = type) %>%
  mutate(vehicle_type = "electric",
         cost_type_1 = "battery_cost") %>%
  relocate(cost_type_1, vehicle_type)



#Scaling battery costs ---------------------------------------------------------

#although the EPA estimates are applicable in the capacity used in the OMEGA model,
#the estimates used for battery capacity is signicantly lower than many other sources.
#in part, this is likely because the estimates have changed rapdily in recent years.

#' the EPA estimates for a 200 mile BEV correspond approximately to a 47-67kWh battery size
#' for SUVs
#' (as outlined in: https://www.mdpi.com/2032-6653/9/3/42)
#' However, in practice, electric suv's are consistently being sold with battery capacities well above this.
#' for example, the small suv hyundai kona has a capacity of 64 kWh (EPA would assume 47-54 kWh
#' for this class) and more expensive SUV's such as the telsa model X, jag I-PACE,
#' Mercedes EQC have batteries in the 80-100 kWh range.
#' The reason for this is because the batteries have ranges that exceed the assumed 200mile
#' capacity estimate used by the EPA
#'
#' However, given that this model aims to provide costs associated with a 'comparable' or
#' higher quality vehicle, using these higher expected capacities with large ranges
#' is likely to give a more reasonable estaimte, particularly in the SUV and LCV segments
#' where consumers may be more range anxious.
#'
#' The resulting scaling factor assumed for this analysis is 85kWh for an SUV (scaled 37% as
#' opposed to US estimate of ~62kWh), and 110kWh for LCVs (77% scaling from 62kWh estimate.)
#' The LCV estimate is particularly conservative (with a very high battery capacity assumed)
#' due to potential constraints surrounding towing and other consumer needs in this segment.

suv_adjusted_costs <- battery_costs %>%
  filter(vehicle_group ==  "suv") %>%
  mutate(aus_base_cost = aus_base_cost * 1.37)

lcv_adjusted_costs <- battery_costs %>%
  filter(vehicle_group ==  "lcv") %>%
  mutate(aus_base_cost = aus_base_cost * 1.77)

battery_costs <- bind_rows(suv_adjusted_costs,
                                    lcv_adjusted_costs,
                                    battery_costs %>%
                                      filter(vehicle_group == "passenger"))

#Combining battery and other vehicle costs estimates ----------------------------

#Although the US EPA provide battery cost forecasts to 2025, these forecasts have been
#found to be highly conservative in practice (they are ~4% decrease per year),
#and are not aligned with most recent industry and academic forecasts.

#' To account for this, the EPA 2017 base figures are used as a base year for battery costs.
#' Following ICCT metholody (built off various battery price forecasts, as outlined here:
#' (https://theicct.org/sites/default/files/publications/EV_cost_2020_2030_20190401.pdf)
#' we are going to assume a battery price improvement of 7% annually from the EPA estimated base
#' costs.


#Battery cost / rate function -------------------------------------------------
#The following function allows us to forecast battery costs with a given rate of improvement

forecast <- function(.data,
                     .rate,
                     i = 2) {

  i <- 2
  while (i <= nrow(.data)) {
    .data$passenger[i] <- .data$passenger[i-1] * .rate
    .data$suv[i] <- .data$suv[i-1] * .rate
    .data$lcv[i] <- .data$lcv[i-1] * .rate
    i <- i + 1
  }

  return(.data %>%
           pivot_longer(cols = (5:7),
                        names_to = "vehicle_group",
                        values_to = "cost_aus"))

}

#using the EPA base year and a 7% rate of price decrease:

battery_forecasts <- battery_costs %>%
  filter(year == 2017) %>%
  mutate(year = as.integer(year)) %>%
  arrange(vehicle_group, year) %>%
  #although we're going to 2050 (and battery prices are unlikely to drop at the same rate until then),
  #beyond the price parity point (likely late 2020s) the incremental cost difference will be assumed ~ $0.
  #therefor the change in battery price beyond this point (and the fact that 7% is likely not reasonable far
  #beyond this) is not an issue
  complete(year = (2018:2050)) %>%
  pivot_wider(names_from = vehicle_group,
              values_from = aus_base_cost) %>%
  arrange(year)

battery_forecasts <- forecast(.data = battery_forecasts,
                          .rate = 0.93) %>%
  ungroup() %>%
  mutate(cost_type_1 = "battery_cost",
         vehicle_type = "electric") %>%
  select(-battery_type)





#The following section builds uses the battery and non-battery costs in this script
#and the previous script (02-ev-battery-costs.R) to generate total cost curves for
#electric vehicles out to 2050.

#Combining battery and non-battery data ----------------------------------


#first we're going to load in our data on non-battery costs and make some
#minor changes to make everything consistent between the two datasets
ad_ubs_costs <- read_rds("data-raw/model_data/temp/adjusted_ubs_costs.rds") %>%
  #and just putting in palceholder values for lcv's
  mutate(lcv_cost = suv_cost * 1.3) %>%
  #we're also going to strip out the base costs and make it into a long format
  select(-base_cost_aus) %>%
  pivot_longer(cols = (4:6),
               names_to = "vehicle_group",
               values_to = "cost_aus") %>%
  mutate(vehicle_group = case_when(
    vehicle_group == "passenger_cost" ~ "passenger",
    vehicle_group == "suv_cost" ~ "suv",
    vehicle_group == "lcv_cost" ~ "lcv"))


#and also going to read in the battery cost forecasts we produced
battery_forecasts <- battery_forecasts %>%
  mutate(vehicle_type = "electric",
         cost_type_1 = "battery_cost")


ev_forecast <- rbind(battery_forecasts, ad_ubs_costs) %>%
  #and just reordering the data nicely
  select(vehicle_type, vehicle_group, cost_type_1, year, cost_aus)




#Indirect electric vehicle costs --------------------------------------------

# - for ev costs, indirect costs are assumed to change over time (following UBS/ICCT estimates)
# - in 2017% it's assumed that the indirect costs are 38%, which falls to
# - 14% in 2025. Beyond that we assume that indirect costs are stable (not at 14% but
# - at the actual figure this represents in 2025. Linear interpolating between the two,
# - this gives 2017: 38.05, 2018: 35, 2019 32, 2020: 29, 2021: 26, 2022: 23, 2023: 20, 2024: 17, 2025: 14
# - this is calculated from ubs/icct as the indirect cost/all direct costs in each year

#putting these values into a df
indirect_proportions <- tribble(
  ~year,   ~proportion,
  2017,    0.38,
  2018,    0.35,
  2019,    0.32,
  2020,    0.29,
  2021,    0.26,
  2022,    0.23,
  2023,    0.20,
  2024,    0.17,
  2025,    0.14
)


electric_indirect_costs <- ev_forecast %>%
  filter(vehicle_type == "electric") %>%
  mutate(direct_indirect = case_when(
    cost_type_1 != "indirect_costs" ~ "direct_cost",
    cost_type_1 == "indirect_costs" ~ "indirect_cost"
  )) %>%
  group_by(direct_indirect, year, vehicle_type, vehicle_group) %>%
  summarise(cost_aus = sum(cost_aus)) %>%
  pivot_wider(names_from = direct_indirect,
              values_from = cost_aus) %>%
  #now we're goig to use the proportions we listed above to get the indirect costs
  #I don't love doing it this way because it's hard coded but can't think of a great
  #way around it
  mutate(indirect_cost = case_when(
    year == 2017 ~ direct_cost * indirect_proportions$proportion[1],
    year == 2018 ~ direct_cost * indirect_proportions$proportion[2],
    year == 2019 ~ direct_cost * indirect_proportions$proportion[3],
    year == 2020 ~ direct_cost * indirect_proportions$proportion[4],
    year == 2021 ~ direct_cost * indirect_proportions$proportion[5],
    year == 2022 ~ direct_cost * indirect_proportions$proportion[6],
    year == 2023 ~ direct_cost * indirect_proportions$proportion[7],
    year == 2024 ~ direct_cost * indirect_proportions$proportion[8],
    year >= 2025 ~ direct_cost * indirect_proportions$proportion[9])) %>%
  mutate(cost_type_1 = "indirect_cost") %>%
  select(-direct_cost) %>%
  rename(cost_aus = indirect_cost)


ev_forecast <- rbind(ev_forecast %>%
                       filter(!is.na(cost_aus)),
                     electric_indirect_costs)




# Incremental electric vehicle costs -------------------------------------------

#Up until this point, our costs have ben absolute costs for a base model vehicle.
#However, what we're eally interested in is the incremental cost difference between
#a base model conventional and EV vehicle.

ev_forecast <- ev_forecast %>%
  group_by(vehicle_type, vehicle_group, year) %>%
  summarise(cost_aus = sum(cost_aus))

ev_incremental <- ev_forecast %>%
  pivot_wider(names_from = vehicle_type,
              values_from = cost_aus) %>%
  mutate(incremental_cost = electric - conventional)




# Accounting for hard to reach vehicle segments --------------------------------

#Based on reviewer feedback, we are also going add the assumption that many LCV's are
#hard to reach sectors - that is, even with the cost curves generated, there will remain some
#loss of utility by switching to EV, or additional cost borne required to purchase an
#equivalent performance EV.

#To account for this, we will assume that LCV vehicles do not reach price parity within the
#model sales date. Instead, we assume that the price falls under the given trajectory until it reaches \$1,500
#shy of price parity. The incremental cost of LCV's are assumed to freeze at this value.

ev_incremental <- ev_incremental %>%
  mutate(incremental_cost = fifelse(
    vehicle_group == "lcv" & incremental_cost <= 1500,
    1500,
    incremental_cost))



# Saving data -----------------------------------------------------------------

write_rds(ev_incremental, "data-raw/model_data/temp/ev_cost_curve.rds")
write_rds(ev_forecast, "data-raw/model_data/temp/ev_forecast.rds")




#Plots -------------------------------------------------------------------------
ev_forecast %>%
  filter(vehicle_type == "electric") %>%
  ggplot(aes(x = year, y = cost_aus)) +
  geom_smooth(aes(colour = vehicle_group),
              se = FALSE) +
  scale_x_continuous_grattan(limits = c(2020, 2035)) +
  grattan_y_continuous(labels = dollar,
                       limits = c(0, 100000)) +
  grattan_colour_manual(3) +
  theme_grattan(legend = "top") +
  labs(title = "EV costs are expected to drop rapidly before 2035",
       subtitle = "Additional cost of purchasing an EV over a comparable ICE")




ev_incremental %>%
  ggplot(aes(x = year, y = incremental_cost)) +
  geom_smooth(aes(colour = vehicle_group),
              se = FALSE) +
  scale_x_continuous_grattan(limits = c(2020, 2035)) +
  grattan_y_continuous(labels = dollar,
                       limits = c(-10000, 40000)) +
  grattan_colour_manual(3) +
  theme_grattan(legend = "top") +
  labs(title = "EV costs reach price parity well before 2035 in all sectors",
       subtitle = "Additional cost of purchasing an EV over a comparable ICE")




















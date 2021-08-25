# 04-ev-battery-costs

#this script prepares the battery costs from us EPA estimates (and ICCT estimates)
#for each car type out to 2035. These are later integrated with the other ev cost
#data to create overall cost curves for ev's out to 2035.



# READ DATA  ===================================================================

#first reading in the data we preapred in the last script, `03-ev-costs-base-years`
#this data has conventional car costs out to 2035 (assumed constant), as well
#as ev costs (excluding battery costs) from 2017-2025
ad_ubs_costs <- read_rds("data/temp/adjusted_ubs_costs.rds") %>%

  mutate(lcv_cost = suv_cost * 1.2)


#we're also now going to read in the US epa battery data costs
#this data has the epa estimates for battery costs from 2017-2025 (in US $ 2015)
#it comes from https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100Q3L4.pdf , pages 478-487
#some explanatory notes are also contained here: https://www.mdpi.com/2032-6653/9/3/42
#

battery_costs <- read_xlsx("data/epa/ev-costs-epa.xlsx",
                           sheet = "batterycosts-ev") %>%
  clean_names()

#we're first going to filter the costs for total costs only - we're not
#interested in the breakdown of costs between direct/indirect at the moment and we factor in

battery_costs <- battery_costs %>%
  filter(cost_type == "TC") %>%
  #we're also going to ditch the last to columns they're unneeded
  select(-(15:16))

#we're now also going to make the assumption that for 75 mil and 100 ile ev's (bev-75
#and bev-100), that the weight reduction (WR) scenario is 10%. And for the bev-200
#the only option epa gives us is 20%, so we'll keep that.
#the differences between the weight reductions are quite marginal anyhow, this is
#somewhat conservative etimates (assuming higher price point estimates)

bev_200 <- battery_costs %>%
  subset(type == "bev-200")
battery_costs <- battery_costs %>%
  subset(type != "bev-200") %>%
  filter(wr_tech == 10)

battery_costs <- rbind(battery_costs, bev_200) %>%
  #and we're now going to drop our weight reduction categories and cost types
  #for simplicity
  select(-(3:5)) %>%
  #we're also now going to move everything into a long format to make it
  #easier to work with
  pivot_longer(cols = (3:11),
               names_to = "year",
               values_to = "base_cost") %>%
  #and now just fixing the formatting of the year names
  mutate(year = str_sub(year, start = 2)) %>%

  #and we're also going to convert all our costs to Aus $2021
  mutate(aus_base_cost = base_cost * 1.14 * 1.32) %>%
  select(-base_cost)


#for the moment we're going add values to the cur weight 6 class that the EPA doesn't provide.
#these will be adjusted later to take acocunt for the increased need for range of lcv's.

lcv_battery_costs <- battery_costs %>%
  subset(curb_weight_class == 5) %>%
  mutate(curb_weight_class = 6)

battery_costs <- rbind(battery_costs, lcv_battery_costs)



#WEIGHTING BATTERY COSTS BY VEHICLE TYPE----------------------------------------
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

vehicle_classes <- read_rds("data/temp/vehicle_classes.RDS")

#from this, we get the following:
    # 1 - small car       :     corresponds to EPA type 1 -         :    weight = 0.0793 (passenger)
    # 2 - standard car    :     corresponds to EPA type 2 -         :    weight = 0.188  (passenger)
    # 3 - large car       :     corresponds to EPA type 4 and 6 -   :    weight = 0.023  (passenger)
    # 4 - small MPV       :     corresponds to EPA type 7 -         :    weight = 0.206 (suv)
    # 5 - large MPV       :     corresponds to EPA type 8 -         :    weight = 0.153 (suv)
    # 6 - truck           :     corresponds to EPA type 11 and 13 - :    weight = 0.351 (lcv)


#adding these weights to the data (but through the vehicle class file not the
#hardcoded numbers above)
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



#before we get to extrapolating the data, the last thing we need to do is to
#decide how much of each type of bev is going to be acceptably sold
#we will assume that all BEV's sold are BEV-200

battery_costs <- battery_costs %>%
  mutate(type_weight = case_when(
    battery_type == "bev-200" ~ 1,
    battery_type == "bev-100" ~ 0,
    battery_type == "bev-75" ~ 0
  )) %>%
  group_by(vehicle_group, year) %>%
  summarise(aus_base_cost = weighted.mean(x = aus_base_cost, w = type_weight)) %>%
  mutate(year = as.integer(year))


#SCALING BATTERY COSTS FOR LARGER ASSUMED BATTERIES
#although the EPA estimates a re thoroughly researched and very good, the battery capacity
#they estimates for larger cars (suv's) is significantly lower than many other sources
#this is as hile they have a category for 200mile range bev's, they have no larger battery bev's
#this corresponds to a 47-67 kWh expected range for suvs (file:///Users/lffox/Downloads/wevj-09-00042-v2%20(1).pdf page 8)
#however, in practice, electric suv's are consistently being sold with battery capacities well above this.
#for example, the small suv hyundai kona has a capacity of 64 kWh (EPA would assume 47-54 kWh) and
#more expensive SUV's such as the telsa model X, jag I-PACE, Mercedes EQC have batteries of in 80-100 kWh
#given the high value consumers in the SUV market are likely to put on range, we are going to scale up the
#battery costs for suvs (the passenger ranges are far more reasonable with what's on the market) to capture the
#fact that people buying SUV's are likely to pay for the range to get a 'comparable' vehicle in their eyes to a conventional
#vehicle.

#although there are limited lcv electric vehicle's on the market at the moment, given the
#market that lcv's sell to, it is likely this preference will also exist there. As a result,
#the scaling factor we will use for suv's is to assume an 85 kWh range, as opposed to the EPA estimates of
#~62kWh. This gives (85/62) = 1.37
#we will apply the scaling lcv's, however up to 110kW instead of 85kW. The extra is
#to encompass the fact that they are towing vehicles, which will require more range.
#this factor is thereofore 110/62 = 1.77

#we'll probably also need to do the same thing for lcv's when we get to it

suv_adjusted_costs <- battery_costs %>%
  filter(vehicle_group ==  "suv") %>%
  mutate(aus_base_cost = aus_base_cost * 1.37)

lcv_adjusted_costs <- battery_costs %>%
  filter(vehicle_group ==  "lcv") %>%
  mutate(aus_base_cost = aus_base_cost * 1.77)

suv_lcv_adjusted_costs <- rbind(suv_adjusted_costs, lcv_adjusted_costs)

battery_costs <- rbind(suv_lcv_adjusted_costs,
                       battery_costs %>%
                         filter(vehicle_group == "passenger"))






#COMBINING BATTERY COSTS TO OTHER VEHICLE COST ESTIMATES ========================
#now that we have our weighted battery costs for each vehicle class over the 2017-2025
#period, we can use ICCT cost reduction estimates and our own estimate to extrapolate
#this data. Then, we can combine these forecasts with the other vehicle costs
#(frozen at 2025 cost estimates from electric vehicles and 2017 for conventional vehicles)

#we're going to make the two estimates seperately.
#first, trend for the EPA (UPPER BOUND) ESTIMATE
#-----------
#work out the improvement rate to extrapolate (we're assuming it continues as constant)

epa_trend <- battery_costs %>%
  group_by(vehicle_group) %>%
  mutate(max = max(aus_base_cost),
         min = min(aus_base_cost),
         annual_change_av = ((max-min)/(2025-2017))/max)

#from this we get our long term average battery price change. Given it's almost exactly the
#same for both vehicle classes, we will just use on rate for both. It gives us approx
#a 4% decrease in the price of batteries per year. This is a VERY conservative rate of
#improvement. It is ends up estimating price parity well beyond what most industry figures
#say (but it's an upper bound after all).

epa_trend <- (1 - mean(epa_trend$annual_change_av))

#now we can use this estimate to forecast the battery costs out to 2035
epa_forecast <- battery_costs %>%
  filter(year == 2025) %>%
  rename(epa_estimate = aus_base_cost) %>%
  pivot_wider(names_from = vehicle_group,
              values_from = epa_estimate) %>%
  complete(year = (2026:2050)) %>%
  arrange(year)


#FUNCTION TO FORECAST ----------------------------------------------
#creating a quick function to apply our trend to the data for the forward years

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
           pivot_longer(cols = (2:4),
                        names_to = "vehicle_group",
                        values_to = "estimate"))

}



#USING FORECASTING FUNCTION TO GET EPA ESIMATES ------------------------------


#using our function on the epa data and putting in back in a long format
epa_forecast <- forecast(.data = epa_forecast,
                         .rate = epa_trend) %>%
  rename(epa_estimate = estimate)

#now we're just going to bind this to the earlier years

epa_forecast <- rbind(epa_forecast %>%
                             filter(year != 2025) %>%
                             mutate(aus_base_cost = NA),
                           battery_costs %>%
                             mutate(epa_estimate = aus_base_cost)) %>%
  arrange(year) %>%
  select(-aus_base_cost)




#USING FORECAST FUNCTION TO GET ICCT ESTIMATES ---------------------------------

#because the ICCT forecasts are a simple 7% decrease in battery price each year,
#this is smpler to do. We are gong to take 2017 as the base year for this

icct_forecast <- battery_costs %>%
  filter(year == 2017) %>%
  #although we're going to 2050, everything beyond 2035 is not going to be used essentially
  complete(year = (2018:2050)) %>%
  pivot_wider(names_from = vehicle_group,
                values_from = aus_base_cost) %>%
  arrange(year)

icct_forecast <- forecast(.data = icct_forecast,
                          .rate = 0.93) %>%
  rename(icct_estimate = estimate)



#PUTTING THIS TOGETHER -------------------------------------------------------

battery_forecasts <- inner_join(epa_forecast, icct_forecast) %>%
  pivot_longer(cols = (3:4),
               names_to = "estimate",
               values_to = "cost_aus"
               )

#saving this file
write_rds(battery_forecasts, "data/temp/battery_forecasts.rds")




battery_forecasts %>%
  ggplot(aes(x = year, y = cost_aus, colour = estimate)) +
  #geom_point() +
  geom_smooth() +
  facet_wrap(~vehicle_group) +
  scale_y_continuous_grattan(limits = c(0, 30000))










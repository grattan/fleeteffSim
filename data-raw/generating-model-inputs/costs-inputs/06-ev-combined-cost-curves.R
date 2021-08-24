# 05-ev-combined-cost-curves

#this script draws on the previous 3 scripts in generating the final
#cost curves for electric vehicles out to 2050. The battery cost estimates
#are taken from script `04-ev-battery-cost` (which had been prepared through
#earlier scripts)

#the non-battery cost components are taken from script `03-ev-costs-base-years`
#and integrated with the battery components to give an incremental estimate
#of the cost of an ev by year and vehicle class out to 2035.

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
library(zoo)
library(scales)
# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)
# READ DATA  ===================================================================

#first we're going to load in our data on non-battery costs and make some
#minor changes to make everything consistent between the two datasets
ad_ubs_costs <- read_rds("data/temp/adjusted_ubs_costs.rds") %>%
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
battery_forecasts <- read_rds("data/temp/battery_forecasts.rds") %>%
  mutate(vehicle_type = "electric",
         cost_type_1 = "battery_cost")





# JOINING DATASETS  ============================================================

#first just duplicating the data that is the same for both epa and icct estimates
ad_ubs_costs <- rbind(ad_ubs_costs %>%
                        mutate(estimate = "icct_estimate"),
                      ad_ubs_costs %>%
                        mutate(estimate = "epa_estimate"))

ev_forecast <- rbind(battery_forecasts, ad_ubs_costs) %>%
  #and just reordering the data nicely
  select(vehicle_type, vehicle_group, cost_type_1, estimate, year, cost_aus)





#CREATING INDIRECT COSTS FOR EV'S ============================================
#sum of ev direct costs

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
  group_by(direct_indirect, year, estimate, vehicle_type, vehicle_group) %>%
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
    year >= 2025 ~ direct_cost * indirect_proportions$proportion[9]
  )) %>%
  #and now getting it back into a form where we can add it to the other data
  mutate(cost_type_1 = "indirect_cost") %>%
  select(-direct_cost) %>%
  rename(cost_aus = indirect_cost)


ev_forecast <- rbind(ev_forecast %>%
        filter(!is.na(cost_aus)),
      electric_indirect_costs)




# #getting incremental costs ====================================================

ev_forecast <- ev_forecast %>%
  group_by(vehicle_type, vehicle_group, year, estimate) %>%
  summarise(cost_aus = sum(cost_aus))



ev_incremental <- ev_forecast %>%
  pivot_wider(names_from = vehicle_type,
              values_from = cost_aus) %>%
  mutate(incremental_cost = electric - conventional)


write_rds(ev_incremental, "data/temp/ev_cost_curve.rds")
write_rds(ev_forecast, "data/temp/ev_forecast.rds")



#as it turns out our estimates are quite conservative really when it comes to purely price

ev_forecast %>%
  mutate(estimate = factor(estimate,
                           levels = c("icct_estimate", "epa_estimate"))) %>%
  filter(vehicle_type == "electric") %>%
  ggplot(aes(x = year, y = cost_aus)) +
  geom_smooth(aes(colour = vehicle_group, linetype = estimate),
              se = FALSE) +
  scale_x_continuous_grattan(limits = c(2020, 2035)) +
  grattan_y_continuous(labels = dollar,
                       limits = c(0, 100000)) +
  grattan_colour_manual(3) +
  theme_grattan(legend = "top") +
  labs(title = "EV costs are expected to drop rapidly before 2035",
       subtitle = "Additional cost of purchasing an EV over a comparable ICE")




ev_incremental %>%
  mutate(estimate = factor(estimate,
                           levels = c("icct_estimate", "epa_estimate"))) %>%
  ggplot(aes(x = year, y = incremental_cost)) +
  geom_smooth(aes(colour = vehicle_group, linetype = estimate),
              se = FALSE) +
  scale_x_continuous_grattan(limits = c(2020, 2035)) +
  grattan_y_continuous(labels = dollar,
                       limits = c(-10000, 40000)) +
  grattan_colour_manual(3) +
  theme_grattan(legend = "top") +
  labs(title = "EV costs reach price parity before 2035 in lower bound scenarios",
       subtitle = "Additional cost of purchasing an EV over a comparable ICE")













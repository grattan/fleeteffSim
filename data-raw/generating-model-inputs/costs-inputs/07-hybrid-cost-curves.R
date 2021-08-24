# 06-hybrid-cost-curves

#this script uses EPA data to generate cost curves for hybrids. 4 types of hybrids are
#included in the analysis - mild hybrids, stronghybrids, and PHEV-20 and PHEV-40 (Mile) hybrids.

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
library(readxl)
# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)
us_exchange <- 1.32
us_inflation_2015 <- 1.14


# READ DATA  ===================================================================

#this epa data is taken from pages 517-520 of the draft TAR
#https://nepis.epa.gov/Exe/ZyPDF.cgi/P100OXEO.PDF?Dockey=P100OXEO.PDF
hybrid_costs <- read_xlsx("data/epa/ev-costs-epa.xlsx",
          sheet = "totalcosts-hybrid") %>%
  clean_names()

#the effectiveness values are also taken from the draft TAR for mild and strong hybrids
#(pages 517-522). However, as no utility or effectiveness data is presented for PHEV-20/PHEV-40
#vehicles, the effectiveness is claculated from the utility data presented in
#https://theicct.org/sites/default/files/publications/CO2-reduction-technologies_white%20paper_10102017_vF.pdf
#pages 37-39

#this is based on European data, and thus the utility values may not perfectly capture the
#Australian setting (i.e. Australians may drive further on an avergae trip than Europeans,
#and thus the 'utility' (the proportion of trips where the battery motor is used vs the engine)
#may be overstated slightly). However, given that the following two sources:
#https://www.sdworx.com/en/press/2018/2018-09-20-more-than-20percent-of-europeans-commute-at-least-90-minutes-daily
#https://www.bitre.gov.au/sites/default/files/is_073.pdf
#inicate that the average commute in Aus/Eu are relatively similar, and given that this
#estimate is likely to be uncertain given pattersn of driving are likely to differ
#between people who buy a hybrid/conventional vehicle anyway, there's reason to believe
#it's a reasonable enough estimate. This is also not a crucial value, and it's unlikely
#that small changes have any meaningful effect.
hybrid_effectiveness <- read_xlsx("data/epa/ev-costs-epa.xlsx",
                                 sheet = "effectiveness-hybrid") %>%
  select((1:4)) %>%
  clean_names()



# COST DATA ============================================================

#first, let's sort out the cost data. We assume, as with EV's, that if there are
#multiple options for weight reduction technology (such as with strong hybrids), that
#the 10% weight reduction scenario will occur. This is a conservative estimate as it is the
#least cost effective solution. Options only exist for strong hybrids in this case.

hybrid_costs_strong <- hybrid_costs %>%
  filter(type == "strong-hybrid",
         w_rtech == 10)

hybrid_costs <- rbind(hybrid_costs %>%
                        filter(type != "strong-hybrid"),
                      hybrid_costs_strong)

#for phev-20, as the only options are weight reduction 15% or 20%, again we are going to
#conservatively estimate it is 15% in all cases.


phev_20_costs <- hybrid_costs %>%
  filter(type == "phev-20",
         w_rtech == 15)

hybrid_costs <- rbind(hybrid_costs %>%
                        filter(type != "phev-20"),
                      phev_20_costs)

#now we're going to clean the dataset and ge it into a long format

hybrid_costs <- hybrid_costs %>%
  select(-(3:5)) %>%
  pivot_longer(cols = (3:11),
               names_to = "year",
               values_to = "us_cost") %>%
  mutate(year = substr(year, start = 2, stop = 5)) %>%
  #and putting in Aus 2021 dollars
  mutate(cost_aus = us_cost * us_exchange * us_inflation_2015) %>%
  select(-us_cost)


#as we did with the ev battery costs, we now have to weight these figures based on sales
#and create estimates for the austrlia fleet broken up into three ategories (lcv/suv/passenger)

#estimates come from
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
hybrid_costs <- hybrid_costs %>%
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
  ))  %>%
  group_by(type, vehicle_group, year) %>%
  summarise(cost_aus =
              weighted.mean(x = cost_aus, w = weight))

#just to visualise and check it all makes sense
hybrid_costs %>%
  filter(vehicle_group == "passenger") %>%
  ggplot() +
  geom_point(aes(colour = type, x = year, y = cost_aus)) +
  scale_y_continuous_grattan(limits = c(0, 25000))



# EFFECTIVENESS  ===============================================================

#now that we have the cost data, we need to assign data on the effectiveness of
#each technology as well.
#although the EPA does provide some data on effectiveness for mild and strong hybrids in the
#draft TAR, unfortunately it is in a slightly different format than the cost data
#it is broken down by vehicle type (small car, standard car, small pv, large mpv, small truck,
#large truck instead of curb weights 1-6). However, it does match quite nicely with our
#definitions: we are going to assign the first 3 categories to passenger, the next 2 to
#suv, and the final 2 to lcv. This is consistent with the breakdown of curbb weights to general vehicle clases
#provided by the EPA.

phev_effectiveness <- hybrid_effectiveness %>%
  filter(technology %in% c("phev-20", "phev-40")) %>%
  rename(type = technology)

hybrid_effectiveness <- hybrid_effectiveness %>%
  #given the PHEV data does not depend on the class we'll filter this for the time being
  filter(technology %in% c("mild-hybrid","strong-hybrid")) %>%
  mutate(vehicle_group = case_when(
    curb_weight %in% c("small car", "standard car", "large car") ~ "passenger",
    curb_weight %in% c("small mpv", "large mpv") ~ "suv",
    curb_weight %in% c("small truck", "large truck") ~ "lcv"
  ),
  #and we also have to assign weights to each of these categories to we
  #can take the weighted average. Given all categories correspond to the curb weight
  #categories except for the trucks (whereas trucks is usually just 'trucks', here
  #it is broken down into small trucks and large trucks), we are just going to use
  #our normal weights from the vehicle classes file, and then take the average of the light
  #and heavy truck categories (which is likely to be conservative; almost certainly more light
  #trucks on the road than heavier ones)
  weight = case_when(
  curb_weight == "small car" ~ vehicle_classes$weight[1],
  curb_weight == "standard car" ~ vehicle_classes$weight[2],
  #note that the indexes DON'T correspond to the class - it's the row of the table
  #so row (index) 3 == type 4 and row (index) 4 == type 6 etc.
  curb_weight == "large car" ~ vehicle_classes$weight[3] + vehicle_classes$weight[4],
  curb_weight == "small mpv" ~ vehicle_classes$weight[5],
  curb_weight == "large mpv" ~ vehicle_classes$weight[6],
  curb_weight %in% c("small truck", "large truck") ~ (vehicle_classes$weight[7] + vehicle_classes$weight[8])/2
  )) %>%
  rename(type = technology) %>%
  group_by(type, vehicle_group) %>%
  summarise(effectiveness =
              weighted.mean(x = effectiveness, w = weight))



#now we want to combine this with our PHEV data as well

phev_effectiveness <- phev_effectiveness %>%
  select(-curb_weight, -utility_factor) %>%
  mutate(lcv = "lcv",
         suv = "suv",
         passenger = "passenger") %>%
  pivot_longer(cols = (3:5),
               names_to = "vehicle_group") %>%
  select(-value)

hybrid_effectiveness <- rbind(hybrid_effectiveness, phev_effectiveness)

#COMBINING DATA  ==============================================================
#now we've got all our data consistent, we can combine it to get our incremental
#costs and effectiveness

hybrid_curves <- inner_join(hybrid_costs, hybrid_effectiveness) %>%
                              mutate(year = as.integer(year))

hybrid_curves <- hybrid_curves %>%
  mutate(year = as.integer(year)) %>%
  #and now we want to fill in the year from 2025-2035, conservatively assuming that
  #the costs freeze on all types/hybrids.
  complete(year = (2026:2035)) %>%
  arrange(type, vehicle_group, year) %>%
  na.locf()

write_rds(hybrid_curves, "data/temp/hybrid_cost_curve.rds")












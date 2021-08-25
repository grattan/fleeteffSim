
# 02-km_travelled
# by Lachlan Fox, Grattan Institute

#This script produces data on the assumed distanced travelled by vehicle type and age

# Setup ------------------------------------------------------------------------

source("data-raw/model_data/00-setup.R")

# KM travelled -----------------------------------------------------------------

#fleet/vehicle use characteristics and forecasts
#this data comes from the motor vehicle survey (tables 1 and 16)
#(https://www.abs.gov.au/statistics/industry/tourism-and-transport/survey-motor-vehicle-use-australia/latest-release)
#table 9 comes from the motor vehicle census:
#(https://www.abs.gov.au/statistics/industry/tourism-and-transport/motor-vehicle-census-australia/latest-release)

km_travelled_type <- read_xlsx("data-raw/external_data/abs/motor-vehicle-use-survey.xlsx",
          sheet = "table_1_input") %>%
  clean_names()

km_travelled_age <- read_xlsx("data-raw/external_data/abs/motor-vehicle-use-survey.xlsx",
                               sheet = "table_16_input") %>%
  clean_names()

no_vehicle_age <- read_xlsx("data-raw/external_data/abs/motor-vehicle-use-survey.xlsx",
                               sheet = "table_9_input") %>%
  clean_names()


#in order to work out a rough relationship between the km travelled and the age of a vehicle,
#we're going to collate the km_travelled_age and no_vehicle_age tables together

#although the categories (years) don't match perfecty, we're going to assume that the
#differences of one year on either side are negligible and equate them to each other into three
#brackets (pre-2005, 2006-2015 and post-2016 vehicles.)


km_travelled_age <- km_travelled_age %>%
  mutate(age = case_when(
    #we're just taking the midpoint of each category. For the before 2004 category,
    #we're assuming the midpoint is approximately ~20 years old
    vehicle_age %in% c("2004 and earlier") ~ 20,
    vehicle_age %in% c("2005 to 2014")  ~ 11.5,
    vehicle_age %in% c("2015 and after") ~ 3.5,
    vehicle_age %in% c("2005 and earlier") ~ 19,
    vehicle_age %in% c("2006 to 2014")  ~ 11,
    vehicle_age %in% c("2016 and after") ~ 2.5)) %>%
  #mutate(kilometres_travelled_thousands = kilometres_travelled_thousands * 1000) %>%
  mutate(km_travelled = kilometres_travelled_thousands * 1000) %>%
  select(-kilometres_travelled_thousands)


#plotting -----------------------------------------------------------------
km_travelled_age %>%
  ggplot(aes(x = age, y = km_travelled)) +
  geom_point(aes(colour = vehicle_type)) +
  geom_smooth(aes(colour = vehicle_type), method = "lm",
              se = FALSE) +
  scale_y_continuous_grattan(limits = c(0, 30000)) +
  theme_grattan(legend = "top") +
  grattan_colour_manual(2) +
  labs(title = "Vehicles travel less as they get older",
       subtitle = "Vehicle age vs average yearly km travelled")



#and getting the equation for passenger vehicles
passenger_regression <- lm(data = km_travelled_age %>%
             filter(vehicle_type == "passenger"),
           km_travelled~age)

#this gives us our equation for passenger vehicles:
# ( average yearly distance travelled (km) = 13617.38 - 250.1 * (age) )
# R^2 = 0.8851

#and for light commercial vehicles
lcv_regression <- lm(data = km_travelled_age %>%
                             filter(vehicle_type == "light_commercial"),
                           km_travelled~age)

#which gives the equation
# ( average yearly distance travelled (km) = 23744.7 - 788.7 * (age) )
# R ^2 = 0.9961

#when comparing to our overall average vehicle km's travelled by vehicle type data
    #km_travelled_type %>% view()
#these values also align nicely with the overall numbers. The trends themselves are
#also expected - light commercial vehicle get driven more, but have a steeper fall over time



#from all this information we're going to create a tibble that encapsulates driving behaviour by
#the age of vehicles. This way when we run the model, depending on the age of the cars in
#the model we can determine how far they are likely to drive in a given year.

#we will assume the average life of a car is 17 years, in line with modelling
#so beyond 17 years we will truncate the km travelled to 0

km_travelled <- tibble() %>%
  mutate(age = 0) %>%
  complete(age = (0:50)) %>%
  #passenger vehicles using the relationship:
  # (average yearly distance travelled (km) = 13617.4 - 250.1 * (age) )
  mutate(passenger = 13617.4 - 250.1 * age) %>%
  mutate(passenger = case_when(
    age > 17 ~ 0,
    age <= 17 ~ passenger)) %>%
  #now doing same thing with light commercial vehicles, relationship is:
  # (average yearly distance travelled (km) = 23744.7 - 788.7 * (age) )
  mutate(lcv = 23744.7 - 788.7 * age) %>%
  mutate(lcv = case_when(
    age > 17 ~ 0,
    age <= 17 ~ lcv)) %>%
  #were also going to lump passenger nad SUV into the same category. Although there are probably differences
  #between them we have no data to suggest what this might be
  mutate(suv = passenger) %>%
  pivot_longer(cols = (2:4),
               names_to = "vehicle_type",
               values_to = "km_travelled")


#now we want to save this data
write_rds(km_travelled, "data-raw/model_data/final-data/km_traveled.rds")




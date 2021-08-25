# 01-cost-curve-creation
# by Lachlan Fox, Grattan Institute

#This script creates cost curves for the categories (passenger,
#suv, and lcv) that were created in the 01-vehicle-classification script.

#It does this by averaging the cost curves (and caps) between the different EPA classes
#contained within each broad class, based on weights that represent the characteristics
#of the Australian fleet.

# Setup ------------------------------------------------------------------------

source("data-raw/generating-model-inputs/00-setup.R")

# Read data: Vehicle classes ---------------------------------------------------

#data on the australia fleet vehicle classes and breakdown into epa
vehicle_classes <- read_xlsx("data-raw/collected/ntc-vehicle-collected-type.xlsx",
                             sheet = "epa_type_breakdown") %>%
  rename(vech_type_no = epa_type,
         vehicle_group = type) %>%
  filter(vech_type_no != "total") %>%
  mutate(vech_type_no = as.double(vech_type_no))

write_rds(vehicle_classes, "data-raw/temp/vehicle_classes.rds")


#Read data: EPA cost curves ----------------------------------------------------

#EPA cost curve data for both 2021 and 2025. This data was obtained from:
#https://www.epa.gov/regulations-emissions-vehicles-and-engines/optimization-model-reducing-emissions-greenhouse-gases
#Data is contained within the downloadable files with the 2016 draft TAR run of the OMEGA
#model for both 2021 and 2025, for all 29 sepcified vehicle types.
#files accompyaning model version OMEGA v1.4.56 were used in this analysis.

#for a broad overview of the OMEGA model and technology packages developed for the modelling approach
#see https://theicct.org/sites/default/files/publications/Camry_OMEGA_WorkingPaper_20180227.pdf,
#and https://theicct.org/sites/default/files/publications/Canada_CAFE_2%20Methods_20180912.pdf

#for further detailed documentation of vehicle types and technology packages used in the OMEGA model, see
#the technical support document to the 2016 draft TAR:
#https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100Q3L4.pdf

packages_2021 <- read_xls("data-raw/epa/2021-technology-packages.xls") %>%
  select(1:8, -4) %>%
  clean_names() %>%
  mutate(year = "2021") %>%
  #removing all ev data from the curves. This will be dealt with seperately
  filter(primary_fuel == "G")


packages_2025 <- read_xls("data-raw/epa/2025-technology-packages.xls") %>%
  select(1:8, -4) %>%
  clean_names() %>%
  mutate(year = "2025") %>%
  #removing all ev data from the curves. This will be dealt with seperately
  filter(primary_fuel == "G")



# Cost curve function ----------------------------------------------------------

#this function creates simplified cost curves for three vehicle types (passenger,
#suv, lcv), by aggregating the EPA cost curve datasets.

#to aggregate the data, weighted averages are taken between the 19 relevant vehicle categories
#determined by the US EPA. These categories were mapped to the Austraian vehicle fleet
#and the assumed proportions of each is available in `vehicle_classes`. Averages
#are weighted by sales volumes.

#However, because we are averaging across packages now they no longer represent
#specific technologies - simply different curves for efficiency improvements and associated costs.

cost_curves <- function(.packages_1 = packages_2021,
                        .packages_2 = packages_2025,
                        .vehicle_type = (1:19),
                        .vehicle_classes = vehicle_classes,
                        #this `simplified` argument just strips some of the
                        #columns we don't need any more from the output - if all columns are wanted
                        #change it to false
                        .simplified = TRUE) {


  #first for the first set of packages
  .packages_1 <- .packages_1 %>%
    filter(vech_type_no %in% .vehicle_type) %>%
    mutate(vehicle_group = case_when(
          vech_type_no %in% c(1, 2, 4, 6) ~ "passenger",
          vech_type_no %in% c(7, 8) ~ "suv",
          vech_type_no %in% c(11, 13) ~ "lcv",
          vech_type_no %in% c(3, 5, 9, 10, 12, 14, 15, 16, 17, 18, 19) ~ "unused"))  #,
         # aie = aie * cap,
         # incr_cost = incr_cost * aie)

   .cost_curves_1 <- .packages_1 %>%
     #just adjusting the costs first for inflation (1.12) and converting to
     #aus dollars (1.34)
      mutate(incr_cost = incr_cost * 1.12 * 1.34)


   #and the second
   .packages_2 <- .packages_2 %>%
     filter(vech_type_no %in% (1:19)) %>%
     filter(vech_type_no %in% .vehicle_type) %>%
     mutate(vehicle_group = case_when(
       vech_type_no %in% c(1,2,4,6) ~ "passenger",
       vech_type_no %in% c(7,8) ~ "suv",
       vech_type_no %in% c(11,13) ~ "lcv",
       vech_type_no %in% c(3,5,9,10,12,14,15,16,17,18,19) ~ "unused"))#,
     #  aie = aie * cap,
    #   incr_cost = incr_cost * aie)

   .cost_curves_2 <- .packages_2 %>%
     #just adjusting the costs first for inflation (1.12) and converting to
     #aus dollars (1.34)
     mutate(incr_cost = incr_cost * 1.12 * 1.34)


   .cost_curves <- bind_rows(.cost_curves_1, .cost_curves_2)


   #adding weights from vehicle classes to data, and dropping unneeded cols
   .cost_curves <- left_join(.cost_curves, .vehicle_classes)

   if (.simplified == TRUE) {
     .cost_curves <- .cost_curves %>%
       select(vech_type_no, tech_pkg_no, cap, aie, incr_cost, year,
              vehicle_group, weight)
   }


   .cost_curves <- .cost_curves %>%
     filter(vehicle_group != "unused") %>%
     group_by(vehicle_group, tech_pkg_no, year) %>%
     summarise(aie =  weighted.mean(x = aie, w = weight),
               incr_cost =  weighted.mean(x = incr_cost, w = weight),
               cap = weighted.mean(x = cap, w = weight)) %>%
     ungroup() %>%

   #using the `cap` to adjust the costs and improvements that can begained from
   #each technology
     mutate(aie = aie * cap,
            incr_cost = incr_cost * cap) %>%
     select(-cap)


   #adding the point 0

   initial <- .vehicle_classes %>%
     select(vehicle_group) %>%
     group_by(vehicle_group) %>%
     mutate(incr_cost = 0,
            tech_pkg_no = 0,
            year21 = 2021,
            year25 = 2025,
            aie = 0) %>%
     pivot_longer(c("year21", "year25"),
                  values_to = "year") %>%
     select(-name) %>%
     mutate(year = as.character(year))

   .cost_curves <- rbind(.cost_curves, initial) %>%
     rename("incr_reduction" = aie)

  return(.cost_curves)

}


#running the function
base_year_cc <- cost_curves() %>%
  unique()



#interpolating between base years ----------------------------------------------

#the previous code established our ICE cost curves for the 2 'base' years; 2021 and 2025.

#The following code extrapolates these costs to include years 2021 to 2025. Data between 2021 and 2025
#is linearly interpolated. Data beyond 2025 is assumed as frozen at 2025 values.
#This is a conservative estimate that assumes manufacturers do not prioritise advancing ICE
#technology beyond 2025.

#Interpolating between 2021 and 2025

#firstly for the incremental costs
ice_costs <- base_year_cc %>%
  select(-incr_reduction) %>%
  pivot_wider(names_from = year,
              values_from = incr_cost) %>%
  mutate(`2022` = `2021` + (`2025`-`2021`)/4 * 1,
         `2023` = `2021` + (`2025`-`2021`)/4 * 2,
         `2024` = `2021` + (`2025`-`2021`)/4 * 3) %>%
  pivot_longer((3:7),
               names_to = "year",
               values_to = "incr_cost")

#and the incremental efficiency improvements
ice_incr_reduction <- base_year_cc %>%
  select(-incr_cost) %>%
  pivot_wider(names_from = year,
              values_from = incr_reduction) %>%
  mutate(`2022` = `2021` + (`2025`-`2021`)/4 * 1,
         `2023` = `2021` + (`2025`-`2021`)/4 * 2,
         `2024` = `2021` + (`2025`-`2021`)/4 * 3) %>%
  pivot_longer((3:7),
               names_to = "year",
               values_to = "incr_reduction")

#combining datasets and freezing values to 2050
ice_cost_curves <- inner_join(ice_costs, ice_incr_reduction) %>%
  mutate(year = as.integer(year)) %>%
  group_by(vehicle_group, tech_pkg_no) %>%
  complete(year = (2026:2050)) %>%
  arrange(vehicle_group, tech_pkg_no, year) %>%
  mutate(incr_cost = case_when(
    tech_pkg_no == 0 ~ 0,
    tech_pkg_no != 0 ~ incr_cost)) %>%
  na.locf()




#Saving completed ICE cost curves  ---------------------------------------------

write_rds(ice_cost_curves, "data-raw/temp/ice_cost_curves.rds")




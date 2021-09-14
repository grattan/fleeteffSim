#05-grid-intensity-forecast
# by Lachlan Fox, Grattan Institute

#This script produces estimates of the electricity consumption of electric vehicles by type
#and year for the fleetEffSim model

# Setup ------------------------------------------------------------------------

source("data-raw/model_data/00-setup.R")

# Read data: AEMO ----------------------------------------------------------------

#We are going to assume the 'step change' AEMO scenario is used. This has also been linearly
#interpolated to reach assumed net 0 by 2050 (calculations in spreadsheet)

energy_intensity <- read_xlsx("data-raw/external_data/AEMO/emissions-intensity-grid.xlsx",
                              sheet = "step_change") %>%
  clean_names() %>%
  select(-old) %>%
  complete(year = (2051:2060)) %>%
  arrange(year) %>%
  na.locf()


write_rds(energy_intensity, "data-raw/model_data/final-data/energy_intensity.rds")

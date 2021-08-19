
#dummy data for tests

# Data for testing upgrade_selector -----------------------

test_cc <- read_rds("data-raw/cost-curves/cost_curves.rds")
test_fleet <- read_rds("data-raw/model-inputs/projected_fleet_in-10.rds")

test_cc <- test_cc %>%
  #pick a somewhat random year
  filter(year == 2023,
         estimate == "icct_estimate")

test_fleet <- test_fleet %>%
  #again choosing 2023
  filter(year == 2023)

#now we're going to make some random changes to our test fleet dummy data,
#so that we have a situation we want to test

#making some vehicles electric (one passenger and an lcv)
test_fleet$electric_applied[1] <- TRUE
test_fleet$current_emissions[1] <- 0
test_fleet$tech_pkg_applied[1] <- 100
test_fleet$electric_applied[8] <- TRUE
test_fleet$current_emissions[8] <- 0
test_fleet$tech_pkg_applied[8] <- 100
test_fleet$electric_applied[5] <- TRUE
test_fleet$current_emissions[5] <- 0
test_fleet$tech_pkg_applied[5] <- 100

#making some vehicles have some tech package upgrades (we're just using made
#up values for how much the tech package reduces emissions, and the costs don't matter
#for this)

test_fleet$tech_pkg_applied[2] <- 4
test_fleet$current_emissions[2] <- 80
test_fleet$tech_pkg_applied[4] <- 6
test_fleet$current_emissions[4] <- 40
test_fleet$tech_pkg_applied[6] <- 4
test_fleet$current_emissions[6] <- 80
test_fleet$tech_pkg_applied[7] <- 4
test_fleet$current_emissions[7] <- 80
test_fleet$tech_pkg_applied[9] <- 5
test_fleet$current_emissions[9] <- 80


#what we're left with is a scenario where it is clear that the upgrade is going to be
#applied to the passenger vehicle with a tech pkg of 0. The test will ensure that is the
#case

usethis::use_data(test_cc, test_fleet, internal = TRUE)








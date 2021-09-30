
# Set to TRUE if you want to replace the reference objects ---------------------
rewrite_compare_objects <- FALSE


# Generate objects to test -----------------------------------------------------
new_default_fleet <- fleet_creator()

if (rewrite_compare_objects) {
  readr::write_rds(new_default_fleet, "tests/testthat/data/fleet-default-original.rds")
}


small_fleet <- fleet_creator(.i_cars = 50)
big_fleet <- fleet_creator(.i_cars = 10000)

co2_fleet <- fleet_creator(.passenger_co2 = 1, .suv_co2 = 2, .lcv_co2 = 3)

growth_fleet <- fleet_creator(.i_cars = 100, .i_passenger_share = .70, .i_suv_share = .20, .i_lcv_share = 0.10,
                              .passenger_growth = 1.1, .suv_growth = 1.2, .lcv_growth = 1.3) %>%
  count(year, vehicle_group)

plus_minus_one <- function(x, y) {
  between(y, x - 1, x + 1)
}

# Run tests --------------------------------------------------------------------

test_that("Fleet creation is as expected", {

  expect_identical(new_default_fleet,
                   readr::read_rds("data/fleet-default-original.rds"))

  expect_true(between(max(small_fleet$id[small_fleet$year == 2021]), 49, 51))
  expect_true(between(max(big_fleet$id[big_fleet$year == 2021]), 9999, 10001))

  expect_true(all(co2_fleet$base_emissions[co2_fleet$vehicle_group == "passenger"] == 1))
  expect_true(all(co2_fleet$base_emissions[co2_fleet$vehicle_group == "suv"] == 2))
  expect_true(all(co2_fleet$base_emissions[co2_fleet$vehicle_group == "lcv"] == 3))

  expect_true(plus_minus_one(max(growth_fleet$n[growth_fleet$vehicle_group == "passenger"]),
                             (0.7 * 100) * 1.1^(2050 - 2021)))

  expect_true(plus_minus_one(max(growth_fleet$n[growth_fleet$vehicle_group == "suv"]),
                             (0.2 * 100) * 1.2^(2050 - 2021)))

  expect_true(plus_minus_one(max(growth_fleet$n[growth_fleet$vehicle_group == "lcv"]),
                             (0.1 * 100) * 1.3^(2050 - 2021)))

})


test_that("Fleet creation error messages are clear", {
  expect_error(fleet_creator(.i_passenger_share = 0.5, .i_suv_share = 0.5, .i_lcv_share = .5),
               regexp = "must sum")

  expect_error(fleet_creator(.i_passenger_share = 0.1, .i_suv_share = 0.1, .i_lcv_share = .1),
               regexp = "must sum")
})

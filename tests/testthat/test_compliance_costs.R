


#importing dummy data for tests, generated in the R script "test_data_upgrade_selector.R"

test_that("Compliance cost function doesn't return an error", {

  expect_message(compliance_costs(.run_to_year = 2023,
                                  .fleet = fleet_creator(),
                                  .target_scenario = "target_central"))

                 })






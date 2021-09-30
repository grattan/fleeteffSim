
# Set to TRUE if you want to replace the reference objects ---------------------
rewrite_compare_objects <- FALSE


# Generate objects to test -----------------------------------------------------
comp_default <- compliance_costs(
  .run_to_year = 2027,
  .fleet = fleet_creator(),
  .target_scenario = "target_central")


if (rewrite_compare_objects) {
  readr::write_rds(comp_default, "tests/testthat/data/compliance-default-original.rds")
}

comp_long <- compliance_costs(
  .penalty_begin = 2024,
  .run_to_year = 2050,
  .fleet = fleet_creator(.i_cars = 10),
  .target_scenario = "target_central")


# Run tests  -------------------------------------------------------------------
test_that("Compliance cost outputs have not changed", {
  expect_identical(comp_default,
                   readr::read_rds("data/compliance-default-original.rds"))

})


test_that("Targets are met", {

  run_year_emissions <- function(t) {
    comp_central <- compliance_costs(
      .penalty_begin = 2024,
      .run_to_year = 2040,
      .fleet = fleet_creator(.i_cars = 10),
      .target_scenario = t) %>%
      group_by(year) %>%
      summarise(emissions = mean(current_emissions)) %>%
      mutate(target_type = t)
  }

  targets <- c("target_central", "target_ambitious", "bau")

  target_runs <- purrr::map_dfr(targets,
                                run_year_emissions) %>%
    left_join(targets_and_bau %>%
                rename(target = value), by = c("year", "target_type")) %>%
    mutate(is_less = emissions <= target)

  expect_identical(target_runs$is_less, rep(TRUE, nrow(target_runs)))

})

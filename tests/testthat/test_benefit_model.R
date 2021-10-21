print(getwd())

# The things we want to test in the benefit model are:
  # the proportions of vehicles by fuel type are right
  # that the km driven by each cars lines up properly
  # that the prices line up properly based on the year
  # that the energy intensity of EV's matches up based on the vehicle type and year
  # that the rebound effect works
  # that the calculations all work properly based on the values set (for prices and emissions)

# First running the functions to get a dataset to work with

compliance <- compliance_costs(.target_scenario = "target_central",
                               .bau_scenario = "bau",
                               .fleet = fleet_creator(),
                               .penalty_begin = 2025,
                               .run_to_year = 2027)

benefit <- benefit_model(.fleet = compliance) %>%
  ungroup()

#these are default assumptions that we will also reference
gap <- 1.2
passenger_diesel <- 0.2
suv_diesel <- 0.21
lcv_diesel <- 0.92
premium_95 <- 0.15
premium_98 <- 0.05


# Now testing the `benefit` file for the things we described above

    fuel_proportions <- benefit %>%
      filter(vehicle_group == "passenger",
             fuel_type != "electric",
             vehicle_age == 0) %>%
      mutate(count = 1) %>%
      group_by(fuel_type) %>%
      summarise(proportion = sum(count)) %>%
      ungroup() %>%
      mutate(proportion = proportion / sum(proportion))

    #checking petrol vehicles first
      proportions_actual <- fuel_proportions %>%
        filter(fuel_type %in% c("petrol_98", "petrol_95"))

      proportions_expected <- tribble(
        ~fuel_type, ~proportion_actual,
        "petrol_95", premium_95,
        "petrol_98", premium_98)

      #determining if all are with 5%
      petrol_proportions <- inner_join(proportions_actual,
                                       proportions_expected) %>%
        mutate(test = between(proportion_actual,
                              proportion - 0.07,
                              proportion + 0.07))

   test_that("Premium petrol proportions are within accurate range", {

      #testing that all are true
      expect_true(
        petrol_proportions %>%
          select(test) %>%
          as_vector() %>%
          all())
})





# That the km drive match up based on vehicle type and
test_that("Vehicle km driven properly assigned", {

  for (i in 1:5) {
    #I know this should probably be done with map but i'll learn it a bit later
    for (j in c("passenger", "lcv", "suv")) {

      #randomnly sampling over ages 1 - 18
      age <- sample(1:18, 1)

  expect_equal(
      object = benefit %>%
        #first we need to reverse engineer to get rid of the rebound factor,
        #so that we can compare with the base dataset
        select(km_driven, rebound_factor, vehicle_age,
               vehicle_group) %>%
        mutate(km_travelled = (km_driven / rebound_factor) * 100) %>%
        select(vehicle_age, vehicle_group, km_travelled) %>%
        filter(vehicle_age == age, vehicle_group == j) %>%
        #to make to sure there's no duplicates - they should all be the same anyhow
        mutate(km_travelled = round(km_travelled, digits = 0)) %>%
        head(1) %>%
        pull(km_travelled),

      expected = km_travelled %>%
        rename("vehicle_age" = age) %>%
        filter(vehicle_age == age, vehicle_type == j) %>%
        pull(km_travelled) %>%
        round(digits = 0))
      }
    }
})



#Now testing that the prices are working properly
test_that("Price data is correct in the final dataset", {

  for (i in 1:5) {

    test_year <- sample(2025:2050, 1)

    #I know this should probably be done with map but I'll learn it a bit later
    for (j in c("91", "98", "95")) {

      expect_equal(

        object = benefit %>%
          select(year,
                 paste("price", j, sep = "_")) %>%
          filter(year == test_year) %>%
          unique() %>%
          pull(paste("price", j, sep = "_")) %>%
          round(digits = 2),


        expected = fuel_prices_tax %>%
          filter(scenario == "central",
                 fuel_type == paste("petrol", j, sep = "_"),
                 year == test_year) %>%
            pull(price) %>%
          round(digits = 2))
    }
  }
})



#Now checking vehicle energy consumption and energy prices
test_that("Energy consumption and price data is accurate", {

  energy <- inner_join(energy_consumption, electricity_prices) %>%
    filter(scenario == "central") %>%
    mutate(test_value = energy_consumption * energy_price)

  benefit_energy <- benefit %>%
    select(year, energy_price, energy_consumption, vehicle_group) %>%
    unique() %>%
    mutate(test_value = energy_consumption * energy_price)

  for (i in 1:5) {

    test_year <- sample(2025:2050, 1)

    #I know this should probably be done with map but I'll learn it a bit later
    for (j in c("passenger", "lcv", "suv")) {

      expect_equal(
        object = benefit_energy %>%
          filter(year == test_year,
                 vehicle_group == j) %>%
          pull(test_value) %>%
          round(digits = 3),

        expected = energy %>%
          filter(year == test_year,
                 vehicle_type == j) %>%
          pull(test_value) %>%
          round(digits = 3))

        }
    }
})



#Checking that the calculations are working properly for fuel
test_that("Overall running costs calculated correctly", {

  #Checking the fuel consumption totals are accurate
  expect_identical(
    object = benefit %>%
      filter(fuel_type != "electric", vehicle_age <= 17) %>%
      mutate(test_consumption = case_when(
        fuel_type %in% c("petrol_91", "petrol_95", "petrol_98") ~ petrol_co2_to_fuel(current_emissions * gap) * km_driven / 100,
        fuel_type == "diesel" ~ diesel_co2_to_fuel(current_emissions * gap) * km_driven / 100),
        test_consumption = round(test_consumption, digits = 0)) %>%
      select(year, test_consumption),

    expected = benefit %>%
      filter(fuel_type != "electric", vehicle_age <= 17) %>%
      select(year, fuel_consumption) %>%
      mutate(fuel_consumption = round(fuel_consumption, digits = 0)) %>%
      rename("test_consumption" = fuel_consumption))


  # Checking the fuel price is correct
  expect_identical(
  object = benefit %>%
    mutate(test_cost = case_when(
      fuel_type == "electric" ~ energy_price * energy_consumption * km_driven,
      fuel_type == "petrol_91" ~ price_91 * fuel_consumption,
      fuel_type == "petrol_95" ~ price_95 * fuel_consumption,
      fuel_type == "petrol_98" ~ price_98 * fuel_consumption,
      fuel_type == "diesel" ~ price_diesel * fuel_consumption)) %>%
    select(year, test_cost),

  expected = benefit %>%
    rename("test_cost" = fuel_cost) %>%
    select(year, test_cost))
})










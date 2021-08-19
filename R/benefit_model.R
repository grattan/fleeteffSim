
#' Title
#'
#' @param .fleet The output of 'compliance_cost' function, with a compliant simulated fleet
#' @param .km_travelled The assumed km travelled for each vehicle segment and vehicle age
#' @param .fuel_prices Assumed future fuel prices (excluding taxes)
#' @param .electricity_prices Assumed future electricity prices
#' @param .energy_consumption Assumed energy consumption of electric vehicles
#' @param .energy_intensity Assumed energy intensity of the electricity grid
#' @param .gap The assumed 'gap' between test cycle and real world emissions for combustion vehicles
#' @param .fuel_scenario The assumed fuel price scenario
#' @param .electricity_scenario Assumed electricity price scenario
#' @param .run_to_year Year the model will run to
#' @param .passenger_diesel Proportion of disel passenger vehicles
#' @param .suv_diesel Proportion of suv diesel vehicles
#' @param .lcv_diesel Proportion of LCV diesel vehicles
#' @param .premium_95 Proportion of vehicles requiring 95oct fuel
#' @param .premium_98 Proportion of vehicles requiring 98oct fuel
#'
#' @return A \code{tibble} including the fuel, emissions, costs for each vehicle in the simulated fleet
#' over it' lifespan
#' @export
#'
#' @examples
#'
#'
benefit_model <- function(.fleet = compliant_fleet,
                          .km_travelled = km_travelled,
                          .fuel_prices = fuel_prices,
                          .electricity_prices = electricity_prices,
                          .energy_consumption = energy_consumption,
                          .energy_intensity = energy_intensity,
                          .gap = 1.2,
                          .fuel_scenario = "central",
                          .electricity_scenario = "central",
                          .run_to_year = 2060,
                          #and the diesel shares in each category
                          .passenger_diesel = 0.02,
                          .suv_diesel = 0.21,
                          .lcv_diesel = 0.92,
                          #the racv says about 20% of cars run on premium fuels. We are assuming that this is
                          #15% 95oct and 5% 98, but there is not hard data on this
                          .premium_95 = 0.15,
                          .premium_98 = 0.05) {



  #now we have our fleet, we're first going to also characterise it by fuel type (diesel/petrol/premium)
  #this is currently quite slow and inefficient, but it works it attributing the right
  #portiong of each year's vehicle with either diesel or premium fuels, characterised by
  #the vehicle group

  .fleet <- .fleet %>%
    mutate(fuel_type = "petrol_91") %>%
    mutate(diesel_share = case_when(
      vehicle_group == "passenger" ~ .passenger_diesel,
      vehicle_group == "suv" ~ .suv_diesel,
      vehicle_group == "lcv" ~ .lcv_diesel))

  #we're going to seperate out the 'ICE fleet' for the purposes of petrol types
  ice_fleet <- .fleet %>%
    filter(electric_applied == FALSE)
  n_ice_fleet <- .fleet %>%
    filter(electric_applied == TRUE)

  #now assigning a proportion of the fleet to each type
  j <- 1
  while (j <= nrow(ice_fleet)) {

    this_year <- ice_fleet$year[j]
    this_vehicle_type <- ice_fleet$vehicle_group[j]
    this_diesel_share <- ice_fleet$diesel_share[j]


    #first checking if we need to add more diesel for this segment/year

    if (
      #all the diesel cars of this year/type
      ice_fleet %>%
      filter(year == this_year,
             vehicle_group == this_vehicle_type,
             fuel_type == "diesel") %>%
      nrow() /
      #div by

      #all the vehicle of this year type
      ice_fleet %>%
      filter(year == this_year,
             vehicle_group == this_vehicle_type) %>%
      nrow()
      #if the proportion isn't up to where we want, we change to diesel
      <= this_diesel_share ) {

      ice_fleet$fuel_type[j] <- "diesel"
      j <- j + 1

    } else if (

      #and if this isn't the case we are going to check for premium fuels as well
      #first for 95
      #all the premium 95 fuel cars of this year/type
      ice_fleet %>%
      filter(year == this_year,
             vehicle_group == this_vehicle_type,
             fuel_type == "petrol_95") %>%
      nrow() /
      #div by

      #all the vehicle of this year type
      ice_fleet %>%
      filter(year == this_year,
             vehicle_group == this_vehicle_type) %>%
      nrow()
      #if the proportion isn't up to where we want, we change to diesel
      <= .premium_95 ) {

      ice_fleet$fuel_type[j] <- "petrol_95"
      j <- j + 1

    } else if (

      #now for 98
      #all the premium 98 fuel cars of this year/type
      ice_fleet %>%
      filter(year == this_year,
             vehicle_group == this_vehicle_type,
             fuel_type == "petrol_98") %>%
      nrow() /
      #div by

      #all the vehicle of this year type
      ice_fleet %>%
      filter(year == this_year,
             vehicle_group == this_vehicle_type) %>%
      nrow()
      #if the proportion isn't up to where we want, we change to diesel
      <= .premium_98 ) {

      ice_fleet$fuel_type[j] <- "petrol_98"
      j <- j + 1

    } else {

      j <- j + 1

    }

  }

  .fleet <- bind_rows(ice_fleet, n_ice_fleet) %>%
    arrange(year, id) %>%
    select(-diesel_share)




  #now into the other stuff

  .year <- 2021
  all_fleet <- tibble()

  while (.year <= .run_to_year) {

    .year_fleet <- .fleet %>%
      filter(year <= .year) %>%

      #just changing the year structure, so we have a purchase year for the vehicle, and
      #a column for the year being assessed (i.e. the year in which the km are driven etc.)
      rename("purchase_year" = year) %>%
      mutate(vehicle_age = .year - purchase_year,
             year = .year) %>%

      #this bit will assign the fuel type to a vehicle based on expected proportions (i.e. petrol/diesel)
      #for the moment that data isn't there so we're just assuming all is petrol
      #also use this to assign the types of ptrol etc.
      mutate(fuel_type = fcase(
        electric_applied == FALSE , fuel_type,
        electric_applied == TRUE , "electric"))


    #we're also going to add the petrol/diesel/electricity prices for this year as columns to use later
    #first fuel
    .year_fuel_prices <- .fuel_prices %>%
      filter(year == .year,
             scenario == .fuel_scenario) %>%
      pivot_wider(names_from = fuel_type,
                  values_from = price)

    .petrol_98 <- .year_fuel_prices$petrol_98[1]
    .petrol_95 <- .year_fuel_prices$petrol_95[1]
    .petrol_91 <- .year_fuel_prices$petrol_91[1]
    .diesel <- .year_fuel_prices$diesel[1]

    #now electricity
    .year_electricity_prices <- .electricity_prices %>%
      filter(scenario == .electricity_scenario,
             year == .year)

    .energy_price <- .year_electricity_prices$energy_price[1]



    #now we have our prices we're going to add them as columns to the dataset
    .year_fleet <- .year_fleet %>%
      mutate(price_98 = .petrol_98,
             price_95 = .petrol_95,
             price_91 = .petrol_91,
             price_diesel = .diesel,
             energy_price = .energy_price)


    #because the energy consumption also depends on the year, we're going to add that
    #inside the loop as well
    .energy_consumption_year <- .energy_consumption %>%
      filter(year == .year) %>%
      pivot_wider(names_from = vehicle_type,
                  values_from = energy_consumption)

    .year_fleet <- .year_fleet %>%
      mutate(energy_consumption = fcase(
        vehicle_group == "passenger" , .energy_consumption_year$passenger[1],
        vehicle_group == "suv" , .energy_consumption_year$suv[1],
        vehicle_group == "lcv" , .energy_consumption_year$lcv[1]
      ))

    #and we're going to add the energy intensity of the grid for each year as well
    .emissions_intensity <- .energy_intensity %>%
      filter(year == .year)

    .year_fleet <- .year_fleet %>%
      mutate(emissions_intensity = .emissions_intensity$ei_g_wh[1])

    all_fleet <- bind_rows(all_fleet, .year_fleet)

    .year <- .year + 1

  }




  #The rest we can do outside of the loop I'm fairly sure, just by grouping by year etc.

  #now we want to assign the km driven for each vehicle based on it's type and
  #vehicle age. We're going to have to loop over the data to do this.

  #initialising the column
  all_fleet <- all_fleet %>%
    mutate(km_driven = 0)

  #now assigning the distance driven based on the vehicles age and vehicle type.
  #were assuming driving behaviour stays the same in all years.

  i <- 1
  while (i <= nrow(all_fleet)) {

    #for this iteration the characteristics are
    .age <- all_fleet$vehicle_age[i]
    .type <- all_fleet$vehicle_group[i]

    #based on this the km driven are:
    .km <- .km_travelled %>%
      filter(age == .age,
             vehicle_type == .type)

    #putting this value in the fleet dataset
    all_fleet$km_driven[i] = .km$km_travelled[1]

    i <- i + 1
  }


  #now creating a column for fuel consumption
  all_fleet <- all_fleet %>%
    #first creating a real world emissions column where we adjust for the 'gap
    mutate(real_emissions = current_emissions * .gap) %>%
    mutate(fuel_consumption = case_when(
      #fuel consumption into L/100km's first, then x (km's driven/100)
      fuel_type %in% c("petrol_91", "petrol_98", "petrol_95") ~ petrol_co2_to_fuel(real_emissions) * km_driven/100,
      fuel_type == "diesel" ~ diesel_co2_to_fuel(real_emissions) * km_driven/100,
      fuel_type == "electric" ~ 0
    )) %>%


    #now we can use the prices to create a yearly cost for that vehicle based on the
    #fuel
    mutate(fuel_cost = case_when(
      fuel_type == "petrol_91" ~ price_91 * fuel_consumption,
      fuel_type == "diesel" ~ price_diesel * fuel_consumption,
      fuel_type == "petrol_98" ~ price_98 * fuel_consumption,
      fuel_type == "petrol_95" ~ price_95 * fuel_consumption,
      fuel_type == "electric" ~ energy_price * energy_consumption * km_driven
    )) %>%


    #and we also want a column for total emissions for each car based on km driven in that year
    mutate(total_emissions = case_when(
      #first for ice's (in grams: g/km * km)
      electric_applied == FALSE ~ real_emissions * km_driven,
      #now calculating the emissions for ev's - we x 1000 as emissions intensity is in g/Wh and we have
      #energy consumption in kWh
      electric_applied == TRUE ~ (emissions_intensity * energy_consumption * 1000 * km_driven))) %>%
    #now just converting from g to tonnes
    mutate(total_emissions = total_emissions / 10^6) %>%


    # one thing we also need to do is adjust the cost of all the cars, so that the cost
    #incurred from the technology is only incurred in the first year (i.e. age = 0)
    #beyond this there is no added cost aside from fuel, therefore
    mutate(cost = case_when(
      vehicle_age == 0 ~ cost,
      vehicle_age != 0 ~ 0
    ))


  return(all_fleet)

}


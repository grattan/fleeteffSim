#' Benefit model
#'
#' @name benefit_model
#'
#' @description Benefit model
#'
#' @param .fleet The output of 'compliance_cost' function, with a compliant simulated fleet
#' @param .km_travelled Defaults to \code{km_travelled}. The assumed distance travelled by each vehicle per year, depending on vehicle age and type.
#' @param .fuel_prices Defaults to \code{fuel_prices}. The assumed future price of fuel.
#' @param .electricity_prices Defaults to \code{electricity_prices}. the assumed future price of electricity.
#' @param .energy_consumption Defaults to \code{energy_consumption}. The assumed future energy consumption of electric vehicles, in kWh/km travelled.
#' @param .energy_intensity Defaults to \code{energy_intensity}. The assumed future energy intensity of the electricity grid, in gCO2 equivalent per wH.
#' @param .gap Defaults to 1.2. The assumed gap between tested and real world emissions. 1.2 is equivalent to 20\%
#' @param .fuel_scenario Defaults to "central". The assumed fuel price scenario from the \code{.in_fuel_prices} \code{tibble}
#' @param .electricity_scenario Defaults to "central". The assumed electricity price scenario from the \code{.in_electricity_prices} \code{tibble}
#' @param .run_to_year Defaults to 2060. The year to which the benefit model will run. Given an assumed vehicle lifetime of 17 years, to account for full vehicle lifetime costs/benefits/emissions, this should be set to over 17 years after the last running year in \code{compliance_costs()}
#' @param .passenger_diesel faults to  0.02. The assumed proportion of passenger vehicles requiring diesel fuel.
#' @param .suv_diesel Defaults to 0.21. The assumed proportion of SUV vehicles requiring diesel fuel.
#' @param .lcv_diesel  Defaults to 0.92. The assumed proportion of LCV vehicles requiring diesel fuel.
#' @param .premium_95 Defaults to 0.15. The assumed proportion of ICE vehicles requiring premium 95ron petrol.
#' @param .premium_98 Defaults to 0.05. The assumed proportion of ICE vehicles requiring premium 98ron petrol.
#'
#' @return A \code{tibble} including the fuel, emissions, costs for each vehicle in the simulated fleet
#' over it' lifespan
#'
#' @export
#'


globalVariables(c("compliant_fleet", "km_travelled", "fuel_prices",
                  "electricity_prices", "energy_consumption", "energy_intensity",
                  "year", "fuel_type", "diesel_share", "purchase_year", "price",
                  "vehicle_type", "age", "current_emissions", "total_emissions",
                  "electric_applied", "price_91", "current_cost_100km",
                  "base_cost_100km", "km_driven", "rebound_factor"))



benefit_model <- function(.fleet,
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
      #also use this to assign the types of petrol etc.
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



  #now we want to assign the km driven for each vehicle based on it's type and
  #vehicle age. We're going to have to loop over the data to do this.

  #now assigning the distance driven based on the vehicles age and vehicle type.
  #were assuming driving behaviour stays the same in all years.
  km_travelled <- km_travelled %>%
    rename("vehicle_age" = age,
           "km_driven" = km_travelled,
           "vehicle_group" = vehicle_type)

  all_fleet <- inner_join(all_fleet, km_travelled,
                          by = c("vehicle_group", "vehicle_age"))


  #however, we need to adjust these km_driven figures to account for the rebound effect
  #to do this, we need to calculate the running costs per 100km for each upgraded vehicle compared
  #to the 'base model', which we assume has the km_driven figures already applied.

  #because the ratio would be the same regardless of the fuel type (as we're dealing with
  #efficiency, for simplicity we pretend all the vehicles are either 'petrol' (petrol_91)
  #or electric)

  all_fleet <- all_fleet %>%
    #cost per 100km for base vehicle
    mutate(base_cost_100km = price_91 * petrol_co2_to_fuel(base_emissions * .gap)) %>%

    #cost per 100 km upgraded vehicle
    mutate(current_cost_100km = case_when(
      electric_applied == TRUE ~ energy_consumption * 100 * energy_price,
      (electric_applied == FALSE)  ~ price_91 * petrol_co2_to_fuel(current_emissions * .gap))) %>%

    #using the difference in running costs to update km travelled with an elasticity of 0.1
    #this assumes a 1% running cost decrease increases driving distance by 0.1%
    rowwise() %>%
    mutate(rebound_factor = ((1 - (current_cost_100km / base_cost_100km)) * 10) + 100,
           km_driven = km_driven * (rebound_factor / 100))


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
      #first for ice's (in grams: g/km * km * 1.2)

      #the 1.2 factor is used to include upstream emissions - that is, emissions that
      #are produced in producing/refining/transporting the fuel to the petrol station
      #this is based on information from : NEED TO FIND THE SOURCE AGAIN
      electric_applied == FALSE ~ real_emissions * km_driven * 1.2,
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


#' Discount function
#'
#' @name discount
#'
#' @description Apply a discount rate to a supplied dataframe.
#'
#' @param data Input data
#' @param rate Discount rate to be applied
#'
#' @return A discounted dataframe
#'
#' @export
#'

discount <- function(data, rate) {

  i <- 1
  while (i <= nrow(data)) {

    data$additional_cost[i] =  (data$additional_cost[i])  / ((1 + rate)^(data$year[i] - 2021))
    data$fuel_cost_savings[i] =  (data$fuel_cost_savings[i])  / ((1 + rate)^(data$year[i] - 2021))
    data$co2_value_20[i] =  (data$co2_value_20[i])  / ((1 + rate)^(data$year[i] - 2021))
    data$co2_value_35[i] =  (data$co2_value_35[i])  / ((1 + rate)^(data$year[i] - 2021))

    i <- i + 1
  }
  return(data)
}



#' @title Summarise fleetEffSim results from the benefit_model() function.
#'
#' @param .data The input data to be summarised
#'
#' @return A \code{tibble} with summarised results fora given discount rate scenario
#' @export
#'



summarise_fes_results <- function(.data) {

  .data <- .data %>%
    #getting the totals from each
    summarise(additional_cost = sum(additional_cost),
              emissions_savings = sum(emissions_savings),
              fuel_cost_savings = sum(fuel_cost_savings),
              co2_value_20 = sum(co2_value_20),
              co2_value_35 = sum(co2_value_35)) %>%

    #calculating overall stats
    mutate(bcr_co2_excluded = fuel_cost_savings/additional_cost,
           bcr_co2_20_dollar = (fuel_cost_savings + co2_value_20) / additional_cost,
           bcr_co2_35_dollar = (fuel_cost_savings + co2_value_35) / additional_cost,

           #Because we're using emissions values in tons we need to put costs back into $ not million $
           abatement_cost = ((additional_cost - fuel_cost_savings) * 1000000)/emissions_savings,

           npv_co2_excluded_m = (fuel_cost_savings - additional_cost),
           npv_co2_20_dollar_m = (fuel_cost_savings + co2_value_20 - additional_cost),
           npv_co2_35_dollar_m = (fuel_cost_savings + co2_value_35 - additional_cost))

  return(.data)


}



#' Generate results
#'
#' @name generate_results
#'
#' @description Generate summarised and discounted results from a business as usual and target scenario output of the benefit_model().
#'
#'
#' @param bau_benefits The bau scenario output of the benefit_model()
#' @param target_benefits The target scenario output of the benefit_model()
#' @param cars The number of cars in the base year of the simulation run. By default, fleet_creator() sets 100 cars.
#'
#' @return summarised results
#'
#' @export
#'

globalVariables(c("vehicle_age", "cost", "fuel_cost", "additional_cost",
                  "emissions_savings", "fuel_cost_savings", "co2_value",
                  "bcr", "npv", "abatement_cost", "co2_value_20", "co2_value_35"))



generate_results <- function(bau_benefits,
                             target_benefits,
                             cars) {

  #pulling the data together from th bau and target summary
  bau_summary <- bau_benefits %>%
    filter(vehicle_age <= 17,
           year > 2021) %>%
    group_by(year) %>%
    #we multiply by 1,100,000 here because that is the estimate number of new sales
    #in the base year. Thus we average to get a 'per car' value for each year, and
    #scale this to the whole fleet size. This is used instead of summing as the number
    #of cars in the simulated fleet can change in different model runs if specified by
    #the user.
    summarise(cost = sum(cost * 1100000 / cars),
              total_emissions = sum(total_emissions *  1100000 / cars),
              fuel_cost = sum(fuel_cost *  1100000 / cars))


  target_summary <- target_benefits %>%
    filter(vehicle_age <= 17,
           year > 2021) %>%
    group_by(year) %>%
    summarise(cost = sum(cost *  1100000 / cars),
              total_emissions = sum(total_emissions *  1100000 / cars),
              fuel_cost = sum(fuel_cost *  1100000 / cars))


  #combining this to get the additionality of each we're interested in
  overall_summary <- bau_summary %>%

    mutate(additional_cost = (target_summary$cost - cost) / 1000000,
           emissions_savings = (total_emissions - target_summary$total_emissions),
           fuel_cost_savings = (fuel_cost - target_summary$fuel_cost) / 1000000,
           co2_value_20 = emissions_savings * 20 / 1000000,
           co2_value_35 = emissions_savings * 35 / 1000000) %>%

    select(-fuel_cost, -total_emissions, -cost)




  # Running with 7 and 3 % discount rates ---------------------------------


  overall_discounted_0 <- discount(overall_summary, rate = 0)

  overall_discounted_0 <- summarise_fes_results(overall_discounted_0) %>%
    mutate(scenario = "discount_0_perc")


  overall_discounted_7 <- discount(overall_summary,rate = 0.07)

  overall_discounted_7 <- summarise_fes_results(overall_discounted_7) %>%
    mutate(scenario = "discount_7_perc")


  overall_discounted_4 <- discount(overall_summary, rate = 0.04)

  overall_discounted_4 <- summarise_fes_results(overall_discounted_4) %>%
    mutate(scenario = "discount_4_perc")




  #Pooling results --------------------------------------------------------
  result <- bind_rows(overall_discounted_7,
                      overall_discounted_4,
                      overall_discounted_0) %>%
    relocate(scenario, abatement_cost) %>%
    pivot_longer(cols = (8:10),
                 values_to = "bcr",
                 names_to = "co2_value") %>%
    mutate(npv = case_when(
      co2_value == "bcr_co2_excluded" ~ npv_co2_excluded_m,
      co2_value == "bcr_co2_20_dollar" ~ npv_co2_20_dollar_m,
      co2_value == "bcr_co2_35_dollar" ~ npv_co2_35_dollar_m),

      co2_value = case_when(
        co2_value == "bcr_co2_excluded" ~ 0,
        co2_value == "bcr_co2_20_dollar" ~ 20,
        co2_value == "bcr_co2_35_dollar" ~ 35)) %>%
    select(scenario, co2_value, bcr, npv, abatement_cost, emissions_savings, fuel_cost_savings, additional_cost, co2_value_20, co2_value_35) %>%
    mutate(emissions_savings = emissions_savings / 1000000)

  return(result)

}



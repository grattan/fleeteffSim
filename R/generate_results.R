#' Discount function
#'
#' @name discount
#'
#' @description Apply a discount rate to a supplied dataframe
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

    data$additional_cost[i] =  (data$additional_cost[i])  / ((1 + rate)^(i))
    data$fuel_cost_savings[i] =  (data$fuel_cost_savings[i])  / ((1 + rate)^(i))

    i <- i + 1
  }
  return(data)
}



#' Generate results
#'
#' @name generate_results
#'
#' @description Generate results
#'
#'
#' @param bau_benefits the bau_benefits data
#' @param target_benefits the target benefits data
#'
#' @return summarised results
#'
#' @export
#'

globalVariables(c("vehicle_age", "cost", "fuel_cost", "additional_cost",
                  "emissions_savings", "fuel_cost_savings"))

generate_results <- function(bau_benefits,
                             target_benefits) {

  #pulling the data together from th bau and target summary
  bau_summary <- bau_benefits %>%
    filter(vehicle_age <= 17,
           year > 2021) %>%
    group_by(year) %>%
    summarise(cost = mean(cost),
              total_emissions = mean(total_emissions),
              fuel_cost = mean(fuel_cost))

  target_summary <- target_benefits %>%
    filter(vehicle_age <= 17,
           year > 2021) %>%
    group_by(year) %>%
    summarise(cost = mean(cost),
              total_emissions = mean(total_emissions),
              fuel_cost = mean(fuel_cost))


  #combining this to get the additionaility of each we're interested in
  overall_summary <- bau_summary %>%
    mutate(additional_cost = target_summary$cost - cost,
           emissions_savings = total_emissions - target_summary$total_emissions,
           fuel_cost_savings = fuel_cost - target_summary$fuel_cost) %>%
    select(-fuel_cost, -total_emissions, -cost)



  #--------------------------
  #undiscounted summary
  #no discount rate applied
  undiscounted_summary <- overall_summary %>%
    summarise(additional_cost = sum(additional_cost),
              emissions_savings = sum(emissions_savings),
              fuel_cost_savings = sum(fuel_cost_savings)) %>%
    mutate(bcr_co2_excluded = fuel_cost_savings/additional_cost,
           bcr_co2_20_dollar = (fuel_cost_savings + emissions_savings * 20) / additional_cost,
           bcr_co2_35_dollar = (fuel_cost_savings + emissions_savings * 35) / additional_cost,
           abatement_cost = (additional_cost - fuel_cost_savings)/emissions_savings) %>%
    mutate(scenario = "no_discount")


  #----------------
  #discount at 7%
  #at the moment we are NOT discounting the emissions savings (not sure how this works)
  overall_discounted_7 <- discount(overall_summary,
                                   rate = 0.07)


  overall_discounted_7_summary <- overall_discounted_7 %>%
    summarise(additional_cost = sum(additional_cost),
              emissions_savings = sum(emissions_savings),
              fuel_cost_savings = sum(fuel_cost_savings)) %>%
    mutate(bcr_co2_excluded = fuel_cost_savings/additional_cost,
           bcr_co2_20_dollar = (fuel_cost_savings + emissions_savings * 20) / additional_cost,
           bcr_co2_35_dollar = (fuel_cost_savings + emissions_savings * 35) / additional_cost,
           abatement_cost = (additional_cost - fuel_cost_savings)/emissions_savings) %>%
    mutate(scenario = "discount_7_perc")


  #---------------------
  #now discounting at 3%
  overall_discounted_3 <- discount(overall_summary,
                                   rate = 0.03)


  overall_discounted_3_summary <- overall_discounted_3 %>%
    summarise(additional_cost = sum(additional_cost),
              emissions_savings = sum(emissions_savings),
              fuel_cost_savings = sum(fuel_cost_savings)) %>%
    mutate(bcr_co2_excluded = fuel_cost_savings/additional_cost,
           bcr_co2_20_dollar = (fuel_cost_savings + emissions_savings * 20) / additional_cost,
           bcr_co2_35_dollar = (fuel_cost_savings + emissions_savings * 35) / additional_cost,
           abatement_cost = (additional_cost - fuel_cost_savings)/emissions_savings) %>%
    mutate(scenario = "discount_3_perc")


  #---------------------------------------
  #POOLING TOGETHER RESULTS
  result <- bind_rows(undiscounted_summary,
                      overall_discounted_3_summary,
                      overall_discounted_7_summary) %>%
    relocate(scenario)




  return(result)

}



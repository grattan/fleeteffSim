
#' Fleet efficiency simulation
#'
#' @param .in_fleet The simulated 'fleet' to be run through the model
#' @param .in_target_file Defaults to \code{targets_and_bau}. The file from where target and bau scenarios are drawn.
#' @param .in_target_scenario Defaults to "target_central". The assumed target trajectory scenario of a fleet standard from those contained in
#' \code{.in_target_file}
#' @param .in_bau_scenario Defaults to "bau". The assumed trajectory of the fleet standard from those contained in
#' \code{.in_target_file}
#' @param .in_cost_curves Defaults to "cost curves". The assumed costs of improving vehicle efficiency by year and vehicle type.
#' @param .in_estimate Defaults to "central". The estimate scenario used from the \code{cost_curves} scenarios. Options are "central",
#' "pp_late", "pp_early".
#' @param .in_suv_existing_tech Defaults to 20. The percentage assumed efficiency improvement of a base model new SUV vehicle when compared to
#' a I4 petrol engine (assumed as 2008 base model equivalent)
#' @param .in_passenger_existing_tech Defaults to 13 The percentage assumed efficiency improvement of a base model new passenger vehicle when compared to
#' a I4 petrol engine (assumed as 2008 base model equivalent)
#' @param .in_lcv_existing_tech Defaults to 13 The percentage assumed efficiency improvement of a base model new LCV vehicle when compared to
#' a I4 petrol engine (assumed as 2008 base model equivalent).
#' @param .in_run_to_year The year to whih the cost model will simulate.
#' @param .fleet The simulated fleet with technology upgrades applied to be run through the benefit model.
#' @param .in_km_travelled Defaults to \code{km_travelled}. The assumed distance travelled by each vehicle per year, depending on vehicle age and type.
#' @param .in_fuel_prices Defaults to \code{fuel_prices}. The assumed future price of fuel.
#' @param .in_electricity_prices Defaults to \code{electricity_prices}. the assumed future price of electricity.
#' @param .in_energy_consumption Defaults to \code{energy_consumption}. The assumed future energy consumption of electric vehicles.
#' @param .in_energy_intensity Defaults to \code{energy_intensity}. The assumed future energy intensity of the electricity grid.
#' @param .in_gap Defaults to 1.2. The assumed gap between tested and real world emissions. 1.2 is equivalent to 20\%
#' @param .in_fuel_scenario Defaults to "central". The assumed fuel price scenario from the \code{.in_fuel_prices} \code{tibble}
#' @param .in_electricity_scenario Defaults to "central". The assumed electricity price scenario from the \code{.in_electricity_prices} \code{tibble}
#' @param .in_benefit_run_to_year Defaults to 2060. The year to which the benefit model will run
#' @param .in_passenger_diesel Defaults to  0.02. The assumed proportion of passenger vehicles requiring diesel fuel.
#' @param .in_suv_diesel Defaults to 0.21. The assumed proportion of SUV vehicles requiring diesel fuel.
#' @param .in_lcv_diesel Defaults to 0.92. The assumed proportion of LCV vehicles requiring diesel fuel.
#' @param .in_premium_95 Defaults to 0.15. The assumed proportion of ICE vehicles requiring premium 95ron petrol.
#' @param .in_premium_98 Defaults to 0.05. The assumed proportion of ICE vehicles requiring premium 98ron petrol.
#'
#' @return
#' @export
#'
#'
#'


globalVariables()


fleet_eff_sim<- function(#cost model inputs
                        .in_fleet,
                        .in_target_file = targets_and_bau,
                        .in_target_scenario = "target_central",
                        .in_bau_scenario = "bau",
                        .in_cost_curves = cost_curves,
                        .in_estimate = "central",
                        .in_suv_existing_tech = 20,
                        .in_passenger_existing_tech = 13,
                        .in_lcv_existing_tech = 15,
                        .in_run_to_year = 2050,

                        #benefit model inputs
                        .fleet,
                        .in_km_travelled = km_travelled,
                        .in_fuel_prices = fuel_prices,
                        .in_electricity_prices = electricity_prices,
                        .in_energy_consumption = energy_consumption,
                        .in_energy_intensity = energy_intensity,
                        .in_gap = 1.2,
                        .in_fuel_scenario = "central",
                        .in_electricity_scenario = "central",
                        .in_benefit_run_to_year = 2060,
                        #and the diesel shares in each category
                        .in_passenger_diesel = 0.02,
                        .in_suv_diesel = 0.21,
                        .in_lcv_diesel = 0.92,
                        #the racv says about 20% of cars run on premium fuels. We are assuming that this is
                        #15% 95oct and 5% 98, but there is not hard data on this
                        .in_premium_95 = 0.15,
                        .in_premium_98 = 0.05) {

  #first running the costs side of model for the target
  .target_compliant <- compliance_costs(.fleet = .in_fleet,
                                        .target_file = .in_target_file,
                                        .target_scenario = .in_target_scenario,
                                        .cost_curves = .in_cost_curves,
                                        .estimate = .in_estimate,
                                        .suv_existing_tech = .in_suv_existing_tech,
                                        .passenger_existing_tech = .in_passenger_existing_tech,
                                        .lcv_existing_tech = .in_lcv_existing_tech,
                                        .run_to_year = .in_run_to_year)


  #and now running costs for the BAU scenario
  .bau_compliant <- compliance_costs(.fleet = .in_fleet,
                                        .target_file = .in_target_file,
                                        .target_scenario = .in_bau_scenario,
                                        .cost_curves = .in_cost_curves,
                                        .estimate = .in_estimate,
                                        .suv_existing_tech = .in_suv_existing_tech,
                                        .passenger_existing_tech = .in_passenger_existing_tech,
                                        .lcv_existing_tech = .in_lcv_existing_tech,
                                        .run_to_year = .in_run_to_year)



  #now onto the benefits side of the model
  .target_done <- benefit_model(.fleet = .target_compliant,
                                .km_travelled = .in_km_travelled,
                                .fuel_prices = .in_fuel_prices,
                                .electricity_prices = .in_electricity_prices,
                                .energy_consumption = .in_energy_consumption,
                                .energy_intensity = .in_energy_intensity,
                                .gap = .in_gap,
                                .fuel_scenario = .in_fuel_scenario,
                                .electricity_scenario = .in_electricity_scenario,
                                .run_to_year = .in_benefit_run_to_year,
                                #and the diesel shares in each category
                                .passenger_diesel = .in_passenger_diesel,
                                .suv_diesel = .in_suv_diesel,
                                .lcv_diesel = .in_lcv_diesel,
                                #the racv says about 20% of cars run on premium fuels. We are assuming that this is
                                #15% 95oct and 5% 98, but there is not hard data on this
                                .premium_95 = .in_premium_95,
                                .premium_98 = .in_premium_98)


  #now onto the benefits side of the model
  .bau_done <- benefit_model(.fleet = .bau_compliant,
                                .km_travelled = .in_km_travelled,
                                .fuel_prices = .in_fuel_prices,
                                .electricity_prices = .in_electricity_prices,
                                .energy_consumption = .in_energy_consumption,
                                .energy_intensity = .in_energy_intensity,
                                .gap = .in_gap,
                                .fuel_scenario = .in_fuel_scenario,
                                .electricity_scenario = .in_electricity_scenario,
                                .run_to_year = .in_benefit_run_to_year,
                                #and the diesel shares in each category
                                .passenger_diesel = .in_passenger_diesel,
                                .suv_diesel = .in_suv_diesel,
                                .lcv_diesel = .in_lcv_diesel,
                                #the racv says about 20% of cars run on premium fuels. We are assuming that this is
                                #15% 95oct and 5% 98, but there is not hard data on this
                                .premium_95 = .in_premium_95,
                                .premium_98 = .in_premium_98)



  #putting it all together to get the results
  results <- generate_results(bau_benefits = .bau_done,
                              target_benefits = .target_done)


  return(results)

}


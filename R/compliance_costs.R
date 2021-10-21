#' Compliance costs
#'
#' @name compliance_costs
#'
#' @description A function that estimates the additional production costs of meeting an emissions standard, for a simulated vehicle fleet.
#'
#' @param .fleet The simulated fleet of new vehicle sales. No defaults. Set to \code{.fleet = fleet_crator()} for default fleet assumptions.
#' @param .target_scenario The target type selected from \code{targets_and_bau} for the compliance cost run. Options include
#' "target_central", "bau", "target_linear", "target_ambitious".
#' @param .bau_scenario The BAU scenario selected from \code{targets_and_bau} for the compliance cost run. Options include "bau" and "bau_slow". "bau" assumes full EV saturation by 2048, whereas "bau_slow" assumes a 2050 emissions value of ~22-23g/km and no EV saturation.
#' @param .target_file The file containing the target scenarios. Should include three columns: `value` (the target emissions), `year`,
#' and `target_type`
#' @param .cost_curves The assumed cost curves for al vehicle types
#' @param .cost_curve_estimate The cost curves estimate type
#' @param .suv_existing_tech The assumed existing technology for SUVs
#' @param .passenger_existing_tech The assumed existing technology for passenger vehicles
#' @param .lcv_existing_tech The assumed existing technology for LCVs
#' @param .run_to_year The year the function will run until
#' @param .penalty_begin The assumed year that targets become binding. This is the start date for the model (inclusive);
#' prior to the specified year, there is assumed to target benefits, regardless of the target values specified.
#'
#'
#'
#' @return A \code{tibble} with a 'compliant' vehicle fleet, where technology upgrades have been
#' applied to meet the specified target in each year
#'
#' @export
#'
#'


globalVariables(c("fleet", "target", "cost_curves", "passenger",
                  "targets_and_bau", "target_type", "value"))





compliance_costs <- function(.fleet,
                             .target_file = targets_and_bau,
                             .target_scenario,
                             .bau_scenario,
                             .cost_curves = cost_curves,
                             .cost_curve_estimate = "central",
                             .suv_existing_tech = 26,
                             .passenger_existing_tech = 22,
                             .lcv_existing_tech = 18,
                             .run_to_year = 2050,
                             .penalty_begin = 2024) {


  #selecting the target required
  .target <- .target_file %>%
    filter(target_type == .target_scenario)

  #also selecting the BAU
  .bau <- .target_file %>%
    filter(target_type == .bau_scenario)


  #in a given year starting at what is set (we'll be looping through years)
  .year <- .penalty_begin
  fleet_out <- tibble()



  #we're going to apply the existing technology to the cost curve using the function
  # add_existing_tech in that script
  .cost_curve_inputs <- tibble(type = c("passenger", "suv", "lcv"),
                               existing_tech = c(.passenger_existing_tech,
                                                 .suv_existing_tech,
                                                 .lcv_existing_tech)
  )

  .cost_curves <- purrr::map2_dfr(
    .x = .cost_curve_inputs$type,
    .y = .cost_curve_inputs$existing_tech,
    .f = add_existing_technology,
    .estimate = .cost_curve_estimate,
    .cost_curves = .cost_curves) %>%
    filter(estimate == .cost_curve_estimate)



  #THE YEAR LOOP
  #--------------------------------
  while (.year <= .run_to_year) {

    message(green$bold("\nProcessing ", .year))

    #setting the parameters based on the year
    .this_year_fleet <- .fleet %>%
      filter(year == .year)

    #setting the target (or if the target is higher than the BAU, it defaults to the BAU value
    .this_year_target <- pmin(.target$value[.target$year == .year],
                              .bau$value[.bau$year == .year])


    #and selecting the cost curves
    .this_year_curves <- .cost_curves %>%
      filter(year == .year)


    #now we'll be looping through the individual vehicle in the fleet
    # THE APPLICATION LOOP (applying the best tech over and over 9selected by the selection loop)
    #----------------------------
    .target_reached = FALSE

    while (.target_reached == FALSE) {

      #this function does the first thing we want - it selects the next best upgrade to apply
      #tic()

      .upgrade <- select_upgrade(.this_year_fleet, .this_year_curves)
      #toc()

      #now we've got our upgrade, we want to apply it to the relevant car
      #we do this below

      #PRINTING TO DEBUG
      #print("This year fleet")
      #print(.this_year_fleet)

      #tic()
      .this_year_fleet <- .this_year_fleet %>%
        mutate(
          #updating the current emissions of the car
          current_emissions = case_when(
            id == .upgrade$id ~ (current_emissions - (current_emissions * .upgrade$incr_reduction)),
            id != .upgrade$id ~ current_emissions),

          #updating the cost
          cost = case_when(
            id == .upgrade$id ~ (cost + .upgrade$incr_cost),
            id != .upgrade$id ~ cost),

          #updating the tech package number
          tech_pkg_applied = case_when(
            id == .upgrade$id ~ (.upgrade$tech_pkg_no),
            id != .upgrade$id ~ tech_pkg_applied),

          #and whether it has gone electric or not
          electric_applied = case_when(
            id == .upgrade$id ~ (.upgrade$type == "ev"),
            id != .upgrade$id ~ electric_applied)
        )
      #toc()
      #message(bold$blue(.year, " emissions at ", round(mean(.this_year_fleet$current_emissions), digits = 2)))
      #message(bold$cyan("Cost at ", round(mean(.this_year_fleet$cost), digits = 2)))
      #checking if we've reached the target (if not the loop continues running)
      .target_reached <- (mean(.this_year_fleet$current_emissions) <= .this_year_target)

    }

    mean_emissions <- mean(.this_year_fleet$current_emissions)
    mean_costs <- mean(.this_year_fleet$cost)
    message(green("\tTarget reached for year ", .year))
    message(blue("\tAverage emission value is ", round(mean_emissions, digits = 2)))
    message(blue("\tAverage cost increase per vehicle was $", round(mean_costs, digits = 2)))

    .year <- .year + 1

    fleet_out <- bind_rows(fleet_out, .this_year_fleet)

  }

  return(fleet_out)

}



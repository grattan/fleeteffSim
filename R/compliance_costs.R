#' Compliance costs
#'
#' @name compliance_costs
#'
#' @description Compliance costs
#'
#' @param .fleet The simulated fleet of new vehicle sales. No defaults. Set to \code{.fleet = fleet_crator()} for default fleet assumptions.
#' @param .target_file The file containing the target scenarios. Should include three columns: `value` (the target emissions), `year`,
#' and `target_type`.
#' @param .target_scenario The target type selected from \code{targets_and_bau} for the compliance cost run. Options include
#' "target_central", "bau", "target_linear", "target_ambitious".
#' @param .cost_curves The assumed cost curves for all vehicle types.
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
                  "targets_and_bau", "target_type", "value", "is_upgrade"))


compliance_costs <- function(.fleet = fleet_creator(),
                             .target_file = targets_and_bau,
                             .target_scenario,
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
    filter(target_type == "bau")


  #in a given year starting at what is set (we'll be looping through years)
  .year <- .penalty_begin
  fleet_out <- tibble()



  #we're going to apply the existing technology to the cost curve using the function
  #add_existing_tech in that script

  .cost_curve_inputs <- tibble(type = c("suv", "passenger", "lcv"),
                               existing_tech = c(.suv_existing_tech,
                                                 .passenger_existing_tech,
                                                 .lcv_existing_tech)
                               )

  .cost_curves <- purrr::map2_dfr(
      .x = .cost_curve_inputs$type,
      .y = .cost_curve_inputs$existing_tech,
      .f = add_existing_technology,
      .estimate = .cost_curve_estimate,
      .cost_curves = .cost_curves) %>%
    filter(estimate == .cost_curve_estimate)


  # THE YEAR LOOP --------------------------------------------------------------
  for (.year in .penalty_begin:.run_to_year) {

    #setting the parameters based on the year
    .this_year_fleet <- .fleet %>%
      filter(year == .year)

    #setting the target (or if the target is higher than the BAU, it defaults to the BAU value
    .this_year_target <- pmin(.target$value[.target$year == .year],
                              .bau$value[.bau$year == .year])

    #and selecting the cost curves
    .this_year_curves <- .cost_curves %>%
      filter(year == .year)


    # APPLICATION LOOP ---------------------------------------------------------
    # now we'll be looping through the individual vehicle in the fleet, applying
    # the best tech over and over, selected by the selection loop)

    .target_reached = FALSE

    while (.target_reached == FALSE) {
      # select the next best upgrade to apply
      tic("select_upgrade")
      .upgrade <- select_upgrade(.this_year_fleet, .this_year_curves)
      toc()

      tic("update row")
      .updated_row <- .upgrade %>%
        left_join(.this_year_fleet, by = "id") %>%
        mutate(current_emissions = current_emissions * (1 - incr_reduction),
               cost = cost + incr_cost,
               tech_pkg_applied = tech_pkg_no,
               electric_applied = type == "ev") %>%
        select(year, vehicle_group, id, base_emissions, current_emissions, electric_applied, tech_pkg_applied, cost)

      # update table
      .this_year_fleet <- rows_update(.this_year_fleet, .updated_row, by = "id")
      toc()

      #checking if we've reached the target (if not the loop continues running)
      .target_reached <- mean(.this_year_fleet$current_emissions) <= .this_year_target

    } # end application loop

    message(bold$green("Target reached for year ", .year))
    message(blue$bold(.year, "average emission value is ", round(mean(.this_year_fleet$current_emissions), digits = 2)))
    message(blue$bold("The average cost increase per vehicle was $", round(mean(.this_year_fleet$cost), digits = 2)))
    message(cyan$bold("Moving to next year"))

    .year <- .year + 1

    fleet_out <- bind_rows(fleet_out, .this_year_fleet)

  } # end year loop

  return(fleet_out)

}



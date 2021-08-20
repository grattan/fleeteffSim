#' Compliance costs
#'
#' @name compliance_costs
#'
#' @description Compliance costs
#'
#' @param .fleet The simulated fleet of new vehicle sales until 2050
#' @param .target The input target of a fleetwide efficiency standard to be evaluated or a business as usual trajectory
#' @param .cost_curves The assumed cost curves for al vehicle types
#' @param .estimate The cost curves estimate type
#' @param .suv_existing_tech The assumed existing technology for SUVs
#' @param .passenger_existing_tech The assumed existing technology for passenger vehicles
#' @param .lcv_existing_tech The assumed existing technology for LCVs
#' @param .run_to_year The year the function will run until
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
                  "targets_and_bau", "target_type"))





compliance_costs <- function(.fleet = fleet,
                             .target_file = targets_and_bau,
                             .target_scenario = "target_central",
                             .cost_curves = cost_curves,
                             .estimate = "central",
                             .suv_existing_tech = 20,
                             .passenger_existing_tech = 13,
                             .lcv_existing_tech = 15,
                             .run_to_year = 2050) {


  #selecting the target required (or BAU)
  .target <- .target_file %>%
    filter(target_type == .target_scenario)


  #in a given year starting at 2021 (we'll be looping through years)
  .year <- 2021
  fleet_out <- tibble()



  #we're going to apply the existing technology to the cost curve using the function
  #add_existing_tech in that script

  .cost_curves <- bind_rows(add_existing_technology(.type = "suv",
                                                    .existing_tech = .suv_existing_tech,
                                                    .estimate = .estimate,
                                                    .cost_curves = cost_curves),
                            add_existing_technology(.type = "passenger",
                                                    .existing_tech = .passenger_existing_tech,
                                                    .estimate = .estimate,
                                                    .cost_curves = cost_curves),
                            add_existing_technology(.type = "lcv",
                                                    .existing_tech = .lcv_existing_tech,
                                                    .estimate = .estimate,
                                                    .cost_curves = cost_curves)) %>%
    filter(estimate == .estimate)



  #THE YEAR LOOP
  #--------------------------------
  while (.year <= .run_to_year) {

    #setting the parameters based on the year
    .this_year_fleet <- .fleet %>%
      filter(year == .year)

    .this_year_target <- .target %>%
      filter(year == .year)

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
      .target_reached <- (mean(.this_year_fleet$current_emissions) <= .this_year_target$value)

    }

    message(bold$green("Target reached for year ", .year))
    message(yellow$bold(.year, "average emission value is ", mean(.this_year_fleet$current_emissions)))
    message(yellow$bold(.year, "average cost increase was $", round(mean(.this_year_fleet$cost), digits = 2)))
    message(yellow$bold("Moving to next year"))

    .year <- .year + 1

    fleet_out <- bind_rows(fleet_out, .this_year_fleet)

  }

  return(fleet_out)

}



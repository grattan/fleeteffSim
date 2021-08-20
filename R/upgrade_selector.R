
#' upgrade_selector
#'
#' @param .this_year_fleet A \code{tibble} containing the simulated fleet of vehicle sales for the iterated year
#' @param .this_year_curves A \code{tibble} containing the cost curves for vehicles sold in the iterated year
#'
#' @return A \code{tibble} with the selected most cost effective upgrade to apply to the fleet
#'
#' @importFrom data.table fcase
#'
#' @export
#'
#'
#'


globalVariables(c("tech_pkg_applied", "id", "scenario", "vehicle_group",
                  "tech_pkg_no", "type", "incr_cost", "incr_reduction",
                  "dollar_per_gram_reduced"))






select_upgrade <- function(.this_year_fleet,
                               .this_year_curves) {

  .tech_options <- tibble()
  #just setting this so it get's ovewritten very easily by the first cost
  .best_incr_cost <- 1000


  #now we're filtering down the fleet to the cars we know will probably take the upgrade -
  #there's no point checking against all cars when we know some of them will be higher cost

  #IF THE CARS WITHIN EACH VEHICLE GROUP HAVE DIFFERENT EMISSIONS THIS MUST BE CHANGED
  #it assumes in each category a vehicle with the least tech applied will be the next vehicle
  #to have tech applied. If vehicle base emissions in each group != each other, this may not hold
  .cars_for_upgrade <- .this_year_fleet %>%
    group_by(vehicle_group) %>%
    slice_min(tech_pkg_applied) %>%
    slice_min(id)


  j <- 1

  while (j <= nrow(.cars_for_upgrade)) {

    #if we haven't reached the target already, we're first going to check that the
    #car we're up to hasn't already been turned to electric
    #if it is electric, we're going to skip it and move on
    .electric_applied <- .cars_for_upgrade$electric_applied[j]

    if (.electric_applied == TRUE) {
      j <- j + 1

    }

    else {

      #if the car we're up to isn't electric (still has emissions to be reduced)
      #we're going to store some characteristics about the car we're up to
      #and what tech has already bee applied


      .vehicle_group <- .cars_for_upgrade$vehicle_group[j]
      .base_emissions <- .cars_for_upgrade$base_emissions[j]
      .tech_pkg_allowed <- .cars_for_upgrade$tech_pkg_applied[j] + 1
      .id <- .cars_for_upgrade$id[j]
      .current_cost <- .cars_for_upgrade$cost[j]


      #printing here to debug
      #print(.id)


      #now we want to filter the cost curves to only select the technologies that are
      # 'available' to this car, based on whatever's been applied already
      #   print(.this_year_curves %>%
      #          filter(vehicle_group == .vehicle_group) %>%
      #         filter(tech_pkg_no %in% c(100, .tech_pkg_allowed)))

      .this_car_curves <- .this_year_curves %>%
        filter(vehicle_group == .vehicle_group) %>%
        filter(tech_pkg_no %in% c(100, .tech_pkg_allowed)) %>%


        #we've also got to adjust the cost curves file to reflect the new incremental
        #price of ev's (because the ev price is compared to base car, we need to decrease the
        #cost if we've already put on other upgrade)

        mutate(incr_cost = fcase(
          type == "ev" , (incr_cost - .current_cost),
          type != "ev" , incr_cost))

      #print(.this_car_curves)

      #now we want to work out which option gives the best carbon reduction /$
      .this_car_curves <- .this_car_curves %>%
        mutate(dollar_per_gram_reduced = incr_cost / (incr_reduction * .base_emissions)) %>%
        mutate(dollar_per_gram_reduced = fcase(
          !is.nan(dollar_per_gram_reduced) , dollar_per_gram_reduced,
          #we are putting in an arbitrary 1% of one cent here so it's non zero
          is.nan(dollar_per_gram_reduced) , 0.00001 )) %>%
        slice_min(dollar_per_gram_reduced, n = 1, with_ties = FALSE) %>%
        mutate(id = .id)


      #PRINTING HERE TO DEBUG
      #print("This car curves")
      #print(.this_car_curves)


      #to try and speed up/avoid binding rows where we can, we're only going to
      #save the value if it is less than the previously saved value.
      if (.this_car_curves$dollar_per_gram_reduced[1] <= .best_incr_cost) {

        #save it as best so far
        .tech_options <- .this_car_curves

        #and reset the best value we've found to this one
        .best_incr_cost <- .tech_options$dollar_per_gram_reduced[1]

        j <- j + 1

      } else{
        j <- j + 1
      }

    }
  }

  #message(green("Upgrade selected. Costs ", round(.tech_options$dollar_per_gram_reduced, digits = 2), "$ per g/co2"))
  return(.tech_options)

}






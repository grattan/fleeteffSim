#' Upgrade selector
#'
#' @name select_upgrade
#'
#' @description Select an upgrade
#'
#'
#' @param .this_year_fleet A \code{tibble} containing the simulated fleet of vehicle sales for the iterated year
#' @param .this_year_curves A \code{tibble} containing the cost curves for vehicles sold in the iterated year
#'
#' @return A \code{tibble} with the selected most cost effective upgrade to apply to the fleet
#'
#'
#' @export
#'

globalVariables(c("tech_pkg_applied", "id", "scenario", "vehicle_group",
                  "tech_pkg_no", "type", "incr_cost", "incr_reduction",
                  "dollar_per_gram_reduced"))

select_upgrade <- function(.this_year_fleet,
                           .this_year_curves) {

  # filtering down the fleet to the cars we know will probably take the upgrade -
  # there's no point checking against all cars when we know some of them will be higher cost

  # IF THE CARS WITHIN EACH VEHICLE GROUP HAVE DIFFERENT EMISSIONS THIS MUST BE CHANGED
  # it assumes in each category a vehicle with the least tech applied will be the next vehicle
  # to have tech applied. If vehicle base emissions in each group != each other, this may not hold
  .cars_for_upgrade <- .this_year_fleet %>%
    group_by(vehicle_group) %>%
    arrange(tech_pkg_applied, id) %>%
    slice(1) %>%
    filter(!electric_applied) %>%
    ungroup()

  if (nrow(.cars_for_upgrade) == 0) message("No cars to upgrade!")

  # select vehicle with lowest cost per abatement
  .tech_options <- .this_year_curves %>%
    inner_join(.cars_for_upgrade, by = c("vehicle_group", "year", "base_emissions")) %>%
    ungroup() %>%
    # filter the cost curves to only select available technologies
    filter(tech_pkg_no == tech_pkg_applied + 1 |  tech_pkg_no == 100) %>%
    #we've also got to adjust the cost curves file to reflect the new incremental
    #price of ev's (because the ev price is compared to base car, we need to decrease the
    #cost if we've already put on other upgrade)
    mutate(
      incr_cost = fifelse(type == "ev",
                          (incr_cost - cost),
                          incr_cost),
      # work out which option gives the best carbon reduction /$
      dollar_per_gram_reduced = (incr_cost / (incr_reduction * base_emissions)) %>%
        replace_na(0.00001)
    ) %>%
    arrange(dollar_per_gram_reduced, id) %>%
    slice(1) %>%
    select(id, incr_reduction, incr_cost, tech_pkg_no, type)

  #message(green("Upgrade selected. Costs ", round(.tech_options$dollar_per_gram_reduced, digits = 2), "$ per g/co2"))
  if (nrow(.tech_options) > 1) stop("select_upgrade returning more than 1 row. This shouldn't happen.")
  return(.tech_options)

}

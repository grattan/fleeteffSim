#' Add existing technology
#'
#' @name add_existing_technology
#'
#' @description Add existing technology
#'
#' @param .type The type of vehicle (ie. passenger, suv) to be applied to
#' @param .existing_tech The assumed level of existing technology, as a percentage reduction of co2 since 2008 average
#' @param .estimate The cost curve estimate type
#' @param .cost_curves The cost curves used in the overall model run
#'
#' @return A \code{tibble} containing updated cost curves, where the costs of assumed 'existing' technology is 0
#'
#' @export
#'

globalVariables(c("estimate", "existing_tech", "cumulative_reduction",
                  "proportion_left", "incr_reduction", "difference"))

add_existing_technology <- function(.type,
                                    .existing_tech,
                                    .estimate,
                                    .cost_curves) {

  .cost_curves <- .cost_curves %>%
    filter(vehicle_group == .type,
           estimate == .estimate) %>%
    mutate(existing_tech = case_when(
      .existing_tech >= cumulative_reduction ~ "existing",
      .existing_tech < cumulative_reduction ~ "not existing"
    ))

  .existing_tech_no <- .cost_curves %>%
    filter(existing_tech == "existing") %>%
    slice_max(order_by = tech_pkg_no)

  #so the package we are 'up to'
  .existing_tech_no <- .existing_tech_no$tech_pkg_no[1]

  #and everything before this is already assumed to be in th car, so we're saying it's
  #all 0 cost 0 benefit
  .zeroed_existing_tech <- .cost_curves %>%
    filter(tech_pkg_no <= .existing_tech_no) %>%
    mutate(incr_reduction = 0,
           incr_cost = 0)

  #now we want to alter the 'next' next to reflect how much of it we may have already incorperated
  #i.e. how far between the last existing tech and this next one we are
  .next_tech <- .cost_curves %>%
    filter(tech_pkg_no == .existing_tech_no + 1) %>%
    mutate(difference = cumulative_reduction - .existing_tech) %>%
    mutate(proportion_left = (cumulative_reduction - .existing_tech) / cumulative_reduction,
           #now linearly working out much much of the next package is 'left' to be applied
           #because it's linear the $/g stays the same, which doesn't affect application
           incr_cost = incr_cost * proportion_left,
           incr_reduction = incr_reduction * proportion_left) %>%
    #now getting rid of excess variables to make the data nice again
    select(-difference, -proportion_left)


  #and lastly we want to keep all the remaining packages the same

  .remaining_packages <- .cost_curves %>%
    filter(tech_pkg_no > .existing_tech_no + 1)


  #and now we have our three parts - the zeroed existing tech, altered next tech and
  #remaining tech, we can combine them all

  .cost_curves <- bind_rows(.zeroed_existing_tech, .next_tech)
  .cost_curves <- bind_rows(.cost_curves, .remaining_packages)


  return(.cost_curves)

}


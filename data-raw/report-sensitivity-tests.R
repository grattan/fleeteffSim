
source("data-raw/model_data/00-setup.R")

#This script runs all the scenarios used for sensitivity testing for the report,
#for the `central` target trajectory

#' The scenarios tested for each scenario are:
  #' central
  #' high fuel cost
  #' low fuel cost
  #' high elec cost
  #' low elec cost
  #' off peak elec cost
  #' high gap
  #' high ice tech costs
  #' ev price parity early
  #' ev price parity late

#' each scenario is run for each of the three target trajectories specified; "central",
#' "linear" and "ambitious". This gives ~ 30 model runs, which takes a while.


#all central trajectory runs ----------------------------------------------
{

#generating the fleet for these scenarios with central costs
#this means that we can run the `fleet_eff_sim()` model function with .in_run_costs = FALSE to
#save re-running basically the same thing where it's only the benefits changing
#in the model runs

# costs for scenarios with central cost runs -------------------------------

central_fleet <- fleet_creator()

central_bau_compliant <- compliance_costs(.fleet = central_fleet,
                                          .target_scenario = "bau")

central_target_compliant <- compliance_costs(.fleet = central_fleet,
                                             .target_scenario = "target_central")


#runs (not repeating cost model) -------------------------------------------

# central run
central <- fleet_eff_sim(.in_run_costs = FALSE,
                         .in_bau_compliant = central_bau_compliant,
                         .in_fleet_compliant = central_target_compliant) %>%
  mutate(run_type = "central")


# high fuel cost
high_fuel <- fleet_eff_sim(.in_run_costs = FALSE,
                           .in_bau_compliant = central_bau_compliant,
                           .in_fleet_compliant = central_target_compliant,
                           .in_fuel_scenario = "high_price") %>%
  mutate(run_type = "high_fuel")

# low fuel
low_fuel <- fleet_eff_sim(.in_run_costs = FALSE,
                          .in_bau_compliant = central_bau_compliant,
                          .in_fleet_compliant = central_target_compliant,
                          .in_fuel_scenario = "low_price") %>%
  mutate(run_type = "low_fuel")


# high elec
high_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                           .in_bau_compliant = central_bau_compliant,
                           .in_fleet_compliant = central_target_compliant,
                           .in_electricity_scenario = "high_price") %>%
  mutate(run_type = "high_elec")

# low elec
low_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                          .in_bau_compliant = central_bau_compliant,
                          .in_fleet_compliant = central_target_compliant,
                          .in_electricity_scenario = "low_price") %>%
  mutate(run_type = "low_elec")

# off peak elec
off_peak_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                               .in_bau_compliant = central_bau_compliant,
                               .in_fleet_compliant = central_target_compliant,
                               .in_electricity_scenario = "off_peak") %>%
  mutate(run_type = "off_peak_elec")

# high_gap
gap_1_3 <- fleet_eff_sim(.in_run_costs = FALSE,
                         .in_bau_compliant = central_bau_compliant,
                         .in_fleet_compliant = central_target_compliant,
                         .in_gap = 1.3) %>%
  mutate(run_type = "gap_1_3")


#scenarios that need to run the cost model again --------------------------

#high ICE costs (added 10 to existing tech baselines, might be very high)
high_ice_costs <- fleet_eff_sim(.in_lcv_existing_tech = 23,
                                .in_suv_existing_tech = 30,
                                .in_passenger_existing_tech = 23) %>%
  mutate(run_type = "high_ice_costs")

#low ICE costs (subtracted 5 to existing tech baselines, might be very high)
low_ice_costs <- fleet_eff_sim(.in_lcv_existing_tech = 8,
                                .in_suv_existing_tech = 15,
                                .in_passenger_existing_tech = 8) %>%
  mutate(run_type = "low_ice_costs")

#PP early
early_pp <- fleet_eff_sim(.in_cost_curves_estimate = "early_pp") %>%
  mutate(run_type = "early_pp")

#PP late
late_pp <- fleet_eff_sim(.in_cost_curves_estimate = "late_pp") %>%
  mutate(run_type = "late_pp")



#binding all results -------------------------------------------------------


all_central_results <- bind_rows(central,
                         high_fuel,
                         low_fuel,
                         high_elec,
                         low_elec,
                         off_peak_elec,
                         gap_1_3,
                         high_ice_costs,
                         low_ice_costs,
                         early_pp,
                         late_pp) %>%
  mutate(target_type = "central")

}


#all linear trajectory runs -----------------------------------------------
{

  #generating the fleet for these scenarios with central costs
  #this means that we can run the `fleet_eff_sim()` model function with .in_run_costs = FALSE to
  #save re-running basically the same thing where it's only the benefits changing
  #in the model runs

  # costs for scenarios with central cost runs -------------------------------

  linear_fleet <- fleet_creator()

  linear_bau_compliant <- compliance_costs(.fleet = linear_fleet,
                                            .target_scenario = "bau")

  linear_target_compliant <- compliance_costs(.fleet = linear_fleet,
                                               .target_scenario = "target_linear")


  #runs (not repeating cost model) -------------------------------------------

  # central run
  lin_central <- fleet_eff_sim(.in_run_costs = FALSE,
                           .in_bau_compliant = linear_bau_compliant,
                           .in_fleet_compliant = linear_target_compliant) %>%
    mutate(run_type = "central")


  # high fuel cost
  lin_high_fuel <- fleet_eff_sim(.in_run_costs = FALSE,
                             .in_bau_compliant = linear_bau_compliant,
                             .in_fleet_compliant = linear_target_compliant,
                             .in_fuel_scenario = "high_price") %>%
    mutate(run_type = "high_fuel")

  # low fuel
  lin_low_fuel <- fleet_eff_sim(.in_run_costs = FALSE,
                            .in_bau_compliant = linear_bau_compliant,
                            .in_fleet_compliant = linear_target_compliant,
                            .in_fuel_scenario = "low_price") %>%
    mutate(run_type = "low_fuel")


  # high elec
  lin_high_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                             .in_bau_compliant = linear_bau_compliant,
                             .in_fleet_compliant = linear_target_compliant,
                             .in_electricity_scenario = "high_price") %>%
    mutate(run_type = "high_elec")

  # low elec
  lin_low_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                            .in_bau_compliant = linear_bau_compliant,
                            .in_fleet_compliant = linear_target_compliant,
                            .in_electricity_scenario = "low_price") %>%
    mutate(run_type = "low_elec")

  # off peak elec
  lin_off_peak_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                                 .in_bau_compliant = linear_bau_compliant,
                                 .in_fleet_compliant = linear_target_compliant,
                                 .in_electricity_scenario = "off_peak") %>%
    mutate(run_type = "off_peak_elec")

  # high_gap
  lin_gap_1_3 <- fleet_eff_sim(.in_run_costs = FALSE,
                               .in_bau_compliant = linear_bau_compliant,
                               .in_fleet_compliant = linear_target_compliant,
                               .in_gap = 1.3) %>%
    mutate(run_type = "gap_1_3")


  #scenarios that need to run the cost model again --------------------------

  #high ICE costs (added 10 to existing tech baselines, might be very high)
  lin_high_ice_costs <- fleet_eff_sim(.in_lcv_existing_tech = 23,
                                      .in_suv_existing_tech = 30,
                                      .in_passenger_existing_tech = 23,
                                      .in_target_scenario = "target_linear") %>%
    mutate(run_type = "high_ice_costs")

  #low ICE costs (subtracted 5 to existing tech baselines, might be very high)
  lin_low_ice_costs <- fleet_eff_sim(.in_lcv_existing_tech = 8,
                                     .in_suv_existing_tech = 15,
                                     .in_passenger_existing_tech = 8,
                                     .in_target_scenario = "target_linear") %>%
    mutate(run_type = "low_ice_costs")

  #PP early
  lin_early_pp <- fleet_eff_sim(.in_cost_curves_estimate = "early_pp",
                                .in_target_scenario = "target_linear") %>%
    mutate(run_type = "early_pp")

  #PP late
  lin_late_pp <- fleet_eff_sim(.in_cost_curves_estimate = "late_pp",
                               .in_target_scenario = "target_linear") %>%
    mutate(run_type = "late_pp")



  #binding all results -------------------------------------------------------


  all_linear_results <- bind_rows(lin_central,
                                   lin_high_fuel,
                                   lin_low_fuel,
                                   lin_high_elec,
                                   lin_low_elec,
                                   lin_off_peak_elec,
                                   lin_gap_1_3,
                                   lin_high_ice_costs,
                                   lin_low_ice_costs,
                                   lin_early_pp,
                                   lin_late_pp) %>%
    mutate(target_type = "linear")

}


#all ambitious trajectory runs --------------------------------------------
{

  #generating the fleet for these scenarios with central costs
  #this means that we can run the `fleet_eff_sim()` model function with .in_run_costs = FALSE to
  #save re-running basically the same thing where it's only the benefits changing
  #in the model runs

  # costs for scenarios with central cost runs -------------------------------

  ambitious_fleet <- fleet_creator()

  ambitious_bau_compliant <- compliance_costs(.fleet = ambitious_fleet,
                                            .target_scenario = "bau")

  ambitious_target_compliant <- compliance_costs(.fleet = ambitious_fleet,
                                               .target_scenario = "target_ambitious")


  #runs (not repeating cost model) -------------------------------------------

  # central run
  amb_central <- fleet_eff_sim(.in_run_costs = FALSE,
                           .in_bau_compliant = ambitious_bau_compliant,
                           .in_fleet_compliant = ambitious_target_compliant) %>%
    mutate(run_type = "central")


  # high fuel cost
  amb_high_fuel <- fleet_eff_sim(.in_run_costs = FALSE,
                             .in_bau_compliant = ambitious_bau_compliant,
                             .in_fleet_compliant = ambitious_target_compliant,
                             .in_fuel_scenario = "high_price") %>%
    mutate(run_type = "high_fuel")

  # low fuel
  amb_low_fuel <- fleet_eff_sim(.in_run_costs = FALSE,
                            .in_bau_compliant = ambitious_bau_compliant,
                            .in_fleet_compliant = ambitious_target_compliant,
                            .in_fuel_scenario = "low_price") %>%
    mutate(run_type = "low_fuel")


  # high elec
  amb_high_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                             .in_bau_compliant = ambitious_bau_compliant,
                             .in_fleet_compliant = ambitious_target_compliant,
                             .in_electricity_scenario = "high_price") %>%
    mutate(run_type = "high_elec")

  # low elec
  amb_low_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                            .in_bau_compliant = ambitious_bau_compliant,
                            .in_fleet_compliant = ambitious_target_compliant,
                            .in_electricity_scenario = "low_price") %>%
    mutate(run_type = "low_elec")

  # off peak elec
  amb_off_peak_elec <- fleet_eff_sim(.in_run_costs = FALSE,
                                 .in_bau_compliant = ambitious_bau_compliant,
                                 .in_fleet_compliant = ambitious_target_compliant,
                                 .in_electricity_scenario = "off_peak") %>%
    mutate(run_type = "off_peak_elec")

  # high_gap
  amb_gap_1_3 <- fleet_eff_sim(.in_run_costs = FALSE,
                           .in_bau_compliant = ambitious_bau_compliant,
                           .in_fleet_compliant = ambitious_target_compliant,
                           .in_gap = 1.3) %>%
    mutate(run_type = "gap_1_3")


  #scenarios that need to run the cost model again --------------------------

  #high ICE costs (added 10 to existing tech baselines, might be very high)
  amb_high_ice_costs <- fleet_eff_sim(.in_lcv_existing_tech = 23,
                                  .in_suv_existing_tech = 30,
                                  .in_passenger_existing_tech = 23,
                                  .in_target_scenario = "target_ambitious") %>%
    mutate(run_type = "high_ice_costs")

  #low ICE costs (subtracted 5 to existing tech baselines, might be very high)
  amb_low_ice_costs <- fleet_eff_sim(.in_lcv_existing_tech = 8,
                                 .in_suv_existing_tech = 15,
                                 .in_passenger_existing_tech = 8,
                                 .in_target_scenario = "target_ambitious") %>%
    mutate(run_type = "low_ice_costs")

  #PP early
  amb_early_pp <- fleet_eff_sim(.in_cost_curves_estimate = "early_pp",
                                .in_target_scenario = "target_ambitious") %>%
    mutate(run_type = "early_pp")

  #PP late
  amb_late_pp <- fleet_eff_sim(.in_cost_curves_estimate = "late_pp",
                               .in_target_scenario = "target_ambitious") %>%
    mutate(run_type = "late_pp")



  #binding all results -------------------------------------------------------


  all_ambitious_results <- bind_rows(amb_central,
                                   amb_high_fuel,
                                   amb_low_fuel,
                                   amb_high_elec,
                                   amb_low_elec,
                                   amb_off_peak_elec,
                                   amb_gap_1_3,
                                   amb_high_ice_costs,
                                   amb_low_ice_costs,
                                   amb_early_pp,
                                   amb_late_pp) %>%
    mutate(target_type = "ambitious")

}


#summary of all results ---------------------------------------------------

all_results <- bind_rows(all_central_results,
                         all_linear_results,
                         all_ambitious_results)


#write_rds(all_results, "data-raw/report_sensitivity_results.rds")

all_results <- read_rds("data-raw/report_sensitivity_results.rds")


# Plotting results --------------------------------------------------------

#formatting data
all_results <- all_results %>%
  mutate(run_type = case_when(
    run_type == "off_peak_elec" ~ "Off peak electricity",
    run_type == "low_ice_costs" ~ "Low cost ICE CO2 reduction",
    run_type == "low_fuel" ~ "Low future fuel price",
    run_type == "low_elec" ~ "Low future electricity price",
    run_type == "late_pp" ~ "EV price parity delayed one year",
    run_type == "high_ice_costs" ~ "High cost ICE CO2 reduction",
    run_type == "high_fuel" ~ "High future fuel costs",
    run_type == "high_elec" ~ "High future electricity price",
    run_type == "gap_1_3" ~ "Increased real world 'gap'",
    run_type == "early_pp" ~ "EV price parity one year early",
    run_type == "central" ~ "Central scenario")) %>%
  mutate(target_type = factor(target_type,
                              levels = c("linear", "central",
                                         "ambitious")))




#Abatement cost
all_results %>%
  mutate(run_type = factor(run_type, levels = all_results %>%
                             filter(target_type == "ambitious") %>%
                             arrange(abatement_cost) %>%
                             pull(run_type) %>%
                             unique())) %>%
  filter(scenario == "discount_7_perc",
         co2_value == 0) %>%
  mutate(colour = case_when(
    abatement_cost >= 0 ~ "above 1",
    abatement_cost < 0 ~ "below 1")) %>%
  ggplot(aes(y = run_type, x = abatement_cost, colour = target_type)) +
  geom_col(position = position_dodge(0.7), width = 0.05) +
  geom_vline(xintercept = 0, colour = grattan_grey4, size = 1) +
  geom_point(size = 5, position = position_dodge(0.7)) +
  scale_x_continuous(limits = c(-80, 50)) +
  theme_grattan(base_size = 12, legend = "top") +
  grattan_fill_manual() +
  grattan_colour_manual() +
  labs(title = "Under all tested scenarios, a fleetwide standard provides cheap abatement",
       subtitle = "Cost of abatement ($/t CO2) of implementing a fleetwide standard")



#NPV chart ($35)

all_results %>%
  mutate(run_type = factor(run_type, levels = all_results %>%
                             filter(target_type == "ambitious") %>%
                             arrange(desc(npv)) %>%
                             pull(run_type) %>%
                             unique())) %>%
  filter(scenario == "discount_7_perc",
         co2_value == 35) %>%
  ggplot() +
  geom_point(aes(x = run_type,
                 y = npv,
                 colour = target_type,
                 size = emissions_savings),
             alpha = 0.9) +
  coord_flip() +
  theme_grattan(legend = "top") +
  scale_y_continuous_grattan(limits = c(0, 35000)) +
  grattan_colour_manual() +
  labs(title = "Under all tested scenarios, a fleetwide standard has very high NPNV",
       subtitle = "NPV ($ millions) of implementing a fleetwide standard")




# BCR chart

all_results %>%
  mutate(run_type = factor(run_type, levels = all_results %>%
                             filter(target_type == "central",
                                    scenario == "discount_7_perc",
                                    co2_value == 0) %>%
                             arrange(desc(bcr)) %>%
                             pull(run_type) %>%
                             unique())) %>%
  filter(scenario == "discount_7_perc",
         co2_value == 0) %>%
  ggplot() +
  geom_point(aes(x = run_type,
                 y = bcr,
                 colour = target_type,
                 size = emissions_savings)) +
  coord_flip() +
  theme_grattan(legend = "top") +
  scale_y_continuous_grattan(limits = c(0, 8)) +
  grattan_colour_manual() +
  labs(title = "Under all tested scenarios, a fleetwide standard has a BCR above 1",
       subtitle = "Cost of abatement ($/t CO2) of implementing a fleetwide standard")




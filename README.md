
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fleeteffSim

<!-- badges: start -->

[![R-CMD-check](https://github.com/grattan/fleeteffSim/workflows/R-CMD-check/badge.svg)](https://github.com/grattan/fleeteffSim/actions)
<!-- badges: end -->

## Installation

You can install the released version of `fleeteffSim` from [Github]()
with:

``` r
# install.packages("remotes")
remotes::install_github("grattan/fleeteffSim")
```

## Example

The `fleet_eff_sim()` function runs simulations of vehicle emissions
standards over the Australian fleet. By default, this uses a grattan
trajectory listed in the ‘Grattan car plan’ report as the ‘central’
scenario, and simulates the use of vehicles sold between 2024 and 2050.

``` r
#library(fleeteffSim)
library(devtools)
library(dplyr)
load_all()
## basic example code

test_run <- fleet_eff_sim()
```

The resulting `tibble` is:

``` r
glimpse(test_run)
#> Rows: 9
#> Columns: 10
#> $ scenario          <chr> "discount_7_perc", "discount_7_perc", "discount_7_pe…
#> $ co2_value         <dbl> 0, 20, 35, 0, 20, 35, 0, 20, 35
#> $ bcr               <dbl> 6.417729, 6.794702, 7.077432, 8.147283, 8.639390, 9.…
#> $ npv               <dbl> 38957.23, 41667.92, 43700.95, 65772.84, 70301.45, 73…
#> $ abatement_cost    <dbl> -80.0684, -80.0684, -80.0684, -135.1823, -135.1823, …
#> $ emissions_savings <dbl> 486.5493, 486.5493, 486.5493, 486.5493, 486.5493, 48…
#> $ fuel_cost_savings <dbl> 46147.92, 46147.92, 46147.92, 74975.33, 74975.33, 74…
#> $ additional_cost   <dbl> 7190.693, 7190.693, 7190.693, 9202.496, 9202.496, 92…
#> $ co2_value_20      <dbl> 2710.697, 2710.697, 2710.697, 4528.615, 4528.615, 45…
#> $ co2_value_35      <dbl> 4743.720, 4743.720, 4743.720, 7925.077, 7925.077, 79…
```

There are many inputs to the `fleet_eff_sim()` function that can be
changed from default settings. For example, this includes future fuel
and electricity prices, as well as forecast future sales volumes by
vehicle category.

The `fleet_eff_sim()` function only provides summarised data over the
entire period run in the model. For more granular output, the following
series of functions can be run.

First, production costs required to meet targets can be estimated:

``` r
#Running the target scenario to estimate vehicle production costs
target_compliance <- compliance_costs(.fleet = fleet_creator(),
                                      .target_scenario = "target_central",
                                      .bau_scenario = "bau")

#Running the 'business as usual'  scenario to estimate vehicle production costs
bau_compliance <- compliance_costs(.fleet = fleet_creator(),
                                   .target_scenario = "bau",
                                   .bau_scenario = "bau")
```

Second, running costs and emissions can be estimated:

``` r
#Running the target scenario to estimate vehicle production costs
target_running <- benefit_model(.fleet = target_compliance)

#Running the 'business as usual'  scenario to estimate vehicle production costs
bau_running <- benefit_model(.fleet = bau_compliance)
```

The `target_running` and `bau_running` datasets can then be compared for
more granular data - however, it must be considered that each
‘simulated’ vehicle under default parameters accounts for 1% of the
total estimated fleet. Thus, runnings costs and emissions must be scaled
up respectively to estimated fleet wide values.

Similarly to the `fleet_eff_sim()` function, summarised results can also
be generated. This accounts for scaling up the fleet:

``` r
generate_results(bau_benefits = bau_running,
                 target_benefits = target_running,
                 cars = 100)
#> # A tibble: 9 × 10
#>   scenario        co2_value   bcr     npv abatement_cost emissions_savings
#>   <chr>               <dbl> <dbl>   <dbl>          <dbl>             <dbl>
#> 1 discount_7_perc         0  6.42  38957.          -80.1              487.
#> 2 discount_7_perc        20  6.79  41668.          -80.1              487.
#> 3 discount_7_perc        35  7.08  43701.          -80.1              487.
#> 4 discount_4_perc         0  8.15  65773.         -135.               487.
#> 5 discount_4_perc        20  8.64  70301.         -135.               487.
#> 6 discount_4_perc        35  9.01  73698.         -135.               487.
#> 7 discount_0_perc         0 11.7  142221.         -292.               487.
#> 8 discount_0_perc        20 12.5  151952.         -292.               487.
#> 9 discount_0_perc        35 13.0  159251.         -292.               487.
#> # … with 4 more variables: fuel_cost_savings <dbl>, additional_cost <dbl>,
#> #   co2_value_20 <dbl>, co2_value_35 <dbl>
```

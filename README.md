
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fleeteffSim

<!-- badges: start -->

[![R-CMD-check](https://github.com/grattan/fleeteffSim/workflows/R-CMD-check/badge.svg)](https://github.com/grattan/fleeteffSim/actions)
<!-- badges: end -->

Things to be fixed:

-   add a test so that the select\_upgrade function always returns the
    absolute lowest value ( particularly important as I have added
    ‘shortcuts’ to getting to the lowest cost which may not always hold
    if future changes are made so that vehicles in the same type have
    different base emissions)

-   need to make sure that there is a specific and known protocol for
    how the algorithm decides which car to apply the electric upgrade to
    when they are all the same cost (i.e. all 0 cost)

-   need to add rebound effect

-   need to add a 1 year earlier and 1 year later price parity scenario
    to ev curves

## Installation

You can install the released version of `fleeteffSim` from [Github]()
with:

``` r
# install.packages("remotes")
remotes::install_github("grattan/fleeteffSim")
```

## Example

``` r
library(fleeteffSim)
## basic example code
```

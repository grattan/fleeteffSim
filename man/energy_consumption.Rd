% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_data_documentation.R
\docType{data}
\name{energy_consumption}
\alias{energy_consumption}
\title{Energy consumption}
\format{
A \code{tibble} object with 3 variables:
\describe{
\item{\code{vehicle_type}}{The energy price scenario to be assumed. Options are "central",
 "low_price", "high_price", and "off_peak". The central and off-peak scenarios assume a constant electricity price. The low price option assume electricity prices decrease over time, while the high price estimate assumes an increase in prices over time.}
\item{\code{year}}{The year the value applies to}
\item{\code{energy_consumption}}{The assumed energy consumption (kW per kilometre)}
}
}
\usage{
energy_consumption
}
\description{
The assumed future energy consumption, in kW consumed per kilometer, of electric vehicles by vehicle type. Data is adapted from ICCT data described here:
(https://theicct.org/sites/default/files/publications/EV_cost_2020_2030_20190401.pdf).
}
\keyword{datasets}

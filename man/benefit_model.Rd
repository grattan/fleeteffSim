% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benefit_model.R
\name{benefit_model}
\alias{benefit_model}
\title{Benefit model}
\arguments{
\item{.fleet}{The output of 'compliance_cost' function, with a compliant simulated fleet}

\item{.km_travelled}{Defaults to \code{km_travelled}. The assumed distance travelled by each vehicle per year, depending on vehicle age and type.}

\item{.fuel_prices}{Defaults to \code{fuel_prices_tax}. The assumed future price of fuel.}

\item{.electricity_prices}{Defaults to \code{electricity_prices}. the assumed future price of electricity.}

\item{.energy_consumption}{Defaults to \code{energy_consumption}. The assumed future energy consumption of electric vehicles, in kWh/km travelled.}

\item{.energy_intensity}{Defaults to \code{energy_intensity}. The assumed future energy intensity of the electricity grid, in gCO2 equivalent per wH.}

\item{.gap}{Defaults to 1.2. The assumed gap between tested and real world emissions. 1.2 is equivalent to 20\%}

\item{.fuel_scenario}{Defaults to "central". The assumed fuel price scenario from the \code{.in_fuel_prices} \code{tibble}}

\item{.electricity_scenario}{Defaults to "central". The assumed electricity price scenario from the \code{.in_electricity_prices} \code{tibble}}

\item{.run_to_year}{Defaults to 2060. The year to which the benefit model will run. Given an assumed vehicle lifetime of 17 years, to account for full vehicle lifetime costs/benefits/emissions, this should be set to over 17 years after the last running year in \code{compliance_costs()}}

\item{.passenger_diesel}{faults to  0.02. The assumed proportion of passenger vehicles requiring diesel fuel.}

\item{.suv_diesel}{Defaults to 0.21. The assumed proportion of SUV vehicles requiring diesel fuel.}

\item{.lcv_diesel}{Defaults to 0.92. The assumed proportion of LCV vehicles requiring diesel fuel.}

\item{.premium_95}{Defaults to 0.15. The assumed proportion of ICE vehicles requiring premium 95ron petrol.}

\item{.premium_98}{Defaults to 0.05. The assumed proportion of ICE vehicles requiring premium 98ron petrol.}
}
\value{
A \code{tibble} including the fuel, emissions, costs for each vehicle in the simulated fleet
over it' lifespan
}
\description{
A function that estimates the running costs and carbon emissions of a simulated vehicle fleet, over each vehicles lifetime and expected usage.
}

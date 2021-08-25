# setup file
# by Lachlan Fox, Grattan Institute
# SET UP =======================================================================
# Packages ---------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(purrr)
library(glue)
library(fst)
library(janitor)
library(grattantheme)
library(spatstat)
library(ggtext)
library(zoo)
library(readxl)
library(data.table)
library(readr)
# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)

#Project values

us_aus_exchange <- 1.30
us_inflation_2017 <- 1.11
us_inflation_2015 <- 1.15

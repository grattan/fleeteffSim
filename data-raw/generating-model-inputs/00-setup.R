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

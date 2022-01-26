#
# import libraries
# libraries used for this aplication
#

library(shiny)
library(dplyr)
library(glue)
library(highcharter)
library(sparkline)
library(lubridate)
library(purrr)
library(visNetwork)
library(shinyWidgets)
library(shinycssloaders)
library(httr)
library(future)
library(promises)
library(roxygen2)
library(mathjaxr)

# utils codes
source("utils/em_function.R")
source("utils/df_geyser.R")
source("utils/ui-utils.R")

# modes codes

source("modules/module_data.R")

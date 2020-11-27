library(data.table)
library(feather)
library(readr)
library(stringr)
library(shiny)
library(DT)
library(shinyWidgets)
library(magrittr)
library(httr)
library(rjson)
library(writexl)
library(readxl)
library(xml2)
library(RPostgres)
library(DBI)
library(tidyverse)
library(batchtools)
# for (i in paste0("source/", list.files("source/"))) {
#   source(i)
# }

for (i in paste0("modules/", list.files("modules/"))) {
  source(i)
}

for (i in paste0("R/", list.files("R/"))) {
  source(i)
}

options(shiny.maxRequestSize = 100 * 1024 ^ 3)
library(data.table)
library(feather)
library(readr)
library(stringr)
library(stringi)
library(shiny)
library(DT)
library(shinyWidgets)
library(httr)
library(rjson)
library(writexl)
library(readxl)
library(xml2)
library(RPostgres)
library(DBI)
library(tidyverse)
library(batchtools)
library(lubridate)
library(parallel)
library(shinycssloaders)
library(shinyAce)
unlink(".RData")

for (i in paste0("modules/", list.files("modules/"))) {
  source(i)
}

for (i in paste0("R/", list.files("R/"))) {
  source(i)
}

options(shiny.maxRequestSize = 100 * 1024 ^ 3)

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DATABASE_NAME"),
  user = Sys.getenv("DATABASE_USER"),
  password = Sys.getenv("DATABASE_PW"),
  host = Sys.getenv("DATABASE_HOST"),
  port = Sys.getenv("DATABASE_PORT"),
  sslmode = "require")

onStop(function() {
  dbDisconnect(conn = conn)
})
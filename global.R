library(data.table)
library(feather)
library(readr)
library(stringr)
library(shiny)
library(DT)
library(shinyWidgets)


# for (i in paste0("source/", list.files("source/"))) {
#   source(i)
# }

for (i in paste0("modules/", list.files("modules/"))) {
  source(i)
}

options(shiny.maxRequestSize = 100 * 1024 ^ 3)
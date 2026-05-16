library(shiny)
library(shinyjs)
library(dplyr)
library(httr)
library(xml2)
library(stringr)
library(DT)
devtools::load_all()
#source("R/mod_goes_downloader.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),

  titlePanel("Mi app principal"),

  mod_goes_downloader_ui("goesdl")
)

server <- function(input, output, session) {

  mod_goes_downloader_server("goesdl")

}

shinyApp(ui = ui, server = server)

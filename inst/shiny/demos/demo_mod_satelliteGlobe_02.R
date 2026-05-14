# ------------------------------------------------------------
# App de prueba
# ------------------------------------------------------------
devtools::load_all()
ui <- fluidPage(
  mod_satelliteGlobe_02_ui("globe")
)

server <- function(input, output, session) {
  mod_satelliteGlobe_02_server("globe")
}

shinyApp(ui, server)

# ------------------------------------------------------------
# App de prueba
# ------------------------------------------------------------
devtools::load_all()
ui <- fluidPage(
  mod_satelliteGlobe_selected_ui("globe")
)

server <- function(input, output, session) {
  mod_satelliteGlobe_selected_server("globe")
}

shinyApp(ui, server)

# ============================================================
# APP COMPLETA DE EJEMPLO
# ============================================================
devtools::load_all()
ui <- fluidPage(
  mod_satelliteGlobe_08_ui("earth")
)

server <- function(input, output, session) {
  mod_satelliteGlobe_08_server("earth")
}

shinyApp(ui, server)

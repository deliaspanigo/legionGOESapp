# --- LANZADOR DE LA APP ---
devtools::load_all()
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  mod_launcher_02_ui("launchpad_instancia_1")



)



server <- function(input, output, session) {
  mod_launcher_02_server(id = "launchpad_instancia_1")



}

shinyApp(ui, server)

library(shiny)
library(leaflet)

# --- UI ---
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),

  # Script para forzar idioma español en los tiles
  tags$script(HTML('
    // Forzar que OpenStreetMap use español
    window.LANG = "es";
  ')),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(
    top = 20, right = 20,
    draggable = TRUE,
    style = "z-index: 1000; background: rgba(255, 255, 255, 0.9); padding: 15px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.2); width: 220px;",
    h4("Map Controls", style = "margin-top: 0; color: #007bff;"),
    radioButtons(
      inputId = "basemap",
      label = "Select Background:",
      choices = list(
        "OpenStreetMap (Español)" = "osm_es",
        "OpenStreetMap (Estándar)" = "osm",
        "Satelital (Esri)" = "esri_sat",
        "Dark Mode (CartoDB)" = "carto_dark",
        "Terrain (Stamen)" = "terrain"
      ),
      selected = "osm_es"
    ),
    hr(),
    helpText("Drag this panel to move it.")
  )
)

# --- SERVER ---
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "osm") %>%
      setView(lng = -60, lat = -34, zoom = 4)
  })

  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearTiles()

    if (input$basemap == "osm_es") {
      # Versión en español de OpenStreetMap
      proxy %>% addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      )
    } else if (input$basemap == "osm") {
      proxy %>% addTiles()
    } else if (input$basemap == "esri_sat") {
      proxy %>% addProviderTiles(providers$Esri.WorldImagery)
    } else if (input$basemap == "carto_dark") {
      proxy %>% addProviderTiles(providers$CartoDB.DarkMatter)
    } else if (input$basemap == "terrain") {
      proxy %>% addProviderTiles(providers$Esri.WorldTerrain)
    }
  })
}

shinyApp(ui, server)

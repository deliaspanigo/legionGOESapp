library(shiny)
library(leaflet)

# --- UI ---
ui <- bootstrapPage(
  # CSS para asegurar que el mapa ocupe TODA la pantalla
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),

  # El mapa de fondo
  leafletOutput("map", width = "100%", height = "100%"),

  # Panel flotante sobre el mapa
  absolutePanel(
    top = 20, right = 20, # Posición: esquina superior derecha
    draggable = TRUE,     # Permite mover el menú con el mouse
    style = "z-index: 1000; background: rgba(255, 255, 255, 0.9); padding: 15px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.2); width: 220px;",

    h4("Map Controls", style = "margin-top: 0; color: #007bff;"),

    radioButtons(
      inputId = "basemap",
      label = "Select Background:",
      choices = list(
        "OpenStreetMap" = "osm",
        "Satelital (Esri)" = "esri_sat",
        "Dark Mode (CartoDB)" = "carto_dark",
        "Terrain (Stamen)" = "terrain"
      ),
      selected = "osm"
    ),

    hr(),
    helpText("Drag this panel to move it.")
  )
)

# --- SERVER ---
server <- function(input, output, session) {

  # Renderizar el mapa inicial
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "osm") %>% # Fondo por defecto
      setView(lng = -60, lat = -34, zoom = 4) # Centrado inicial
  })

  # Observador para cambiar el fondo dinámicamente sin recargar todo el mapa
  observe({
    proxy <- leafletProxy("map")

    # Limpiar capas previas
    proxy %>% clearTiles()

    # Aplicar la nueva capa según la selección
    if (input$basemap == "osm") {
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

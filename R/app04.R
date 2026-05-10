library(shiny)
library(leaflet)
library(sf)  # Para trabajar con geometrías

# Coordenadas aproximadas de las Islas Malvinas
malvinas_coords <- list(
  lng = -59.5,
  lat = -51.75
)

# --- UI ---
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
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

  # Mapa inicial
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "osm") %>%
      setView(lng = -60, lat = -34, zoom = 4) %>%
      # Agregar marcador invisible con label personalizado
      addMarkers(
        lng = malvinas_coords$lng,
        lat = malvinas_coords$lat,
        label = HTML("<strong>Islas Malvinas</strong>"),
        labelOptions = labelOptions(
          noHide = TRUE,           # Siempre visible
          direction = "right",
          offset = c(20, -20),
          style = list(
            "font-weight" = "bold",
            "color" = "#d9534f",
            "background" = "rgba(255,255,255,0.9)",
            "padding" = "5px 10px",
            "border-radius" = "5px",
            "border" = "1px solid #d9534f"
          )
        ),
        icon = makeIcon(         # Icono invisible
          iconUrl = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
          iconWidth = 1, iconHeight = 1
        ),
        options = markerOptions(opacity = 0)
      )
  })

  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearTiles()

    if (input$basemap == "osm") {
      proxy %>% addTiles()
    } else if (input$basemap == "esri_sat") {
      proxy %>% addProviderTiles(providers$Esri.WorldImagery)
    } else if (input$basemap == "carto_dark") {
      proxy %>% addProviderTiles(providers$CartoDB.DarkMatter)
    } else if (input$basemap == "terrain") {
      proxy %>% addProviderTiles(providers$Esri.WorldTerrain)
    }

    # Volver a agregar el label después de cambiar capas
    proxy %>% addMarkers(
      lng = malvinas_coords$lng,
      lat = malvinas_coords$lat,
      label = HTML("<strong>Islas Malvinas</strong>"),
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = "right",
        offset = c(20, -20),
        style = list(
          "font-weight" = "bold",
          "color" = "#d9534f",
          "background" = "rgba(255,255,255,0.9)",
          "padding" = "5px 10px",
          "border-radius" = "5px",
          "border" = "1px solid #d9534f"
        )
      ),
      icon = makeIcon(
        iconUrl = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
        iconWidth = 1, iconHeight = 1
      ),
      options = markerOptions(opacity = 0)
    )
  })
}

shinyApp(ui, server)

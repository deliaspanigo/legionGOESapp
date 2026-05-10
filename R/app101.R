library(shiny)
library(leaflet)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),

    tags$style(HTML("
      html, body {
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }

      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        width: 100%;
        height: 100%;
      }

      #map {
        position: fixed !important;
        top: 0;
        left: 0;
        width: 100vw !important;
        height: 100vh !important;
        z-index: 1;
      }

      #floating_menu {
        position: absolute;
        top: 20px;
        left: 20px;
        width: 280px;
        background: white;
        border-radius: 12px;
        z-index: 9999;
        box-shadow: 0 4px 15px rgba(0,0,0,0.35);
        font-family: Arial, sans-serif;
        overflow: visible !important;
      }

      #floating_menu_header {
        background: #2c3e50;
        color: white;
        padding: 10px 14px;
        font-weight: bold;
        cursor: move;
        user-select: none;
        border-radius: 12px 12px 0 0;
      }

      #floating_menu_body {
        padding: 14px;
        overflow: visible !important;
      }

      #floating_menu .form-group {
        margin-bottom: 8px;
      }

      /* Esto hace que las opciones del select queden sobre el mapa */
      .selectize-control {
        z-index: 10000 !important;
      }

      .selectize-input {
        z-index: 10000 !important;
      }

      .selectize-dropdown {
        z-index: 10001 !important;
      }

      .selectize-dropdown-content {
        z-index: 10001 !important;
      }
    ")),

    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('#floating_menu').draggable({
          handle: '#floating_menu_header',
          containment: 'window',
          scroll: false
        });
      });
    "))
  ),

  leafletOutput("map"),

  div(
    id = "floating_menu",

    div(
      id = "floating_menu_header",
      "Mover menú"
    ),

    div(
      id = "floating_menu_body",
      selectInput(
        inputId = "base_map",
        label = "Fondo del mapa:",
        choices = c(
          "OpenStreetMap" = "osm",
          "CartoDB claro" = "carto_light",
          "CartoDB oscuro" = "carto_dark",
          "Satélite Esri" = "esri_sat",
          "Topográfico Esri" = "esri_topo",
          "Relieve Esri" = "esri_terrain"
        ),
        selected = "osm"
      )
    )
  )
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      setView(lng = -64.1888, lat = -31.4201, zoom = 5) %>%
      addProviderTiles(providers$OpenStreetMap)
  })

  observeEvent(input$base_map, {

    proxy <- leafletProxy("map") %>%
      clearTiles()

    if (input$base_map == "osm") {
      proxy %>% addProviderTiles(providers$OpenStreetMap)
    }

    if (input$base_map == "carto_light") {
      proxy %>% addProviderTiles(providers$CartoDB.Positron)
    }

    if (input$base_map == "carto_dark") {
      proxy %>% addProviderTiles(providers$CartoDB.DarkMatter)
    }

    if (input$base_map == "esri_sat") {
      proxy %>% addProviderTiles(providers$Esri.WorldImagery)
    }

    if (input$base_map == "esri_topo") {
      proxy %>% addProviderTiles(providers$Esri.WorldTopoMap)
    }

    if (input$base_map == "esri_terrain") {
      proxy %>% addProviderTiles(providers$Esri.WorldTerrain)
    }
  })
}

shinyApp(ui, server)

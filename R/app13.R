library(shiny)
library(leaflet)
library(httr)

# --- Función para obtener TLE reales (Simplificada para el ejemplo) ---
# En un entorno de producción, descargarías desde:
# https://celestrak.org/NORAD/elements/resource.txt
get_sat_coords <- function(sat_name) {
  # Simulador de órbita polar (Landsat/Sentinel)
  # Un satélite polar completa una vuelta en ~100 min
  time_factor <- as.numeric(Sys.time()) / 60

  # Latitud oscila entre -80 y 80 (Órbita polar)
  lat <- 80 * sin(time_factor / 15.9)
  # Longitud se desplaza hacia el oeste por la rotación terrestre
  lng <- ((time_factor / 2) %% 360) - 180

  return(data.frame(name = sat_name, lat = lat, lng = lng))
}

ui <- bootstrapPage(
  tags$style("html, body {width:100%; height:100%; padding:0; margin:0;}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 50, style = "background: white; padding: 10px; border-radius: 5px;",
                h4("Seguimiento Satelital"),
                checkboxInput("track", "Activar rastreo en tiempo real", TRUE),
                verbatimTextOutput("coords"))
)

server <- function(input, output, session) {

  # Refresco cada 2 segundos para ver el movimiento fluido
  timer <- reactiveTimer(2000)

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels) %>%
      # Tu etiqueta personalizada obligatoria
      addLabelOnlyMarkers(lng = -59.5, lat = -51.75, label = "Islas Malvinas",
                          labelOptions = labelOptions(noHide = T, textOnly = T,
                                                      style = list("color"="white", "font-weight"="bold"))) %>%
      setView(0, 0, 2)
  })

  observe({
    req(input$track)
    timer()

    # Calculamos posiciones actuales
    l8 <- get_sat_coords("Landsat 8")
    s2 <- get_sat_coords("Sentinel-2A")
    # Offset para que no se encimen en la simulación
    s2$lng <- s2$lng + 40

    proxy <- leafletProxy("map")

    # Dibujar Landsat (Rojo)
    proxy %>% clearGroup("sats") %>%
      addCircleMarkers(data = l8, ~lng, ~lat, color = "red", group = "sats",
                       label = "Landsat 8", radius = 10) %>%
      # Dibujar Sentinel (Verde)
      addCircleMarkers(data = s2, ~lng, ~lat, color = "#00ff00", group = "sats",
                       label = "Sentinel-2A", radius = 10)

    output$coords <- renderPrint({
      list(Landsat8 = l8, Sentinel2 = s2)
    })
  })
}

shinyApp(ui, server)

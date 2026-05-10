library(shiny)
library(leaflet)
library(sp)

# Función simplificada para simular posición (En producción usarías TLE real)
# Aquí definimos las posiciones de los GOES (Geoestacionarios)
goes_stations <- data.frame(
  name = c("GOES-16 (East)", "GOES-17 (West)", "GOES-18 (Central)"),
  lng = c(-75.2, -137.2, -137.0),
  lat = c(0, 0, 0)
)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%;}"),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(
    top = 10, right = 10, style = "z-index:1000; background:rgba(255,255,255,0.8); padding:15px; border-radius:10px;",
    h3("Satellite Tracker"),
    checkboxGroupInput("show_sat", "Ver Satélites:",
                       choices = c("GOES", "Landsat 8/9", "Sentinel-2"),
                       selected = c("GOES")),
    hr(),
    textOutput("time_display")
  )
)

server <- function(input, output, session) {

  # Timer para refrescar posición cada 5 segundos
  autoInvalidate <- reactiveTimer(5000)

  output$time_display <- renderText({
    autoInvalidate()
    paste("UTC Time:", Sys.time())
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      # Agregamos los nombres en español como pediste antes
      addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels) %>%
      setView(lng = -60, lat = -30, zoom = 3) %>%
      # Etiqueta fija para Malvinas
      addLabelOnlyMarkers(lng = -59.5, lat = -51.75, label = "Islas Malvinas",
                          labelOptions = labelOptions(noHide = T, textOnly = T,
                                                      style = list("color"="white", "font-weight"="bold")))
  })

  observe({
    autoInvalidate() # Se activa cada 5 segundos
    proxy <- leafletProxy("map")

    # 1. Dibujar GOES (Son fijos respecto a la Tierra)
    if("GOES" %in% input$show_sat) {
      proxy %>% clearGroup("goes") %>%
        addCircleMarkers(data = goes_stations, lng = ~lng, lat = ~lat,
                         group = "goes", color = "cyan", radius = 8,
                         label = ~name)
    } else { proxy %>% clearGroup("goes") }

    # 2. Simulación de Satélites de órbita baja (Landsat/Sentinel)
    # En un caso real, aquí calcularías la posición con la librería 'orbital'
    if("Landsat 8/9" %in% input$show_sat) {
      # Simulación de movimiento orbital
      t <- as.numeric(Sys.time()) / 100
      lng_ls <- (t %% 360) - 180
      lat_ls <- 40 * sin(t / 10)

      proxy %>% clearGroup("landsat") %>%
        addMarkers(lng = lng_ls, lat = lat_ls, group = "landsat",
                   icon = makeIcon(iconUrl = "https://img.icons8.com/color/48/satellite.png", 24, 24),
                   label = "Landsat 9")
    } else { proxy %>% clearGroup("landsat") }
  })
}

shinyApp(ui, server)

library(shiny)
library(leaflet)

# ============================================================
# DATOS DE SATÉLITES (posiciones y órbitas)
# ============================================================

# Satélites GEOESTACIONARIOS (fijos sobre el ecuador)
# Longitud = punto sobre el que están fijos
geoestacionarios <- data.frame(
  nombre = c("GOES-16", "GOES-17", "GOES-19", "METEOSAT-11", "METEOSAT-12", "Himawari-8", "Himawari-9"),
  longitud = c(-75, -137, -75, 0, 0, 140, 140),  # Grados Oeste/Este
  latitud = 0,  # Sobre el ecuador
  descripcion = c("GOES-Este (América)", "GOES-Oeste (Pacífico)", "GOES-Este (nuevo)",
                  "Europa/África", "Europa/África (nuevo)", "Asia-Pacífico", "Asia-Pacífico (backup)"),
  color = "#ff4444",
  icono = "🛰️"
)

# Satélites de ÓRBITA BAJA (se mueven)
# Datos orbitales simplificados: inclinación, velocidad angular (grados/segundo simulado)
orbita_baja <- data.frame(
  nombre = c("Landsat 8", "Landsat 9", "Sentinel-2A", "Sentinel-2B",
             "ISS", "Aqua", "Terra", "NOAA-20", "Suomi NPP"),
  inclinacion = c(98.2, 98.2, 98.5, 98.5, 51.6, 98.2, 98.2, 98.7, 98.7),  # Grados
  velocidad = c(0.8, 0.8, 0.9, 0.9, 1.2, 0.7, 0.7, 0.85, 0.85),  # Grados/segundo simulado
  longitud_inicial = c(-60, -65, -50, -55, -70, -40, -45, -80, -85),
  latitud_inicial = c(-34, -30, -20, -25, -28, -15, -10, -40, -35),
  color = c("#33ff33", "#33ff33", "#ffaa33", "#ffaa33", "#33aaff", "#ff33aa", "#aa33ff", "#ff6633", "#33ffaa"),
  descripcion = c("Imágenes terrestres NASA/USGS", "Imágenes terrestres NASA/USGS",
                  "Copernicus (UE)", "Copernicus (UE)", "Estación Espacial Internacional",
                  "NASA (agua)", "NASA (tierra)", "NOAA (clima)", "NOAA (clima)"),
  icono = c("🛰️", "🛰️", "🛰️", "🛰️", "🚀", "🛰️", "🛰️", "🛰️", "🛰️")
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: linear-gradient(135deg, #0a0a2a 0%, #0a0a3a 100%); color: white; }
      .well { background: rgba(0,0,0,0.6); backdrop-filter: blur(10px); border: 1px solid #00aaff; border-radius: 15px; }
      .control-label, h4 { color: #00aaff; }
      .btn { border-radius: 20px; font-weight: bold; }
      .sat-info { font-size: 12px; color: #ccc; }
    "))
  ),

  titlePanel(tagList(
    icon("satellite", class = "fa-2x"),
    "🛰️ Rastreo de Satélites en Tiempo Real"
  )),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(style = "text-align: center; margin-bottom: 20px;",
          h4("🌍 Control de Simulación"),
          p("Velocidad x5 (tiempo real simulando los datos satelitales)")
      ),

      actionButton("play", "▶️ INICIAR", class = "btn-success btn-block"),
      actionButton("stop", "⏹️ DETENER", class = "btn-danger btn-block"),
      br(),

      h4("📡 Satélites en Mapa"),
      selectInput("show_sats", "Mostrar:",
                  choices = c("Todos" = "all",
                              "Geoestacionarios" = "geo",
                              "Órbita Baja" = "leo")),

      hr(),
      h4("📍 Posiciones Actuales"),
      div(class = "sat-info",
          verbatimTextOutput("sat_positions", placeholder = TRUE)
      ),

      hr(),
      p("✅ Los satélites GEOESTACIONARIOS (GOES, METEOSAT, Himawari) están FIJOS sobre el ecuador"),
      p("✅ Los satélites de ÓRBITA BAJA (Landsat, Sentinel, ISS) se mueven en tiempo real")
    ),

    mainPanel(
      width = 9,
      leafletOutput("mapa", height = "700px"),
      div(style = "text-align: center; margin-top: 10px;",
          p("🔴 Rojo: Geoestacionarios | 🟢 Verde: Landsat | 🟡 Naranja: Sentinel | 🔵 Azul: ISS | Otros colores según misión"))
    )
  )
)

server <- function(input, output, session) {

  # Estado de los satélites en movimiento
  sat_positions <- reactiveValues()

  # Inicializar posiciones de satélites en órbita baja
  for(i in 1:nrow(orbita_baja)) {
    sat_positions[[paste0("lon_", i)]] <- orbita_baja$longitud_inicial[i]
    sat_positions[[paste0("lat_", i)]] <- orbita_baja$latitud_inicial[i]
  }

  running <- reactiveVal(FALSE)
  timer <- reactiveVal(NULL)

  # Función para actualizar posiciones de satélites en órbita baja
  actualizar_todos <- function() {
    for(i in 1:nrow(orbita_baja)) {
      # Movimiento longitudinal (órbita principal)
      lon_key <- paste0("lon_", i)
      current_lon <- sat_positions[[lon_key]]
      new_lon <- (current_lon + orbita_baja$velocidad[i]) %% 360
      if(new_lon > 180) new_lon <- new_lon - 360
      sat_positions[[lon_key]] <- new_lon

      # Movimiento latitudinal (oscilación por inclinación orbital)
      lat_key <- paste0("lat_", i)
      # Simular órbita inclinada: latitud varía sinusoidalmente con la longitud
      inclinacion_rad <- orbita_baja$inclinacion[i] * pi / 180
      new_lat <- orbita_baja$inclinacion[i] * sin(new_lon * pi / 180)
      # Limitar a ±inclinación
      new_lat <- min(max(new_lat, -orbita_baja$inclinacion[i]), orbita_baja$inclinacion[i])
      sat_positions[[lat_key]] <- new_lat
    }
  }

  # Función para actualizar todo el mapa
  update_map <- function() {
    proxy <- leafletProxy("mapa")
    proxy %>% clearMarkers() %>% clearControls()

    # 1. Agregar satélites GEOESTACIONARIOS (fijos)
    geo_to_show <- if(input$show_sats == "leo") c() else 1:nrow(geoestacionarios)

    for(i in geo_to_show) {
      proxy <- proxy %>% addCircleMarkers(
        lng = geoestacionarios$longitud[i],
        lat = geoestacionarios$latitud[i],
        radius = 12,
        color = geoestacionarios$color[i],
        fillColor = geoestacionarios$color[i],
        fillOpacity = 0.9,
        popup = paste0("<b>", geoestacionarios$icono[i], " ", geoestacionarios$nombre[i], "</b><br>",
                       "<i>Tipo:</i> Geoestacionario<br>",
                       "<i>Posición:</i> ", abs(geoestacionarios$longitud[i]), "°",
                       ifelse(geoestacionarios$longitud[i] < 0, "O", "E"), "<br>",
                       "<i>Descripción:</i> ", geoestacionarios$descripcion[i]),
        label = paste(geoestacionarios$nombre[i], "📡"),
        group = "geo"
      )
    }

    # 2. Agregar satélites en ÓRBITA BAJA (en movimiento)
    leo_to_show <- if(input$show_sats == "geo") c() else 1:nrow(orbita_baja)

    for(i in leo_to_show) {
      lon_key <- paste0("lon_", i)
      lat_key <- paste0("lat_", i)

      proxy <- proxy %>% addCircleMarkers(
        lng = sat_positions[[lon_key]],
        lat = sat_positions[[lat_key]],
        radius = 10,
        color = orbita_baja$color[i],
        fillColor = orbita_baja$color[i],
        fillOpacity = 0.85,
        popup = paste0("<b>", orbita_baja$icono[i], " ", orbita_baja$nombre[i], "</b><br>",
                       "<i>Tipo:</i> Órbita Baja (LEO)<br>",
                       "<i>Latitud:</i> ", round(sat_positions[[lat_key]], 2), "°<br>",
                       "<i>Longitud:</i> ", round(sat_positions[[lon_key]], 2), "°<br>",
                       "<i>Inclinación:</i> ", orbita_baja$inclinacion[i], "°<br>",
                       "<i>Descripción:</i> ", orbita_baja$descripcion[i]),
        label = paste(orbita_baja$nombre[i], "🛰️"),
        group = "leo"
      )
    }

    # Actualizar panel de posiciones
    output$sat_positions <- renderText({
      texto <- "=== SATÉLITES GEOESTACIONARIOS (FIJOS) ===\n"
      for(i in geo_to_show) {
        texto <- paste0(texto, geoestacionarios$nombre[i], ": ",
                        abs(geoestacionarios$longitud[i]), "°",
                        ifelse(geoestacionarios$longitud[i] < 0, "O", "E"), " sobre ecuador\n")
      }

      texto <- paste0(texto, "\n=== ÓRBITA BAJA (EN MOVIMIENTO) ===\n")
      for(i in leo_to_show) {
        texto <- paste0(texto, orbita_baja$nombre[i], ": ",
                        sprintf("%.1f°", sat_positions[[paste0("lat_", i)]]),
                        ifelse(sat_positions[[paste0("lat_", i)]] >= 0, "N", "S"), " / ",
                        sprintf("%.1f°", abs(sat_positions[[paste0("lon_", i)]]), ""),
                        ifelse(sat_positions[[paste0("lon_", i)]] >= 0, "E", "O"), "\n")
      }
      texto
    })
  }

  # Bucle de animación
  observeEvent(input$play, {
    if(!running()) {
      running(TRUE)

      # Crear timer
      new_timer <- reactiveTimer(500)
      timer(new_timer)

      obs <- observe({
        new_timer()
        if(running()) {
          actualizar_todos()
          update_map()
        }
      })

      # Guardar observer para limpiar después
      session$onSessionEnded(function() {
        obs$destroy()
      })
    }
  })

  observeEvent(input$stop, {
    running(FALSE)
  })

  observeEvent(input$show_sats, {
    update_map()
  })

  # Mapa inicial
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -60, lat = 0, zoom = 2) %>%
      addScaleBar(position = "bottomleft") %>%
      addControlGPS()
  })

  # Inicializar posiciones
  for(i in 1:nrow(orbita_baja)) {
    sat_positions[[paste0("lon_", i)]] <- orbita_baja$longitud_inicial[i]
    sat_positions[[paste0("lat_", i)]] <- orbita_baja$latitud_inicial[i]
  }

  update_map()
}

shinyApp(ui, server)

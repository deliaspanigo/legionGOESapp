library(shiny)
library(threejs)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #0a0a2a; color: white; }
      .well { background-color: rgba(0,0,0,0.7); border: 1px solid #444; }
    "))
  ),

  titlePanel("🛰️ Simulación de Trayectoria Satelital"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("play", "▶️ Iniciar", icon = icon("play"),
                   class = "btn-success btn-block"),
      actionButton("stop", "⏹️ Detener", icon = icon("stop"),
                   class = "btn-danger btn-block"),
      hr(),
      h4("📍 Posición actual:"),
      verbatimTextOutput("posicion"),
      hr(),
      p("💡 NOTA: El globo se actualiza CADA VEZ que presionas INICIAR"),
      p("(La animación continua NO es posible con threejs::globejs)")
    ),

    mainPanel(
      width = 9,
      globeOutput("orbita", height = "700px")
    )
  )
)

server <- function(input, output, session) {

  # Estado de la animación
  valores <- reactiveValues(
    lat = -34.6,   # Buenos Aires
    lon = -58.4,
    paso = 1,
    corriendo = FALSE
  )

  # Bucle de actualización - PERO OJO: esto NO actualiza el globo en tiempo real
  observe({
    if(valores$corriendo) {
      invalidateLater(100)
      # Actualizar posición
      valores$lon <- (valores$lon + valores$paso) %% 360
      valores$lat <- 20 * sin(valores$lon * pi / 180)
    }
  })

  # Mostrar posición actual
  output$posicion <- renderText({
    sprintf("Lat: %.2f°\nLon: %.2f°", valores$lat, valores$lon)
  })

  # ACTUALIZAR EL GLOBO solo cuando cambia la posición (esto SÍ funciona)
  observeEvent(c(valores$lat, valores$lon, valores$corriendo), {
    # Solo actualizar si está corriendo o en estado inicial
    isolate({
      output$orbita <- renderGlobe({
        punto_color <- if(valores$corriendo) "#ff3333" else "#33ff33"

        globejs(
          lat = valores$lat,
          long = valores$lon,
          color = punto_color,
          pointsize = 6,
          atmosphere = TRUE,
          bg = "black",
          bodycolor = "#222222",
          lightcolor = "#ffffff",
          emissive = "#333333"
        )
      })
    })
  })

  observeEvent(input$play, {
    valores$corriendo <- TRUE
    # Forzar actualización inicial
    valores$lon <- valores$lon + 0.01
  })

  observeEvent(input$stop, {
    valores$corriendo <- FALSE
  })
}

shinyApp(ui, server)

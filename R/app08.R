library(shiny)
library(threejs)

# Órbita simulada (por ejemplo, Estación Espacial Internacional)
# Datos orbitales simplificados: lat/lon cambian con el tiempo
ui <- fluidPage(
  actionButton("play", "▶️ Iniciar simulación"),
  actionButton("stop", "⏹️ Detener"),
  globeOutput("orbita", height = "700px")
)

server <- function(input, output, session) {

  valores <- reactiveValues(
    lat = 0,
    lon = 0,
    corriendo = FALSE
  )

  # Bucle de actualización
  observe({
    if(valores$corriendo) {
      invalidateLater(100)  # Actualizar cada 100ms
      valores$lon <- (valores$lon + 1) %% 360  # Avance longitudinal
      valores$lat <- 20 * sin(valores$lon * pi / 180)  # Oscilación latitudinal
    }
  })

  observeEvent(input$play, { valores$corriendo <- TRUE })
  observeEvent(input$stop, { valores$corriendo <- FALSE })

  output$orbita <- renderGlobe({
    globejs(
      lat = valores$lat,
      long = valores$lon,
      color = "red",
      pointsize = 5,
      atmosphere = TRUE,
      bg = "black"
    )
  })
}

shinyApp(ui, server)

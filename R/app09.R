library(shiny)
library(threejs)

# Generar puntos en órbitas concéntricas
generar_orbita <- function(n_puntos, radio, inclinacion) {
  set.seed(123)
  theta <- runif(n_puntos, 0, 2*pi)
  phi <- runif(n_puntos, -inclinacion, inclinacion)

  # Convertir a lat/lon
  lat <- phi * 180/pi
  lon <- theta * 180/pi

  return(data.frame(lat, lon))
}

ui <- fluidPage(
  sliderInput("altura", "Altitud de datos (radio):",
              min = 0.2, max = 2, value = 0.5, step = 0.05),
  sliderInput("n_puntos", "Cantidad de puntos:",
              min = 10, max = 500, value = 100),
  globeOutput("nube", height = "700px")
)

server <- function(input, output, session) {

  datos_orbita <- reactive({
    req(input$n_puntos)
    generar_orbita(input$n_puntos, input$altura, inclinacion = 30)
  })

  output$nube <- renderGlobe({
    df <- datos_orbita()

    # Altura de los puntos (distancia desde el centro)
    # Cuanto más lejos, más "flotan"
    alturas <- rep(input$altura, nrow(df))

    globejs(
      lat = df$lat,
      long = df$lon,
      value = alturas * 10,  # Controla la distancia radial
      color = rainbow(nrow(df)),
      pointsize = 2,
      atmosphere = TRUE,
      bg = "black",
      bodycolor = "#111"
    )
  })
}

shinyApp(ui, server)

library(shiny)
library(threejs)

ui <- fluidPage(
  style = "background-color: #050505; color: white;",
  tags$head(tags$style(HTML("body, html {height: 100%; overflow: hidden;}"))),

  column(12, align = "center",
         h2("Rscience: Global Orbit Visualization"),
         globeOutput("globe", width = "100%", height = "85vh")
  )
)

server <- function(input, output, session) {

  output$globe <- renderGlobe({
    # Generamos la órbita completa (360 grados) para que tenga sentido visual
    steps <- seq(0, 360, length.out = 500)

    # --- Landsat 8 (Órbita Polar Real) ---
    # Una órbita polar pasa por los polos (Lat 90 a -90)
    l8_path <- data.frame(
      lat = 90 * sin(steps * pi/180),
      lon = (steps * 0.5) %% 360 - 180, # Ligera inclinación por rotación terrestre
      color = "red",
      size = 0.1
    )

    # --- Sentinel-2 (Órbita Helio-sincrónica) ---
    s2_path <- data.frame(
      lat = 85 * cos(steps * pi/180),
      lon = (steps * 0.8 + 60) %% 360 - 180,
      color = "#00ff00",
      size = 0.1
    )

    # --- Posición Actual (El "Satélite" hoy) ---
    # Calculamos un solo punto más grande para representar dónde están AHORA
    t_actual <- as.numeric(Sys.time()) / 100
    actual_sats <- data.frame(
      lat = c(90 * sin(t_actual), 85 * cos(t_actual + 2)),
      lon = c((t_actual * 5) %% 360 - 180, (t_actual * 4 + 60) %% 360 - 180),
      color = c("red", "#00ff00"),
      size = 1.5 # Más grande para que destaque
    )

    # Malvinas como referencia
    malvinas <- data.frame(lat = -51.75, lon = -59.5, color = "white", size = 0.8)

    all_data <- rbind(l8_path, s2_path, actual_sats, malvinas)

    globejs(
      img = system.file("images/world.jpg", package="threejs"),
      lat = all_data$lat,
      long = all_data$lon,
      color = all_data$color,
      pointsize = all_data$size,
      atmosphere = TRUE,
      bg = "black"
    )
  })
}

shinyApp(ui, server)

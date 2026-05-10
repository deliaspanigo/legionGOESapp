# app.R
# Globo 3D rotatorio con Sentinel y Landsat en Shiny
# Requisitos:
# install.packages(c("shiny", "plotly", "dplyr"))

library(shiny)
library(plotly)
library(dplyr)

# -----------------------------
# Funciones auxiliares
# -----------------------------

make_sphere <- function(n_lon = 90, n_lat = 45, r = 1) {
  lon <- seq(-pi, pi, length.out = n_lon)
  lat <- seq(-pi / 2, pi / 2, length.out = n_lat)

  x <- outer(cos(lat), cos(lon)) * r
  y <- outer(cos(lat), sin(lon)) * r
  z <- outer(sin(lat), rep(1, length(lon))) * r

  list(x = x, y = y, z = z)
}

# Convierte lat/lon a coordenadas 3D sobre una esfera
latlon_to_xyz <- function(lat, lon, r = 1.08) {
  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180

  data.frame(
    x = r * cos(lat_rad) * cos(lon_rad),
    y = r * cos(lat_rad) * sin(lon_rad),
    z = r * sin(lat_rad)
  )
}

# Simulación visual de órbitas tipo polar/heliosincrónica.
# No es efeméride real: sirve para visualización dinámica.
sat_position <- function(name, t, inclination = 98, period_sec = 6000, phase = 0, r = 1.15) {
  theta <- 2 * pi * ((t %% period_sec) / period_sec) + phase
  inc <- inclination * pi / 180

  # Órbita inclinada simple
  x0 <- cos(theta)
  y0 <- sin(theta)
  z0 <- 0

  x <- x0
  y <- y0 * cos(inc) - z0 * sin(inc)
  z <- y0 * sin(inc) + z0 * cos(inc)

  data.frame(
    satellite = name,
    x = r * x,
    y = r * y,
    z = r * z
  )
}

sat_orbit <- function(inclination = 98, phase = 0, r = 1.15, n = 240) {
  theta <- seq(0, 2 * pi, length.out = n) + phase
  inc <- inclination * pi / 180

  x0 <- cos(theta)
  y0 <- sin(theta)
  z0 <- 0

  data.frame(
    x = r * x0,
    y = r * (y0 * cos(inc) - z0 * sin(inc)),
    z = r * (y0 * sin(inc) + z0 * cos(inc))
  )
}

# Algunos puntos de referencia para que el globo no sea una esfera vacía
reference_points <- data.frame(
  city = c("Córdoba", "Roma", "Washington", "Tokyo", "Sydney", "Cape Town"),
  lat = c(-31.42, 41.90, 38.90, 35.68, -33.86, -33.92),
  lon = c(-64.18, 12.50, -77.04, 139.69, 151.21, 18.42)
) %>%
  bind_cols(latlon_to_xyz(.$lat, .$lon, r = 1.025))

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { background-color: #0b1020; color: #f4f7fb; }\n      .well { background-color: #141b34; border: 1px solid #26314f; }\n      .control-label { color: #f4f7fb; }\n      h2, h4 { color: #ffffff; }\n      .selectize-input { background: #ffffff; }\n    "))
  ),

  titlePanel("Globo 3D con Sentinel y Landsat"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controles"),
      checkboxInput("rotate", "Rotar cámara automáticamente", TRUE),
      sliderInput("speed", "Velocidad visual", min = 0.2, max = 5, value = 1.5, step = 0.1),
      checkboxGroupInput(
        "satellites",
        "Satélites visibles",
        choices = c("Sentinel-2A", "Sentinel-2B", "Landsat 8", "Landsat 9"),
        selected = c("Sentinel-2A", "Sentinel-2B", "Landsat 8", "Landsat 9")
      ),
      helpText("Esta versión usa movimiento orbital simulado para visualización. Para posiciones reales se puede conectar CelesTrak/NORAD TLE o una API orbital.")
    ),

    mainPanel(
      width = 9,
      plotlyOutput("globe", height = "720px")
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {

  start_time <- Sys.time()

  timer <- reactiveTimer(100)

  current_t <- reactive({
    timer()
    as.numeric(difftime(Sys.time(), start_time, units = "secs")) * input$speed
  })

  output$globe <- renderPlotly({
    t <- current_t()
    earth <- make_sphere()

    # Rotación visual de cámara
    camera_angle <- if (isTRUE(input$rotate)) t * 0.18 else 0.8

    camera <- list(
      eye = list(
        x = 2.1 * cos(camera_angle),
        y = 2.1 * sin(camera_angle),
        z = 1.15
      )
    )

    sat_specs <- data.frame(
      satellite = c("Sentinel-2A", "Sentinel-2B", "Landsat 8", "Landsat 9"),
      inclination = c(98.62, 98.62, 98.20, 98.20),
      period_sec = c(5900, 5900, 5950, 5950),
      phase = c(0, pi / 1.8, pi / 3, pi / 3 + pi / 1.7),
      color = c("#ff4d4d", "#ff9f43", "#00d2d3", "#54a0ff")
    ) %>%
      filter(satellite %in% input$satellites)

    p <- plot_ly() %>%
      add_surface(
        x = earth$x,
        y = earth$y,
        z = earth$z,
        opacity = 0.95,
        showscale = FALSE,
        colorscale = list(
          c(0, "#123c69"),
          c(0.45, "#1b75bb"),
          c(0.55, "#2ecc71"),
          c(1, "#f6e58d")
        ),
        hoverinfo = "skip"
      ) %>%
      add_markers(
        data = reference_points,
        x = ~x, y = ~y, z = ~z,
        text = ~city,
        hoverinfo = "text",
        marker = list(size = 3, color = "white", opacity = 0.85),
        name = "Referencias"
      )

    if (nrow(sat_specs) > 0) {
      for (i in seq_len(nrow(sat_specs))) {
        spec <- sat_specs[i, ]

        pos <- sat_position(
          name = spec$satellite,
          t = t,
          inclination = spec$inclination,
          period_sec = spec$period_sec,
          phase = spec$phase
        )

        orbit <- sat_orbit(
          inclination = spec$inclination,
          phase = spec$phase
        )

        p <- p %>%
          add_trace(
            data = orbit,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(color = spec$color, width = 2),
            opacity = 0.35,
            hoverinfo = "skip",
            showlegend = FALSE
          ) %>%
          add_markers(
            data = pos,
            x = ~x, y = ~y, z = ~z,
            text = ~satellite,
            hoverinfo = "text",
            marker = list(size = 7, color = spec$color, symbol = "circle"),
            name = spec$satellite
          )
      }
    }

    p %>%
      layout(
        paper_bgcolor = "#0b1020",
        plot_bgcolor = "#0b1020",
        scene = list(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          zaxis = list(visible = FALSE),
          aspectmode = "data",
          camera = camera,
          bgcolor = "#0b1020"
        ),
        legend = list(
          font = list(color = "white"),
          bgcolor = "rgba(20,27,52,0.7)"
        ),
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
  })
}

shinyApp(ui, server)

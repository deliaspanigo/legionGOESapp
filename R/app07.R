# Cargar librerías
library(shiny)
library(threejs)

# Datos de ejemplo
datos_satelitales <- data.frame(
  ciudad = c("Buenos Aires", "Córdoba", "Mendoza", "Bariloche", "Ushuaia", "Malvinas"),
  lat = c(-34.6037, -31.4201, -32.8902, -41.1335, -54.8019, -51.75),
  lon = c(-58.3816, -64.1888, -68.8448, -71.3105, -68.3030, -59.5),
  temperatura = c(25, 22, 20, 12, 5, 8)
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #0a0a2a; color: white; }
      .well { background-color: rgba(0,0,0,0.7); color: white; border: 1px solid #444; }
      .control-label { color: white; }
    "))
  ),

  titlePanel("🌍 Visualización Satelital GOES-19 - Globo 3D"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controles"),

      selectInput("textura",
                  "Textura del Globo:",
                  choices = c(
                    "🌍 Tierra (color)" = "color",
                    "🌙 Tierra (topográfica)" = "topo",
                    "⭐ Tierra (relieve)" = "relieve",
                    "🛰️ Oscuro (GOES nocturno)" = "dark"
                  ),
                  selected = "color"),

      checkboxInput("atmosfera", "🌫️ Mostrar atmósfera", value = TRUE),
      checkboxInput("mostrar_datos", "📍 Mostrar datos satelitales", value = TRUE),

      sliderInput("brillo", "💡 Brillo del globo:", min = 0.3, max = 1.5, value = 0.9, step = 0.05),

      hr(),
      h5("📊 Datos simulados GOES-19"),
      tableOutput("tabla_datos"),

      hr(),
      p("💡 Tips:", style = "font-weight:bold"),
      tags$ul(
        tags$li("🖱️ Arrastrar para rotar el globo"),
        tags$li("🔍 Click derecho + arrastrar para zoom"),
        tags$li("✨ Pasar mouse sobre puntos para ver valores")
      )
    ),

    mainPanel(
      width = 9,
      globeOutput("globo", height = "750px"),
      div(
        style = "text-align: center; margin-top: 10px; color: #aaa;",
        p("🔴 Puntos rojos = temperaturas altas | 🔵 Puntos azules = temperaturas bajas")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Mostrar tabla de datos
  output$tabla_datos <- renderTable({
    if(input$mostrar_datos) {
      datos_satelitales[, c("ciudad", "temperatura")]
    } else {
      data.frame(Info = "Datos ocultos")
    }
  }, digits = 1)

  # Función para obtener la textura correcta (usando URLs que FUNCIONAN)
  obtener_textura <- reactive({
    switch(input$textura,
           "color" = "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/textures/planets/earth_atmos_2048.jpg",
           "topo" = "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/textures/planets/earth_day_2048x1024.jpg",
           "relieve" = "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/textures/planets/earth_normal_2048.jpg",
           "dark" = "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/textures/planets/earth_nightmap_2048.jpg",
           "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/textures/planets/earth_atmos_2048.jpg"
    )
  })

  # Colores según temperatura
  colores_puntos <- reactive({
    if(input$mostrar_datos) {
      temp <- datos_satelitales$temperatura
      # Escala de colores: rojo (caliente) a azul (frío)
      r <- (temp - min(temp)) / (max(temp) - min(temp))
      rgb(r, 0.2, 1 - r, maxColorValue = 1)
    } else {
      rep("rgba(255,255,255,0.5)", nrow(datos_satelitales))
    }
  })

  # Valores para altura de barras
  valores_barras <- reactive({
    if(input$mostrar_datos) {
      # Normalizar temperaturas para altura
      temp <- datos_satelitales$temperatura
      (temp - min(temp)) / (max(temp) - min(temp)) * 8 + 1
    } else {
      rep(0, nrow(datos_satelitales))
    }
  })

  # Renderizar el globo
  output$globo <- renderGlobe({
    textura_url <- obtener_textura()

    if(input$mostrar_datos) {
      # Globo con datos superpuestos y MEJOR ILUMINACIÓN
      globejs(
        img = textura_url,
        lat = datos_satelitales$lat,
        long = datos_satelitales$lon,
        value = valores_barras(),
        color = colores_puntos(),
        pointsize = 2.5,
        atmosphere = input$atmosfera,
        bg = "black",
        bodycolor = "#222222",
        lightcolor = "#ffffff",
        emissive = "#333333",      # Luz propia
        specular = "#555555",      # Reflectividad
        shininess = 25,            # Brillo
        rotationlat = -0.3,
        rotationlong = -0.8,
        fov = 35,                  # Campo de visión
        alpha = 0.9,
        label = paste0(datos_satelitales$ciudad, ": ",
                       round(datos_satelitales$temperatura, 1), "°C")
      )
    } else {
      # Solo globo sin datos
      globejs(
        img = textura_url,
        atmosphere = input$atmosfera,
        bg = "black",
        bodycolor = "#222222",
        lightcolor = "#ffffff",
        emissive = "#333333",
        fov = 35
      )
    }
  })

  # Observador para cambios que requieren recarga
  observeEvent(c(input$textura, input$atmosfera, input$mostrar_datos), {
    # Forzar recarga del globo
    output$globo <- renderGlobe({
      textura_url <- obtener_textura()

      if(input$mostrar_datos) {
        globejs(
          img = textura_url,
          lat = datos_satelitales$lat,
          long = datos_satelitales$lon,
          value = valores_barras(),
          color = colores_puntos(),
          pointsize = 2.5,
          atmosphere = input$atmosfera,
          bg = "black",
          bodycolor = "#222222",
          lightcolor = "#ffffff",
          emissive = "#333333",
          specular = "#555555",
          shininess = 25,
          rotationlat = -0.3,
          rotationlong = -0.8,
          fov = 35,
          alpha = 0.9,
          label = paste0(datos_satelitales$ciudad, ": ",
                         round(datos_satelitales$temperatura, 1), "°C")
        )
      } else {
        globejs(
          img = textura_url,
          atmosphere = input$atmosfera,
          bg = "black",
          bodycolor = "#222222",
          lightcolor = "#ffffff",
          emissive = "#333333",
          fov = 35
        )
      }
    })
  })
}

# Ejecutar
shinyApp(ui = ui, server = server)

# Cargar librerías
library(shiny)
library(threejs)

# Datos de ejemplo (coordenadas de ciudades argentinas con datos satelitales simulados)
datos_satelitales <- data.frame(
  ciudad = c("Buenos Aires", "Córdoba", "Mendoza", "Bariloche", "Ushuaia", "Malvinas"),
  lat = c(-34.6037, -31.4201, -32.8902, -41.1335, -54.8019, -51.75),
  lon = c(-58.3816, -64.1888, -68.8448, -71.3105, -68.3030, -59.5),
  temperatura = c(25, 22, 20, 12, 5, 8),   # Simula temperatura satelital
  intensidad = c(0.8, 0.6, 0.5, 0.4, 0.2, 0.3)  # Intensidad para colorear
)

# UI
ui <- fluidPage(
  titlePanel("🌍 Visualización Satelital GOES - Globo 3D Interactivo"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controles"),

      selectInput("textura",
                  "Textura del Globo:",
                  choices = c(
                    "Por defecto (Tierra)" = "default",
                    "Mapa base" = "mapa",
                    "Satelital simple" = "satelite",
                    "Oscuro (para datos nocturnos)" = "oscuro"
                  ),
                  selected = "default"),

      checkboxInput("atmosfera",
                    "Mostrar atmósfera",
                    value = TRUE),

      checkboxInput("mostrar_datos",
                    "Mostrar datos satelitales",
                    value = TRUE),

      sliderInput("opacidad",
                  "Opacidad de los puntos:",
                  min = 0, max = 1, value = 0.8, step = 0.05),

      hr(),
      p("Globo 3D interactivo. Puedes:"),
      tags$ul(
        tags$li("Arrastrar con el mouse para rotar"),
        tags$li("Click derecho + arrastrar para hacer zoom"),
        tags$li("Hover sobre puntos para ver valores")
      ),
      hr(),
      p("Los puntos representan datos simulados del satélite GOES-19"),
      p("(Temperatura superficial en °C)")
    ),

    mainPanel(
      width = 9,
      # Aquí se renderiza el globo 3D
      globeOutput("globo", height = "700px"),
      br(),
      div(
        style = "text-align: center;",
        p("📍 Los puntos rojos indican temperaturas más altas, los azules más bajas")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Generar colores según temperatura
  colores <- reactive({
    if(input$mostrar_datos) {
      # Escala de color: rojo (caliente), amarillo (templado), azul (frío)
      cols <- colorRampPalette(c("darkblue", "cyan", "yellow", "red"))(100)
      colores_puntos <- cols[cut(datos_satelitales$temperatura, breaks = 100)]
      return(colores_puntos)
    } else {
      return(rep("grey", nrow(datos_satelitales)))
    }
  })

  # Seleccionar textura según la opción elegida
  textura_imagen <- reactive({
    switch(input$textura,
           "default" = system.file("images/world.jpg", package = "threejs"),
           "mapa" = "https://threejs.org/examples/textures/planets/earth_atmos_2048.jpg",
           "satelite" = "https://upload.wikimedia.org/wikipedia/commons/9/97/The_earth_at_night.jpg",
           "oscuro" = "https://threejs.org/examples/textures/planets/earth_nightmap_2048.jpg",
           system.file("images/world.jpg", package = "threejs")
    )
  })

  # Función para formatear tooltip
  tooltip_text <- reactive({
    if(input$mostrar_datos) {
      paste0(datos_satelitales$ciudad, ": ",
             round(datos_satelitales$temperatura, 1), "°C")
    } else {
      rep("", nrow(datos_satelitales))
    }
  })

  # Renderizar el globo
  output$globo <- renderGlobe({
    # Validar que la textura existe
    textura <- tryCatch({
      textura_imagen()
    }, error = function(e) {
      system.file("images/world.jpg", package = "threejs")
    })

    if(input$mostrar_datos) {
      # Globo con datos superpuestos
      globejs(
        img = textura,
        lat = datos_satelitales$lat,
        long = datos_satelitales$lon,
        value = datos_satelitales$intensidad * 10,  # Altura de las barras
        color = colores(),
        pointsize = 1.5,
        atmosphere = input$atmosfera,
        bg = "black",
        bodycolor = "#111111",
        lightcolor = "#ffffff",
        rotationlat = -0.5,
        rotationlong = -1.2,
        fov = 45,
        alpha = input$opacidad / 2,
        label = tooltip_text()
      )
    } else {
      # Globo sin datos (solo textura)
      globejs(
        img = textura,
        atmosphere = input$atmosfera,
        bg = "black",
        bodycolor = "#111111",
        lightcolor = "#ffffff",
        fov = 45
      )
    }
  })
}

# Ejecutar la app
shinyApp(ui = ui, server = server)

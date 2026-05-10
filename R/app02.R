library(shiny)
library(leaflet)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      # 1. FONDO: Mapa base sin ninguna etiqueta (Base mofológica)
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%

      # 2. RESTO DEL MUNDO: Etiquetas oficiales (ciudades, países, etc.)
      addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>%

      setView(lng = -59.5, lat = -51.75, zoom = 7) %>%

      # 3. TUS ETIQUETAS: Personalización total sobre las islas
      addLabelOnlyMarkers(
        lng = -59.5, lat = -51.75,
        label = "ISLAS MALVINAS",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "#444",         # Color gris oscuro similar al de CartoDB
            "font-family" = "serif",  # Fuente elegante
            "font-weight" = "bold",
            "font-size" = "18px"
          )
        )
      ) %>%
      # Agregamos puntos clave con nombres en español
      addMarkers(lng = -57.85, lat = -51.69, label = "Puerto Argentino") %>%
      addLabelOnlyMarkers(lng = -58.5, lat = -51.4, label = "Isla Soledad",
                          labelOptions = labelOptions(noHide = T, textOnly = T)) %>%
      addLabelOnlyMarkers(lng = -60.1, lat = -51.4, label = "Isla Gran Malvina",
                          labelOptions = labelOptions(noHide = T, textOnly = T))
  })
}

shinyApp(ui, server)

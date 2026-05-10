library(shiny)
library(leaflet)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() %>%
      # 1. CAPA BASE: Mapa mudo (sin nombres de ningún país o ciudad)
      # Esto garantiza que el nombre en inglés NO EXISTA en el mapa.
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%

      # 2. CAPA DE ETIQUETAS: Solo nombres del resto del mundo
      # Nota: Estas etiquetas suelen ser transparentes y se ven sobre el fondo
      addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>%

      # 3. TU ETIQUETA PERSONALIZADA:
      # Ubicada estratégicamente para que sea el único nombre en esa zona
      setView(lng = -59.5, lat = -51.75, zoom = 7) %>%

      addLabelOnlyMarkers(
        lng = -59.5, lat = -51.75,
        label = "ISLAS MALVINAS",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "#444",
            "font-family" = "serif",
            "font-weight" = "bold",
            "font-size" = "16px"
          )
        )
      ) %>%
      # Agregamos nombres específicos de la geografía en español
      addLabelOnlyMarkers(lng = -58.4, lat = -51.3, label = "Isla Soledad",
                          labelOptions = labelOptions(noHide = T, textOnly = T,
                                                      style = list("font-size" = "12px", "color" = "#666"))) %>%
      addLabelOnlyMarkers(lng = -60.2, lat = -51.3, label = "Isla Gran Malvina",
                          labelOptions = labelOptions(noHide = T, textOnly = T,
                                                      style = list("font-size" = "12px", "color" = "#666")))
  })
}

shinyApp(ui, server)

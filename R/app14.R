library(shiny)
library(leaflet)
library(dplyr)

# --- Motor de Órbita ---
get_sat_trajectory <- function(sat_name, offset_lng = 0, hours_ahead = 0) {
  time_factor <- (as.numeric(Sys.time()) / 60) + (hours_ahead * 60)
  lat <- 80 * sin(time_factor / 15.9)
  lng <- (((time_factor / 2) + offset_lng) %% 360) - 180
  return(data.frame(name = sat_name, lat = lat, lng = lng,
                    time = Sys.time() + (hours_ahead * 3600),
                    stringsAsFactors = FALSE))
}

ui <- bootstrapPage(
  tags$style("html, body {width:100%; height:100%; padding:0; margin:0;}"),
  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(top = 10, left = 50, style = "background: white; padding: 15px; border-radius: 8px; width: 250px; z-index:1000;",
                h4("Satellite Control"),
                checkboxInput("show_tail", "Show Tail (Estela)", TRUE),
                checkboxInput("show_path", "Predict 24h Path", FALSE),
                actionButton("clear_tail", "Clear Tail", class="btn-sm btn-danger"),
                hr(),
                verbatimTextOutput("coords"))
)

server <- function(input, output, session) {

  timer <- reactiveTimer(2000)

  # Usamos un reactiveValues para la persistencia
  vals <- reactiveValues(history = data.frame())

  observeEvent(input$clear_tail, { vals$history <- data.frame() })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels) %>%
      addLabelOnlyMarkers(lng = -59.5, lat = -51.75, label = "Islas Malvinas",
                          labelOptions = labelOptions(noHide = T, textOnly = T,
                                                      style = list("color"="white", "font-weight"="bold"))) %>%
      setView(-60, -35, 3)
  })

  # El observador principal gatillado por el timer
  observeEvent(timer(), {
    # 1. Posiciones actuales
    l8 <- get_sat_trajectory("Landsat 8", offset_lng = 0)
    s2 <- get_sat_trajectory("Sentinel-2A", offset_lng = 40)
    current_sats <- rbind(l8, s2)

    # 2. Lógica de la Estela (Tail)
    if (input$show_tail) {
      # Añadimos los nuevos puntos al histórico
      vals$history <- rbind(vals$history, current_sats) %>% tail(300)
    }

    proxy <- leafletProxy("map")

    # 3. Dibujar Estela (Círculos pequeños y estables)
    if (input$show_tail && nrow(vals$history) > 0) {
      proxy %>% clearGroup("tail_group") %>%
        addCircleMarkers(data = vals$history, ~lng, ~lat,
                         color = "white", radius = 1, weight = 1,
                         opacity = 0.4, fillOpacity = 0.4, group = "tail_group")
    } else {
      proxy %>% clearGroup("tail_group")
    }

    # 4. Dibujar Predicción 24hs
    proxy %>% clearGroup("pred_group")
    if (input$show_path) {
      future_steps <- seq(0, 24, length.out = 80)
      path_l8 <- do.call(rbind, lapply(future_steps, function(h) get_sat_trajectory("Landsat 8", 0, h)))
      path_s2 <- do.call(rbind, lapply(future_steps, function(h) get_sat_trajectory("Sentinel-2A", 40, h)))

      proxy %>%
        addPolylines(data = path_l8, ~lng, ~lat, color = "red", weight = 1, dashArray = "4", group = "pred_group") %>%
        addPolylines(data = path_s2, ~lng, ~lat, color = "#00ff00", weight = 1, dashArray = "4", group = "pred_group")
    }

    # 5. Dibujar Satélites Actuales
    proxy %>% clearGroup("live_group") %>%
      addCircleMarkers(data = current_sats, ~lng, ~lat,
                       color = ifelse(current_sats$name == "Landsat 8", "red", "#00ff00"),
                       fillOpacity = 0.9, radius = 8, group = "live_group",
                       popup = paste0("<b>", current_sats$name, "</b><br>Time: ", format(current_sats$time, "%H:%M:%S")))

    output$coords <- renderPrint({ current_sats })
  })
}

shinyApp(ui, server)

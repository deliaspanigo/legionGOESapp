# R/mod_satellite_globe.R

library(shiny)

# ============================================================
# MÓDULO INTERMEDIO UI
# Agrega botón de salida sin tocar el módulo 3D interno
# ============================================================

mod_satelliteGlobe_selected_ui <- function(
    id,
    home_button_left = "20px",
    home_button_bottom = "20px",
    height = "100vh"
) {
  ns <- NS(id)

  root_id <- ns("satellite_globe_selected_root")

  tagList(
    tags$head(
      tags$style(HTML(sprintf("
        /* ====================================================
           CSS AUTOCONTENIDO DEL WRAPPER DEL GLOBO
           Todo queda limitado a #%s
        ==================================================== */

        #%s {
          position: relative !important;
          width: 100%% !important;
          height: %s !important;
          min-height: %s !important;
          overflow: hidden !important;
          margin: 0 !important;
          padding: 0 !important;
        }

        #%s .satellite-globe-home-floating {
          position: absolute !important;
          left: %s !important;
          bottom: %s !important;
          z-index: 20000 !important;

          width: auto;
          min-width: 130px;
          border: 1px solid rgba(255,255,255,0.25) !important;
          border-radius: 12px !important;
          padding: 10px 14px !important;

          background: rgba(249, 115, 22, 0.92) !important;
          color: white !important;

          cursor: pointer;
          font-weight: bold !important;
          box-shadow: 0 10px 30px rgba(0,0,0,0.35);
          backdrop-filter: blur(8px);
          pointer-events: auto;
        }

        #%s .satellite-globe-home-floating:hover {
          background: rgba(234, 88, 12, 0.98) !important;
          color: white !important;
        }

        #%s .earth3d-module-root {
          width: 100%% !important;
          height: 100%% !important;
          min-height: 100%% !important;
          max-height: 100%% !important;
        }

        #%s .earth3d-container {
          width: 100%% !important;
          height: 100%% !important;
          min-height: 100%% !important;
          max-height: 100%% !important;
        }

        #%s canvas {
          display: block !important;
        }
      ",
                              root_id,
                              root_id,
                              height,
                              height,
                              root_id,
                              home_button_left,
                              home_button_bottom,
                              root_id,
                              root_id,
                              root_id,
                              root_id
      )))
    ),

    div(
      id = root_id,

      mod_satelliteGlobe_09_ui(ns("globe_core")),

      actionButton(
        inputId = ns("btn_go_home"),
        label = "← Launcher",
        class = "satellite-globe-home-floating"
      )
    )
  )
}


# ============================================================
# MÓDULO INTERMEDIO SERVER
# ============================================================

mod_satelliteGlobe_selected_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    mod_satelliteGlobe_09_server("globe_core")

    list(
      go_home = reactive(input$btn_go_home)
    )
  })
}

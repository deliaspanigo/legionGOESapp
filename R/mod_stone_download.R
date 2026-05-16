# ==============================================================================
# WRAPPER PARA MODULO DE DESCARGA
# Agrega botón de salida y permite scroll interno
# ==============================================================================

library(shiny)

mod_stone_download_ui <- function(
    id,
    home_button_left = "20px",
    home_button_bottom = "20px",
    height = "100vh"
) {
  ns <- NS(id)

  root_id <- ns("download_selected_root")
  content_id <- ns("download_selected_content")

  tagList(
    tags$head(
      tags$style(HTML(sprintf("
        /* ====================================================
           WRAPPER AUTOCONTENIDO PARA DOWNLOAD
           Todo queda limitado a #%s
        ==================================================== */

        #%s {
          position: relative !important;
          width: 100%% !important;
          height: %s !important;
          min-height: %s !important;
          max-height: %s !important;
          margin: 0 !important;
          padding: 0 !important;
          overflow: hidden !important;
          background: #f2f4f7 !important;
        }

        #%s {
          position: absolute !important;
          inset: 0 !important;
          width: 100%% !important;
          height: 100%% !important;
          overflow-y: auto !important;
          overflow-x: hidden !important;
          padding: 0 0 80px 0 !important;
          box-sizing: border-box !important;
          background: #f2f4f7 !important;
        }

        #%s .goes-downloader-root {
          width: 100%% !important;
          min-height: 100%% !important;
          height: auto !important;
          overflow: visible !important;
          box-sizing: border-box !important;
          padding-bottom: 90px !important;
        }

        #%s .download-home-floating {
          position: fixed !important;
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

        #%s .download-home-floating:hover {
          background: rgba(234, 88, 12, 0.98) !important;
          color: white !important;
        }
      ",
                              root_id,
                              root_id,
                              height,
                              height,
                              height,
                              content_id,
                              content_id,
                              root_id,
                              home_button_left,
                              home_button_bottom,
                              root_id
      )))
    ),

    div(
      id = root_id,

      div(
        id = content_id,
        # mod_goes_downloader_ui(ns("download_core"))
        legionGOES.downloader::mod_goes_downloader_selected_ui(ns("download_core"))
      ),

      actionButton(
        inputId = ns("btn_go_home"),
        label = "← Launcher",
        class = "download-home-floating"
      )
    )
  )
}


mod_stone_download_server <- function(id, str_folder_path_data_raw) {
  moduleServer(id, function(input, output, session) {

    legionGOES.downloader::mod_goes_downloader_selected_server("download_core", str_folder_path_data_raw = str_folder_path_data_raw)
    # mod_goes_downloader_server("download_core")

    list(
      go_home = reactive(input$btn_go_home)
    )
  })
}

# ==============================================================================
# ORQUESTADOR RSCIENCE 2027 - BSLIB + LOGO FIX
# ==============================================================================
library(shiny)
library(bslib)
library(shinyjs)

# Nota: Al estar ejecutando desde la carpeta local,
# devtools::load_all("../../../") cargará tu paquete legionGOESapp
devtools::load_all()

# UI - page_fillable
ui <- page_fillable(
  title = "LegionGoes - GIS Analysis",
  padding = 0,
  gap = 0,
  fillable_mobile = TRUE,

  # theme = bs_theme(
  #   version = 5,
  #   bg = "#0b1218",
  #   fg = "#ffffff",
  #   primary = "#00d4ff"
  # ),

  shinyjs::useShinyjs(),

  tags$head(
    # tags$style(HTML("
    #   html, body {
    #     margin: 0 !important;
    #     padding: 0 !important;
    #     width: 100%;
    #     height: 100%;
    #     overflow: hidden;
    #     background: #0b1218;
    #   }
    #
    #   body > .container-fluid {
    #     padding: 0 !important;
    #     margin: 0 !important;
    #     max-width: none !important;
    #     width: 100% !important;
    #   }
    #
    #   .bslib-page-fill {
    #     padding: 0 !important;
    #     margin: 0 !important;
    #     gap: 0 !important;
    #   }
    #
    #   .tab-content,
    #   .tab-pane,
    #   .nav-hidden-content {
    #     padding: 0 !important;
    #     margin: 0 !important;
    #     width: 100%;
    #     height: 100%;
    #   }
    # ")),

    tags$link(rel = "icon", type = "image/png", href = paste0("logo.png?v=", Sys.time())),
    tags$link(rel = "shortcut icon", href = paste0("logo.png?v=", Sys.time())),
    tags$link(rel = "apple-touch-icon", href = paste0("logo.png?v=", Sys.time()))
  ),

  navset_hidden(
    id = "main_nav",

    nav_panel_hidden(
      value = "page_launchpad",
      mod_01_launchpad_ui("launchpad_v1")
    ),

    nav_panel_hidden(
      value = "page_engine",
      mod_satelliteGlobe_ui("sat01")
    ),
    nav_panel_hidden(
      value = "page_glm",
      mod_glm_ui("glm01")
    )
  )
)

# SERVER
server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Estado global de navegación
  # ---------------------------------------------------------------------------
  current_tab <- reactiveVal("page_launchpad")

  # Guarda el último trigger del launchpad ya procesado
  last_launchpad_trigger <- reactiveVal(0)

  # ---------------------------------------------------------------------------
  # Módulos
  # ---------------------------------------------------------------------------

  launchpad_res <- mod_01_launchpad_server("launchpad_v1")

  mod_satelliteGlobe_server("sat01")

  mod_glm_server("glm01")

  # ---------------------------------------------------------------------------
  # Navegación: Launchpad -> Engine
  # ---------------------------------------------------------------------------

  observeEvent(launchpad_res(), {
    status <- launchpad_res()

    req(!is.null(status$nav_trigger))
    req(!is.null(status$target_page))

    # Si este trigger ya fue procesado, no hacer nada
    if (status$nav_trigger <= last_launchpad_trigger()) {
      return()
    }

    # Registrar que ya procesamos este trigger
    last_launchpad_trigger(status$nav_trigger)

    if (status$target_page == "engine") {
      current_tab("page_engine")
    }

    if (status$target_page == "glm") {
      current_tab("page_glm")
    }

  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # Navegación: Engine -> Launchpad
  # ---------------------------------------------------------------------------

  observeEvent(input[["sat01-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)


  observeEvent(input[["glm01-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # Único lugar donde realmente se cambia el navset
  # ---------------------------------------------------------------------------

  observeEvent(current_tab(), {
    nav_select(
      id = "main_nav",
      selected = current_tab(),
      session = session
    )
  }, ignoreInit = FALSE)
}

# LANZAR APLICACIÓN
shinyApp(
  ui = ui,
  server = server
)

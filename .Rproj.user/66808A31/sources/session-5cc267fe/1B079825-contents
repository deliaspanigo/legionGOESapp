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
  # Título que aparece en la pestaña
  title = "LegionGoes - GIS Analysis",
  theme = bs_theme(
    version = 5,
    bg = "#0b1218",
    fg = "#ffffff",
    primary = "#00d4ff"
  ),

  # Inicializar shinyjs
  shinyjs::useShinyjs(),

  # Configuración del Logo (Favicon)
  tags$head(
    # IMPORTANTE: Shiny sirve lo que está en /www directamente en la raíz "/"
    # Por eso usamos "logo.png" y no "www/logo.png"
    tags$link(rel = "icon", type = "image/png", href =  paste0("logo.png?v=", Sys.time())),
    tags$link(rel = "shortcut icon", href =  paste0("logo.png?v=", Sys.time())),
    tags$link(rel = "apple-touch-icon", href =  paste0("logo.png?v=", Sys.time()))
  ),

  # Estructura de Navegación
  navset_hidden(
    id = "main_nav",

    nav_panel_hidden(
      value = "page_launchpad",
      mod_01_launchpad_ui("launchpad_v1")
    ),

    nav_panel_hidden(
      value = "page_engine",
      mod_satelliteGlobe_ui("sat01")
    )
  )
)

# SERVER
server <- function(input, output, session) {

  # Lógica del Launchpad
  launchpad_res <- mod_01_launchpad_server("launchpad_v1")

  # Lógica del Satélite/Globo
  mod_satelliteGlobe_server("sat01")

  # Navegación: Launchpad -> Engine
  observeEvent(launchpad_res(), {
    status <- launchpad_res()
    req(status$nav_trigger > 0)

    if (status$target_page == "engine") {
      nav_select(
        id = "main_nav",
        selected = "page_engine",
        session = session
      )
    }
  })

  # Navegación: Engine -> Launchpad
  observeEvent(input[["sat01-btn_go_home"]], {
    nav_select(
      id = "main_nav",
      selected = "page_launchpad",
      session = session
    )
  })
}

# LANZAR APLICACIÓN
shinyApp(
  ui = ui,
  server = server
)

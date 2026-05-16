# ==============================================================================
# ORQUESTADOR RSCIENCE 2027 - SIN navset_hidden - FULLSCREEN ESTABLE
# ==============================================================================

source(file = "global.R")
devtools::load_all()

# ==============================================================================
# UI
# ==============================================================================

ui <- page_fillable(
  title = "LegionGoes - GIS Analysis",
  padding = 0,
  gap = 0,
  fillable_mobile = TRUE,

  shinyjs::useShinyjs(),

  tags$head(
    tags$style(HTML("
      html,
      body {
        margin: 0 !important;
        padding: 0 !important;
        width: 100vw !important;
        height: 100vh !important;
        overflow: hidden !important;
        background: #0b1218 !important;
      }

      body > .bslib-page-fill,
      .bslib-page-fill {
        width: 100vw !important;
        height: 100vh !important;
        min-height: 100vh !important;
        padding: 0 !important;
        margin: 0 !important;
        gap: 0 !important;
        overflow: hidden !important;
      }

      #app_pages {
        position: relative !important;
        width: 100vw !important;
        height: 100vh !important;
        min-height: 100vh !important;
        padding: 0 !important;
        margin: 0 !important;
        overflow: hidden !important;
        background: #0b1218 !important;
      }

      .app-page {
        position: absolute !important;
        inset: 0 !important;
        width: 100vw !important;
        height: 100vh !important;
        min-height: 100vh !important;
        padding: 0 !important;
        margin: 0 !important;
        overflow: hidden !important;

        visibility: hidden !important;
        opacity: 0 !important;
        pointer-events: none !important;
        z-index: 0 !important;
      }

      .app-page.active {
        visibility: visible !important;
        opacity: 1 !important;
        pointer-events: auto !important;
        z-index: 10 !important;
      }

      .app-page > * {
        width: 100% !important;
        height: 100% !important;
      }

      .app-page .earth3d-module-root {
        width: 100vw !important;
        height: 100vh !important;
        min-height: 100vh !important;
        max-height: 100vh !important;
      }

      .app-page .earth3d-container {
        width: 100% !important;
        height: 100% !important;
        min-height: 100% !important;
      }

      .app-page canvas {
        display: block !important;
      }
    ")),

    tags$script(HTML("
      window.setLegionPage = function(pageValue) {
        const pages = document.querySelectorAll('#app_pages .app-page');

        pages.forEach(function(page) {
          if (page.dataset.page === pageValue) {
            page.classList.add('active');
          } else {
            page.classList.remove('active');
          }
        });

        if (pageValue === 'page_engine') {
          requestAnimationFrame(function() {
            window.dispatchEvent(new Event('resize'));
          });
        }
      };
    ")),

    tags$link(
      rel = "icon",
      type = "image/png",
      href = paste0("logo.png?v=", Sys.time())
    ),
    tags$link(
      rel = "shortcut icon",
      href = paste0("logo.png?v=", Sys.time())
    ),
    tags$link(
      rel = "apple-touch-icon",
      href = paste0("logo.png?v=", Sys.time())
    )
  ),

  div(
    id = "app_pages",

    div(
      class = "app-page active",
      `data-page` = "page_launchpad",
      mod_launcher_02_ui("launchpad_v1")
    ),

    div(
      class = "app-page",
      `data-page` = "page_engine",
      mod_satelliteGlobe_selected_ui("sat01")
    ),

    div(
      class = "app-page",
      `data-page` = "page_glm",
      mod_glm_ui("glm01")
    ),

    div(
      class = "app-page",
      `data-page` = "page_fdcf",
      mod_fdcf_ui("fdcf01")
    ),

    div(
      class = "app-page",
      `data-page` = "page_lstf",
      mod_lstf_ui("lstf01")
    ),

    div(
      class = "app-page",
      `data-page` = "page_lstf_new",
      mod_lstf_new_ui("lstf_new")
    ),

    div(
      class = "app-page",
      `data-page` = "page_fdcf_new",
      mod_fdcf_new_ui("fdcf_new")
    ),
    div(
      class = "app-page",
      `data-page` = "page_download_new",
      # mod_download_selected_ui("download_new")
      mod_stone_download_ui("download_new")
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  current_tab <- reactiveVal("page_launchpad")
  last_launchpad_trigger <- reactiveVal(0)

  # ---------------------------------------------------------------------------
  # Módulos
  # ---------------------------------------------------------------------------

  launchpad_res <- mod_launcher_02_server("launchpad_v1")

  mod_satelliteGlobe_selected_server("sat01")

  mod_glm_server("glm01")
  mod_fdcf_server("fdcf01")
  mod_lstf_server("lstf01")
  mod_lstf_new_server("lstf_new")
  mod_fdcf_new_server("fdcf_new")

  mod_stone_download_server(id = "download_new", str_folder_path_data_raw = Sys.getenv("LEGION_DOWNLOADS_DIR"))

  # ---------------------------------------------------------------------------
  # Navegación: Launchpad -> páginas
  # ---------------------------------------------------------------------------

  observeEvent(launchpad_res(), {
    status <- launchpad_res()

    req(!is.null(status$nav_trigger))
    req(!is.null(status$target_page))

    if (status$nav_trigger <= last_launchpad_trigger()) {
      return()
    }

    last_launchpad_trigger(status$nav_trigger)

    if (status$target_page == "engine") {
      current_tab("page_engine")
    }

    if (status$target_page == "glm") {
      current_tab("page_glm")
    }

    if (status$target_page == "fdcf") {
      current_tab("page_fdcf")
    }

    if (status$target_page == "lstf") {
      current_tab("page_lstf")
    }

    if (status$target_page == "lstf_new") {
      current_tab("page_lstf_new")
    }

    if (status$target_page == "fdcf_new") {
      current_tab("page_fdcf_new")
    }

    if (status$target_page == "download_new") {
      current_tab("page_download_new")
    }

  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # Navegación: módulos -> Launchpad
  # ---------------------------------------------------------------------------

  observeEvent(input[["sat01-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  observeEvent(input[["glm01-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  observeEvent(input[["fdcf01-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  observeEvent(input[["lstf01-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  observeEvent(input[["lstf_new-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  observeEvent(input[["fdcf_new-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  observeEvent(input[["download_new-btn_go_home"]], {
    current_tab("page_launchpad")
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # Cambio real de página
  # ---------------------------------------------------------------------------

  observeEvent(current_tab(), {
    shinyjs::runjs(
      sprintf(
        "window.setLegionPage('%s');",
        current_tab()
      )
    )
  }, ignoreInit = FALSE)
}

# ==============================================================================
# LANZAR APP
# ==============================================================================

shinyApp(
  ui = ui,
  server = server
)

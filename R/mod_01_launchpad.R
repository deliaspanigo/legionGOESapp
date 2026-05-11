# ==============================================================================
# MÓDULO LAUNCHPAD - v.0.0.1
# GOES Showcase + Launcher + Extra Gallery
# ==============================================================================

library("bslib")
library("shiny")

mod_01_launchpad_ui <- function(id) {
  ns <- NS(id)

  wrapper_id <- ns("launch_wrapper")

  str_package_folder_path <- fn_my_folder_package()

  css_folder <- file.path(str_package_folder_path, "www", "css")
  if (css_folder == "") css_folder <- "www/css"
  try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)

  www_folder <- file.path(str_package_folder_path, "www")
  if (www_folder == "") www_folder <- "www"
  try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

  tagList(
    tags$head(
      shinyjs::useShinyjs(),

      tags$style(HTML(paste0("

        /* ============================================================
           CONTENEDOR PRINCIPAL
        ============================================================ */

        #", wrapper_id, " {
          margin: 0 !important;
          padding: 0 !important;
          height: 100vh !important;
          width: 100vw !important;
          overflow: hidden;
          background: #ffffff;
          font-family: 'Inter', Arial, sans-serif;
          display: block;
          position: relative;
        }

        #", wrapper_id, " .container-fluid {
          padding: 0 !important;
          margin: 0 !important;
        }

        #", wrapper_id, " .main-body {
          display: flex;
          height: 75vh;
          width: 100vw;
          overflow: hidden;
        }

        /* ============================================================
           PANEL IZQUIERDO
        ============================================================ */

        #", wrapper_id, " .left-panel {
          flex: 0 0 38%;
          padding: 40px;
          border-right: 2px solid #00d4ff;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          background: #ffffff;
          z-index: 2;
        }

        #", wrapper_id, " .floating-logo {
          width: 280px;
          margin-bottom: 1rem;
          animation: floatVertical_", id, " 4s ease-in-out infinite;
          filter: drop-shadow(0 10px 15px rgba(0,212,255,0.25));
        }

        @keyframes floatVertical_", id, " {
          0%, 100% {
            transform: translateY(0px);
          }
          50% {
            transform: translateY(-12px);
          }
        }

        #", wrapper_id, " .btn-rscience {
          background: #00d4ff;
          border: none;
          color: white;
          font-weight: 800;
          padding: 14px;
          border-radius: 12px;
          transition: 0.3s;
          text-transform: uppercase;
          letter-spacing: 1px;
        }

        #", wrapper_id, " .btn-rscience:hover {
          transform: translateY(-3px);
          box-shadow: 0 10px 20px rgba(0,212,255,0.4);
          background: #00b8e6;
          color: white;
        }

        #", wrapper_id, " .btn-launch-main {
          width: 320px;
          margin-bottom: 20px;
        }

        #", wrapper_id, " .utility-container {
          display: flex;
          gap: 8px;
          width: 320px;
          justify-content: center;
          margin-bottom: 15px;
        }

        #", wrapper_id, " .btn-utility {
          background: #f8f9fa;
          border: 1px solid #e9ecef;
          color: #6c757d;
          font-weight: 700;
          padding: 8px 5px;
          border-radius: 8px;
          font-size: 0.6rem;
          text-transform: uppercase;
          transition: 0.2s;
          flex: 1;
        }

        #", wrapper_id, " .btn-utility:hover {
          background: #ffffff;
          color: #00d4ff;
          border-color: #00d4ff;
        }

        #", wrapper_id, " .social-bar {
          display: flex;
          gap: 20px;
          margin-top: 5px;
        }

        #", wrapper_id, " .social-icon {
          color: #dee2e6;
          font-size: 1.3rem;
          transition: 0.3s;
          text-decoration: none;
        }

        #", wrapper_id, " .social-icon:hover {
          color: #00d4ff;
          transform: scale(1.1);
        }

        /* ============================================================
           PANEL DERECHO
        ============================================================ */

        #", wrapper_id, " .right-panel {
          flex: 0 0 62%;
          padding: 32px 50px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          background:
            radial-gradient(circle at top right, rgba(0,212,255,0.16), transparent 34%),
            linear-gradient(135deg, #f8fbff 0%, #f7f7f7 100%);
          position: relative;
          overflow: hidden;
        }

        #", wrapper_id, " .toolbar-right {
          display: flex;
          gap: 10px;
          margin-bottom: 22px;
          width: 100%;
          justify-content: center;
          flex-wrap: wrap;
          z-index: 4;
        }

        #", wrapper_id, " .btn-category {
          padding: 10px 20px;
          font-size: 0.8rem;
          width: auto;
          min-width: 120px;
        }

        /* ============================================================
           GOES SHOWCASE
        ============================================================ */

        #", wrapper_id, " .showcase-card {
          width: 100%;
          max-width: 760px;
          height: 390px;
          border-radius: 28px;
          overflow: hidden;
          position: relative;
          background: #000000;
          box-shadow:
            0 24px 55px rgba(0, 30, 60, 0.22),
            0 0 0 1px rgba(0, 212, 255, 0.18);
          isolation: isolate;
        }

        #", wrapper_id, " .showcase-media-wrap {
          position: absolute;
          inset: 0;
          overflow: hidden;
          background: #000000;
        }

        #", wrapper_id, " .showcase-media-img {
          width: 116%;
          height: 116%;
          object-fit: cover;
          object-position: center center;
          animation: satelliteScan_", id, " 18s ease-in-out infinite alternate;
          filter: saturate(1.12) contrast(1.04);
        }

        #", wrapper_id, " .showcase-media-video {
          width: 116%;
          height: 116%;
          object-fit: cover;
          object-position: center center;
          animation: satelliteScan_", id, " 20s ease-in-out infinite alternate;
          filter: saturate(1.12) contrast(1.04);
        }

        @keyframes satelliteScan_", id, " {
          0% {
            transform: scale(1.04) translate(-2%, -2%);
          }
          45% {
            transform: scale(1.15) translate(3%, 1%);
          }
          100% {
            transform: scale(1.10) translate(-1%, 3%);
          }
        }

        #", wrapper_id, " .showcase-shade {
          position: absolute;
          inset: 0;
          z-index: 2;
          background:
            linear-gradient(90deg, rgba(0,0,0,0.72) 0%, rgba(0,0,0,0.30) 43%, rgba(0,0,0,0.06) 100%),
            linear-gradient(0deg, rgba(0,0,0,0.52) 0%, rgba(0,0,0,0.03) 46%);
          pointer-events: none;
        }

        #", wrapper_id, " .showcase-glow {
          position: absolute;
          inset: auto -90px -130px auto;
          width: 320px;
          height: 320px;
          background: radial-gradient(circle, rgba(0,212,255,0.42), transparent 64%);
          z-index: 3;
          pointer-events: none;
        }

        #", wrapper_id, " .showcase-content {
          position: absolute;
          z-index: 4;
          left: 34px;
          bottom: 34px;
          max-width: 470px;
          color: white;
        }

        #", wrapper_id, " .showcase-kicker {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          padding: 6px 10px;
          border-radius: 999px;
          background: rgba(0,212,255,0.20);
          border: 1px solid rgba(255,255,255,0.22);
          backdrop-filter: blur(8px);
          font-size: 0.66rem;
          font-weight: 900;
          text-transform: uppercase;
          letter-spacing: 1.4px;
          margin-bottom: 10px;
        }

        #", wrapper_id, " .showcase-kicker-dot {
          width: 8px;
          height: 8px;
          border-radius: 50%;
          background: #00e5ff;
          box-shadow: 0 0 12px rgba(0,229,255,0.9);
        }

        #", wrapper_id, " .showcase-title {
          font-size: 2.05rem;
          line-height: 1.05;
          font-weight: 900;
          margin: 0 0 8px 0;
          letter-spacing: -0.04em;
          text-shadow: 0 5px 20px rgba(0,0,0,0.55);
        }

        #", wrapper_id, " .showcase-subtitle {
          font-size: 0.94rem;
          line-height: 1.4;
          margin: 0;
          color: rgba(255,255,255,0.88);
          text-shadow: 0 3px 14px rgba(0,0,0,0.45);
        }

        #", wrapper_id, " .showcase-tags {
          display: flex;
          gap: 8px;
          flex-wrap: wrap;
          margin-top: 14px;
        }

        #", wrapper_id, " .showcase-tag {
          padding: 5px 8px;
          border-radius: 8px;
          background: rgba(255,255,255,0.14);
          border: 1px solid rgba(255,255,255,0.18);
          font-size: 0.64rem;
          font-weight: 800;
          text-transform: uppercase;
          letter-spacing: 0.8px;
          backdrop-filter: blur(8px);
        }

        #", wrapper_id, " .showcase-dots {
          display: flex;
          gap: 9px;
          margin-top: 18px;
          justify-content: center;
          align-items: center;
          z-index: 4;
        }

        #", wrapper_id, " .showcase-dot-btn {
          width: 10px;
          height: 10px;
          border-radius: 999px;
          border: none;
          background: #d6dde7;
          transition: 0.35s;
          cursor: pointer;
          padding: 0;
        }

        #", wrapper_id, " .showcase-dot-btn.active {
          width: 34px;
          background: #00d4ff;
          box-shadow: 0 0 14px rgba(0,212,255,0.58);
        }

        #", wrapper_id, " .showcase-caption-row {
          width: 100%;
          max-width: 760px;
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-top: 12px;
          color: #64748b;
          font-size: 0.72rem;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: 0.7px;
        }

        #", wrapper_id, " .showcase-caption-row span:first-child {
          color: #00a8cc;
        }

        /* ============================================================
           GALERÍA EXTRA
        ============================================================ */

        .goes-extra-gallery {
          max-height: 70vh;
          overflow-y: auto;
          padding-right: 6px;
        }

        .goes-extra-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(170px, 1fr));
          gap: 14px;
        }

        .goes-extra-card {
          border: 1px solid rgba(0,0,0,0.10);
          border-radius: 14px;
          overflow: hidden;
          background: #ffffff;
          box-shadow: 0 6px 18px rgba(0,0,0,0.08);
          cursor: pointer;
          transition: transform 0.18s ease, box-shadow 0.18s ease;
        }

        .goes-extra-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 10px 24px rgba(0,212,255,0.22);
        }

        .goes-extra-thumb {
          width: 100%;
          height: 120px;
          object-fit: cover;
          background: #000000;
          display: block;
        }

        .goes-extra-card-title {
          padding: 8px 9px;
          font-size: 0.68rem;
          font-weight: 800;
          color: #334155;
          line-height: 1.2;
          word-break: break-word;
        }

        .goes-extra-video-box {
          width: 100%;
          height: 120px;
          background:
            radial-gradient(circle at center, rgba(0,212,255,0.28), transparent 45%),
            linear-gradient(135deg, #020617, #0f172a);
          display: flex;
          align-items: center;
          justify-content: center;
          color: white;
          font-weight: 900;
          font-size: 0.75rem;
          letter-spacing: 1px;
        }

        .goes-extra-preview-img,
        .goes-extra-preview-video {
          width: 100%;
          max-height: 78vh;
          object-fit: contain;
          background: #000000;
          border-radius: 12px;
        }

        /* ============================================================
           FOOTER LOGOS
        ============================================================ */

        #", wrapper_id, " .footer-logos {
          height: 25vh;
          border-top: 2px solid #00d4ff;
          background: white;
          display: flex;
          flex-direction: column;
          justify-content: center;
          overflow: hidden;
        }

        #", wrapper_id, " .marquee-row {
          display: flex;
          align-items: center;
          width: 100%;
          height: 40px;
          margin: 4px 0;
        }

        #", wrapper_id, " .category-label {
          flex: 0 0 130px;
          font-weight: 800;
          font-size: 0.6rem;
          text-transform: uppercase;
          color: #adb5bd;
          border-right: 2px solid #00d4ff;
          text-align: right;
          padding-right: 15px;
          background: white;
          z-index: 10;
        }

        #", wrapper_id, " .marquee-container {
          flex: 1;
          overflow: hidden;
          display: flex;
        }

        #", wrapper_id, " .marquee-content {
          display: flex;
          width: max-content;
          animation: scrollLeft_", id, " linear infinite;
        }

        #", wrapper_id, " .marquee-content img {
          height: 30px;
          margin: 0 40px;
          opacity: 0.4;
          filter: grayscale(1);
        }

        #", wrapper_id, " .scroll-slow {
          animation-duration: 45s;
        }

        #", wrapper_id, " .scroll-fast {
          animation-duration: 30s;
        }

        @keyframes scrollLeft_", id, " {
          from {
            transform: translateX(0);
          }
          to {
            transform: translateX(-50%);
          }
        }

        /* ============================================================
           RESPONSIVE
        ============================================================ */

        @media (max-width: 1100px) {
          #", wrapper_id, " .main-body {
            flex-direction: column;
            height: 82vh;
          }

          #", wrapper_id, " .left-panel {
            flex: 0 0 auto;
            height: 42vh;
            border-right: none;
            border-bottom: 2px solid #00d4ff;
            padding: 24px;
          }

          #", wrapper_id, " .right-panel {
            flex: 1;
            padding: 20px;
          }

          #", wrapper_id, " .floating-logo {
            width: 210px;
          }

          #", wrapper_id, " .showcase-card {
            height: 280px;
          }

          #", wrapper_id, " .showcase-title {
            font-size: 1.55rem;
          }

          #", wrapper_id, " .footer-logos {
            height: 18vh;
          }
        }

      ")))
    ),

    div(
      id = wrapper_id,

      div(
        class = "main-body",

        div(
          class = "left-panel",

          img(
            src = paste0("WWW-FOLDER/legion_goes_logo_transparent.png?v=", as.numeric(Sys.time())),
            class = "floating-logo"
          ),

          div(
            class = "text-center",
            style = "margin-top: -15px; margin-bottom: 25px;",
            span(
              "v.0.0.1",
              class = "badge bg-dark",
              style = "font-family: monospace;"
            )
          ),

          actionButton(
            ns("btn_enter"),
            "Launch Engine",
            class = "btn-rscience btn-launch-main"
          ),

          div(
            class = "utility-container",
            actionButton(ns("btn_cite"), "Cite", class = "btn-utility"),
            actionButton(ns("btn_contact"), "Contact", class = "btn-utility"),
            actionButton(ns("btn_info"), "Info", class = "btn-utility"),
            actionButton(ns("btn_who"), "Team", class = "btn-utility")
          ),

          div(
            class = "social-bar",
            tags$a(
              href = "https://github.com",
              target = "_blank",
              class = "social-icon",
              icon("github")
            ),
            tags$a(
              href = "https://linkedin.com",
              target = "_blank",
              class = "social-icon",
              icon("linkedin")
            )
          )
        ),

        div(
          class = "content-panel right-panel",

          div(
            class = "toolbar-right",
            actionButton(ns("btn_sec_enter"), "Engine", class = "btn-rscience btn-category"),
            actionButton(ns("btn_glm"), "GLM", class = "btn-rscience btn-category"),
            actionButton(ns("btn_lstf_new"), "LSTF", class = "btn-rscience btn-category"),
            actionButton(ns("btn_fdcf_new"), "FDCF", class = "btn-rscience btn-category"),
            actionButton(ns("btn_extra_gallery"), "Extra", class = "btn-rscience btn-category")
          ),

          div(
            class = "showcase-card",

            div(
              class = "showcase-media-wrap",
              uiOutput(ns("showcase_media_ui"))
            ),

            div(class = "showcase-shade"),
            div(class = "showcase-glow"),

            div(
              class = "showcase-content",

              div(
                class = "showcase-kicker",
                span(class = "showcase-kicker-dot"),
                textOutput(ns("showcase_kicker"), inline = TRUE)
              ),

              h2(
                class = "showcase-title",
                textOutput(ns("showcase_title"), inline = TRUE)
              ),

              p(
                class = "showcase-subtitle",
                textOutput(ns("showcase_subtitle"), inline = TRUE)
              ),

              uiOutput(ns("showcase_tags_ui"))
            )
          ),

          div(
            class = "showcase-caption-row",
            span("GOES Visual Engine"),
            span(textOutput(ns("showcase_counter"), inline = TRUE))
          ),

          uiOutput(ns("showcase_dots_ui"))
        )
      ),

      div(
        class = "footer-logos",

        div(
          class = "marquee-row",
          div(class = "category-label", "Institutions"),
          div(class = "marquee-container", uiOutput(ns("ui_institutions")))
        ),

        div(
          class = "marquee-row",
          div(class = "category-label", "Universities"),
          div(class = "marquee-container", uiOutput(ns("ui_universities")))
        )
      )
    )
  )
}


mod_01_launchpad_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_path <- system.file("www", package = "legionGOESapp")

    nav_trigger     <- reactiveVal(0)
    target_page     <- reactiveVal("welcome")
    last_click_time <- reactiveVal(NULL)

    current_slide <- reactiveVal(1)

    showcase_items <- list(
      list(
        type = "image",
        src = "WWW-FOLDER/super_bag/SP-01-simple_G19-EAST-s20260031700228_CRS-GoesEAST_MCMIPF-fnp01-TrueColor-DayOnly.png",
        kicker = "Visible spectrum",
        title = "GOES-19 True Color",
        subtitle = "Full-disk visible composition for rapid visual interpretation of clouds, land and ocean structure.",
        tags = c("True Color", "GOES-19", "Full Disk")
      ),
      list(
        type = "image",
        src = "WWW-FOLDER/super_bag/SP-01-simple_G19-EAST-s20260031700228_CRS-GoesEAST_MCMIPF-fnp02-IR_Colorized.png",
        kicker = "Thermal structure",
        title = "Colorized Infrared",
        subtitle = "Enhanced infrared visualization for cloud-top temperature patterns and large-scale convective systems.",
        tags = c("Infrared", "Cloud Tops", "Thermal")
      ),
      list(
        type = "image",
        src = "WWW-FOLDER/super_bag/focos_de_calor_20hs_UTC.png",
        kicker = "Fire monitoring",
        title = "Focos de calor",
        subtitle = "Satellite-based fire hotspot visualization for quick interpretation of thermal activity.",
        tags = c("Fire", "Hotspots", "FDCF")
      ),
      list(
        type = "gif",
        src = "WWW-FOLDER/super_bag/gif/sa002_sp01_set03_ppp003_1hour_MCMIPF_OP_FD_animation.gif",
        kicker = "Temporal analysis",
        title = "One-hour Animation",
        subtitle = "Animated GOES sequence for visualizing atmospheric motion and short-term satellite evolution.",
        tags = c("Animation", "1 Hour", "MCMIPF")
      ),
      list(
        type = "video",
        src = "WWW-FOLDER/super_bag/videos/step02_p010s01_Fusion002_LSTF_TrueColor_FD_ProjEPSG_PixelOrig_Pixel2km.mp4",
        kicker = "Dynamic product",
        title = "Full-disk Video",
        subtitle = "Video-based visualization of full-disk satellite products for smooth communication and exploration.",
        tags = c("Video", "Full Disk", "2 km")
      )
    )

    fire_navigation <- function(dest) {
      target_page(dest)
      nav_trigger(nav_trigger() + 1)
      last_click_time(Sys.time())

      if (show_debug) {
        message(sprintf(
          "[DEBUG] Trigger: %d | Destino: %s",
          nav_trigger(),
          dest
        ))
      }
    }

    observeEvent(input$btn_enter,     { fire_navigation("engine") })
    observeEvent(input$btn_sec_enter, { fire_navigation("engine") })
    observeEvent(input$btn_glm,       { fire_navigation("glm") })
    observeEvent(input$btn_fdcf,      { fire_navigation("fdcf") })
    observeEvent(input$btn_lstf,      { fire_navigation("lstf") })
    observeEvent(input$btn_lstf_new,  { fire_navigation("lstf_new") })
    observeEvent(input$btn_fdcf_new,  { fire_navigation("fdcf_new") })

    # ============================================================
    # GALERÍA EXTRA: PNG + MP4 desde www/super_bag
    # ============================================================

    extra_base_relative <- "super_bag"
    extra_base_physical <- file.path(base_path, extra_base_relative)

    file_to_web_path <- function(full_path) {
      base_norm <- gsub(
        "\\\\",
        "/",
        normalizePath(base_path, winslash = "/", mustWork = FALSE)
      )

      file_norm <- gsub(
        "\\\\",
        "/",
        normalizePath(full_path, winslash = "/", mustWork = FALSE)
      )

      rel <- sub(
        paste0("^", base_norm, "/?"),
        "",
        file_norm
      )

      paste0("WWW-FOLDER/", rel)
    }

    short_file_name <- function(x, max_chars = 46) {
      b <- basename(x)

      if (nchar(b) <= max_chars) {
        b
      } else {
        paste0(substr(b, 1, max_chars - 3), "...")
      }
    }

    escape_js_string <- function(x) {
      x <- gsub("\\\\", "\\\\\\\\", x)
      x <- gsub("'", "\\\\'", x)
      x <- gsub("\"", "\\\\\"", x)
      x
    }

    get_extra_files <- function(pattern) {
      if (!dir.exists(extra_base_physical)) {
        return(character())
      }

      list.files(
        path = extra_base_physical,
        pattern = pattern,
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    }

    make_png_gallery <- function(files) {
      if (length(files) == 0) {
        return(
          div(
            class = "text-muted",
            "No se encontraron archivos PNG en www/super_bag."
          )
        )
      }

      div(
        class = "goes-extra-gallery",
        div(
          class = "goes-extra-grid",
          lapply(files, function(f) {
            src <- file_to_web_path(f)
            title <- short_file_name(f)

            div(
              class = "goes-extra-card",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {src: '%s', type: 'image', title: '%s', nonce: Math.random()}, {priority: 'event'})",
                ns("open_extra_asset"),
                escape_js_string(src),
                escape_js_string(basename(f))
              ),
              tags$img(
                src = src,
                class = "goes-extra-thumb",
                loading = "lazy"
              ),
              div(class = "goes-extra-card-title", title)
            )
          })
        )
      )
    }

    make_mp4_gallery <- function(files) {
      if (length(files) == 0) {
        return(
          div(
            class = "text-muted",
            "No se encontraron archivos MP4 en www/super_bag."
          )
        )
      }

      div(
        class = "goes-extra-gallery",
        div(
          class = "goes-extra-grid",
          lapply(files, function(f) {
            src <- file_to_web_path(f)
            title <- short_file_name(f)

            div(
              class = "goes-extra-card",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {src: '%s', type: 'video', title: '%s', nonce: Math.random()}, {priority: 'event'})",
                ns("open_extra_asset"),
                escape_js_string(src),
                escape_js_string(basename(f))
              ),
              div(
                class = "goes-extra-video-box",
                icon("play"),
                span(style = "margin-left: 8px;", "MP4")
              ),
              div(class = "goes-extra-card-title", title)
            )
          })
        )
      )
    }

    observeEvent(input$btn_extra_gallery, {
      png_files <- get_extra_files("\\.png$")
      mp4_files <- get_extra_files("\\.mp4$")

      showModal(
        modalDialog(
          title = "Material extra disponible",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Cerrar"),

          tabsetPanel(
            tabPanel(
              title = paste0("PNG (", length(png_files), ")"),
              br(),
              make_png_gallery(png_files)
            ),
            tabPanel(
              title = paste0("MP4 (", length(mp4_files), ")"),
              br(),
              make_mp4_gallery(mp4_files)
            )
          )
        )
      )
    })

    observeEvent(input$open_extra_asset, {
      asset <- input$open_extra_asset

      if (is.null(asset$src) || is.null(asset$type)) {
        return()
      }

      if (identical(asset$type, "image")) {
        body_ui <- tags$img(
          src = asset$src,
          class = "goes-extra-preview-img"
        )
      } else if (identical(asset$type, "video")) {
        body_ui <- tags$video(
          src = asset$src,
          class = "goes-extra-preview-video",
          controls = NA,
          autoplay = NA,
          muted = NA,
          loop = NA,
          playsinline = NA
        )
      } else {
        body_ui <- div("Tipo de archivo no reconocido.")
      }

      showModal(
        modalDialog(
          title = asset$title,
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Cerrar"),
          body_ui
        )
      )
    })

    # ============================================================
    # SHOWCASE AUTOMÁTICO
    # ============================================================

    auto_timer <- reactiveTimer(7000)

    observe({
      auto_timer()

      isolate({
        n <- length(showcase_items)

        if (current_slide() >= n) {
          current_slide(1)
        } else {
          current_slide(current_slide() + 1)
        }
      })
    })

    observeEvent(input$go_to_slide, {
      idx <- suppressWarnings(as.integer(input$go_to_slide))

      if (!is.na(idx) && idx >= 1 && idx <= length(showcase_items)) {
        current_slide(idx)
      }
    })

    current_item <- reactive({
      showcase_items[[current_slide()]]
    })

    output$showcase_media_ui <- renderUI({
      item <- current_item()

      if (item$type %in% c("image", "gif")) {
        tags$img(
          src = item$src,
          class = "showcase-media-img"
        )
      } else if (item$type == "video") {
        tags$video(
          src = item$src,
          class = "showcase-media-video",
          autoplay = NA,
          muted = NA,
          loop = NA,
          playsinline = NA
        )
      } else {
        tags$div()
      }
    })

    output$showcase_kicker <- renderText({
      current_item()$kicker
    })

    output$showcase_title <- renderText({
      current_item()$title
    })

    output$showcase_subtitle <- renderText({
      current_item()$subtitle
    })

    output$showcase_tags_ui <- renderUI({
      tags <- current_item()$tags

      div(
        class = "showcase-tags",
        lapply(tags, function(x) {
          span(class = "showcase-tag", x)
        })
      )
    })

    output$showcase_counter <- renderText({
      paste0(current_slide(), " / ", length(showcase_items))
    })

    output$showcase_dots_ui <- renderUI({
      div(
        class = "showcase-dots",
        lapply(seq_along(showcase_items), function(i) {
          is_active <- if (i == current_slide()) "active" else ""

          tags$button(
            class = paste("showcase-dot-btn", is_active),
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'})",
              ns("go_to_slide"),
              i
            )
          )
        })
      )
    })

    # ============================================================
    # LOGOS FOOTER
    # ============================================================

    output$ui_institutions <- renderUI({
      physical_path <- file.path(base_path, "f01_institutions")
      files <- list.files(physical_path)

      if (length(files) == 0) {
        return(NULL)
      }

      div(
        class = "marquee-content scroll-slow",
        lapply(c(files, files), function(f) {
          img(src = paste0("WWW-FOLDER/f01_institutions/", f))
        })
      )
    })

    output$ui_universities <- renderUI({
      physical_path <- file.path(base_path, "f02_universities")
      files <- list.files(physical_path)

      if (length(files) == 0) {
        return(NULL)
      }

      div(
        class = "marquee-content scroll-fast",
        lapply(c(files, files), function(f) {
          img(src = paste0("WWW-FOLDER/f02_universities/", f))
        })
      )
    })

    # ============================================================
    # MODALES INFO
    # ============================================================

    observeEvent(input$btn_cite, {
      showModal(
        modalDialog(
          title = "Cite Legion GOES",
          p("Legion GOES Team (2026). v.0.0.1."),
          easyClose = TRUE
        )
      )
    })

    observeEvent(input$btn_contact, {
      showModal(
        modalDialog(
          title = "Contact",
          p("Contact information can be added here."),
          easyClose = TRUE
        )
      )
    })

    observeEvent(input$btn_info, {
      showModal(
        modalDialog(
          title = "Info",
          p("Legion GOES integrates satellite visualization, fire products, animations and scientific workflows."),
          easyClose = TRUE
        )
      )
    })

    observeEvent(input$btn_who, {
      showModal(
        modalDialog(
          title = "Team",
          p("Team information can be added here."),
          easyClose = TRUE
        )
      )
    })

    # ============================================================
    # RETORNO DEL MÓDULO
    # ============================================================

    launchpad_status <- reactive({
      list(
        nav_trigger = nav_trigger(),
        target_page = target_page(),
        last_click_time = last_click_time(),
        current_slide = current_slide()
      )
    })

    if (show_debug) {
      insertUI(
        selector = paste0("#", ns("showcase_dots_ui")),
        where = "afterEnd",
        ui = div(
          style = "
            margin-top:20px;
            padding:12px;
            background:#fff3cd;
            border:1px solid #ffeeba;
            border-radius:8px;
            font-family:monospace;
            font-size:0.75rem;
            color:#856404;
            width:100%;
            max-width:760px;
          ",
          h6(
            "Launchpad Debug Monitor",
            style = "margin:0 0 5px 0; font-weight:bold;"
          ),
          verbatimTextOutput(ns("debug_console"))
        )
      )

      output$debug_console <- renderPrint({
        launchpad_status()
      })
    }

    return(launchpad_status)
  })
}

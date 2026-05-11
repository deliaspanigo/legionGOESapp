library(shiny)
library(leaflet)
library(shinyjs)
library(httr)
library(xml2)
library(ncdf4)
library(dplyr)
library(sf)
library(rnaturalearth)
library(maps)
library(bslib)

# ============================================================
# MÓDULO ÚNICO: MAPA + CAPAS + GOES FDCF
# ============================================================

mod_fdcf_ui <- function(
    id,

    base_menu_top = "80px",
    base_menu_left = "20px",

    overlay_menu_top = "400px",
    overlay_menu_left = "20px",

    fdcf_menu_top = "20px",
    fdcf_menu_right = "20px",

    home_button_left = "20px",
    home_button_bottom = "20px"
) {
  ns <- NS(id)

  root_id <- ns("fdcf_module_root")
  map_id <- ns("map")
  base_menu_id <- ns("base_menu")
  overlay_menu_id <- ns("overlay_menu")
  fdcf_menu_id <- ns("fdcf_menu")

  tagList(
    useShinyjs(),

    tags$head(
      tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),

      tags$style(HTML(sprintf("
        /* ====================================================
           CSS AUTOCONTENIDO DEL MÓDULO
           Todo queda limitado a #%s
        ==================================================== */

        #%s {
          position: fixed !important;
          inset: 0 !important;
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
          margin: 0 !important;
          padding: 0 !important;
          overflow: hidden !important;
          z-index: 1;
        }

        #%s * {
          box-sizing: border-box;
        }

        #%s .fdcf-map-container {
          position: fixed !important;
          inset: 0 !important;
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
          margin: 0 !important;
          padding: 0 !important;
          z-index: 1;
        }

        #%s {
          position: fixed !important;
          inset: 0 !important;
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
          margin: 0 !important;
          padding: 0 !important;
          z-index: 1;
        }

        #%s.shiny-bound-output {
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
        }

        #%s .leaflet,
        #%s .leaflet-container {
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
        }

        #%s .floating-menu {
          position: absolute;
          width: 310px;
          background: white;
          border-radius: 12px;
          z-index: 9999;
          box-shadow: 0 4px 15px rgba(0,0,0,0.35);
          font-family: Arial, sans-serif;
          overflow: visible !important;
        }

        #%s .floating-header {
          background: #7f1d1d;
          color: white;
          padding: 10px 14px;
          font-weight: bold;
          cursor: move;
          user-select: none;
          border-radius: 12px 12px 0 0;
        }

        #%s .floating-body {
          padding: 14px;
          overflow: visible !important;
        }

        #%s {
          top: %s;
          left: %s;
        }

        #%s {
          top: %s;
          left: %s;
        }

        #%s {
          top: %s;
          right: %s;
        }

        #%s .status-box {
          font-size: 13px;
          line-height: 1.45;
        }

        #%s .status-value {
          font-weight: bold;
        }

        #%s .checkbox {
          margin-top: 4px !important;
          margin-bottom: 4px !important;
        }

        #%s .btn {
          margin-bottom: 6px;
        }

        #%s .fdcf-home-floating {
          position: absolute;
          left: %s;
          bottom: %s;
          z-index: 20000;
          width: auto;
          min-width: 130px;
          border: 1px solid rgba(255,255,255,0.25);
          border-radius: 12px;
          padding: 10px 14px;
          background: rgba(249, 115, 22, 0.92);
          color: white;
          cursor: pointer;
          font-weight: bold;
          box-shadow: 0 10px 30px rgba(0,0,0,0.35);
          backdrop-filter: blur(8px);
          pointer-events: auto;
        }

        #%s .fdcf-home-floating:hover {
          background: rgba(234, 88, 12, 0.98);
          color: white;
        }

        #%s .fire-legend {
          margin-top: 8px;
          padding: 8px;
          border-radius: 8px;
          background: #fff7ed;
          border: 1px solid #fed7aa;
          font-size: 12px;
        }
      ",
                              root_id,
                              root_id,
                              root_id,
                              root_id,
                              map_id,
                              map_id,
                              map_id,
                              map_id,
                              root_id,
                              root_id,
                              root_id,

                              base_menu_id,
                              base_menu_top,
                              base_menu_left,

                              overlay_menu_id,
                              overlay_menu_top,
                              overlay_menu_left,

                              fdcf_menu_id,
                              fdcf_menu_top,
                              fdcf_menu_right,

                              root_id,
                              root_id,
                              root_id,
                              root_id,

                              root_id,
                              home_button_left,
                              home_button_bottom,
                              root_id,
                              root_id
      ))),

      tags$script(HTML(sprintf("
        $(document).on('shiny:connected', function() {
          $('#%s').draggable({
            handle: '#%s',
            containment: 'window',
            scroll: false
          });

          $('#%s').draggable({
            handle: '#%s',
            containment: 'window',
            scroll: false
          });

          $('#%s').draggable({
            handle: '#%s',
            containment: 'window',
            scroll: false
          });
        });
      ",
                               ns("base_menu"), ns("base_menu_header"),
                               ns("overlay_menu"), ns("overlay_menu_header"),
                               ns("fdcf_menu"), ns("fdcf_menu_header")
      )))
    ),

    div(
      id = ns("fdcf_module_root"),

      div(
        class = "fdcf-map-container",
        leafletOutput(
          outputId = ns("map"),
          width = "100vw",
          height = "100vh"
        )
      ),

      actionButton(
        inputId = ns("btn_go_home"),
        label = "← Launcher",
        class = "fdcf-home-floating"
      ),

      # ========================================================
      # MENÚ 1: FONDO DEL MAPA
      # ========================================================

      div(
        id = ns("base_menu"),
        class = "floating-menu",

        div(
          id = ns("base_menu_header"),
          class = "floating-header",
          "Mover menú de mapa"
        ),

        div(
          class = "floating-body",

          radioButtons(
            inputId = ns("base_map"),
            label = "Fondo del mapa:",
            choices = c(
              "OpenStreetMap" = "osm",
              "CartoDB claro" = "carto_light",
              "CartoDB oscuro" = "carto_dark",
              "Satélite Esri" = "esri_sat",
              "Topográfico Esri" = "esri_topo",
              "Relieve Esri" = "esri_terrain"
            ),
            selected = "carto_dark"
          )
        )
      ),

      # ========================================================
      # MENÚ 2: CAPAS
      # ========================================================

      div(
        id = ns("overlay_menu"),
        class = "floating-menu",

        div(
          id = ns("overlay_menu_header"),
          class = "floating-header",
          "Mover menú de capas"
        ),

        div(
          class = "floating-body",

          checkboxGroupInput(
            inputId = ns("overlay_layers"),
            label = "Capas sobre el mapa:",
            choices = c(
              "Límites internacionales" = "limites_internacionales",
              "Límites provinciales / estados" = "limites_provinciales",
              "Capitales del mundo" = "capitales",
              "Ciudades importantes" = "ciudades"
            ),
            selected = c(
              "limites_internacionales",
              "limites_provinciales",
              "capitales"
            )
          )
        )
      ),

      # ========================================================
      # MENÚ 3: FDCF
      # ========================================================

      div(
        id = ns("fdcf_menu"),
        class = "floating-menu",

        div(
          id = ns("fdcf_menu_header"),
          class = "floating-header",
          "Estado GOES FDCF"
        ),

        div(
          class = "floating-body status-box",

          actionButton(
            inputId = ns("start_fdcf"),
            label = "Iniciar descarga FDCF",
            width = "100%"
          ),

          actionButton(
            inputId = ns("pause_fdcf"),
            label = "Pausar descarga FDCF",
            width = "100%"
          ),

          actionButton(
            inputId = ns("clear_fdcf"),
            label = "Borrar acumulado",
            width = "100%"
          ),

          hr(),

          checkboxGroupInput(
            inputId = ns("fire_classes"),
            label = "Clases de fuego:",
            choices = c(
              "Procesado / bueno" = "10",
              "Saturado" = "11",
              "Parcialmente nublado" = "12",
              "Alta probabilidad" = "13",
              "Media probabilidad" = "14",
              "Baja probabilidad" = "15",
              "Procesado filtrado" = "30",
              "Saturado filtrado" = "31",
              "Nublado filtrado" = "32",
              "Alta prob. filtrado" = "33",
              "Media prob. filtrado" = "34",
              "Baja prob. filtrado" = "35"
            ),
            selected = c("10", "11", "13", "30", "31", "33")
          ),

          hr(),

          div("Estado: ", span(textOutput(ns("fdcf_status"), inline = TRUE), class = "status-value")),
          div("Última búsqueda: ", span(textOutput(ns("fdcf_last_check"), inline = TRUE), class = "status-value")),
          div("Último archivo: ", span(textOutput(ns("fdcf_last_file"), inline = TRUE), class = "status-value")),
          div("Archivos descargados: ", span(textOutput(ns("fdcf_files_count"), inline = TRUE), class = "status-value")),
          div("Fuegos acumulados: ", span(textOutput(ns("fdcf_fire_count"), inline = TRUE), class = "status-value")),

          div(
            class = "fire-legend",
            HTML("<b>Nota:</b> se acumulan detecciones desde que se inicia la app. Cada punto representa un píxel FDCF clasificado como fuego.")
          )
        )
      )
    )
  )
}


mod_fdcf_server <- function(
    id,
    satellite = "goes19",
    product = "ABI-L2-FDCF",
    interval_ms = 600000,
    n_files = 2,
    initial_lng = -75,
    initial_lat = 5,
    initial_zoom = 3
) {
  moduleServer(id, function(input, output, session) {

    # ========================================================
    # FUNCIONES INTERNAS
    # ========================================================

    get_goes_prefixes <- function(product) {
      now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

      times <- c(
        now_utc,
        now_utc - 3600
      )

      unique(vapply(times, function(t) {
        year <- format(t, "%Y", tz = "UTC")
        doy  <- format(t, "%j", tz = "UTC")
        hour <- format(t, "%H", tz = "UTC")
        paste0(product, "/", year, "/", doy, "/", hour, "/")
      }, character(1)))
    }


    get_latest_fdcf_files <- function(
    satellite = "goes19",
    product = "ABI-L2-FDCF",
    n_files = 2
    ) {
      bucket_url <- paste0("https://noaa-", satellite, ".s3.amazonaws.com/")
      prefixes <- get_goes_prefixes(product)

      all_keys <- character(0)

      for (prefix in prefixes) {
        url <- paste0(bucket_url, "?list-type=2&prefix=", prefix)

        res <- tryCatch({
          httr::GET(url, httr::timeout(15))
        }, error = function(e) {
          NULL
        })

        if (is.null(res) || httr::status_code(res) != 200) {
          next
        }

        xml_txt <- httr::content(res, as = "text", encoding = "UTF-8")
        doc <- xml2::read_xml(xml_txt)

        ns_xml <- xml2::xml_ns(doc)
        keys <- xml2::xml_text(xml2::xml_find_all(doc, ".//d1:Key", ns = ns_xml))
        keys <- keys[grepl("\\.nc$", keys)]

        all_keys <- c(all_keys, keys)
      }

      if (length(all_keys) == 0) {
        return(character(0))
      }

      all_keys <- sort(unique(all_keys))
      latest_keys <- tail(all_keys, n_files)

      paste0(bucket_url, latest_keys)
    }


    fixed_grid_to_lonlat <- function(x, y, proj_attrs) {
      req <- as.numeric(proj_attrs$semi_major_axis)
      rpol <- as.numeric(proj_attrs$semi_minor_axis)
      h <- as.numeric(proj_attrs$perspective_point_height)
      lon0 <- as.numeric(proj_attrs$longitude_of_projection_origin) * pi / 180

      H <- h + req

      x_rad <- as.numeric(x)
      y_rad <- as.numeric(y)

      a <- sin(x_rad)^2 +
        cos(x_rad)^2 *
        (cos(y_rad)^2 + (req^2 / rpol^2) * sin(y_rad)^2)

      b <- -2 * H * cos(x_rad) * cos(y_rad)
      c <- H^2 - req^2

      disc <- b^2 - 4 * a * c

      lon <- rep(NA_real_, length(x_rad))
      lat <- rep(NA_real_, length(x_rad))

      ok <- is.finite(disc) & disc >= 0

      if (any(ok)) {
        rs <- (-b[ok] - sqrt(disc[ok])) / (2 * a[ok])

        sx <- rs * cos(x_rad[ok]) * cos(y_rad[ok])
        sy <- -rs * sin(x_rad[ok])
        sz <- rs * cos(x_rad[ok]) * sin(y_rad[ok])

        lat[ok] <- atan(
          (req^2 / rpol^2) *
            (sz / sqrt((H - sx)^2 + sy^2))
        ) * 180 / pi

        lon[ok] <- (lon0 - atan(sy / (H - sx))) * 180 / pi
      }

      data.frame(
        lon = lon,
        lat = lat
      )
    }


    fire_label <- function(mask) {
      dplyr::case_when(
        mask == 10 ~ "Fuego procesado",
        mask == 11 ~ "Fuego saturado",
        mask == 12 ~ "Fuego parcialmente nublado",
        mask == 13 ~ "Fuego alta probabilidad",
        mask == 14 ~ "Fuego media probabilidad",
        mask == 15 ~ "Fuego baja probabilidad",
        mask == 30 ~ "Fuego procesado filtrado",
        mask == 31 ~ "Fuego saturado filtrado",
        mask == 32 ~ "Fuego nublado filtrado",
        mask == 33 ~ "Fuego alta prob. filtrado",
        mask == 34 ~ "Fuego media prob. filtrado",
        mask == 35 ~ "Fuego baja prob. filtrado",
        TRUE ~ "Otro"
      )
    }


    fire_color <- function(mask) {
      dplyr::case_when(
        mask %in% c(10, 30) ~ "#dc2626",
        mask %in% c(11, 31) ~ "#ffffff",
        mask %in% c(12, 32) ~ "#64748b",
        mask %in% c(13, 33) ~ "#f97316",
        mask %in% c(14, 34) ~ "#a855f7",
        mask %in% c(15, 35) ~ "#2563eb",
        TRUE ~ "#facc15"
      )
    }


    read_fdcf_fires <- function(url, selected_masks) {
      temp_file <- tempfile(fileext = ".nc")

      ok <- tryCatch({
        download.file(
          url,
          temp_file,
          mode = "wb",
          quiet = TRUE
        )
        TRUE
      }, error = function(e) {
        FALSE
      })

      if (!ok || !file.exists(temp_file)) {
        return(data.frame())
      }

      nc <- tryCatch({
        ncdf4::nc_open(temp_file)
      }, error = function(e) {
        NULL
      })

      if (is.null(nc)) {
        return(data.frame())
      }

      on.exit({
        try(ncdf4::nc_close(nc), silent = TRUE)
        try(unlink(temp_file), silent = TRUE)
      }, add = TRUE)

      vars <- names(nc$var)

      if (!all(c("Mask", "x", "y", "goes_imager_projection") %in% vars)) {
        return(data.frame())
      }

      mask_arr <- ncdf4::ncvar_get(nc, "Mask")
      x <- ncdf4::ncvar_get(nc, "x")
      y <- ncdf4::ncvar_get(nc, "y")

      selected_masks <- as.integer(selected_masks)

      idx <- which(mask_arr %in% selected_masks, arr.ind = TRUE)

      if (nrow(idx) == 0) {
        return(data.frame())
      }

      dims <- dim(mask_arr)

      if (length(dims) != 2) {
        return(data.frame())
      }

      # Detectar si la primera dimensión corresponde a x o a y
      if (dims[1] == length(x) && dims[2] == length(y)) {
        x_idx <- idx[, 1]
        y_idx <- idx[, 2]
      } else if (dims[1] == length(y) && dims[2] == length(x)) {
        y_idx <- idx[, 1]
        x_idx <- idx[, 2]
      } else {
        return(data.frame())
      }

      mask_vals <- mask_arr[idx]

      proj_attrs <- nc$var[["goes_imager_projection"]]$att

      lonlat <- fixed_grid_to_lonlat(
        x = x[x_idx],
        y = y[y_idx],
        proj_attrs = proj_attrs
      )

      # Variables opcionales
      temp_vals <- rep(NA_real_, length(mask_vals))
      area_vals <- rep(NA_real_, length(mask_vals))
      power_vals <- rep(NA_real_, length(mask_vals))

      if ("Temp" %in% vars) {
        temp_arr <- ncdf4::ncvar_get(nc, "Temp")
        temp_vals <- temp_arr[idx]
      }

      if ("Area" %in% vars) {
        area_arr <- ncdf4::ncvar_get(nc, "Area")
        area_vals <- area_arr[idx]
      }

      if ("Power" %in% vars) {
        power_arr <- ncdf4::ncvar_get(nc, "Power")
        power_vals <- power_arr[idx]
      }

      df <- data.frame(
        lon = lonlat$lon,
        lat = lonlat$lat,
        mask = as.integer(mask_vals),
        fire_class = fire_label(as.integer(mask_vals)),
        temp_k = as.numeric(temp_vals),
        area_m2 = as.numeric(area_vals),
        power_mw = as.numeric(power_vals),
        color = fire_color(as.integer(mask_vals)),
        file = basename(url),
        downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )

      df <- df[is.finite(df$lon) & is.finite(df$lat), ]

      df
    }


    load_geo_layers <- function() {

      world_borders <- rnaturalearth::ne_countries(
        scale = "medium",
        returnclass = "sf"
      )

      world_borders <- st_transform(world_borders, 4326)

      if ("name" %in% names(world_borders)) {
        world_borders$nombre_pais <- world_borders$name
      } else if ("admin" %in% names(world_borders)) {
        world_borders$nombre_pais <- world_borders$admin
      } else {
        world_borders$nombre_pais <- "País"
      }

      admin1_lines <- rnaturalearth::ne_download(
        scale = 50,
        type = "admin_1_states_provinces_lines",
        category = "cultural",
        returnclass = "sf"
      )

      admin1_lines <- st_transform(admin1_lines, 4326)

      if ("name" %in% names(admin1_lines)) {
        admin1_lines$nombre_admin1 <- admin1_lines$name
      } else if ("name_en" %in% names(admin1_lines)) {
        admin1_lines$nombre_admin1 <- admin1_lines$name_en
      } else if ("gn_name" %in% names(admin1_lines)) {
        admin1_lines$nombre_admin1 <- admin1_lines$gn_name
      } else if ("region" %in% names(admin1_lines)) {
        admin1_lines$nombre_admin1 <- admin1_lines$region
      } else {
        admin1_lines$nombre_admin1 <- "Límite administrativo"
      }

      data("world.cities", package = "maps")

      capitales <- subset(
        maps::world.cities,
        capital == 1
      )

      ciudades <- subset(
        maps::world.cities,
        pop > 1000000 & capital == 0
      )

      list(
        world_borders = world_borders,
        admin1_lines = admin1_lines,
        capitales = capitales,
        ciudades = ciudades
      )
    }


    # ========================================================
    # DATOS Y ESTADOS REACTIVOS
    # ========================================================

    geo <- load_geo_layers()

    downloaded_files <- reactiveVal(character(0))
    fdcf_data <- reactiveVal(data.frame())
    fdcf_running <- reactiveVal(FALSE)

    fdcf_status <- reactiveVal("Detenido. Presione iniciar.")
    fdcf_last_check <- reactiveVal("-")
    fdcf_last_file <- reactiveVal("-")


    # ========================================================
    # MAPA INICIAL
    # ========================================================

    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
        setView(
          lng = initial_lng,
          lat = initial_lat,
          zoom = initial_zoom
        ) %>%
        addProviderTiles(providers$CartoDB.DarkMatter)
    })


    # ========================================================
    # FONDO DEL MAPA
    # ========================================================

    observeEvent(input$base_map, {

      proxy <- leafletProxy("map") %>%
        clearTiles()

      if (input$base_map == "osm") {
        proxy %>% addProviderTiles(providers$OpenStreetMap)
      }

      if (input$base_map == "carto_light") {
        proxy %>% addProviderTiles(providers$CartoDB.Positron)
      }

      if (input$base_map == "carto_dark") {
        proxy %>% addProviderTiles(providers$CartoDB.DarkMatter)
      }

      if (input$base_map == "esri_sat") {
        proxy %>% addProviderTiles(providers$Esri.WorldImagery)
      }

      if (input$base_map == "esri_topo") {
        proxy %>% addProviderTiles(providers$Esri.WorldTopoMap)
      }

      if (input$base_map == "esri_terrain") {
        proxy %>% addProviderTiles(providers$Esri.WorldTerrain)
      }

    }, ignoreInit = TRUE)


    # ========================================================
    # CAPAS SUPERIORES
    # ========================================================

    observe({

      layers <- input$overlay_layers

      proxy <- leafletProxy("map") %>%
        clearGroup("limites_internacionales") %>%
        clearGroup("limites_provinciales") %>%
        clearGroup("capitales") %>%
        clearGroup("ciudades")

      if (is.null(layers)) {
        return()
      }

      if ("limites_internacionales" %in% layers) {
        proxy %>%
          addPolygons(
            data = geo$world_borders,
            group = "limites_internacionales",
            fillOpacity = 0,
            color = "black",
            weight = 1,
            opacity = 0.9,
            label = ~nombre_pais
          )
      }

      if ("limites_provinciales" %in% layers) {
        proxy %>%
          addPolylines(
            data = geo$admin1_lines,
            group = "limites_provinciales",
            color = "gray30",
            weight = 0.8,
            opacity = 0.85,
            label = ~nombre_admin1
          )
      }

      if ("capitales" %in% layers) {
        proxy %>%
          addCircleMarkers(
            data = geo$capitales,
            lng = ~long,
            lat = ~lat,
            group = "capitales",
            radius = 4,
            color = "red",
            fillColor = "red",
            fillOpacity = 0.9,
            stroke = TRUE,
            weight = 1,
            label = ~paste0(name, " - ", country.etc),
            popup = ~paste0(
              "<b>", name, "</b><br>",
              "País: ", country.etc, "<br>",
              "Población: ", pop
            )
          )
      }

      if ("ciudades" %in% layers) {
        proxy %>%
          addCircleMarkers(
            data = geo$ciudades,
            lng = ~long,
            lat = ~lat,
            group = "ciudades",
            radius = 3,
            color = "blue",
            fillColor = "blue",
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 1,
            label = ~paste0(name, " - ", country.etc),
            popup = ~paste0(
              "<b>", name, "</b><br>",
              "País: ", country.etc, "<br>",
              "Población: ", pop
            )
          )
      }
    })


    # ========================================================
    # FDCF
    # ========================================================

    download_fdcf_step <- function() {

      selected_masks <- input$fire_classes

      if (is.null(selected_masks) || length(selected_masks) == 0) {
        fdcf_status("Seleccione al menos una clase de fuego")
        return()
      }

      fdcf_status("Buscando imágenes FDCF...")
      fdcf_last_check(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

      latest_urls <- tryCatch({
        get_latest_fdcf_files(
          satellite = satellite,
          product = product,
          n_files = n_files
        )
      }, error = function(e) {
        character(0)
      })

      if (length(latest_urls) == 0) {
        fdcf_status("Sin archivos FDCF encontrados")
        return()
      }

      old_files <- downloaded_files()
      new_urls <- latest_urls[!basename(latest_urls) %in% old_files]

      if (length(new_urls) == 0) {
        fdcf_status("Sin archivos nuevos")
        return()
      }

      all_new_data <- list()

      for (url in new_urls) {
        fdcf_status(paste("Descargando:", basename(url)))
        fdcf_last_file(basename(url))

        df <- tryCatch({
          read_fdcf_fires(
            url = url,
            selected_masks = selected_masks
          )
        }, error = function(e) {
          data.frame()
        })

        if (nrow(df) > 0) {
          all_new_data[[length(all_new_data) + 1]] <- df
        }
      }

      downloaded_files(unique(c(old_files, basename(new_urls))))

      if (length(all_new_data) > 0) {
        new_df <- bind_rows(all_new_data)
        current_df <- fdcf_data()

        if (nrow(current_df) == 0) {
          total_df <- new_df
        } else {
          total_df <- bind_rows(current_df, new_df)
        }

        fdcf_data(total_df)
        fdcf_status("FDCF actualizado")
      } else {
        fdcf_status("Archivos descargados, sin fuegos seleccionados")
      }
    }


    observeEvent(input$start_fdcf, {
      fdcf_running(TRUE)
      fdcf_status("FDCF iniciado. Buscando archivos...")
      download_fdcf_step()
    })


    observeEvent(input$pause_fdcf, {
      fdcf_running(FALSE)
      fdcf_status("Pausado")
    })


    observeEvent(input$clear_fdcf, {
      fdcf_data(data.frame())
      downloaded_files(character(0))
      fdcf_status("Acumulado borrado")
      fdcf_last_file("-")

      leafletProxy("map") %>%
        clearGroup("Fuegos FDCF")
    })


    observe({
      invalidateLater(interval_ms, session)

      if (!isTRUE(fdcf_running())) {
        return()
      }

      download_fdcf_step()
    })


    observe({
      df <- fdcf_data()

      proxy <- leafletProxy("map") %>%
        clearGroup("Fuegos FDCF")

      if (nrow(df) == 0) {
        return()
      }

      proxy %>%
        addCircleMarkers(
          data = df,
          lng = ~lon,
          lat = ~lat,
          group = "Fuegos FDCF",
          radius = ~pmin(9, pmax(4, sqrt(ifelse(is.na(power_mw), 10, power_mw)) / 7)),
          stroke = TRUE,
          weight = 1,
          color = ~color,
          fillColor = ~color,
          fillOpacity = 0.85,
          opacity = 1,
          popup = ~paste0(
            "<b>GOES FDCF</b><br>",
            "Clase: ", fire_class, "<br>",
            "Mask: ", mask, "<br>",
            "Lat: ", round(lat, 3), "<br>",
            "Lon: ", round(lon, 3), "<br>",
            "Temp: ", ifelse(is.na(temp_k), "NA", paste0(round(temp_k, 1), " K")), "<br>",
            "Área: ", ifelse(is.na(area_m2), "NA", paste0(round(area_m2, 1), " m²")), "<br>",
            "Power: ", ifelse(is.na(power_mw), "NA", paste0(round(power_mw, 1), " MW")), "<br>",
            "Archivo: ", file, "<br>",
            "Descargado: ", downloaded_at
          )
        )
    })


    # ========================================================
    # TEXTOS DEL MENÚ FDCF
    # ========================================================

    output$fdcf_status <- renderText({
      fdcf_status()
    })

    output$fdcf_last_check <- renderText({
      fdcf_last_check()
    })

    output$fdcf_last_file <- renderText({
      fdcf_last_file()
    })

    output$fdcf_files_count <- renderText({
      length(downloaded_files())
    })

    output$fdcf_fire_count <- renderText({
      nrow(fdcf_data())
    })
  })
}


# ============================================================
# APP PRINCIPAL DE PRUEBA FDCF
# ============================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#ff6b00"),
  style = "padding: 0; margin: 0;",

  mod_fdcf_ui(
    "fdcf01",

    base_menu_top = "80px",
    base_menu_left = "20px",

    overlay_menu_top = "400px",
    overlay_menu_left = "20px",

    fdcf_menu_top = "20px",
    fdcf_menu_right = "20px",

    home_button_left = "20px",
    home_button_bottom = "20px"
  )
)
#
# server <- function(input, output, session) {
#
#   mod_fdcf_server(
#     id = "fdcf01",
#     satellite = "goes19",
#     product = "ABI-L2-FDCF",
#     interval_ms = 600000,
#     n_files = 2,
#
#     # Vista inicial: continente americano completo
#     initial_lng = -75,
#     initial_lat = 5,
#     initial_zoom = 3
#   )
#
#   observeEvent(input[["fdcf01-btn_go_home"]], {
#     showNotification("Botón ← Launcher presionado", type = "message")
#   }, ignoreInit = TRUE)
# }
#
# shinyApp(ui, server)

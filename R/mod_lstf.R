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
# MÓDULO ÚNICO: MAPA + CAPAS + GOES LSTF
# ============================================================

mod_lstf_ui <- function(
    id,

    base_menu_top = "80px",
    base_menu_left = "20px",

    overlay_menu_top = "400px",
    overlay_menu_left = "20px",

    lstf_menu_top = "20px",
    lstf_menu_right = "20px",

    legend_top = "20px",
    legend_left = "360px",

    plot_top = "260px",
    plot_right = "20px",

    stats_top = "520px",
    stats_right = "20px",

    home_button_left = "20px",
    home_button_bottom = "20px"
) {
  ns <- NS(id)

  root_id <- ns("lstf_module_root")
  map_id <- ns("map")
  base_menu_id <- ns("base_menu")
  overlay_menu_id <- ns("overlay_menu")
  lstf_menu_id <- ns("lstf_menu")
  legend_id <- ns("lstf_legend")
  plot_id <- ns("lstf_plot_panel")
  stats_id <- ns("lstf_stats_panel")

  tagList(
    useShinyjs(),

    tags$head(
      tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),

      tags$style(HTML(sprintf("
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

        #%s .lstf-map-container {
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

        #%s .floating-menu,
        #%s .floating-panel {
          position: absolute;
          width: 310px;
          background: white;
          border-radius: 12px;
          z-index: 9999;
          box-shadow: 0 4px 15px rgba(0,0,0,0.35);
          font-family: Arial, sans-serif;
          overflow: visible !important;
        }

        #%s .floating-panel-wide {
          width: 390px;
        }

        #%s .floating-header {
          background: #1e3a8a;
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

        #%s {
          top: %s;
          left: %s;
        }

        #%s {
          top: %s;
          right: %s;
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

        #%s .lstf-home-floating {
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

        #%s .lstf-home-floating:hover {
          background: rgba(234, 88, 12, 0.98);
          color: white;
        }

        #%s .legend-gradient {
          width: 100%%;
          height: 18px;
          border-radius: 8px;
          border: 1px solid rgba(0,0,0,0.25);
          background: linear-gradient(to right,
            #313695,
            #4575b4,
            #74add1,
            #abd9e9,
            #e0f3f8,
            #ffffbf,
            #fee090,
            #fdae61,
            #f46d43,
            #d73027,
            #a50026
          );
        }

        #%s .legend-labels {
          display: flex;
          justify-content: space-between;
          font-size: 11px;
          margin-top: 4px;
        }

        #%s table {
          width: 100%%;
          font-size: 12px;
        }

        #%s table td,
        #%s table th {
          padding: 4px 6px;
        }
      ",
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
                              root_id,
                              root_id,

                              base_menu_id,
                              base_menu_top,
                              base_menu_left,

                              overlay_menu_id,
                              overlay_menu_top,
                              overlay_menu_left,

                              lstf_menu_id,
                              lstf_menu_top,
                              lstf_menu_right,

                              legend_id,
                              legend_top,
                              legend_left,

                              plot_id,
                              plot_top,
                              plot_right,

                              stats_id,
                              stats_top,
                              stats_right,

                              root_id,
                              root_id,
                              root_id,
                              root_id,

                              root_id,
                              home_button_left,
                              home_button_bottom,
                              root_id,

                              root_id,
                              root_id,
                              root_id,
                              root_id,
                              root_id
      ))),

      tags$script(HTML(sprintf("
        $(document).on('shiny:connected', function() {
          $('#%s').draggable({ handle: '#%s', containment: 'window', scroll: false });
          $('#%s').draggable({ handle: '#%s', containment: 'window', scroll: false });
          $('#%s').draggable({ handle: '#%s', containment: 'window', scroll: false });
          $('#%s').draggable({ handle: '#%s', containment: 'window', scroll: false });
          $('#%s').draggable({ handle: '#%s', containment: 'window', scroll: false });
          $('#%s').draggable({ handle: '#%s', containment: 'window', scroll: false });
        });
      ",
                               ns("base_menu"), ns("base_menu_header"),
                               ns("overlay_menu"), ns("overlay_menu_header"),
                               ns("lstf_menu"), ns("lstf_menu_header"),
                               ns("lstf_legend"), ns("lstf_legend_header"),
                               ns("lstf_plot_panel"), ns("lstf_plot_header"),
                               ns("lstf_stats_panel"), ns("lstf_stats_header")
      )))
    ),

    div(
      id = ns("lstf_module_root"),

      div(
        class = "lstf-map-container",
        leafletOutput(
          outputId = ns("map"),
          width = "100vw",
          height = "100vh"
        )
      ),

      actionButton(
        inputId = ns("btn_go_home"),
        label = "← Launcher",
        class = "lstf-home-floating"
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
      # MENÚ 3: LSTF
      # ========================================================

      div(
        id = ns("lstf_menu"),
        class = "floating-menu",

        div(
          id = ns("lstf_menu_header"),
          class = "floating-header",
          "Estado GOES LSTF"
        ),

        div(
          class = "floating-body status-box",

          actionButton(
            inputId = ns("start_lstf"),
            label = "Iniciar descarga LSTF",
            width = "100%"
          ),

          actionButton(
            inputId = ns("pause_lstf"),
            label = "Pausar descarga LSTF",
            width = "100%"
          ),

          actionButton(
            inputId = ns("clear_lstf"),
            label = "Borrar capa",
            width = "100%"
          ),

          hr(),

          sliderInput(
            inputId = ns("sample_stride"),
            label = "Muestreo de píxeles:",
            min = 4,
            max = 40,
            value = 16,
            step = 2
          ),

          checkboxInput(
            inputId = ns("dqf_good_only"),
            label = "Usar solo DQF = 0",
            value = TRUE
          ),

          hr(),

          div("Estado: ", span(textOutput(ns("lstf_status"), inline = TRUE), class = "status-value")),
          div("Última búsqueda: ", span(textOutput(ns("lstf_last_check"), inline = TRUE), class = "status-value")),
          div("Último archivo: ", span(textOutput(ns("lstf_last_file"), inline = TRUE), class = "status-value")),
          div("Píxeles mostrados: ", span(textOutput(ns("lstf_pixel_count"), inline = TRUE), class = "status-value"))
        )
      ),

      # ========================================================
      # ESCALA MOVIBLE
      # ========================================================

      div(
        id = ns("lstf_legend"),
        class = "floating-panel",

        div(
          id = ns("lstf_legend_header"),
          class = "floating-header",
          "Escala LST °C"
        ),

        div(
          class = "floating-body",
          div(class = "legend-gradient"),
          div(
            class = "legend-labels",
            span("-60"),
            span("-30"),
            span("0"),
            span("30"),
            span("60")
          ),
          div(
            style = "font-size: 12px; margin-top: 8px;",
            "Temperatura de superficie terrestre convertida a °C."
          )
        )
      ),

      # ========================================================
      # GRÁFICO MOVIBLE
      # ========================================================

      div(
        id = ns("lstf_plot_panel"),
        class = "floating-panel floating-panel-wide",

        div(
          id = ns("lstf_plot_header"),
          class = "floating-header",
          "Histograma LST °C"
        ),

        div(
          class = "floating-body",
          plotOutput(ns("lstf_hist"), height = "220px")
        )
      ),

      # ========================================================
      # TABLA ESTADÍSTICA MOVIBLE
      # ========================================================

      div(
        id = ns("lstf_stats_panel"),
        class = "floating-panel floating-panel-wide",

        div(
          id = ns("lstf_stats_header"),
          class = "floating-header",
          "Estadísticas LST °C"
        ),

        div(
          class = "floating-body",
          tableOutput(ns("lstf_stats_table"))
        )
      )
    )
  )
}


mod_lstf_server <- function(
    id,
    satellite = "goes19",
    product = "ABI-L2-LSTF",
    interval_ms = 600000,
    n_files = 1,
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
        now_utc - 3600,
        now_utc - 7200
      )

      unique(vapply(times, function(t) {
        year <- format(t, "%Y", tz = "UTC")
        doy  <- format(t, "%j", tz = "UTC")
        hour <- format(t, "%H", tz = "UTC")
        paste0(product, "/", year, "/", doy, "/", hour, "/")
      }, character(1)))
    }


    get_latest_lstf_files <- function(
    satellite = "goes19",
    product = "ABI-L2-LSTF",
    n_files = 1
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


    lst_color <- function(temp_c) {
      pal <- colorRampPalette(c(
        "#313695",
        "#4575b4",
        "#74add1",
        "#abd9e9",
        "#e0f3f8",
        "#ffffbf",
        "#fee090",
        "#fdae61",
        "#f46d43",
        "#d73027",
        "#a50026"
      ))(121)

      idx <- round(pmax(-60, pmin(60, temp_c)) + 61)
      pal[idx]
    }


    read_lstf <- function(url, stride = 16, good_only = TRUE) {
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
        return(list(points = data.frame(), values = numeric(0)))
      }

      nc <- tryCatch({
        ncdf4::nc_open(temp_file)
      }, error = function(e) {
        NULL
      })

      if (is.null(nc)) {
        return(list(points = data.frame(), values = numeric(0)))
      }

      on.exit({
        try(ncdf4::nc_close(nc), silent = TRUE)
        try(unlink(temp_file), silent = TRUE)
      }, add = TRUE)

      vars <- names(nc$var)

      if (!all(c("LST", "x", "y", "goes_imager_projection") %in% vars)) {
        return(list(points = data.frame(), values = numeric(0)))
      }

      lst_k <- ncdf4::ncvar_get(nc, "LST")
      x <- ncdf4::ncvar_get(nc, "x")
      y <- ncdf4::ncvar_get(nc, "y")

      dqf <- NULL
      if ("DQF" %in% vars) {
        dqf <- ncdf4::ncvar_get(nc, "DQF")
      }

      lst_c <- as.numeric(lst_k) - 273.15
      lst_c_mat <- matrix(lst_c, nrow = dim(lst_k)[1], ncol = dim(lst_k)[2])

      if (!is.null(dqf) && isTRUE(good_only)) {
        dqf_vec <- as.numeric(dqf)
        lst_c_mat[dqf_vec != 0] <- NA_real_
      }

      valid_values <- as.numeric(lst_c_mat)
      valid_values <- valid_values[is.finite(valid_values)]

      dims <- dim(lst_c_mat)

      idx1 <- seq(1, dims[1], by = stride)
      idx2 <- seq(1, dims[2], by = stride)

      grid <- expand.grid(i = idx1, j = idx2)

      vals <- lst_c_mat[cbind(grid$i, grid$j)]

      keep <- is.finite(vals) & vals >= -80 & vals <= 80

      grid <- grid[keep, , drop = FALSE]
      vals <- vals[keep]

      if (nrow(grid) == 0) {
        return(list(points = data.frame(), values = valid_values))
      }

      # Detectar orientación de matriz
      if (dims[1] == length(x) && dims[2] == length(y)) {
        x_sel <- x[grid$i]
        y_sel <- y[grid$j]
      } else if (dims[1] == length(y) && dims[2] == length(x)) {
        y_sel <- y[grid$i]
        x_sel <- x[grid$j]
      } else {
        return(list(points = data.frame(), values = valid_values))
      }

      proj_attrs <- nc$var[["goes_imager_projection"]]$att

      lonlat <- fixed_grid_to_lonlat(
        x = x_sel,
        y = y_sel,
        proj_attrs = proj_attrs
      )

      df <- data.frame(
        lon = lonlat$lon,
        lat = lonlat$lat,
        lst_c = vals,
        color = lst_color(vals),
        file = basename(url),
        downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )

      df <- df[is.finite(df$lon) & is.finite(df$lat), ]

      list(
        points = df,
        values = valid_values
      )
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
    # ESTADOS
    # ========================================================

    geo <- load_geo_layers()

    lstf_points <- reactiveVal(data.frame())
    lstf_values <- reactiveVal(numeric(0))
    lstf_running <- reactiveVal(FALSE)

    lstf_status <- reactiveVal("Detenido. Presione iniciar.")
    lstf_last_check <- reactiveVal("-")
    lstf_last_file <- reactiveVal("-")


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
    # LSTF
    # ========================================================

    download_lstf_step <- function() {

      lstf_status("Buscando imágenes LSTF...")
      lstf_last_check(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

      latest_urls <- tryCatch({
        get_latest_lstf_files(
          satellite = satellite,
          product = product,
          n_files = n_files
        )
      }, error = function(e) {
        character(0)
      })

      if (length(latest_urls) == 0) {
        lstf_status("Sin archivos LSTF encontrados")
        return()
      }

      url <- tail(latest_urls, 1)

      lstf_status(paste("Descargando:", basename(url)))
      lstf_last_file(basename(url))

      res <- tryCatch({
        read_lstf(
          url = url,
          stride = input$sample_stride,
          good_only = input$dqf_good_only
        )
      }, error = function(e) {
        list(points = data.frame(), values = numeric(0))
      })

      lstf_points(res$points)
      lstf_values(res$values)

      if (nrow(res$points) > 0) {
        lstf_status("LSTF actualizado")
      } else {
        lstf_status("Archivo descargado, sin píxeles válidos")
      }
    }


    observeEvent(input$start_lstf, {
      lstf_running(TRUE)
      lstf_status("LSTF iniciado. Buscando archivos...")
      download_lstf_step()
    })


    observeEvent(input$pause_lstf, {
      lstf_running(FALSE)
      lstf_status("Pausado")
    })


    observeEvent(input$clear_lstf, {
      lstf_points(data.frame())
      lstf_values(numeric(0))
      lstf_status("Capa borrada")
      lstf_last_file("-")

      leafletProxy("map") %>%
        clearGroup("LSTF")
    })


    observeEvent(
      list(input$sample_stride, input$dqf_good_only),
      {
        if (isTRUE(lstf_running())) {
          download_lstf_step()
        }
      },
      ignoreInit = TRUE
    )


    observe({
      invalidateLater(interval_ms, session)

      if (!isTRUE(lstf_running())) {
        return()
      }

      download_lstf_step()
    })


    observe({
      df <- lstf_points()

      proxy <- leafletProxy("map") %>%
        clearGroup("LSTF")

      if (nrow(df) == 0) {
        return()
      }

      proxy %>%
        addCircleMarkers(
          data = df,
          lng = ~lon,
          lat = ~lat,
          group = "LSTF",
          radius = 2.2,
          stroke = FALSE,
          fillColor = ~color,
          fillOpacity = 0.75,
          popup = ~paste0(
            "<b>GOES LSTF</b><br>",
            "LST: ", round(lst_c, 2), " °C<br>",
            "Lat: ", round(lat, 3), "<br>",
            "Lon: ", round(lon, 3), "<br>",
            "Archivo: ", file, "<br>",
            "Descargado: ", downloaded_at
          )
        )
    })


    # ========================================================
    # GRÁFICO
    # ========================================================

    output$lstf_hist <- renderPlot({
      vals <- lstf_values()

      if (length(vals) == 0) {
        plot.new()
        text(0.5, 0.5, "Sin datos LSTF")
        return()
      }

      vals <- vals[is.finite(vals)]
      vals <- vals[vals >= -80 & vals <= 80]

      hist(
        vals,
        breaks = seq(-60, 60, by = 5),
        main = "Distribución LST",
        xlab = "Temperatura superficial (°C)",
        ylab = "Frecuencia",
        col = "gray70",
        border = "white",
        xlim = c(-60, 60)
      )

      abline(v = mean(vals, na.rm = TRUE), lwd = 2)
      abline(v = median(vals, na.rm = TRUE), lwd = 2, lty = 2)
      legend(
        "topright",
        legend = c("Media", "Mediana"),
        lwd = 2,
        lty = c(1, 2),
        bty = "n",
        cex = 0.8
      )
    })


    # ========================================================
    # TABLA ESTADÍSTICA
    # ========================================================

    output$lstf_stats_table <- renderTable({
      vals <- lstf_values()
      vals <- vals[is.finite(vals)]
      vals <- vals[vals >= -80 & vals <= 80]

      if (length(vals) == 0) {
        return(data.frame(
          Estadistica = "Sin datos",
          Valor = NA
        ))
      }

      data.frame(
        Estadistica = c(
          "n píxeles válidos",
          "mínimo °C",
          "p25 °C",
          "media °C",
          "mediana °C",
          "p75 °C",
          "máximo °C",
          "desvío estándar °C"
        ),
        Valor = c(
          length(vals),
          round(min(vals), 2),
          round(quantile(vals, 0.25), 2),
          round(mean(vals), 2),
          round(median(vals), 2),
          round(quantile(vals, 0.75), 2),
          round(max(vals), 2),
          round(sd(vals), 2)
        )
      )
    })


    # ========================================================
    # TEXTOS DEL MENÚ LSTF
    # ========================================================

    output$lstf_status <- renderText({
      lstf_status()
    })

    output$lstf_last_check <- renderText({
      lstf_last_check()
    })

    output$lstf_last_file <- renderText({
      lstf_last_file()
    })

    output$lstf_pixel_count <- renderText({
      nrow(lstf_points())
    })
  })
}


# ============================================================
# APP PRINCIPAL DE PRUEBA LSTF
# ============================================================
#
# ui <- fluidPage(
#   theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2563eb"),
#   style = "padding: 0; margin: 0;",
#
#   mod_lstf_ui(
#     "lstf01",
#
#     base_menu_top = "80px",
#     base_menu_left = "20px",
#
#     overlay_menu_top = "400px",
#     overlay_menu_left = "20px",
#
#     lstf_menu_top = "20px",
#     lstf_menu_right = "20px",
#
#     legend_top = "20px",
#     legend_left = "360px",
#
#     plot_top = "260px",
#     plot_right = "20px",
#
#     stats_top = "520px",
#     stats_right = "20px",
#
#     home_button_left = "20px",
#     home_button_bottom = "20px"
#   )
# )
#
# server <- function(input, output, session) {
#
#   mod_lstf_server(
#     id = "lstf01",
#     satellite = "goes19",
#     product = "ABI-L2-LSTF",
#     interval_ms = 600000,
#     n_files = 1,
#
#     initial_lng = -75,
#     initial_lat = 5,
#     initial_zoom = 3
#   )
#
#   observeEvent(input[["lstf01-btn_go_home"]], {
#     showNotification("Botón ← Launcher presionado", type = "message")
#   }, ignoreInit = TRUE)
# }
#
# shinyApp(ui, server)

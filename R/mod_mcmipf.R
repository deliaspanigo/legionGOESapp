# ==============================================================================
# GOES TRUE COLOR APP - MCMIPF
# Descarga última imagen MCMIPF y genera PNG True Color
# ==============================================================================

library(shiny)
library(leaflet)
library(htmlwidgets)
library(terra)
library(httr)
library(xml2)
library(stringr)
library(lubridate)
library(purrr)
library(png)
library(plotly)
library(sf)
library(rnaturalearth)

# ============================================================
# 0) CONFIGURACIÓN GENERAL
# ============================================================

BUCKET  <- "noaa-goes19"
PRODUCT <- "ABI-L2-MCMIPF"

NC_DIR <- file.path(tempdir(), "goes_truecolor_downloads")
dir.create(NC_DIR, showWarnings = FALSE, recursive = TRUE)

dir.create("www", showWarnings = FALSE, recursive = TRUE)

TC_ORIGINAL_PNG <- "www/truecolor_original_goes.png"
TC_WGS84_PNG    <- "www/truecolor_global_wgs84.png"
TC_3857_PNG     <- "www/truecolor_global_3857.png"

PROJECTION_CHOICES <- c(
  "WGS84" = "wgs84",
  "Mercator" = "mercator",
  "GOES original" = "goes"
)

DEFAULT_PROJECTION <- "wgs84"

LEFT_TOOLBAR_TOP <- "10px"
LEFT_TOOLBAR_LEFT <- "60px"

RIGHT_TOOLBAR_TOP <- "10px"
RIGHT_TOOLBAR_RIGHT <- "12px"

GIBS_WMS_EPSG4326 <- "https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi"
TERRASTRIS_OSM_WMS <- "https://ows.terrestris.de/osm/service"

message("Carpeta temporal de descarga True Color: ", NC_DIR)

# ============================================================
# 1) FUNCIONES AUXILIARES
# ============================================================

default_status_fun <- function(txt) {
  message(txt)
}

timestamp_ms <- function() {
  format(Sys.time(), "%H:%M:%OS3")
}

format_mb <- function(bytes) {
  if (is.na(bytes)) return("NA MB")
  paste(round(bytes / 1024^2, 2), "MB")
}

get_leaflet_provider <- function(provider_name) {
  if (is.null(provider_name) || provider_name == "none") {
    return(NULL)
  }

  tryCatch({
    provider_expr <- paste0("leaflet::providers$", provider_name)
    eval(parse(text = provider_expr))
  }, error = function(e) {
    NULL
  })
}

provider_available <- function(provider_name) {
  if (provider_name == "none") return(TRUE)
  !is.null(get_leaflet_provider(provider_name))
}

# ============================================================
# 2) FONDOS POR PROYECCIÓN
# ============================================================

BASE_MAPS_MERCATOR <- c(
  "Oscuro - CartoDB DarkMatter" = "CartoDB.DarkMatter",
  "Claro - CartoDB Positron"    = "CartoDB.Positron",
  "OpenStreetMap"               = "OpenStreetMap",
  "ESRI Imagen satelital"       = "Esri.WorldImagery",
  "ESRI Calles"                 = "Esri.WorldStreetMap",
  "ESRI Topográfico"            = "Esri.WorldTopoMap"
)

BASE_MAPS_MERCATOR <- BASE_MAPS_MERCATOR[
  vapply(BASE_MAPS_MERCATOR, provider_available, logical(1))
]

if (length(BASE_MAPS_MERCATOR) == 0) {
  BASE_MAPS_MERCATOR <- c("OpenStreetMap" = "OpenStreetMap")
}

BASE_MAPS_WGS84 <- c(
  "NASA GIBS Blue Marble NG"          = "gibs_blue_marble_ng",
  "NASA GIBS VIIRS luces nocturnas"   = "gibs_viirs_night_2012",
  "OSM WMS - terrestris calles/rutas" = "wms_terrestris_osm",
  "Sin fondo geográfico"              = "none"
)

BASE_MAPS_GOES <- c(
  "Sin fondo geográfico" = "none"
)

BASE_MAPS_BY_PROJECTION <- list(
  mercator = BASE_MAPS_MERCATOR,
  wgs84 = BASE_MAPS_WGS84,
  goes = BASE_MAPS_GOES
)

GIBS_WGS84_LAYERS <- list(
  gibs_blue_marble_ng = list(
    layer = "BlueMarble_NextGeneration",
    format = "image/jpeg",
    transparent = FALSE
  ),
  gibs_viirs_night_2012 = list(
    layer = "VIIRS_CityLights_2012",
    format = "image/jpeg",
    transparent = FALSE
  )
)

default_base_for_projection <- function(projection) {
  choices <- BASE_MAPS_BY_PROJECTION[[projection]]

  if (is.null(choices) || length(choices) == 0) {
    return("none")
  }

  if (projection == "mercator" && "CartoDB.DarkMatter" %in% choices) {
    return("CartoDB.DarkMatter")
  }

  if (projection == "wgs84" && "gibs_blue_marble_ng" %in% choices) {
    return("gibs_blue_marble_ng")
  }

  unname(choices[[1]])
}

add_base_layer_to_map <- function(m, projection, base_id) {
  if (is.null(base_id) || base_id == "none") {
    return(m)
  }

  if (projection == "mercator") {
    provider <- get_leaflet_provider(base_id)

    if (!is.null(provider)) {
      m <- m %>%
        addProviderTiles(provider = provider, group = "base_tiles")
    }

    return(m)
  }

  if (projection == "wgs84") {

    if (base_id == "wms_terrestris_osm") {
      m <- m %>%
        addWMSTiles(
          baseUrl = TERRASTRIS_OSM_WMS,
          layers = "OSM-WMS",
          group = "base_tiles",
          options = WMSTileOptions(
            format = "image/png",
            transparent = FALSE,
            version = "1.1.1"
          )
        )

      return(m)
    }

    layer_info <- GIBS_WGS84_LAYERS[[base_id]]

    if (!is.null(layer_info)) {
      m <- m %>%
        addWMSTiles(
          baseUrl = GIBS_WMS_EPSG4326,
          layers = layer_info$layer,
          group = "base_tiles",
          options = WMSTileOptions(
            format = layer_info$format,
            transparent = layer_info$transparent,
            version = "1.1.1"
          )
        )
    }

    return(m)
  }

  m
}

add_base_layer_to_proxy <- function(proxy, projection, base_id) {
  proxy <- proxy %>% clearGroup("base_tiles")

  if (is.null(base_id) || base_id == "none") {
    return(proxy)
  }

  if (projection == "mercator") {
    provider <- get_leaflet_provider(base_id)

    if (!is.null(provider)) {
      proxy <- proxy %>%
        addProviderTiles(provider = provider, group = "base_tiles")
    }

    return(proxy)
  }

  if (projection == "wgs84") {

    if (base_id == "wms_terrestris_osm") {
      proxy <- proxy %>%
        addWMSTiles(
          baseUrl = TERRASTRIS_OSM_WMS,
          layers = "OSM-WMS",
          group = "base_tiles",
          options = WMSTileOptions(
            format = "image/png",
            transparent = FALSE,
            version = "1.1.1"
          )
        )

      return(proxy)
    }

    layer_info <- GIBS_WGS84_LAYERS[[base_id]]

    if (!is.null(layer_info)) {
      proxy <- proxy %>%
        addWMSTiles(
          baseUrl = GIBS_WMS_EPSG4326,
          layers = layer_info$layer,
          group = "base_tiles",
          options = WMSTileOptions(
            format = layer_info$format,
            transparent = layer_info$transparent,
            version = "1.1.1"
          )
        )
    }

    return(proxy)
  }

  proxy
}

# ============================================================
# 3) CAPAS REFERENCIALES
# ============================================================

load_reference_layers <- function() {
  countries <- tryCatch({
    rnaturalearth::ne_countries(
      scale = "medium",
      returnclass = "sf"
    )
  }, error = function(e) {
    message("No se pudieron cargar países: ", e$message)
    NULL
  })

  admin1_argentina <- tryCatch({
    rnaturalearth::ne_states(
      country = "Argentina",
      returnclass = "sf"
    )
  }, error = function(e) {
    message("No se pudieron cargar provincias: ", e$message)
    NULL
  })

  capitals <- tryCatch({
    pp <- rnaturalearth::ne_download(
      scale = 110,
      type = "populated_places",
      category = "cultural",
      returnclass = "sf"
    )

    if ("FEATURECLA" %in% names(pp)) {
      pp <- pp[pp$FEATURECLA %in% c("Admin-0 capital", "Admin-0 capital alt"), ]
    }

    pp
  }, error = function(e) {
    message("No se pudieron cargar capitales: ", e$message)
    NULL
  })

  list(
    countries = countries,
    admin1_argentina = admin1_argentina,
    capitals = capitals
  )
}

REF_LAYERS <- load_reference_layers()

# ============================================================
# 4) LISTAR ARCHIVOS GOES EN S3
# ============================================================

make_goes_prefix <- function(time_utc, product = PRODUCT) {
  time_utc <- lubridate::with_tz(time_utc, "UTC")

  year <- format(time_utc, "%Y")
  doy  <- format(time_utc, "%j")
  hour <- format(time_utc, "%H")

  paste0(product, "/", year, "/", doy, "/", hour, "/")
}

list_s3_keys <- function(bucket, prefix) {
  url <- paste0(
    "https://", bucket, ".s3.amazonaws.com/",
    "?list-type=2&prefix=", utils::URLencode(prefix, reserved = TRUE)
  )

  res <- httr::GET(url)

  if (httr::status_code(res) != 200) {
    warning("No se pudo listar S3. HTTP: ", httr::status_code(res))
    return(character())
  }

  txt <- httr::content(res, as = "text", encoding = "UTF-8")

  doc <- xml2::read_xml(txt)
  xml2::xml_ns_strip(doc)

  keys <- xml2::xml_text(xml2::xml_find_all(doc, ".//Contents/Key"))

  keys[keys != ""]
}

get_latest_mcmipf_key <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  status_fun("Buscando la última imagen MCMIPF disponible...")

  now_utc <- lubridate::with_tz(Sys.time(), "UTC")

  times <- seq(
    from = now_utc - lubridate::hours(hours_back),
    to   = now_utc,
    by   = "1 hour"
  )

  prefixes <- unique(vapply(
    times,
    make_goes_prefix,
    character(1),
    product = product
  ))

  keys <- purrr::map(prefixes, function(px) {
    status_fun(paste("Listando:", bucket, "/", px))
    list_s3_keys(bucket, px)
  }) |>
    unlist(use.names = FALSE) |>
    unique()

  keys <- keys[stringr::str_detect(keys, "\\.nc$")]

  if (length(keys) == 0) {
    stop("No se encontraron archivos recientes para ", product)
  }

  latest_key <- sort(keys) |> tail(1)

  status_fun(paste("Último archivo encontrado:", basename(latest_key)))

  latest_key
}

# ============================================================
# 5) DESCARGA ROBUSTA
# ============================================================

get_online_file_size <- function(bucket, key) {
  url <- paste0("https://", bucket, ".s3.amazonaws.com/", key)

  res <- httr::HEAD(url)

  if (httr::status_code(res) != 200) {
    warning("No se pudo consultar tamaño online. HTTP: ", httr::status_code(res))
    return(NA_real_)
  }

  size <- httr::headers(res)[["content-length"]]

  if (is.null(size)) {
    return(NA_real_)
  }

  as.numeric(size)
}

download_s3_object <- function(
    bucket,
    key,
    dest_dir = NC_DIR,
    status_fun = default_status_fun
) {
  filename <- basename(key)
  dest <- file.path(dest_dir, filename)

  url <- paste0("https://", bucket, ".s3.amazonaws.com/", key)

  status_fun("Consultando tamaño del archivo remoto...")

  online_size <- get_online_file_size(bucket, key)

  if (!is.na(online_size)) {
    status_fun(paste("Peso online:", format_mb(online_size)))
  } else {
    status_fun("Peso online no disponible.")
  }

  if (file.exists(dest)) {
    local_size <- file.info(dest)$size

    status_fun(paste("Archivo local existente. Peso local:", format_mb(local_size)))

    if (!is.na(online_size) &&
        !is.na(local_size) &&
        online_size == local_size) {
      status_fun("El archivo local coincide con el remoto. Se reutiliza.")
      return(dest)
    }

    status_fun("El archivo local no coincide con el remoto. Se borra y se descarga de nuevo.")
    unlink(dest)
  }

  status_fun("Descargando archivo NetCDF MCMIPF...")

  ok <- tryCatch({
    utils::download.file(
      url = url,
      destfile = dest,
      mode = "wb",
      quiet = FALSE
    )
    TRUE
  }, error = function(e) {
    warning("Error descargando archivo: ", e$message)
    FALSE
  })

  if (!ok) {
    return(NA_character_)
  }

  if (!file.exists(dest)) {
    return(NA_character_)
  }

  local_size <- file.info(dest)$size

  status_fun(paste("Descarga finalizada. Peso descargado:", format_mb(local_size)))

  if (!is.na(online_size) &&
      !is.na(local_size) &&
      online_size != local_size) {
    unlink(dest)
    stop(
      "La descarga quedó incompleta. ",
      "Online: ", online_size, " bytes; ",
      "Local: ", local_size, " bytes."
    )
  }

  dest
}

download_latest_mcmipf <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  latest_key <- get_latest_mcmipf_key(
    bucket = bucket,
    product = product,
    hours_back = hours_back,
    status_fun = status_fun
  )

  latest_path <- download_s3_object(
    bucket = bucket,
    key = latest_key,
    dest_dir = NC_DIR,
    status_fun = status_fun
  )

  if (is.na(latest_path) || !file.exists(latest_path)) {
    stop("No se pudo descargar el último archivo MCMIPF.")
  }

  list(
    key = latest_key,
    path = latest_path
  )
}

# ============================================================
# 6) TRUE COLOR: NETCDF MCMIPF -> RGB
# ============================================================

find_mcmipf_band <- function(sds_obj, band_name) {
  s_names <- names(sds_obj)

  i <- which(s_names == band_name)

  if (length(i) == 0) {
    i <- grep(paste0("^", band_name, "$"), s_names)
  }

  if (length(i) == 0) {
    stop("No se encontró la banda ", band_name, " dentro del NetCDF.")
  }

  sds_obj[[i[1]]]
}

clamp01 <- function(x) {
  terra::ifel(x < 0, 0, terra::ifel(x > 1, 1, x))
}

gamma_correct <- function(r, gamma = 2.2) {
  clamp01(r)^(1 / gamma)
}

rgb_rasters_to_png <- function(
    r_red,
    r_green,
    r_blue,
    output_png,
    gamma = 2.2,
    flip_vertical = FALSE,
    status_fun = default_status_fun
) {
  status_fun(paste("Generando PNG RGB:", basename(output_png)))

  r_red   <- gamma_correct(r_red, gamma = gamma)
  r_green <- gamma_correct(r_green, gamma = gamma)
  r_blue  <- gamma_correct(r_blue, gamma = gamma)

  red_m   <- terra::as.matrix(r_red, wide = TRUE)
  green_m <- terra::as.matrix(r_green, wide = TRUE)
  blue_m  <- terra::as.matrix(r_blue, wide = TRUE)

  if (isTRUE(flip_vertical)) {
    red_m   <- red_m[nrow(red_m):1, ]
    green_m <- green_m[nrow(green_m):1, ]
    blue_m  <- blue_m[nrow(blue_m):1, ]
  }

  alpha_m <- ifelse(
    is.na(red_m) | is.na(green_m) | is.na(blue_m),
    0,
    1
  )

  red_m[is.na(red_m)] <- 0
  green_m[is.na(green_m)] <- 0
  blue_m[is.na(blue_m)] <- 0

  img <- array(0, dim = c(nrow(red_m), ncol(red_m), 4))

  img[, , 1] <- red_m
  img[, , 2] <- green_m
  img[, , 3] <- blue_m
  img[, , 4] <- alpha_m

  png::writePNG(img, target = output_png)

  status_fun(paste("PNG creado:", basename(output_png)))

  invisible(output_png)
}

mcmipf_to_truecolor_pngs <- function(
    nc_file,
    output_original_png = TC_ORIGINAL_PNG,
    output_wgs84_png = TC_WGS84_PNG,
    output_3857_png = TC_3857_PNG,
    width_wgs84 = 3600,
    height_wgs84 = 1800,
    width_3857 = 3600,
    height_3857 = 3400,
    gamma = 2.2,
    status_fun = default_status_fun
) {
  if (!file.exists(nc_file)) {
    stop("No existe el NetCDF: ", nc_file)
  }

  nc_file_norm <- normalizePath(nc_file, winslash = "/", mustWork = TRUE)

  status_fun("Leyendo archivo NetCDF MCMIPF...")
  status_fun(basename(nc_file_norm))

  s <- terra::sds(nc_file_norm)
  s_names <- names(s)

  status_fun(paste("Variables encontradas:", paste(s_names, collapse = ", ")))

  status_fun("Leyendo bandas ABI C01, C02 y C03...")

  c01_blue  <- find_mcmipf_band(s, "CMI_C01")
  c02_red   <- find_mcmipf_band(s, "CMI_C02")
  c03_veg   <- find_mcmipf_band(s, "CMI_C03")

  status_fun("Aplicando fórmula de verde sintético...")

  r_red <- clamp01(c02_red)
  r_blue <- clamp01(c01_blue)

  r_green <- clamp01(
    0.45 * r_red +
      0.10 * clamp01(c03_veg) +
      0.45 * r_blue
  )

  status_fun("Generando PNG en proyección original GOES...")

  rgb_rasters_to_png(
    r_red = r_red,
    r_green = r_green,
    r_blue = r_blue,
    output_png = output_original_png,
    gamma = gamma,
    flip_vertical = FALSE,
    status_fun = status_fun
  )

  status_fun("Creando grilla global WGS84 EPSG:4326...")

  template_wgs84 <- terra::rast(
    ncols = width_wgs84,
    nrows = height_wgs84,
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90,
    crs = "EPSG:4326"
  )

  rgb_stack <- c(r_red, r_green, r_blue)
  names(rgb_stack) <- c("red", "green", "blue")

  status_fun("Reproyectando RGB a WGS84...")

  rgb_wgs84 <- terra::project(
    rgb_stack,
    template_wgs84,
    method = "bilinear"
  )

  status_fun("Generando PNG True Color WGS84...")

  rgb_rasters_to_png(
    r_red = rgb_wgs84[[1]],
    r_green = rgb_wgs84[[2]],
    r_blue = rgb_wgs84[[3]],
    output_png = output_wgs84_png,
    gamma = gamma,
    flip_vertical = FALSE,
    status_fun = status_fun
  )

  status_fun("Creando grilla global WebMercator EPSG:3857...")

  merc_ext <- terra::ext(
    -20037508.342789244,
    20037508.342789244,
    -20037508.342789244,
    20037508.342789244
  )

  template_3857 <- terra::rast(
    ncols = width_3857,
    nrows = height_3857,
    ext = merc_ext,
    crs = "EPSG:3857"
  )

  status_fun("Reproyectando RGB a EPSG:3857...")

  rgb_3857 <- terra::project(
    rgb_stack,
    template_3857,
    method = "bilinear"
  )

  status_fun("Generando PNG True Color Mercator...")

  rgb_rasters_to_png(
    r_red = rgb_3857[[1]],
    r_green = rgb_3857[[2]],
    r_blue = rgb_3857[[3]],
    output_png = output_3857_png,
    gamma = gamma,
    flip_vertical = FALSE,
    status_fun = status_fun
  )

  invisible(list(
    original_png = output_original_png,
    original_nrow = terra::nrow(r_red),
    original_ncol = terra::ncol(r_red),
    wgs84_png = output_wgs84_png,
    mercator_png = output_3857_png
  ))
}

update_truecolor_image <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    gamma = 2.2,
    status_fun = default_status_fun
) {
  status_fun("Iniciando actualización True Color...")

  latest <- download_latest_mcmipf(
    bucket = bucket,
    product = product,
    hours_back = hours_back,
    status_fun = status_fun
  )

  status_fun("Archivo NetCDF listo. Iniciando composición RGB...")

  processed <- mcmipf_to_truecolor_pngs(
    nc_file = latest$path,
    output_original_png = TC_ORIGINAL_PNG,
    output_wgs84_png = TC_WGS84_PNG,
    output_3857_png = TC_3857_PNG,
    width_wgs84 = 3600,
    height_wgs84 = 1800,
    width_3857 = 3600,
    height_3857 = 3400,
    gamma = gamma,
    status_fun = status_fun
  )

  status_fun("Procesamiento True Color completo.")

  list(
    key = latest$key,
    path = latest$path,
    processed = processed
  )
}

# ============================================================
# 7) UI
# ============================================================

ui <- tagList(
  tags$head(
    tags$style(HTML("
      html, body {
        margin: 0 !important;
        padding: 0 !important;
        width: 100vw !important;
        height: 100vh !important;
        overflow: hidden !important;
        background: #000000 !important;
      }

      #truecolor_app {
        position: fixed;
        inset: 0;
        width: 100vw;
        height: 100vh;
        overflow: hidden;
        font-family: Arial, sans-serif;
        background: #000000 !important;
      }

      #map {
        width: 100vw !important;
        height: 100vh !important;
      }

      .leaflet-container {
        background: #000000 !important;
      }

      .top-toolbar {
        position: absolute;
        z-index: 10000;
        background: rgba(0, 0, 0, 0.72);
        padding: 8px 10px;
        border-radius: 8px;
        display: flex;
        gap: 8px;
        align-items: center;
      }

      #map_toolbar_left {
        top: 10px;
        left: 60px;
      }

      #map_toolbar_right {
        top: 10px;
        right: 12px;
      }

      .toolbar-btn {
        background: #1f78b4;
        color: white;
        border: none;
        padding: 7px 11px;
        border-radius: 5px;
        cursor: pointer;
        font-size: 13px;
      }

      .toolbar-btn:hover {
        background: #145a86;
      }

      .floating-panel {
        position: absolute;
        z-index: 9999;
        border-radius: 8px;
        font-family: Arial, sans-serif;
        box-shadow: 0 4px 16px rgba(0,0,0,0.35);
      }

      .panel-header {
        cursor: move;
        user-select: none;
        font-weight: bold;
        padding: 8px 10px;
        border-radius: 8px 8px 0 0;
      }

      .panel-body {
        padding: 10px 12px;
      }

      #panel_base {
        top: 62px;
        left: 12px;
        width: 380px;
        max-height: 78vh;
        overflow-y: auto;
        background: rgba(255, 255, 255, 0.96);
        color: black;
      }

      #panel_base .panel-header {
        background: rgba(235, 235, 235, 0.98);
      }

      #panel_layers {
        top: 62px;
        left: 410px;
        width: 350px;
        background: rgba(255, 255, 255, 0.96);
        color: black;
      }

      #panel_layers .panel-header {
        background: rgba(235, 235, 235, 0.98);
      }

      #panel_process {
        top: 62px;
        right: 12px;
        width: 600px;
        max-width: 600px;
        background: rgba(0, 0, 0, 0.84);
        color: white;
      }

      #panel_process .panel-header {
        background: rgba(20, 20, 20, 0.95);
      }

      #panel_process pre {
        color: white;
        background: transparent;
        border: none;
        padding: 0;
        margin: 8px 0 0 0;
        white-space: pre-wrap;
        max-height: 560px;
        overflow-y: auto;
      }

      #panel_downloads {
        top: 62px;
        right: 630px;
        width: 330px;
        background: rgba(255, 255, 255, 0.96);
        color: black;
      }

      #panel_downloads .panel-header {
        background: rgba(235, 235, 235, 0.98);
      }

      #panel_downloads .btn,
      #panel_downloads button,
      #panel_downloads a {
        width: 100%;
        margin-bottom: 7px;
      }

      #refresh_truecolor {
        background: #00a870;
        color: white;
        border: none;
        padding: 7px 11px;
        border-radius: 5px;
        cursor: pointer;
        margin-top: 8px;
      }

      #refresh_truecolor:hover {
        background: #007c54;
      }

      #projection_container {
        display: flex !important;
        align-items: center !important;
        height: 30px;
        margin: 0 !important;
        padding: 0 !important;
      }

      #projection_container .form-group {
        margin: 0 !important;
        padding: 0 !important;
      }

      #projection_container select {
        height: 30px !important;
        min-height: 30px !important;
        padding: 3px 28px 3px 8px !important;
        font-size: 13px !important;
        border-radius: 5px !important;
        border: none !important;
      }

      #truecolor_toggle_container {
        display: flex !important;
        align-items: center !important;
        height: 30px;
        color: white !important;
      }

      #truecolor_toggle_container label {
        color: white !important;
        margin: 0 !important;
        padding: 0 !important;
        font-size: 13px !important;
        font-weight: normal !important;
        display: flex !important;
        align-items: center !important;
        gap: 4px;
      }

      #truecolor_toggle_container input[type='checkbox'] {
        position: static !important;
        margin: 0 4px 0 0 !important;
        opacity: 1 !important;
        display: inline-block !important;
        width: auto !important;
        height: auto !important;
      }

      .small-note {
        font-size: 12px;
        color: #555;
      }
    ")),

    tags$script(HTML("
      function tc_togglePanel(id) {
        var el = document.getElementById(id);
        if (!el) return;

        if (el.style.display === 'none') {
          el.style.display = 'block';
        } else {
          el.style.display = 'none';
        }
      }

      function tc_makePanelDraggable(panelId) {
        var panel = document.getElementById(panelId);
        if (!panel) return;

        var header = panel.querySelector('.panel-header');
        if (!header) return;

        var isDown = false;
        var offsetX = 0;
        var offsetY = 0;

        header.addEventListener('mousedown', function(e) {
          isDown = true;

          var rect = panel.getBoundingClientRect();

          offsetX = e.clientX - rect.left;
          offsetY = e.clientY - rect.top;

          panel.style.left = rect.left + 'px';
          panel.style.top = rect.top + 'px';
          panel.style.right = 'auto';
          panel.style.bottom = 'auto';

          e.preventDefault();
        });

        document.addEventListener('mousemove', function(e) {
          if (!isDown) return;

          panel.style.left = (e.clientX - offsetX) + 'px';
          panel.style.top  = (e.clientY - offsetY) + 'px';
        });

        document.addEventListener('mouseup', function() {
          isDown = false;
        });
      }

      function tc_addOrUpdateOverlay(map, message) {
        var url = message.url;
        var visible = message.visible;
        var projection = message.projection;

        var imageBounds;

        if (projection === 'goes') {
          var ncol = message.ncol || 5424;
          var nrow = message.nrow || 5424;

          imageBounds = [
            [0, 0],
            [nrow, ncol]
          ];

        } else if (projection === 'wgs84') {

          imageBounds = [
            [-90, -180],
            [ 90,  180]
          ];

        } else {

          imageBounds = [
            [-85.05112878, -180],
            [ 85.05112878,  180]
          ];
        }

        map.truecolorUrl = url;
        map.truecolorProjection = projection;
        map.truecolorBounds = imageBounds;
        map.truecolorNrow = message.nrow;
        map.truecolorNcol = message.ncol;

        if (!map.getPane('pngPane')) {
          map.createPane('pngPane');
          map.getPane('pngPane').style.zIndex = 450;
        }

        if (!map.getPane('refPane')) {
          map.createPane('refPane');
          map.getPane('refPane').style.zIndex = 650;
        }

        if (map.truecolorOverlay) {
          map.removeLayer(map.truecolorOverlay);
          map.truecolorOverlay = null;
        }

        if (!visible) {
          return;
        }

        var overlay = L.imageOverlay(
          url,
          imageBounds,
          {
            opacity: 1,
            interactive: false,
            pane: 'pngPane'
          }
        );

        overlay.on('load', function() {
          console.log('True Color cargado:', url);
        });

        overlay.on('error', function(e) {
          console.error('ERROR cargando True Color:', url, e);
          alert('No se pudo cargar la imagen True Color.');
        });

        overlay.addTo(map);
        map.truecolorOverlay = overlay;
      }

      document.addEventListener('DOMContentLoaded', function() {
        tc_makePanelDraggable('panel_base');
        tc_makePanelDraggable('panel_layers');
        tc_makePanelDraggable('panel_process');
        tc_makePanelDraggable('panel_downloads');
      });

      Shiny.addCustomMessageHandler('reload-truecolor', function(message) {
        var widget = HTMLWidgets.find('#map');

        if (!widget) {
          console.error('No se encontró el widget Leaflet');
          return;
        }

        var map = widget.getMap();

        if (!map) {
          console.error('No se encontró el objeto Leaflet');
          return;
        }

        tc_addOrUpdateOverlay(map, message);
      });

      Shiny.addCustomMessageHandler('toggle-truecolor', function(message) {
        var widget = HTMLWidgets.find('#map');

        if (!widget) {
          console.error('No se encontró el widget Leaflet');
          return;
        }

        var map = widget.getMap();

        if (!map) {
          console.error('No se encontró el objeto Leaflet');
          return;
        }

        if (!map.truecolorUrl) {
          return;
        }

        tc_addOrUpdateOverlay(map, {
          url: map.truecolorUrl,
          visible: message.visible,
          projection: map.truecolorProjection,
          nrow: map.truecolorNrow,
          ncol: map.truecolorNcol
        });
      });

      Shiny.addCustomMessageHandler('update-status-live-truecolor', function(message) {
        var el = document.getElementById('truecolor_status');

        if (el) {
          el.textContent = message.text;
          el.scrollTop = el.scrollHeight;
        }
      });
    "))
  ),

  div(
    id = "truecolor_app",

    div(
      id = "map_toolbar_left",
      class = "top-toolbar",

      div(
        id = "projection_container",
        selectInput(
          inputId = "map_projection",
          label = NULL,
          choices = PROJECTION_CHOICES,
          selected = DEFAULT_PROJECTION,
          width = "150px"
        )
      ),

      tags$button(
        class = "toolbar-btn",
        onclick = "tc_togglePanel('panel_base')",
        "Fondos"
      ),

      div(
        id = "truecolor_toggle_container",
        checkboxInput(
          inputId = "show_truecolor",
          label = "True Color",
          value = TRUE
        )
      ),

      tags$button(
        class = "toolbar-btn",
        onclick = "tc_togglePanel('panel_layers')",
        "Capas"
      )
    ),

    div(
      id = "map_toolbar_right",
      class = "top-toolbar",

      tags$button(
        class = "toolbar-btn",
        onclick = "tc_togglePanel('panel_process')",
        "Procesamiento"
      ),

      tags$button(
        class = "toolbar-btn",
        onclick = "tc_togglePanel('panel_downloads')",
        "Descargas"
      )
    ),

    div(
      id = "panel_base",
      class = "floating-panel",
      style = "display: none;",
      div(class = "panel-header", "Fondos de mapa"),
      div(
        class = "panel-body",
        radioButtons(
          inputId = "base_map",
          label = NULL,
          choices = BASE_MAPS_BY_PROJECTION[[DEFAULT_PROJECTION]],
          selected = default_base_for_projection(DEFAULT_PROJECTION)
        ),
        div(
          class = "small-note",
          "Cada proyección tiene su propio menú de fondos."
        )
      )
    ),

    div(
      id = "panel_layers",
      class = "floating-panel",
      style = "display: none;",
      div(class = "panel-header", "Capas referenciales"),
      div(
        class = "panel-body",
        checkboxGroupInput(
          inputId = "reference_layers",
          label = NULL,
          choices = c(
            "Límites internacionales" = "countries",
            "Límites provinciales / administrativos" = "admin1",
            "Capitales" = "capitals"
          ),
          selected = character()
        ),
        div(
          class = "small-note",
          "Las capas geográficas se muestran encima de True Color. En GOES original se desactivan."
        )
      )
    ),

    div(
      id = "panel_process",
      class = "floating-panel",
      div(class = "panel-header", "Procesamiento GOES True Color"),
      div(
        class = "panel-body",

        numericInput(
          inputId = "hours_back",
          label = "Buscar últimas horas hacia atrás",
          value = 6,
          min = 1,
          max = 48,
          step = 1
        ),

        numericInput(
          inputId = "gamma",
          label = "Gamma",
          value = 2.2,
          min = 1,
          max = 4,
          step = 0.1
        ),

        actionButton(
          inputId = "refresh_truecolor",
          label = "Descargar y procesar última imagen True Color"
        ),

        br(),
        br(),

        strong("Estado:"),
        verbatimTextOutput("truecolor_status")
      )
    ),

    div(
      id = "panel_downloads",
      class = "floating-panel",
      style = "display: none;",
      div(class = "panel-header", "Descargas"),
      div(
        class = "panel-body",

        downloadButton(
          outputId = "download_png_wgs84",
          label = "Descargar PNG WGS84"
        ),

        downloadButton(
          outputId = "download_png_mercator",
          label = "Descargar PNG Mercator"
        ),

        downloadButton(
          outputId = "download_png_goes",
          label = "Descargar PNG GOES original"
        )
      )
    ),

    leafletOutput("map")
  )
)

# ============================================================
# 8) SERVER
# ============================================================

server <- function(input, output, session) {

  current_key <- reactiveVal(NULL)
  current_path <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)
  last_processed <- reactiveVal(NULL)

  status_text <- reactiveVal(
    "App iniciada.\nMapa listo.\nPresioná el botón para descargar y procesar la última imagen True Color."
  )

  selected_truecolor_url <- function(projection, processed) {
    if (is.null(processed)) {
      return(NULL)
    }

    cache_buster <- as.numeric(Sys.time())

    if (projection == "goes") {
      return(paste0("truecolor_original_goes.png?v=", cache_buster))
    }

    if (projection == "wgs84") {
      return(paste0("truecolor_global_wgs84.png?v=", cache_buster))
    }

    paste0("truecolor_global_3857.png?v=", cache_buster)
  }

  output$truecolor_status <- renderText({
    extra <- ""

    if (!is.null(current_key())) {
      extra <- paste0(
        "\n\nÚltimo archivo:\n",
        basename(current_key()),
        "\n\nNetCDF temporal:\n",
        current_path(),
        "\n\nActualizado:\n",
        format(last_update(), "%Y-%m-%d %H:%M:%OS3")
      )
    }

    paste0(status_text(), extra)
  })

  output$map <- renderLeaflet({

    projection <- input$map_projection

    if (is.null(projection)) {
      projection <- DEFAULT_PROJECTION
    }

    base_selected <- isolate(input$base_map)

    if (is.null(base_selected)) {
      base_selected <- default_base_for_projection(projection)
    }

    if (projection == "goes") {
      return(
        leaflet(
          options = leafletOptions(
            crs = leafletCRS(crsClass = "L.CRS.Simple"),
            zoomControl = TRUE,
            minZoom = -5,
            maxZoom = 5,
            attributionControl = FALSE
          )
        ) %>%
          addMapPane("pngPane", zIndex = 450) %>%
          addMapPane("refPane", zIndex = 650) %>%
          setView(lng = 0, lat = 0, zoom = -2)
      )
    }

    if (projection == "wgs84") {
      m <- leaflet(
        options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.EPSG4326"),
          zoomControl = TRUE,
          minZoom = 1,
          maxZoom = 10,
          zoomSnap = 0.10,
          zoomDelta = 0.10,
          worldCopyJump = TRUE,
          attributionControl = TRUE
        )
      ) %>%
        addMapPane("pngPane", zIndex = 450) %>%
        addMapPane("refPane", zIndex = 650) %>%
        setView(lng = 0, lat = 0, zoom = 2)

      m <- add_base_layer_to_map(
        m = m,
        projection = projection,
        base_id = base_selected
      )

      return(m)
    }

    m <- leaflet(
      options = leafletOptions(
        zoomControl = TRUE,
        minZoom = 1,
        maxZoom = 10,
        zoomSnap = 0.10,
        zoomDelta = 0.10,
        worldCopyJump = FALSE,
        attributionControl = TRUE
      )
    ) %>%
      addMapPane("pngPane", zIndex = 450) %>%
      addMapPane("refPane", zIndex = 650) %>%
      setView(lng = 0, lat = 0, zoom = 2)

    m <- add_base_layer_to_map(
      m = m,
      projection = projection,
      base_id = base_selected
    )

    m
  })

  observeEvent(input$map_projection, {

    projection_now <- isolate(input$map_projection)

    if (is.null(projection_now)) {
      projection_now <- DEFAULT_PROJECTION
    }

    choices_now <- BASE_MAPS_BY_PROJECTION[[projection_now]]

    if (is.null(choices_now) || length(choices_now) == 0) {
      choices_now <- c("Sin fondo geográfico" = "none")
    }

    updateRadioButtons(
      session = session,
      inputId = "base_map",
      choices = choices_now,
      selected = default_base_for_projection(projection_now)
    )

    processed <- last_processed()

    session$onFlushed(function() {

      if (is.null(processed)) {
        return()
      }

      session$sendCustomMessage(
        type = "reload-truecolor",
        message = list(
          url = selected_truecolor_url(projection_now, processed),
          visible = isTRUE(isolate(input$show_truecolor)),
          projection = projection_now,
          nrow = processed$original_nrow,
          ncol = processed$original_ncol
        )
      )

    }, once = TRUE)

  }, ignoreInit = TRUE)

  observeEvent(input$show_truecolor, {
    session$sendCustomMessage(
      type = "toggle-truecolor",
      message = list(
        visible = isTRUE(input$show_truecolor)
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$base_map, {

    projection <- isolate(input$map_projection)

    if (is.null(projection)) {
      projection <- DEFAULT_PROJECTION
    }

    proxy <- leafletProxy("map", session = session)

    proxy <- add_base_layer_to_proxy(
      proxy = proxy,
      projection = projection,
      base_id = input$base_map
    )

  }, ignoreInit = TRUE)

  observe({

    selected <- input$reference_layers

    projection <- input$map_projection

    if (is.null(projection)) {
      projection <- DEFAULT_PROJECTION
    }

    proxy <- leafletProxy("map", session = session) %>%
      clearGroup("countries") %>%
      clearGroup("admin1") %>%
      clearGroup("capitals")

    if (projection == "goes") {
      return()
    }

    if (is.null(selected) || length(selected) == 0) {
      return()
    }

    if ("countries" %in% selected && !is.null(REF_LAYERS$countries)) {
      proxy <- proxy %>%
        addPolygons(
          data = REF_LAYERS$countries,
          group = "countries",
          fill = FALSE,
          color = "#ffffff",
          weight = 1.2,
          opacity = 1,
          label = ~name,
          options = pathOptions(pane = "refPane")
        )
    }

    if ("admin1" %in% selected && !is.null(REF_LAYERS$admin1_argentina)) {
      label_col <- if ("name" %in% names(REF_LAYERS$admin1_argentina)) {
        REF_LAYERS$admin1_argentina$name
      } else {
        rep("Admin 1", nrow(REF_LAYERS$admin1_argentina))
      }

      proxy <- proxy %>%
        addPolygons(
          data = REF_LAYERS$admin1_argentina,
          group = "admin1",
          fill = FALSE,
          color = "#00ffff",
          weight = 1.6,
          opacity = 1,
          label = label_col,
          options = pathOptions(pane = "refPane")
        )
    }

    if ("capitals" %in% selected && !is.null(REF_LAYERS$capitals)) {
      cap_label <- if ("NAME" %in% names(REF_LAYERS$capitals)) {
        REF_LAYERS$capitals$NAME
      } else if ("name" %in% names(REF_LAYERS$capitals)) {
        REF_LAYERS$capitals$name
      } else {
        rep("Capital", nrow(REF_LAYERS$capitals))
      }

      proxy <- proxy %>%
        addCircleMarkers(
          data = REF_LAYERS$capitals,
          group = "capitals",
          radius = 3,
          stroke = TRUE,
          weight = 1,
          color = "#000000",
          fillColor = "#ffff00",
          fillOpacity = 0.95,
          opacity = 1,
          label = cap_label,
          options = pathOptions(pane = "refPane")
        )
    }
  })

  output$download_png_wgs84 <- downloadHandler(
    filename = function() {
      paste0("truecolor_wgs84_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      validate(
        need(file.exists(TC_WGS84_PNG), "Todavía no existe el PNG WGS84. Primero procesá una imagen.")
      )

      file.copy(TC_WGS84_PNG, file, overwrite = TRUE)
    },
    contentType = "image/png"
  )

  output$download_png_mercator <- downloadHandler(
    filename = function() {
      paste0("truecolor_mercator_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      validate(
        need(file.exists(TC_3857_PNG), "Todavía no existe el PNG Mercator. Primero procesá una imagen.")
      )

      file.copy(TC_3857_PNG, file, overwrite = TRUE)
    },
    contentType = "image/png"
  )

  output$download_png_goes <- downloadHandler(
    filename = function() {
      paste0("truecolor_goes_original_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      validate(
        need(file.exists(TC_ORIGINAL_PNG), "Todavía no existe el PNG GOES original. Primero procesá una imagen.")
      )

      file.copy(TC_ORIGINAL_PNG, file, overwrite = TRUE)
    },
    contentType = "image/png"
  )

  observeEvent(input$refresh_truecolor, {

    status_log <- character()

    add_status <- function(txt) {
      txt <- as.character(txt)

      status_log <<- c(
        status_log,
        paste0(timestamp_ms(), " - ", txt)
      )

      nuevo_texto <- paste(status_log, collapse = "\n")

      status_text(nuevo_texto)

      session$sendCustomMessage(
        type = "update-status-live-truecolor",
        message = list(text = nuevo_texto)
      )

      try(session$flushReact(), silent = TRUE)

      message(txt)
    }

    withProgress(message = "Procesando GOES True Color", value = 0, {

      step <- 0

      progress_status <- function(txt) {
        step <<- min(step + 1, 20)

        incProgress(
          amount = 1 / 20,
          detail = txt
        )

        add_status(txt)
      }

      info <- tryCatch({
        update_truecolor_image(
          bucket = BUCKET,
          product = PRODUCT,
          hours_back = input$hours_back,
          gamma = input$gamma,
          status_fun = progress_status
        )
      }, error = function(e) {
        add_status(paste("ERROR:", e$message))

        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 10
        )

        return(NULL)
      })

      if (is.null(info)) {
        return()
      }

      progress_status("Cargando PNG según proyección seleccionada...")

      current_key(info$key)
      current_path(info$path)
      last_update(Sys.time())
      last_processed(info$processed)

      projection <- input$map_projection

      if (is.null(projection)) {
        projection <- DEFAULT_PROJECTION
      }

      session$sendCustomMessage(
        type = "reload-truecolor",
        message = list(
          url = selected_truecolor_url(projection, info$processed),
          visible = isTRUE(input$show_truecolor),
          projection = projection,
          nrow = info$processed$original_nrow,
          ncol = info$processed$original_ncol
        )
      )

      progress_status("Imagen True Color cargada en el mapa.")

      showNotification(
        paste("Imagen True Color actualizada:", basename(info$key)),
        type = "message",
        duration = 6
      )
    })
  })
}

# ============================================================
# 9) LANZAR APP
# ============================================================

shinyApp(ui = ui, server = server)

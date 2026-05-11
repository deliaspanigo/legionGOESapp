library(shiny)
library(leaflet)
library(terra)
library(httr)
library(xml2)
library(stringr)
library(lubridate)
library(purrr)
library(png)

# ============================================================
# 0) CONFIGURACIÓN GENERAL
# ============================================================

BUCKET  <- "noaa-goes19"
PRODUCT <- "ABI-L2-FDCF"

NC_DIR <- file.path(tempdir(), "goes_fdcf_downloads")
dir.create(NC_DIR, showWarnings = FALSE, recursive = TRUE)

dir.create("www", showWarnings = FALSE, recursive = TRUE)

FDCF_PNGS <- list(
  fn01 = "www/fdcf_fn01_wgs84.png",
  fn02 = "www/fdcf_fn02_wgs84.png",
  fn03 = "www/fdcf_fn03_wgs84.png",
  fn04 = "www/fdcf_fn04_wgs84.png",
  fn05 = "www/fdcf_fn05_wgs84.png"
)

FDCF_CHOICES <- c(
  "FN01 - Clasificación completa" = "fn01",
  "FN02 - Magma completa" = "fn02",
  "FN03 - Fuegos + glint" = "fn03",
  "FN04 - Solo glint" = "fn04",
  "FN05 - Solo focos de calor" = "fn05"
)

DEFAULT_FDCF_STYLE <- "fn05"

GIBS_WMS_EPSG4326 <- "https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi"

message("Carpeta temporal de descarga: ", NC_DIR)


# ============================================================
# 1) FUNCIONES AUXILIARES
# ============================================================

timestamp_ms <- function() {
  format(Sys.time(), "%H:%M:%OS3")
}

default_status_fun <- function(txt) {
  message(txt)
}


# ============================================================
# 2) VALORES Y ETIQUETAS FDCF
# ============================================================

fdcf_values <- c(
  -99, 0,
  10, 11, 12, 13, 14, 15,
  16, 17, 18, 19, 20, 21, 22,
  30, 31, 32, 33, 34, 35,
  40, 50, 60,
  100,
  120, 121, 123, 124, 125, 126, 127,
  151, 152, 153,
  200, 215
)

fdcf_labels <- c(
  "-99: Initialization value",
  "0: Non-processed region",
  "10: Processed fire pixel",
  "11: Saturated fire pixel",
  "12: Cloud contaminated fire",
  "13: High probability fire",
  "14: Medium probability fire",
  "15: Low probability fire",
  "16: Reserved category 1",
  "17: Reserved category 2",
  "18: Reserved category 3",
  "19: Reserved category 4",
  "20: Reserved category 5",
  "21: Reserved category 6",
  "22: Reserved category 7",
  "30: TF Processed fire pixel",
  "31: TF Saturated fire pixel",
  "32: TF Cloud contaminated fire pixel",
  "33: TF High probability fire pixel",
  "34: TF Medium probability fire pixel",
  "35: TF Low probability fire pixel",
  "40: Space pixel",
  "50: Atmospheric Limb",
  "60: Solar Glint / Block-out",
  "100: Processed region - Land",
  "120: Bad L1b data",
  "121: Extremely cloudy pixel",
  "123: Value out of range",
  "124: Sensor calibration error",
  "125: Pixel not processed by algorithm",
  "126: Outside scan area",
  "127: Fill Value",
  "151: Sea water",
  "152: Coastline fringe",
  "153: Inland water",
  "200: Cloud test",
  "215: Cloud test day"
)


# ============================================================
# 3) PALETAS FDCF
# ============================================================

make_palette_matrix <- function(x) {
  matrix(x, ncol = 4, byrow = TRUE)
}

FDCF_PALETTES <- list(

  fn01 = make_palette_matrix(c(
    18, 18, 18, 255,
    10, 18, 30, 255,
    255, 0, 0, 255,
    255, 255, 0, 255,
    255, 140, 0, 255,
    255, 69, 0, 255,
    255, 165, 0, 255,
    255, 215, 0, 255,
    138, 43, 226, 255,
    128, 0, 128, 255,
    75, 0, 130, 255,
    255, 0, 255, 255,
    218, 112, 214, 255,
    221, 160, 221, 255,
    216, 191, 216, 255,
    213, 0, 249, 255,
    101, 31, 255, 255,
    61, 90, 254, 255,
    0, 176, 255, 255,
    0, 229, 255, 255,
    29, 233, 182, 255,
    0, 0, 0, 255,
    255, 0, 127, 255,
    0, 255, 200, 255,
    20, 51, 17, 255,
    18, 18, 18, 255,
    47, 79, 79, 255,
    11, 11, 11, 255,
    15, 15, 15, 255,
    5, 5, 5, 255,
    0, 0, 0, 255,
    0, 0, 0, 255,
    0, 51, 102, 255,
    0, 91, 150, 255,
    0, 119, 190, 255,
    207, 216, 220, 255,
    236, 239, 241, 255
  )),

  fn02 = make_palette_matrix(c(
    0, 0, 3, 255,
    0, 0, 3, 255,
    2, 0, 10, 255,
    7, 0, 20, 255,
    14, 2, 35, 255,
    24, 6, 52, 255,
    37, 9, 70, 255,
    51, 10, 88, 255,
    65, 13, 104, 255,
    80, 18, 118, 255,
    95, 24, 125, 255,
    111, 30, 128, 255,
    127, 37, 128, 255,
    144, 44, 126, 255,
    160, 51, 122, 255,
    176, 58, 116, 255,
    190, 67, 109, 255,
    205, 77, 101, 255,
    218, 88, 91, 255,
    230, 101, 81, 255,
    240, 116, 70, 255,
    247, 134, 58, 255,
    251, 153, 49, 255,
    252, 173, 47, 255,
    251, 193, 56, 255,
    247, 213, 75, 255,
    241, 233, 103, 255,
    241, 243, 140, 255,
    245, 250, 175, 255,
    250, 252, 206, 255,
    252, 253, 233, 255,
    255, 255, 255, 255,
    200, 200, 200, 255,
    180, 180, 180, 255,
    160, 160, 160, 255,
    140, 140, 140, 255,
    120, 120, 120, 255
  )),

  fn03 = make_palette_matrix(c(
    0, 0, 3, 0,
    0, 0, 3, 0,
    2, 0, 10, 255,
    7, 0, 20, 255,
    14, 2, 35, 255,
    24, 6, 52, 255,
    37, 9, 70, 255,
    51, 10, 88, 255,
    65, 13, 104, 0,
    80, 18, 118, 0,
    95, 24, 125, 0,
    111, 30, 128, 0,
    127, 37, 128, 0,
    144, 44, 126, 0,
    160, 51, 122, 0,
    176, 58, 116, 0,
    190, 67, 109, 255,
    205, 77, 101, 255,
    218, 88, 91, 255,
    230, 101, 81, 255,
    240, 116, 70, 255,
    247, 134, 58, 0,
    251, 153, 49, 0,
    252, 173, 47, 255,
    251, 193, 56, 0,
    247, 213, 75, 0,
    241, 233, 103, 0,
    241, 243, 140, 0,
    245, 250, 175, 0,
    250, 252, 206, 0,
    252, 253, 233, 0,
    255, 255, 255, 0,
    200, 200, 200, 0,
    180, 180, 180, 0,
    160, 160, 160, 0,
    140, 140, 140, 0,
    120, 120, 120, 0
  )),

  fn04 = make_palette_matrix(c(
    0, 0, 3, 0,
    0, 0, 3, 0,
    2, 0, 10, 0,
    7, 0, 20, 0,
    14, 2, 35, 0,
    24, 6, 52, 0,
    37, 9, 70, 0,
    51, 10, 88, 0,
    65, 13, 104, 0,
    80, 18, 118, 0,
    95, 24, 125, 0,
    111, 30, 128, 0,
    127, 37, 128, 0,
    144, 44, 126, 0,
    160, 51, 122, 0,
    176, 58, 116, 0,
    190, 67, 109, 0,
    205, 77, 101, 0,
    218, 88, 91, 0,
    230, 101, 81, 0,
    240, 116, 70, 0,
    247, 134, 58, 0,
    251, 153, 49, 0,
    252, 173, 47, 255,
    251, 193, 56, 0,
    247, 213, 75, 0,
    241, 233, 103, 0,
    241, 243, 140, 0,
    245, 250, 175, 0,
    250, 252, 206, 0,
    252, 253, 233, 0,
    255, 255, 255, 0,
    200, 200, 200, 0,
    180, 180, 180, 0,
    160, 160, 160, 0,
    140, 140, 140, 0,
    120, 120, 120, 0
  )),

  fn05 = make_palette_matrix(c(
    0, 0, 3, 0,
    0, 0, 3, 0,
    2, 0, 10, 255,
    7, 0, 20, 255,
    14, 2, 35, 255,
    24, 6, 52, 255,
    37, 9, 70, 255,
    51, 10, 88, 255,
    65, 13, 104, 0,
    80, 18, 118, 0,
    95, 24, 125, 0,
    111, 30, 128, 0,
    127, 37, 128, 0,
    144, 44, 126, 0,
    160, 51, 122, 0,
    176, 58, 116, 0,
    190, 67, 109, 255,
    205, 77, 101, 255,
    218, 88, 91, 255,
    230, 101, 81, 255,
    240, 116, 70, 255,
    247, 134, 58, 0,
    251, 153, 49, 0,
    252, 173, 47, 0,
    251, 193, 56, 0,
    247, 213, 75, 0,
    241, 233, 103, 0,
    241, 243, 140, 0,
    245, 250, 175, 0,
    250, 252, 206, 0,
    252, 253, 233, 0,
    255, 255, 255, 0,
    200, 200, 200, 0,
    180, 180, 180, 0,
    160, 160, 160, 0,
    140, 140, 140, 0,
    120, 120, 120, 0
  ))
)


# ============================================================
# 4) LISTADO Y DESCARGA DESDE S3
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

  res <- httr::GET(url, httr::timeout(30))

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

get_latest_fdcf_key <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  status_fun("Buscando último archivo FDCF disponible...")

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

get_online_file_size <- function(bucket, key) {
  url <- paste0("https://", bucket, ".s3.amazonaws.com/", key)

  res <- httr::HEAD(url, httr::timeout(30))

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

  status_fun("Consultando tamaño remoto...")

  online_size <- get_online_file_size(bucket, key)

  if (!is.na(online_size)) {
    status_fun(paste("Peso online:", round(online_size / 1024^2, 2), "MB"))
  } else {
    status_fun("Peso online no disponible.")
  }

  if (file.exists(dest)) {
    local_size <- file.info(dest)$size

    status_fun(paste(
      "Archivo local existente. Peso local:",
      round(local_size / 1024^2, 2),
      "MB"
    ))

    if (!is.na(online_size) &&
        !is.na(local_size) &&
        online_size == local_size) {
      status_fun("El archivo local coincide con el remoto. Se reutiliza.")
      return(dest)
    }

    status_fun("El archivo local no coincide. Se descarga nuevamente.")
    unlink(dest)
  }

  status_fun("Descargando archivo NetCDF FDCF...")

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

  if (!ok || !file.exists(dest)) {
    return(NA_character_)
  }

  local_size <- file.info(dest)$size

  status_fun(paste(
    "Descarga finalizada. Peso descargado:",
    round(local_size / 1024^2, 2),
    "MB"
  ))

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

download_latest_fdcf <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  latest_key <- get_latest_fdcf_key(
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
    stop("No se pudo descargar el último archivo FDCF.")
  }

  list(
    key = latest_key,
    path = latest_path
  )
}


# ============================================================
# 5) COLORIZAR MATRIZ A PNG
# ============================================================

fdcf_matrix_to_png <- function(
    m,
    palette_rgba,
    output_png,
    status_fun = default_status_fun
) {
  status_fun(paste("Generando PNG:", basename(output_png)))

  img <- array(0, dim = c(nrow(m), ncol(m), 4))

  # Fondo por defecto transparente.
  # Esto aplica a NA, out_of_range y fill no reconocido.
  img[, , 1] <- 0
  img[, , 2] <- 0
  img[, , 3] <- 0
  img[, , 4] <- 0

  for (i in seq_along(fdcf_values)) {
    val <- fdcf_values[i]
    idx <- which(m == val, arr.ind = TRUE)

    if (nrow(idx) > 0) {
      r_col <- palette_rgba[i, 1]
      g_col <- palette_rgba[i, 2]
      b_col <- palette_rgba[i, 3]
      a_col <- palette_rgba[i, 4]

      # Regla global pedida:
      # todo píxel negro puro pasa a transparente.
      if (r_col == 0 && g_col == 0 && b_col == 0) {
        a_col <- 0
      }

      img[cbind(idx[, 1], idx[, 2], 1)] <- r_col / 255
      img[cbind(idx[, 1], idx[, 2], 2)] <- g_col / 255
      img[cbind(idx[, 1], idx[, 2], 3)] <- b_col / 255
      img[cbind(idx[, 1], idx[, 2], 4)] <- a_col / 255
    }
  }

  png::writePNG(img, target = output_png)

  invisible(output_png)
}


make_style_matrix <- function(m, style) {
  # Esto crea 5 matrices distintas a partir de la matriz Mask.
  # fn01 y fn02 conservan todo.
  # fn03 conserva focos de calor + TF + glint.
  # fn04 conserva solo glint.
  # fn05 conserva solo focos de calor y TF.

  fire_values <- c(10, 11, 12, 13, 14, 15)
  tf_fire_values <- c(31, 32, 33, 34, 35)
  glint_values <- c(60)

  if (style == "fn01") {
    return(m)
  }

  if (style == "fn02") {
    return(m)
  }

  if (style == "fn03") {
    keep <- c(fire_values, tf_fire_values, glint_values)
    m2 <- m
    m2[!(m2 %in% keep)] <- NA
    return(m2)
  }

  if (style == "fn04") {
    keep <- glint_values
    m2 <- m
    m2[!(m2 %in% keep)] <- NA
    return(m2)
  }

  if (style == "fn05") {
    keep <- c(fire_values, tf_fire_values)
    m2 <- m
    m2[!(m2 %in% keep)] <- NA
    return(m2)
  }

  m
}


# ============================================================
# 6) NETCDF FDCF A 5 PNG WGS84
# ============================================================

fdcf_netcdf_to_wgs84_pngs <- function(
    nc_file,
    output_pngs = FDCF_PNGS,
    width = 3600,
    height = 1800,
    status_fun = default_status_fun
) {
  if (!file.exists(nc_file)) {
    stop("No existe el NetCDF: ", nc_file)
  }

  nc_file_norm <- normalizePath(nc_file, winslash = "/", mustWork = TRUE)

  status_fun("Leyendo archivo NetCDF FDCF...")
  status_fun(basename(nc_file_norm))

  s <- terra::sds(nc_file_norm)

  s_names <- names(s)

  status_fun(paste("Variables encontradas:", paste(s_names, collapse = ", ")))

  i_mask <- which(s_names == "Mask")

  if (length(i_mask) == 0) {
    i_mask <- grep("Mask", s_names, ignore.case = TRUE)
  }

  if (length(i_mask) == 0) {
    stop("No se encontró la variable Mask dentro del NetCDF FDCF.")
  }

  i_mask <- i_mask[1]

  status_fun(paste("Variable seleccionada:", s_names[i_mask]))

  r_mask <- s[[i_mask]]

  if (!terra::hasValues(r_mask)) {
    stop("La variable Mask fue encontrada, pero terra no cargó valores.")
  }

  status_fun("Creando grilla WGS84 global 3600 x 1800...")

  template_wgs84 <- terra::rast(
    ncols = width,
    nrows = height,
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90,
    crs = "EPSG:4326"
  )

  status_fun("Reproyectando Mask FDCF a WGS84 con método nearest...")

  r_wgs84 <- terra::project(
    r_mask,
    template_wgs84,
    method = "near"
  )

  status_fun("Convirtiendo raster WGS84 a matriz base...")

  m_base <- terra::as.matrix(r_wgs84, wide = TRUE)

  for (style in names(output_pngs)) {
    status_fun(paste("Creando matriz derivada:", style))

    m_style <- make_style_matrix(
      m = m_base,
      style = style
    )

    fdcf_matrix_to_png(
      m = m_style,
      palette_rgba = FDCF_PALETTES[[style]],
      output_png = output_pngs[[style]],
      status_fun = status_fun
    )
  }

  invisible(list(
    pngs = output_pngs,
    r_wgs84 = r_wgs84
  ))
}


update_fdcf_images <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  status_fun("Iniciando actualización FDCF...")

  latest <- download_latest_fdcf(
    bucket = bucket,
    product = product,
    hours_back = hours_back,
    status_fun = status_fun
  )

  status_fun("Archivo NetCDF listo. Iniciando procesamiento WGS84...")

  processed <- fdcf_netcdf_to_wgs84_pngs(
    nc_file = latest$path,
    output_pngs = FDCF_PNGS,
    width = 3600,
    height = 1800,
    status_fun = status_fun
  )

  status_fun("Procesamiento FDCF completo.")

  list(
    key = latest$key,
    path = latest$path,
    processed = processed
  )
}


# ============================================================
# 7) APP SHINY
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
        background: #000000;
      }

      .container-fluid {
        width: 100%;
        height: 100%;
        padding: 0 !important;
        margin: 0 !important;
      }

      #map {
        width: 100vw !important;
        height: 100vh !important;
        background: #000000 !important;
      }

      .leaflet-container {
        background: #000000 !important;
      }

      #panel_control {
        position: absolute;
        top: 12px;
        right: 12px;
        z-index: 9999;
        width: 470px;
        max-height: 86vh;
        overflow-y: auto;
        background: rgba(0, 0, 0, 0.84);
        color: white;
        padding: 12px 14px;
        border-radius: 10px;
        font-family: Arial, sans-serif;
        font-size: 13px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.4);
      }

      #panel_control .shiny-input-container {
        width: 100%;
      }

      #refresh_fdcf {
        background: #e85d04;
        color: white;
        border: none;
        padding: 8px 12px;
        border-radius: 6px;
        cursor: pointer;
        margin-bottom: 8px;
      }

      #refresh_fdcf:hover {
        background: #dc2f02;
      }

      #fdcf_status {
        color: white;
        background: transparent;
        border: none;
        white-space: pre-wrap;
        max-height: 340px;
        overflow-y: auto;
      }

      #legend_panel {
        position: absolute;
        left: 12px;
        bottom: 12px;
        z-index: 9999;
        width: 390px;
        max-height: 55vh;
        overflow-y: auto;
        background: rgba(255, 255, 255, 0.95);
        color: black;
        border-radius: 10px;
        font-family: Arial, sans-serif;
        font-size: 12px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.4);
      }

      #legend_header {
        cursor: move;
        user-select: none;
        padding: 9px 11px;
        font-weight: bold;
        border-radius: 10px 10px 0 0;
        background: rgba(235,235,235,0.98);
      }

      #legend_body {
        padding: 10px 12px;
      }

      .legend-row {
        display: flex;
        align-items: center;
        margin-bottom: 4px;
      }

      .legend-color {
        width: 18px;
        height: 14px;
        margin-right: 7px;
        border: 1px solid rgba(0,0,0,0.25);
      }

      .small-note {
        font-size: 12px;
        color: #ddd;
      }
    ")),

    tags$script(HTML("
      function makeDraggable(panelId, headerId) {
        var panel = document.getElementById(panelId);
        var header = document.getElementById(headerId);

        if (!panel || !header) return;

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

      document.addEventListener('DOMContentLoaded', function() {
        makeDraggable('legend_panel', 'legend_header');
      });

      Shiny.addCustomMessageHandler('reload-fdcf', function(message) {
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

        var imageBounds = [
          [-90, -180],
          [ 90,  180]
        ];

        if (!map.getPane('fdcfPane')) {
          map.createPane('fdcfPane');
          map.getPane('fdcfPane').style.zIndex = 450;
        }

        if (map.fdcfOverlay) {
          map.removeLayer(map.fdcfOverlay);
          map.fdcfOverlay = null;
        }

        var overlay = L.imageOverlay(
          message.url,
          imageBounds,
          {
            opacity: 1,
            interactive: false,
            pane: 'fdcfPane'
          }
        );

        overlay.on('load', function() {
          console.log('FDCF cargado:', message.url);
        });

        overlay.on('error', function(e) {
          console.error('ERROR cargando FDCF:', message.url, e);
          alert('No se pudo cargar el PNG FDCF.');
        });

        overlay.addTo(map);
        map.fdcfOverlay = overlay;
      });

      Shiny.addCustomMessageHandler('update-status-live', function(message) {
        var el = document.getElementById('fdcf_status');

        if (el) {
          el.textContent = message.text;
          el.scrollTop = el.scrollHeight;
        }
      });
    "))
  ),

  div(
    id = "panel_control",
    strong("GOES FDCF - focos de calor"),
    br(),
    br(),

    selectInput(
      inputId = "fdcf_style",
      label = "PNG / matriz a mostrar:",
      choices = FDCF_CHOICES,
      selected = DEFAULT_FDCF_STYLE
    ),

    actionButton("refresh_fdcf", "Descargar y procesar FDCF"),

    br(),
    br(),

    div(
      class = "small-note",
      "Se generan 5 PNG WGS84 desde la variable Mask. Los píxeles negros puros se vuelven transparentes."
    ),

    br(),
    strong("Estado:"),
    verbatimTextOutput("fdcf_status")
  ),

  div(
    id = "legend_panel",
    div(id = "legend_header", textOutput("legend_title", inline = TRUE)),
    div(
      id = "legend_body",
      uiOutput("fdcf_legend")
    )
  ),

  leafletOutput("map")
)


server <- function(input, output, session) {

  current_key <- reactiveVal(NULL)
  current_path <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)
  processed_ready <- reactiveVal(FALSE)

  status_text <- reactiveVal(
    "App iniciada.\nMapa WGS84 listo.\nPresioná el botón para descargar y procesar FDCF."
  )

  selected_png_url <- function(style) {
    png_file <- basename(FDCF_PNGS[[style]])
    paste0(png_file, "?v=", as.numeric(Sys.time()))
  }

  output$fdcf_status <- renderText({
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
    leaflet(
      options = leafletOptions(
        crs = leafletCRS(crsClass = "L.CRS.EPSG4326"),
        zoomControl = TRUE,
        minZoom = 1,
        maxZoom = 8,
        zoomSnap = 0.10,
        zoomDelta = 0.10,
        worldCopyJump = TRUE
      )
    ) %>%
      addWMSTiles(
        baseUrl = GIBS_WMS_EPSG4326,
        layers = "VIIRS_CityLights_2012",
        group = "base_tiles",
        options = WMSTileOptions(
          format = "image/jpeg",
          transparent = FALSE,
          version = "1.1.1"
        )
      ) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })

  output$legend_title <- renderText({
    label <- names(FDCF_CHOICES)[FDCF_CHOICES == input$fdcf_style]
    paste("Leyenda FDCF -", label)
  })

  output$fdcf_legend <- renderUI({
    style <- input$fdcf_style

    if (is.null(style)) {
      style <- DEFAULT_FDCF_STYLE
    }

    pal <- FDCF_PALETTES[[style]]

    visible_idx <- which(pal[, 4] > 0)

    rows <- lapply(visible_idx, function(i) {
      rgba <- pal[i, ]

      alpha <- rgba[4] / 255

      # En la leyenda también se respeta la regla:
      # negro puro se considera transparente.
      if (rgba[1] == 0 && rgba[2] == 0 && rgba[3] == 0) {
        alpha <- 0
      }

      if (alpha == 0) {
        return(NULL)
      }

      tags$div(
        class = "legend-row",
        tags$div(
          class = "legend-color",
          style = paste0(
            "background: rgba(",
            rgba[1], ",", rgba[2], ",", rgba[3], ",", alpha,
            ");"
          )
        ),
        tags$span(fdcf_labels[i])
      )
    })

    rows <- rows[!vapply(rows, is.null, logical(1))]

    if (length(rows) == 0) {
      return(tags$em("Esta paleta no tiene categorías visibles."))
    }

    tagList(rows)
  })

  observeEvent(input$fdcf_style, {
    if (!isTRUE(processed_ready())) {
      return()
    }

    style <- input$fdcf_style

    if (is.null(style)) {
      style <- DEFAULT_FDCF_STYLE
    }

    session$sendCustomMessage(
      type = "reload-fdcf",
      message = list(
        url = selected_png_url(style)
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$refresh_fdcf, {

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
        type = "update-status-live",
        message = list(text = nuevo_texto)
      )

      try(session$flushReact(), silent = TRUE)

      message(txt)
    }

    withProgress(message = "Procesando GOES FDCF", value = 0, {

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
        update_fdcf_images(
          bucket = BUCKET,
          product = PRODUCT,
          hours_back = 6,
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

      current_key(info$key)
      current_path(info$path)
      last_update(Sys.time())
      processed_ready(TRUE)

      style <- input$fdcf_style

      if (is.null(style)) {
        style <- DEFAULT_FDCF_STYLE
      }

      progress_status(paste("Cargando PNG seleccionado:", style))

      session$sendCustomMessage(
        type = "reload-fdcf",
        message = list(
          url = selected_png_url(style)
        )
      )

      progress_status("FDCF cargado en el mapa.")

      showNotification(
        paste("FDCF actualizado:", basename(info$key)),
        type = "message",
        duration = 6
      )
    })
  })
}


shinyApp(ui, server)

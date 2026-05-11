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
library(sf)
library(rnaturalearth)
library(base64enc)

# ============================================================
# 0) CONFIGURACIÓN GLOBAL FDCF
# ============================================================

fdcf_bucket  <- "noaa-goes19"
fdcf_product <- "ABI-L2-FDCF"

fdcf_nc_dir <- file.path(tempdir(), "goes_fdcf_downloads")
dir.create(fdcf_nc_dir, showWarnings = FALSE, recursive = TRUE)

fdcf_asset_dir <- file.path(tempdir(), "goes_fdcf_assets")
dir.create(fdcf_asset_dir, showWarnings = FALSE, recursive = TRUE)

fdcf_asset_prefix <- "fdcf_assets"

fdcf_register_asset_path <- function() {
  dir.create(fdcf_asset_dir, showWarnings = FALSE, recursive = TRUE)

  suppressWarnings(
    try(
      shiny::addResourcePath(
        prefix = fdcf_asset_prefix,
        directoryPath = fdcf_asset_dir
      ),
      silent = TRUE
    )
  )

  invisible(TRUE)
}

fdcf_register_asset_path()

fdcf_png_fn01 <- file.path(fdcf_asset_dir, "fdcf_fn01_wgs84.png")
fdcf_png_fn02 <- file.path(fdcf_asset_dir, "fdcf_fn02_wgs84.png")
fdcf_png_fn03 <- file.path(fdcf_asset_dir, "fdcf_fn03_wgs84.png")
fdcf_png_fn04 <- file.path(fdcf_asset_dir, "fdcf_fn04_wgs84.png")
fdcf_png_fn05 <- file.path(fdcf_asset_dir, "fdcf_fn05_wgs84.png")

fdcf_map_screenshot_png <- file.path(fdcf_asset_dir, "fdcf_map_screenshot.png")

fdcf_png_files <- c(
  fn01 = fdcf_png_fn01,
  fn02 = fdcf_png_fn02,
  fn03 = fdcf_png_fn03,
  fn04 = fdcf_png_fn04,
  fn05 = fdcf_png_fn05
)

fdcf_style_choices <- c(
  "FN01 - clasificación completa" = "fn01",
  "FN02 - magma completa" = "fn02",
  "FN03 - solo fuego + glint" = "fn03",
  "FN04 - solo glint" = "fn04",
  "FN05 - fuego vivido" = "fn05"
)

fdcf_default_style <- "fn05"

fdcf_fire_classes <- c(10, 11, 12, 13, 14, 15, 30, 31, 32, 33, 34, 35)

fdcf_fire_labels <- c(
  "10" = "Processed fire pixel",
  "11" = "Saturated fire pixel",
  "12" = "Cloud contaminated fire",
  "13" = "High probability fire",
  "14" = "Medium probability fire",
  "15" = "Low probability fire",
  "30" = "TF processed fire pixel",
  "31" = "TF saturated fire pixel",
  "32" = "TF cloud contaminated fire",
  "33" = "TF high probability fire",
  "34" = "TF medium probability fire",
  "35" = "TF low probability fire"
)

fdcf_left_toolbar_top <- "10px"
fdcf_left_toolbar_left <- "60px"

fdcf_right_toolbar_top <- "10px"
fdcf_right_toolbar_right <- "12px"

fdcf_wgs84_width <- 3600
fdcf_wgs84_height <- 1800

fdcf_gibs_wms_epsg4326 <- "https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi"
fdcf_terrestris_osm_wms <- "https://ows.terrestris.de/osm/service"

message("Carpeta temporal de descarga FDCF: ", fdcf_nc_dir)
message("Carpeta temporal de PNG FDCF: ", fdcf_asset_dir)


# ============================================================
# 1) FUNCIONES AUXILIARES FDCF
# ============================================================

fdcf_default_status_fun <- function(txt) {
  message(txt)
}

fdcf_timestamp_ms <- function() {
  format(Sys.time(), "%H:%M:%OS3")
}

fdcf_make_goes_prefix <- function(time_utc, product = fdcf_product) {
  time_utc <- lubridate::with_tz(time_utc, "UTC")

  year <- format(time_utc, "%Y")
  doy  <- format(time_utc, "%j")
  hour <- format(time_utc, "%H")

  paste0(product, "/", year, "/", doy, "/", hour, "/")
}

fdcf_list_s3_keys <- function(bucket, prefix) {
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

fdcf_get_latest_key <- function(
    bucket = fdcf_bucket,
    product = fdcf_product,
    hours_back = 8,
    status_fun = fdcf_default_status_fun
) {
  status_fun("Buscando el último archivo FDCF disponible...")

  now_utc <- lubridate::with_tz(Sys.time(), "UTC")

  times <- seq(
    from = now_utc - lubridate::hours(hours_back),
    to   = now_utc,
    by   = "1 hour"
  )

  prefixes <- unique(vapply(
    times,
    fdcf_make_goes_prefix,
    character(1),
    product = product
  ))

  keys <- purrr::map(prefixes, function(px) {
    status_fun(paste("Listando:", bucket, "/", px))
    fdcf_list_s3_keys(bucket, px)
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

fdcf_get_online_file_size <- function(bucket, key) {
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

fdcf_download_s3_object <- function(
    bucket,
    key,
    dest_dir = fdcf_nc_dir,
    status_fun = fdcf_default_status_fun
) {
  filename <- basename(key)
  dest <- file.path(dest_dir, filename)

  url <- paste0("https://", bucket, ".s3.amazonaws.com/", key)

  status_fun("Consultando tamaño del archivo remoto...")

  online_size <- fdcf_get_online_file_size(bucket, key)

  if (!is.na(online_size)) {
    status_fun(paste(
      "Peso online:",
      round(online_size / 1024^2, 2),
      "MB"
    ))
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

    status_fun("El archivo local no coincide con el remoto. Se borra y se descarga de nuevo.")
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

fdcf_download_latest <- function(
    bucket = fdcf_bucket,
    product = fdcf_product,
    hours_back = 8,
    status_fun = fdcf_default_status_fun
) {
  latest_key <- fdcf_get_latest_key(
    bucket = bucket,
    product = product,
    hours_back = hours_back,
    status_fun = status_fun
  )

  latest_path <- fdcf_download_s3_object(
    bucket = bucket,
    key = latest_key,
    dest_dir = fdcf_nc_dir,
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
# 2) PALETAS FDCF
# ============================================================

fdcf_palette_values <- c(
  -99, 0, 10, 11, 12, 13, 14, 15,
  16, 17, 18, 19, 20, 21, 22,
  30, 31, 32, 33, 34, 35,
  40, 50, 60, 100, 120, 121, 123,
  124, 125, 126, 127, 151, 152, 153,
  200, 215
)

fdcf_palette_df <- function(values, colors) {
  out <- data.frame(
    value = values,
    r = colors[, 1],
    g = colors[, 2],
    b = colors[, 3],
    a = colors[, 4]
  )

  rownames(out) <- NULL
  out
}

fdcf_palettes <- list(
  fn01 = fdcf_palette_df(
    fdcf_palette_values,
    matrix(c(
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
    ), ncol = 4, byrow = TRUE)
  ),

  fn02 = fdcf_palette_df(
    fdcf_palette_values,
    matrix(c(
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
    ), ncol = 4, byrow = TRUE)
  ),

  fn03 = fdcf_palette_df(
    fdcf_palette_values,
    matrix(c(
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
    ), ncol = 4, byrow = TRUE)
  ),

  fn04 = fdcf_palette_df(
    fdcf_palette_values,
    matrix(c(
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
    ), ncol = 4, byrow = TRUE)
  ),

  fn05 = fdcf_palette_df(
    fdcf_palette_values,
    matrix(c(
      0, 0, 0, 0,
      0, 0, 0, 0,
      255, 0, 0, 255,
      255, 255, 255, 255,
      255, 140, 0, 255,
      255, 40, 0, 255,
      255, 180, 0, 255,
      255, 230, 0, 255,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      255, 0, 255, 255,
      180, 0, 255, 255,
      80, 90, 255, 255,
      0, 180, 255, 255,
      0, 255, 255, 255,
      0, 255, 150, 255,
      0, 0, 0, 0,
      0, 0, 0, 0,
      255, 255, 0, 180,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0
    ), ncol = 4, byrow = TRUE)
  )
)


# ============================================================
# 3) FONDOS WGS84 FDCF
# ============================================================

fdcf_base_maps <- c(
  "Negro total / sin fondo" = "none",
  "OSM WMS - carreteras" = "wms_terrestris_osm",
  "NASA GIBS luces nocturnas" = "gibs_viirs_night_2012",
  "NASA GIBS Blue Marble" = "gibs_blue_marble_ng"
)

fdcf_reference_layer_choices <- c(
  "Sin capas" = "none",
  "Límites internacionales" = "countries",
  "Provincias / estados del mundo" = "admin1",
  "Límites + provincias / estados del mundo" = "countries_admin1"
)

fdcf_gibs_layers <- list(
  gibs_viirs_night_2012 = list(
    layer = "VIIRS_CityLights_2012",
    format = "image/jpeg",
    transparent = FALSE
  ),
  gibs_blue_marble_ng = list(
    layer = "BlueMarble_NextGeneration",
    format = "image/jpeg",
    transparent = FALSE
  )
)

fdcf_add_base_layer_to_map <- function(m, base_id) {
  if (is.null(base_id) || base_id == "none") {
    return(m)
  }

  if (base_id == "wms_terrestris_osm") {
    return(
      m %>%
        addWMSTiles(
          baseUrl = fdcf_terrestris_osm_wms,
          layers = "OSM-WMS",
          group = "fdcf_base_tiles",
          options = WMSTileOptions(
            format = "image/png",
            transparent = FALSE,
            version = "1.1.1"
          )
        )
    )
  }

  layer_info <- fdcf_gibs_layers[[base_id]]

  if (!is.null(layer_info)) {
    return(
      m %>%
        addWMSTiles(
          baseUrl = fdcf_gibs_wms_epsg4326,
          layers = layer_info$layer,
          group = "fdcf_base_tiles",
          options = WMSTileOptions(
            format = layer_info$format,
            transparent = layer_info$transparent,
            version = "1.1.1"
          )
        )
    )
  }

  m
}

fdcf_add_base_layer_to_proxy <- function(proxy, base_id) {
  proxy <- proxy %>% clearGroup("fdcf_base_tiles")

  if (is.null(base_id) || base_id == "none") {
    return(proxy)
  }

  if (base_id == "wms_terrestris_osm") {
    return(
      proxy %>%
        addWMSTiles(
          baseUrl = fdcf_terrestris_osm_wms,
          layers = "OSM-WMS",
          group = "fdcf_base_tiles",
          options = WMSTileOptions(
            format = "image/png",
            transparent = FALSE,
            version = "1.1.1"
          )
        )
    )
  }

  layer_info <- fdcf_gibs_layers[[base_id]]

  if (!is.null(layer_info)) {
    return(
      proxy %>%
        addWMSTiles(
          baseUrl = fdcf_gibs_wms_epsg4326,
          layers = layer_info$layer,
          group = "fdcf_base_tiles",
          options = WMSTileOptions(
            format = layer_info$format,
            transparent = layer_info$transparent,
            version = "1.1.1"
          )
        )
    )
  }

  proxy
}


# ============================================================
# 4) CAPAS REFERENCIALES FDCF
# ============================================================

fdcf_load_reference_layers <- function() {
  countries <- tryCatch({
    rnaturalearth::ne_countries(
      scale = "medium",
      returnclass = "sf"
    )
  }, error = function(e) {
    message("No se pudieron cargar límites internacionales FDCF: ", e$message)
    NULL
  })

  admin1_global <- tryCatch({
    admin1 <- rnaturalearth::ne_download(
      scale = 50,
      type = "admin_1_states_provinces_lines",
      category = "cultural",
      returnclass = "sf"
    )

    admin1 <- sf::st_transform(admin1, 4326)

    if ("name" %in% names(admin1)) {
      admin1$fdcf_admin1_label <- admin1$name
    } else if ("name_en" %in% names(admin1)) {
      admin1$fdcf_admin1_label <- admin1$name_en
    } else if ("gn_name" %in% names(admin1)) {
      admin1$fdcf_admin1_label <- admin1$gn_name
    } else if ("region" %in% names(admin1)) {
      admin1$fdcf_admin1_label <- admin1$region
    } else {
      admin1$fdcf_admin1_label <- "Límite administrativo"
    }

    admin1
  }, error = function(e) {
    message("No se pudieron cargar provincias/estados globales FDCF: ", e$message)
    NULL
  })

  list(
    countries = countries,
    admin1_global = admin1_global
  )
}

fdcf_ref_layers <- fdcf_load_reference_layers()


# ============================================================
# 5) PROCESAMIENTO FDCF
# ============================================================

fdcf_colorize_matrix_to_png <- function(
    mat,
    palette_df,
    output_png,
    status_fun = fdcf_default_status_fun
) {
  status_fun(paste("Generando PNG:", basename(output_png)))

  output_dir <- dirname(output_png)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (!dir.exists(output_dir)) {
    stop("No existe la carpeta de salida para PNG: ", output_dir)
  }

  mat_round <- round(mat)

  nr <- nrow(mat_round)
  nc <- ncol(mat_round)

  img <- array(0, dim = c(nr, nc, 4))

  for (i in seq_len(nrow(palette_df))) {
    value_i <- palette_df$value[i]

    idx <- which(mat_round == value_i, arr.ind = TRUE)

    if (nrow(idx) > 0) {
      img[cbind(idx[, 1], idx[, 2], 1)] <- palette_df$r[i] / 255
      img[cbind(idx[, 1], idx[, 2], 2)] <- palette_df$g[i] / 255
      img[cbind(idx[, 1], idx[, 2], 3)] <- palette_df$b[i] / 255
      img[cbind(idx[, 1], idx[, 2], 4)] <- palette_df$a[i] / 255
    }
  }

  black_pixels <- img[, , 1] == 0 &
    img[, , 2] == 0 &
    img[, , 3] == 0

  img[, , 4][black_pixels] <- 0
  img[, , 4][is.na(mat_round)] <- 0

  tryCatch({
    png::writePNG(img, target = output_png)
  }, error = function(e) {
    stop(
      "No se pudo escribir el PNG en: ", output_png,
      "\nDetalle: ", e$message
    )
  })

  invisible(output_png)
}

fdcf_extract_fire_points <- function(r_wgs84) {
  vals <- terra::values(r_wgs84, mat = FALSE)
  vals_round <- round(vals)

  fire_cells <- which(vals_round %in% fdcf_fire_classes)

  if (length(fire_cells) == 0) {
    return(data.frame(
      cell = integer(),
      lon = numeric(),
      lat = numeric(),
      fdcf_class = integer(),
      class_name = character()
    ))
  }

  xy <- terra::xyFromCell(r_wgs84, fire_cells)
  classes <- vals_round[fire_cells]

  out <- data.frame(
    cell = fire_cells,
    lon = xy[, 1],
    lat = xy[, 2],
    fdcf_class = as.integer(classes),
    class_name = unname(fdcf_fire_labels[as.character(classes)])
  )

  out$class_name[is.na(out$class_name)] <- "Fire pixel"

  out
}

fdcf_compute_statistics <- function(
    r_wgs84,
    nc_filename,
    status_fun = fdcf_default_status_fun
) {
  status_fun("Calculando estadísticas FDCF...")

  vals <- terra::values(r_wgs84, mat = FALSE)
  vals_round <- round(vals)

  total_pixels <- length(vals_round)
  valid_pixels <- sum(is.finite(vals_round))
  fire_pixels <- sum(vals_round %in% fdcf_fire_classes, na.rm = TRUE)

  class_table <- as.data.frame(table(vals_round), stringsAsFactors = FALSE)
  names(class_table) <- c("fdcf_class", "n_pixels")
  class_table$fdcf_class <- as.integer(as.character(class_table$fdcf_class))
  class_table <- class_table[order(class_table$fdcf_class), ]
  rownames(class_table) <- NULL

  fire_table <- class_table[class_table$fdcf_class %in% fdcf_fire_classes, ]
  fire_table$class_name <- unname(fdcf_fire_labels[as.character(fire_table$fdcf_class)])
  fire_table$class_name[is.na(fire_table$class_name)] <- "Fire pixel"
  fire_table$percent_total <- round((fire_table$n_pixels / total_pixels) * 100, 6)

  general <- data.frame(
    Medida = c(
      "Archivo NetCDF",
      "Producto",
      "Variable procesada",
      "Resolución WGS84 X, grados",
      "Resolución WGS84 Y, grados",
      "Filas PNG WGS84",
      "Columnas PNG WGS84",
      "Píxeles totales",
      "Píxeles válidos",
      "Píxeles con focos de calor",
      "Porcentaje de focos sobre total"
    ),
    Valor = c(
      nc_filename,
      fdcf_product,
      "Mask",
      round(terra::res(r_wgs84)[1], 6),
      round(terra::res(r_wgs84)[2], 6),
      terra::nrow(r_wgs84),
      terra::ncol(r_wgs84),
      total_pixels,
      valid_pixels,
      fire_pixels,
      round((fire_pixels / total_pixels) * 100, 6)
    )
  )

  status_fun("Estadísticas FDCF calculadas.")

  list(
    general = general,
    class_table = class_table,
    fire_table = fire_table
  )
}

fdcf_netcdf_to_pngs <- function(
    nc_file,
    status_fun = fdcf_default_status_fun
) {
  if (!file.exists(nc_file)) {
    stop("No existe el NetCDF: ", nc_file)
  }

  fdcf_register_asset_path()

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

  r0 <- s[[i_mask]]

  if (!terra::hasValues(r0)) {
    stop("El raster Mask fue encontrado, pero terra no cargó valores.")
  }

  status_fun("Creando grilla global WGS84 3600 x 1800...")

  template_wgs84 <- terra::rast(
    ncols = fdcf_wgs84_width,
    nrows = fdcf_wgs84_height,
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90,
    crs = "EPSG:4326"
  )

  status_fun("Reproyectando FDCF a WGS84 con vecino más cercano...")

  r_wgs84 <- terra::project(
    r0,
    template_wgs84,
    method = "near"
  )

  status_fun("Extrayendo matriz WGS84...")

  mat <- terra::as.matrix(r_wgs84, wide = TRUE)

  status_fun("Generando los 5 PNG FDCF...")

  for (style_id in names(fdcf_png_files)) {
    fdcf_colorize_matrix_to_png(
      mat = mat,
      palette_df = fdcf_palettes[[style_id]],
      output_png = fdcf_png_files[[style_id]],
      status_fun = status_fun
    )
  }

  fire_points <- fdcf_extract_fire_points(r_wgs84)

  stats <- fdcf_compute_statistics(
    r_wgs84 = r_wgs84,
    nc_filename = basename(nc_file_norm),
    status_fun = status_fun
  )

  invisible(list(
    r_wgs84 = r_wgs84,
    png_files = fdcf_png_files,
    fire_points = fire_points,
    general_stats = stats$general,
    class_table = stats$class_table,
    fire_table = stats$fire_table
  ))
}

fdcf_update_image <- function(
    bucket = fdcf_bucket,
    product = fdcf_product,
    hours_back = 8,
    status_fun = fdcf_default_status_fun
) {
  status_fun("Iniciando actualización FDCF...")

  fdcf_register_asset_path()

  latest <- fdcf_download_latest(
    bucket = bucket,
    product = product,
    hours_back = hours_back,
    status_fun = status_fun
  )

  status_fun("Archivo NetCDF listo. Iniciando procesamiento FDCF...")

  processed <- fdcf_netcdf_to_pngs(
    nc_file = latest$path,
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
# 6) MÓDULO UI FDCF
# ============================================================

mod_fdcf_new_ui <- function(id) {
  ns <- NS(id)
  js_id <- gsub("[^A-Za-z0-9_]", "_", id)

  root_id <- ns("fdcf_root")

  tagList(
    singleton(
      tags$head(
        tags$script(src = "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js")
      )
    ),

    tags$head(
      tags$style(HTML(paste0("
        #", root_id, " {
          position: fixed !important;
          inset: 0 !important;
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
          margin: 0 !important;
          padding: 0 !important;
          overflow: hidden !important;
          font-family: Arial, sans-serif;
          background: #000000 !important;
          z-index: 1;
        }

        #", root_id, " * {
          box-sizing: border-box;
        }

        #", root_id, " #", ns("map"), " {
          position: absolute !important;
          inset: 0 !important;
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
          margin: 0 !important;
          padding: 0 !important;
          z-index: 1;
        }

        #", root_id, " #", ns("map"), ".shiny-bound-output {
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
        }

        #", root_id, " .leaflet,
        #", root_id, " .leaflet-container {
          width: 100vw !important;
          height: 100vh !important;
          min-width: 100vw !important;
          min-height: 100vh !important;
          background: #000000 !important;
        }

        #", root_id, " .leaflet-pane,
        #", root_id, " .leaflet-map-pane,
        #", root_id, " .leaflet-tile-pane,
        #", root_id, " .leaflet-overlay-pane,
        #", root_id, " .leaflet-marker-pane,
        #", root_id, " .leaflet-tooltip-pane,
        #", root_id, " .leaflet-popup-pane {
          max-width: none !important;
        }

        #", root_id, " .leaflet-popup-content-wrapper {
          max-width: 460px !important;
          min-width: 285px !important;
        }

        #", root_id, " .leaflet-popup-content {
          width: auto !important;
          min-width: 260px !important;
          max-width: 430px !important;
          white-space: normal !important;
          line-height: 1.4 !important;
          font-size: 13px !important;
        }

        #", root_id, " .leaflet-tooltip {
          max-width: 460px !important;
          white-space: normal !important;
          line-height: 1.4 !important;
          font-size: 13px !important;
        }

        #", root_id, " .fdcf-top-toolbar {
          position: absolute !important;
          z-index: 10000 !important;
          display: flex !important;
          gap: 8px !important;
          align-items: center !important;
          font-family: Arial, sans-serif;
          pointer-events: auto;
          backdrop-filter: blur(8px);
        }

        #", root_id, " #", ns("toolbar_left"), " {
          top: ", fdcf_left_toolbar_top, ";
          left: ", fdcf_left_toolbar_left, ";
          background: transparent !important;
          padding: 0 !important;
          border-radius: 0 !important;
        }

        #", root_id, " .fdcf-left-card {
          display: flex !important;
          align-items: flex-end !important;
          gap: 10px !important;
          padding: 10px 12px !important;
          border-radius: 14px !important;
          background: rgba(255, 85, 0, 0.78) !important;
          border: 1px solid rgba(255,255,255,0.24) !important;
          box-shadow: 0 8px 26px rgba(0,0,0,0.38) !important;
          backdrop-filter: blur(8px);
        }

        #", root_id, " .fdcf-left-card .shiny-input-container {
          margin-bottom: 0 !important;
        }

        #", root_id, " .fdcf-left-card label.control-label {
          color: #ffffff !important;
          font-size: 11px !important;
          font-weight: 800 !important;
          margin-bottom: 4px !important;
          letter-spacing: 0.35px !important;
          text-transform: uppercase;
          text-shadow: 0 1px 3px rgba(0,0,0,0.35);
        }

        #", root_id, " .fdcf-left-card select {
          height: 30px !important;
          min-height: 30px !important;
          border-radius: 7px !important;
          border: 1px solid rgba(255,255,255,0.30) !important;
          font-size: 13px !important;
          background: rgba(255,255,255,0.95) !important;
          color: #111827 !important;
        }

        #", root_id, " #", ns("toolbar_right"), " {
          top: ", fdcf_right_toolbar_top, ";
          right: ", fdcf_right_toolbar_right, ";
          background: rgba(0, 220, 255, 0.68) !important;
          padding: 8px 10px !important;
          border-radius: 12px !important;
          border: 1px solid rgba(255,255,255,0.26) !important;
          box-shadow: 0 8px 26px rgba(0,0,0,0.34) !important;
        }

        #", root_id, " #", ns("toolbar_right"), " .fdcf-toolbar-btn {
          background: rgba(0, 82, 112, 0.88) !important;
          color: #ffffff !important;
          border: 1px solid rgba(255,255,255,0.30) !important;
        }

        #", root_id, " #", ns("toolbar_right"), " .fdcf-toolbar-btn:hover {
          background: rgba(0, 125, 160, 0.96) !important;
        }

        #", root_id, " .fdcf-toolbar-btn {
          color: white;
          border: none;
          padding: 7px 11px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 13px;
          line-height: 1.2;
          font-weight: 700;
        }

        #", root_id, " .fdcf-toggle-wrap {
          height: 49px !important;
          display: flex !important;
          align-items: flex-end !important;
          padding-bottom: 2px !important;
        }

        #", root_id, " .fdcf-toggle {
          display: inline-flex !important;
          align-items: center !important;
          gap: 8px !important;
          margin: 0 !important;
          cursor: pointer !important;
          user-select: none !important;
          white-space: nowrap !important;
        }

        #", root_id, " .fdcf-toggle input[type='checkbox'] {
          position: absolute !important;
          opacity: 0 !important;
          width: 0 !important;
          height: 0 !important;
          margin: 0 !important;
          padding: 0 !important;
          pointer-events: none !important;
        }

        #", root_id, " .fdcf-toggle-slider {
          position: relative !important;
          width: 46px !important;
          height: 24px !important;
          border-radius: 999px !important;
          background: rgba(55, 65, 81, 0.95) !important;
          box-shadow:
            inset 0 0 0 1px rgba(255,255,255,0.22),
            0 2px 8px rgba(0,0,0,0.30) !important;
          transition: all 0.18s ease !important;
          flex: 0 0 auto !important;
        }

        #", root_id, " .fdcf-toggle-slider::before {
          content: '' !important;
          position: absolute !important;
          width: 18px !important;
          height: 18px !important;
          left: 3px !important;
          top: 3px !important;
          border-radius: 50% !important;
          background: #ffffff !important;
          box-shadow: 0 2px 8px rgba(0,0,0,0.38) !important;
          transition: transform 0.18s ease !important;
        }

        #", root_id, " .fdcf-toggle input[type='checkbox']:checked + .fdcf-toggle-slider {
          background: #00e676 !important;
          box-shadow:
            inset 0 0 0 1px rgba(255,255,255,0.25),
            0 0 16px rgba(0,230,118,0.55) !important;
        }

        #", root_id, " .fdcf-toggle input[type='checkbox']:checked + .fdcf-toggle-slider::before {
          transform: translateX(22px) !important;
        }

        #", root_id, " .fdcf-toggle-label {
          color: #ffffff !important;
          font-size: 12px !important;
          font-weight: 800 !important;
          letter-spacing: 0.25px !important;
          text-shadow: 0 1px 3px rgba(0,0,0,0.40);
        }

        #", root_id, " .fdcf-floating-panel {
          position: absolute !important;
          z-index: 9999 !important;
          border-radius: 8px;
          font-family: Arial, sans-serif;
          box-shadow: 0 4px 16px rgba(0,0,0,0.35);
          pointer-events: auto;
        }

        #", root_id, " .fdcf-panel-header {
          cursor: move;
          user-select: none;
          font-weight: bold;
          padding: 8px 10px;
          border-radius: 8px 8px 0 0;
        }

        #", root_id, " .fdcf-panel-body {
          padding: 10px 12px;
        }

        #", root_id, " #", ns("panel_layers"), " {
          top: 78px;
          left: 60px;
          width: 430px;
          background: rgba(255, 255, 255, 0.96);
          color: black;
        }

        #", root_id, " #", ns("panel_layers"), " .fdcf-panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", root_id, " #", ns("panel_process"), " {
          top: 62px;
          right: 12px;
          width: 580px;
          max-width: 580px;
          background: rgba(0, 0, 0, 0.86);
          color: #ffffff !important;
        }

        #", root_id, " #", ns("panel_process"), " .fdcf-panel-header {
          background: rgba(20, 20, 20, 0.96);
          color: #ffffff !important;
        }

        #", root_id, " #", ns("panel_process"), " pre {
          color: #ffffff !important;
          background: transparent !important;
          border: none !important;
          padding: 0 !important;
          margin: 8px 0 0 0 !important;
          white-space: pre-wrap !important;
          max-height: 560px !important;
          overflow-y: auto !important;
          opacity: 1 !important;
          text-shadow: none !important;
        }

        #", root_id, " #", ns("fdcf_status"), " {
          color: #ffffff !important;
          background: transparent !important;
          max-height: 560px !important;
          overflow-y: auto !important;
          white-space: pre-wrap !important;
          display: block !important;
          opacity: 1 !important;
          font-weight: 500 !important;
          text-shadow: none !important;
        }

        #", root_id, " #", ns("fdcf_status"), " * {
          color: #ffffff !important;
          opacity: 1 !important;
        }

        #", root_id, " #", ns("refresh_fdcf"), " {
          background: #00a870;
          color: white;
          border: none;
          padding: 7px 11px;
          border-radius: 5px;
          cursor: pointer;
          margin-top: 8px;
        }

        #", root_id, " #", ns("refresh_fdcf"), ":hover {
          background: #007c54;
        }

        #", root_id, " #", ns("panel_stats"), " {
          right: 12px;
          top: 62px;
          width: 850px;
          max-height: calc(100vh - 86px);
          overflow-y: auto;
          background: rgba(255, 255, 255, 0.96);
          color: black;
        }

        #", root_id, " #", ns("panel_stats"), " .fdcf-panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", root_id, " #", ns("panel_reference"), " {
          right: 14px;
          bottom: 14px;
          width: 680px;
          background: rgba(255, 255, 255, 0.96);
          color: black;
          overflow-y: auto;
          max-height: 55vh;
        }

        #", root_id, " #", ns("panel_reference"), " .fdcf-panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", root_id, " #", ns("panel_downloads"), " {
          top: 62px;
          right: 610px;
          width: 330px;
          background: rgba(255, 255, 255, 0.96);
          color: black;
        }

        #", root_id, " #", ns("panel_downloads"), " .fdcf-panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", root_id, " #", ns("panel_downloads"), " .btn,
        #", root_id, " #", ns("panel_downloads"), " button,
        #", root_id, " #", ns("panel_downloads"), " a {
          width: 100%;
          margin-bottom: 7px;
        }

        #", root_id, " #", ns("take_screenshot"), " {
          background: #7b2cbf;
          color: white;
          border: none;
          padding: 7px 11px;
          border-radius: 5px;
          cursor: pointer;
          width: 100%;
          margin-bottom: 7px;
        }

        #", root_id, " #", ns("take_screenshot"), ":hover {
          background: #5a189a;
        }

        #", root_id, " #", ns("btn_go_home"), " {
          position: absolute !important;
          left: 20px;
          bottom: 20px;
          z-index: 20000 !important;
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

        #", root_id, " #", ns("btn_go_home"), ":hover {
          background: rgba(234, 88, 12, 0.98);
          color: white;
        }

        #", root_id, " table {
          font-size: 12px;
        }

        #", root_id, " .fdcf-table-section-title {
          margin-top: 12px;
          margin-bottom: 6px;
          font-weight: bold;
        }

        #", root_id, " .shiny-input-container {
          width: 100%;
        }

        #", root_id, " .fdcf-small-note {
          font-size: 12px;
          color: #555;
        }

        #", root_id, " .fdcf-legend-row {
          display: flex;
          align-items: center;
          gap: 8px;
          margin-bottom: 4px;
          font-size: 12px;
        }

        #", root_id, " .fdcf-legend-swatch {
          width: 22px;
          height: 14px;
          border: 1px solid rgba(0,0,0,0.25);
          flex: 0 0 auto;
        }

        #", root_id, " .fdcf-stats-source-box {
          background: rgba(245, 245, 245, 0.98);
          border: 1px solid rgba(0, 0, 0, 0.12);
          border-radius: 6px;
          padding: 8px 10px;
          font-size: 13px;
          margin-bottom: 8px;
        }

        #", root_id, " .selectize-control,
        #", root_id, " .selectize-input,
        #", root_id, " .selectize-dropdown,
        #", root_id, " .selectize-dropdown-content {
          z-index: 30000 !important;
        }
      "))),

      tags$script(HTML(paste0("
        (function() {
          window.fdcf_getEl_", js_id, " = function(id) {
            return document.getElementById(id);
          };

          window.fdcf_scrollStatus_", js_id, " = function() {
            var el = window.fdcf_getEl_", js_id, "('", ns("fdcf_status"), "');
            if (el) {
              setTimeout(function() {
                el.scrollTop = el.scrollHeight;
              }, 20);
            }
          };

          window.fdcf_togglePanel_", js_id, " = function(id) {
            var el = window.fdcf_getEl_", js_id, "(id);
            if (!el) return;

            if (el.style.display === 'none') {
              el.style.display = 'block';
            } else {
              el.style.display = 'none';
            }
          };

          window.fdcf_makePanelDraggable_", js_id, " = function(panelId) {
            var panel = window.fdcf_getEl_", js_id, "(panelId);
            if (!panel) return;

            if (panel.getAttribute('data-fdcf-draggable') === '1') return;
            panel.setAttribute('data-fdcf-draggable', '1');

            var header = panel.querySelector('.fdcf-panel-header');
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
          };

          window.fdcf_initPanels_", js_id, " = function() {
            window.fdcf_makePanelDraggable_", js_id, "('", ns("panel_layers"), "');
            window.fdcf_makePanelDraggable_", js_id, "('", ns("panel_process"), "');
            window.fdcf_makePanelDraggable_", js_id, "('", ns("panel_stats"), "');
            window.fdcf_makePanelDraggable_", js_id, "('", ns("panel_reference"), "');
            window.fdcf_makePanelDraggable_", js_id, "('", ns("panel_downloads"), "');
          };

          window.fdcf_captureMapScreenshot_", js_id, " = function() {
            var mapEl = window.fdcf_getEl_", js_id, "('", ns("map"), "');

            if (!mapEl) {
              alert('No se encontró el mapa para capturar.');
              return;
            }

            if (typeof html2canvas === 'undefined') {
              alert('html2canvas no está cargado.');
              return;
            }

            html2canvas(mapEl, {
              useCORS: true,
              allowTaint: false,
              backgroundColor: '#000000',
              scale: 1
            }).then(function(canvas) {
              var dataUrl = canvas.toDataURL('image/png');

              Shiny.setInputValue(
                '", ns("map_screenshot_data"), "',
                {
                  data: dataUrl,
                  nonce: Math.random()
                },
                {priority: 'event'}
              );
            }).catch(function(error) {
              console.error('Error capturando screenshot:', error);
              alert('No se pudo capturar el mapa. Puede ser un problema CORS de algún fondo externo.');
            });
          };

          window.fdcf_addOrUpdateOverlay_", js_id, " = function(map, message) {
            var url = message.url;
            var visible = message.visible;

            var imageBounds = [
              [-90, -180],
              [ 90,  180]
            ];

            map.fdcfUrl = url;
            map.fdcfVisible = visible;

            if (!map.getPane('fdcfPngPane')) {
              map.createPane('fdcfPngPane');
              map.getPane('fdcfPngPane').style.zIndex = 450;
            }

            if (!map.getPane('fdcfMarkerPane')) {
              map.createPane('fdcfMarkerPane');
              map.getPane('fdcfMarkerPane').style.zIndex = 700;
            }

            if (map.fdcfOverlay) {
              map.removeLayer(map.fdcfOverlay);
              map.fdcfOverlay = null;
            }

            if (!visible || !url) {
              return;
            }

            var overlay = L.imageOverlay(
              url,
              imageBounds,
              {
                opacity: 1,
                interactive: false,
                pane: 'fdcfPngPane'
              }
            );

            overlay.on('load', function() {
              console.log('FDCF cargado:', url);
            });

            overlay.on('error', function(e) {
              console.error('ERROR cargando FDCF:', url, e);
              alert('No se pudo cargar la imagen FDCF. URL: ' + url);
            });

            overlay.addTo(map);
            map.fdcfOverlay = overlay;
          };

          setTimeout(window.fdcf_initPanels_", js_id, ", 100);
          document.addEventListener('DOMContentLoaded', window.fdcf_initPanels_", js_id, ");
          document.addEventListener('shiny:connected', function() {
            setTimeout(window.fdcf_initPanels_", js_id, ", 100);
          });

          if (!window.fdcf_handlers_", js_id, ") {
            window.fdcf_handlers_", js_id, " = true;

            Shiny.addCustomMessageHandler('fdcf-reload-", id, "', function(message) {
              var widget = HTMLWidgets.find('#", ns("map"), "');

              if (!widget) {
                console.error('No se encontró el widget Leaflet FDCF');
                return;
              }

              var map = widget.getMap();

              if (!map) {
                console.error('No se encontró el objeto Leaflet FDCF');
                return;
              }

              window.fdcf_addOrUpdateOverlay_", js_id, "(map, message);
            });

            Shiny.addCustomMessageHandler('fdcf-toggle-", id, "', function(message) {
              var widget = HTMLWidgets.find('#", ns("map"), "');

              if (!widget) {
                console.error('No se encontró el widget Leaflet FDCF');
                return;
              }

              var map = widget.getMap();

              if (!map) {
                console.error('No se encontró el objeto Leaflet FDCF');
                return;
              }

              if (!map.fdcfUrl) {
                return;
              }

              window.fdcf_addOrUpdateOverlay_", js_id, "(map, {
                url: map.fdcfUrl,
                visible: message.visible
              });
            });

            Shiny.addCustomMessageHandler('fdcf-status-", id, "', function(message) {
              var el = window.fdcf_getEl_", js_id, "('", ns("fdcf_status"), "');

              if (el) {
                el.textContent = message.text;
                el.scrollTop = el.scrollHeight;
                window.fdcf_scrollStatus_", js_id, "();
              }
            });
          }
        })();
      ")))
    ),

    div(
      id = ns("fdcf_root"),

      actionButton(
        inputId = ns("btn_go_home"),
        label = "← Launcher"
      ),

      div(
        id = ns("toolbar_left"),
        class = "fdcf-top-toolbar",

        div(
          class = "fdcf-left-card",

          selectInput(
            inputId = ns("base_map"),
            label = "Fondo WGS84",
            choices = fdcf_base_maps,
            selected = "none",
            width = "210px"
          ),

          selectInput(
            inputId = ns("fdcf_style"),
            label = "FDCF options",
            choices = fdcf_style_choices,
            selected = fdcf_default_style,
            width = "230px"
          ),

          div(
            id = ns("show_fdcf_container"),
            class = "fdcf-toggle-wrap",
            tags$label(
              class = "fdcf-toggle",
              tags$input(
                id = ns("show_fdcf"),
                type = "checkbox",
                checked = "checked"
              ),
              tags$span(class = "fdcf-toggle-slider"),
              tags$span(class = "fdcf-toggle-label", "FDCF")
            )
          ),

          div(
            id = ns("show_fire_markers_container"),
            class = "fdcf-toggle-wrap",
            tags$label(
              class = "fdcf-toggle",
              tags$input(
                id = ns("show_fire_markers"),
                type = "checkbox",
                checked = "checked"
              ),
              tags$span(class = "fdcf-toggle-slider"),
              tags$span(class = "fdcf-toggle-label", "Focos")
            )
          ),

          selectInput(
            inputId = ns("reference_layers_select"),
            label = "Capas",
            choices = fdcf_reference_layer_choices,
            selected = "countries",
            width = "260px"
          )
        )
      ),

      div(
        id = ns("toolbar_right"),
        class = "fdcf-top-toolbar",

        tags$button(
          class = "fdcf-toolbar-btn",
          onclick = paste0("fdcf_togglePanel_", js_id, "('", ns("panel_layers"), "')"),
          "Ayuda"
        ),

        tags$button(
          class = "fdcf-toolbar-btn",
          onclick = paste0("fdcf_togglePanel_", js_id, "('", ns("panel_process"), "')"),
          "Procesamiento"
        ),

        tags$button(
          class = "fdcf-toolbar-btn",
          onclick = paste0("fdcf_togglePanel_", js_id, "('", ns("panel_stats"), "')"),
          "Estadísticas"
        ),

        tags$button(
          class = "fdcf-toolbar-btn",
          onclick = paste0("fdcf_togglePanel_", js_id, "('", ns("panel_reference"), "')"),
          "Referencia"
        ),

        tags$button(
          class = "fdcf-toolbar-btn",
          onclick = paste0("fdcf_togglePanel_", js_id, "('", ns("panel_downloads"), "')"),
          "Descargas"
        )
      ),

      div(
        id = ns("panel_layers"),
        class = "fdcf-floating-panel",
        style = "display: none;",

        div(class = "fdcf-panel-header", "Controles visibles"),

        div(
          class = "fdcf-panel-body",
          div(
            class = "fdcf-small-note",
            "Los controles principales están fijos arriba a la izquierda: fondo, PNG FDCF, mostrar PNG, marcar focos y capas referenciales."
          ),
          tags$hr(),
          tags$ul(
            tags$li(strong("Fondo WGS84:"), " cambia el mapa base."),
            tags$li(strong("FDCF options:"), " cambia el procesamiento FDCF."),
            tags$li(strong("FDCF:"), " prende o apaga la imagen FDCF."),
            tags$li(strong("Focos:"), " prende o apaga los círculos grandes sobre píxeles de fuego."),
            tags$li(strong("Capas:"), " agrega límites internacionales o provincias/estados globales.")
          )
        )
      ),

      div(
        id = ns("panel_process"),
        class = "fdcf-floating-panel",

        div(class = "fdcf-panel-header", "Procesamiento GOES FDCF"),

        div(
          class = "fdcf-panel-body",
          actionButton(
            inputId = ns("refresh_fdcf"),
            label = "Descargar y procesar último FDCF"
          ),
          br(),
          br(),
          strong("Estado:"),
          verbatimTextOutput(ns("fdcf_status"))
        )
      ),

      div(
        id = ns("panel_stats"),
        class = "fdcf-floating-panel",
        style = "display: none;",

        div(class = "fdcf-panel-header", "Estadísticas FDCF"),

        div(
          class = "fdcf-panel-body",

          div(
            class = "fdcf-stats-source-box",
            strong("Archivo NetCDF original:"),
            br(),
            textOutput(ns("stats_source_file")),
            br(),
            tags$small(
              "Las estadísticas se calculan desde la variable Mask reproyectada a WGS84 por vecino más cercano."
            )
          ),

          div(class = "fdcf-table-section-title", "Resumen general"),
          tableOutput(ns("general_stats_table")),

          div(class = "fdcf-table-section-title", "Píxeles con focos de calor por clase"),
          tableOutput(ns("fire_table")),

          div(class = "fdcf-table-section-title", "Todas las clases presentes"),
          tableOutput(ns("class_table")),

          div(class = "fdcf-table-section-title", "Coordenadas de píxeles con focos de calor"),
          tableOutput(ns("fire_points_table")),

          div(
            class = "fdcf-small-note",
            "La tabla de coordenadas muestra hasta los primeros 500 píxeles con fuego."
          )
        )
      ),

      div(
        id = ns("panel_reference"),
        class = "fdcf-floating-panel",
        style = "display: none;",

        div(class = "fdcf-panel-header", "Referencia del PNG seleccionado"),

        div(
          class = "fdcf-panel-body",
          uiOutput(ns("fdcf_legend"))
        )
      ),

      div(
        id = ns("panel_downloads"),
        class = "fdcf-floating-panel",
        style = "display: none;",

        div(class = "fdcf-panel-header", "Descargas"),

        div(
          class = "fdcf-panel-body",

          downloadButton(
            outputId = ns("download_selected_png"),
            label = "Descargar PNG seleccionado"
          ),

          downloadButton(
            outputId = ns("download_fire_points_csv"),
            label = "Descargar coordenadas CSV"
          ),

          tags$hr(),

          tags$button(
            id = ns("take_screenshot"),
            type = "button",
            onclick = paste0("fdcf_captureMapScreenshot_", js_id, "();"),
            "Capturar screenshot del mapa"
          ),

          downloadButton(
            outputId = ns("download_map_screenshot"),
            label = "Descargar screenshot"
          ),

          tags$small(
            "El screenshot captura el mapa visible. Algunos fondos externos pueden bloquear la captura por CORS."
          )
        )
      ),

      leafletOutput(
        outputId = ns("map"),
        width = "100vw",
        height = "100vh"
      )
    )
  )
}


# ============================================================
# 7) MÓDULO SERVER FDCF
# ============================================================

mod_fdcf_new_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    bucket <- fdcf_bucket
    product <- fdcf_product
    hours_back <- 8

    initial_lng <- 0
    initial_lat <- 0
    initial_zoom <- 1.5

    initial_reference_layer <- "countries"

    fdcf_register_asset_path()

    current_key <- reactiveVal(NULL)
    current_path <- reactiveVal(NULL)
    last_update <- reactiveVal(NULL)
    last_processed <- reactiveVal(NULL)
    stats_source_file <- reactiveVal(NULL)

    status_text <- reactiveVal(
      "App FDCF iniciada.\nMapa listo.\nPresioná el botón para descargar y procesar el último producto FDCF."
    )

    general_stats_data <- reactiveVal(NULL)
    class_table_data <- reactiveVal(NULL)
    fire_table_data <- reactiveVal(NULL)
    fire_points_data <- reactiveVal(data.frame())

    fdcf_selected_url <- function(style_id) {
      if (is.null(style_id) || !style_id %in% names(fdcf_png_files)) {
        style_id <- fdcf_default_style
      }

      paste0(
        fdcf_asset_prefix,
        "/",
        basename(fdcf_png_files[[style_id]]),
        "?v=",
        as.numeric(Sys.time())
      )
    }

    fdcf_send_overlay <- function() {
      processed <- last_processed()

      if (is.null(processed)) {
        return()
      }

      style_id <- input$fdcf_style

      if (is.null(style_id)) {
        style_id <- fdcf_default_style
      }

      session$sendCustomMessage(
        type = paste0("fdcf-reload-", id),
        message = list(
          url = fdcf_selected_url(style_id),
          visible = isTRUE(input$show_fdcf)
        )
      )
    }

    fdcf_add_reference_layers_to_proxy <- function(selected_raw) {
      selected <- character()

      if (is.null(selected_raw) || selected_raw == "none") {
        selected <- character()
      }

      if (identical(selected_raw, "countries")) {
        selected <- "countries"
      }

      if (identical(selected_raw, "admin1")) {
        selected <- "admin1"
      }

      if (identical(selected_raw, "countries_admin1")) {
        selected <- c("countries", "admin1")
      }

      proxy <- leafletProxy("map", session = session) %>%
        clearGroup("fdcf_countries") %>%
        clearGroup("fdcf_admin1")

      if (length(selected) == 0) {
        return(invisible(NULL))
      }

      if ("countries" %in% selected && !is.null(fdcf_ref_layers$countries)) {
        proxy <- proxy %>%
          addPolygons(
            data = fdcf_ref_layers$countries,
            group = "fdcf_countries",
            fill = FALSE,
            color = "#ffffff",
            weight = 1.1,
            opacity = 1,
            label = ~name,
            options = pathOptions(pane = "fdcfReferencePane")
          )
      }

      if ("admin1" %in% selected && !is.null(fdcf_ref_layers$admin1_global)) {
        proxy <- proxy %>%
          addPolylines(
            data = fdcf_ref_layers$admin1_global,
            group = "fdcf_admin1",
            color = "#00ffff",
            weight = 0.8,
            opacity = 0.9,
            label = ~fdcf_admin1_label,
            options = pathOptions(pane = "fdcfReferencePane")
          )
      }

      invisible(NULL)
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

    observeEvent(status_text(), {
      session$sendCustomMessage(
        type = paste0("fdcf-status-", id),
        message = list(text = status_text())
      )
    }, ignoreInit = TRUE)

    output$stats_source_file <- renderText({
      x <- stats_source_file()

      if (is.null(x)) {
        "Sin archivo procesado todavía."
      } else {
        x
      }
    })

    output$map <- renderLeaflet({
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
        addMapPane("fdcfPngPane", zIndex = 450) %>%
        addMapPane("fdcfReferencePane", zIndex = 650) %>%
        addMapPane("fdcfMarkerPane", zIndex = 700) %>%
        setView(
          lng = initial_lng,
          lat = initial_lat,
          zoom = initial_zoom
        )

      m <- fdcf_add_base_layer_to_map(
        m = m,
        base_id = "none"
      )

      if (initial_reference_layer %in% c("countries", "countries_admin1")) {
        if (!is.null(fdcf_ref_layers$countries)) {
          m <- m %>%
            addPolygons(
              data = fdcf_ref_layers$countries,
              group = "fdcf_countries",
              fill = FALSE,
              color = "#ffffff",
              weight = 1.1,
              opacity = 1,
              label = ~name,
              options = pathOptions(pane = "fdcfReferencePane")
            )
        }
      }

      if (initial_reference_layer %in% c("admin1", "countries_admin1")) {
        if (!is.null(fdcf_ref_layers$admin1_global)) {
          m <- m %>%
            addPolylines(
              data = fdcf_ref_layers$admin1_global,
              group = "fdcf_admin1",
              color = "#00ffff",
              weight = 0.8,
              opacity = 0.9,
              label = ~fdcf_admin1_label,
              options = pathOptions(pane = "fdcfReferencePane")
            )
        }
      }

      m
    })

    observeEvent(TRUE, {
      session$onFlushed(function() {
        isolate({
          selected_raw <- input$reference_layers_select

          if (is.null(selected_raw)) {
            selected_raw <- initial_reference_layer
          }

          fdcf_add_reference_layers_to_proxy(selected_raw)
        })
      }, once = TRUE)
    }, once = TRUE)

    observeEvent(input$base_map, {
      proxy <- leafletProxy("map", session = session)

      fdcf_add_base_layer_to_proxy(
        proxy = proxy,
        base_id = input$base_map
      )
    }, ignoreInit = TRUE)

    observeEvent(input$fdcf_style, {
      fdcf_send_overlay()
    }, ignoreInit = TRUE)

    observeEvent(input$show_fdcf, {
      session$sendCustomMessage(
        type = paste0("fdcf-toggle-", id),
        message = list(
          visible = isTRUE(input$show_fdcf)
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$reference_layers_select, {
      fdcf_add_reference_layers_to_proxy(input$reference_layers_select)
    }, ignoreInit = TRUE)

    observe({
      df <- fire_points_data()
      show_markers <- isTRUE(input$show_fire_markers)

      proxy <- leafletProxy("map", session = session) %>%
        clearGroup("fdcf_fire_markers")

      if (!show_markers || is.null(df) || nrow(df) == 0) {
        return()
      }

      df_plot <- df

      if (nrow(df_plot) > 5000) {
        df_plot <- df_plot[seq_len(5000), ]
      }

      proxy %>%
        addCircleMarkers(
          data = df_plot,
          lng = ~lon,
          lat = ~lat,
          group = "fdcf_fire_markers",
          radius = 7,
          stroke = TRUE,
          weight = 2,
          color = "#ffffff",
          fillColor = "#ff0000",
          fillOpacity = 0.9,
          opacity = 1,
          label = ~paste0(
            "Clase ", fdcf_class,
            " - ", class_name,
            " | Lon: ", round(lon, 4),
            " | Lat: ", round(lat, 4)
          ),
          popup = ~paste0(
            "<b>Foco de calor FDCF</b><br>",
            "Clase: ", fdcf_class, "<br>",
            "Tipo: ", class_name, "<br>",
            "Lon: ", round(lon, 5), "<br>",
            "Lat: ", round(lat, 5), "<br>",
            "Celda WGS84: ", cell
          ),
          options = pathOptions(pane = "fdcfMarkerPane")
        )
    })

    output$general_stats_table <- renderTable({
      x <- general_stats_data()

      if (is.null(x)) {
        data.frame(
          Medida = "Sin datos todavía",
          Valor = ""
        )
      } else {
        x
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$class_table <- renderTable({
      x <- class_table_data()

      if (is.null(x)) {
        data.frame(
          fdcf_class = "Sin datos todavía",
          n_pixels = ""
        )
      } else {
        x
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$fire_table <- renderTable({
      x <- fire_table_data()

      if (is.null(x) || nrow(x) == 0) {
        data.frame(
          fdcf_class = "Sin focos de calor detectados",
          n_pixels = "",
          class_name = "",
          percent_total = ""
        )
      } else {
        x
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$fire_points_table <- renderTable({
      x <- fire_points_data()

      if (is.null(x) || nrow(x) == 0) {
        data.frame(
          cell = "Sin focos de calor detectados",
          lon = "",
          lat = "",
          fdcf_class = "",
          class_name = ""
        )
      } else {
        head(x, 500)
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$fdcf_legend <- renderUI({
      style_id <- input$fdcf_style

      if (is.null(style_id) || !style_id %in% names(fdcf_palettes)) {
        style_id <- fdcf_default_style
      }

      pal <- fdcf_palettes[[style_id]]
      visible <- pal[pal$a > 0, ]

      if (nrow(visible) == 0) {
        return(tags$div("El estilo seleccionado no tiene clases visibles."))
      }

      legend_rows <- lapply(seq_len(nrow(visible)), function(i) {
        rgba <- paste0(
          "rgba(",
          visible$r[i], ",",
          visible$g[i], ",",
          visible$b[i], ",",
          round(visible$a[i] / 255, 3),
          ")"
        )

        label <- fdcf_fire_labels[as.character(visible$value[i])]

        if (is.na(label)) {
          label <- paste("Clase", visible$value[i])
        } else {
          label <- paste("Clase", visible$value[i], "-", label)
        }

        div(
          class = "fdcf-legend-row",
          span(
            class = "fdcf-legend-swatch",
            style = paste0("background:", rgba, ";")
          ),
          span(label)
        )
      })

      tagList(
        div(
          class = "fdcf-small-note",
          paste(
            "Referencia visible para:",
            names(fdcf_style_choices)[fdcf_style_choices == style_id]
          )
        ),
        tags$hr(),
        legend_rows
      )
    })

    output$download_selected_png <- downloadHandler(
      filename = function() {
        style_id <- input$fdcf_style

        if (is.null(style_id)) {
          style_id <- fdcf_default_style
        }

        paste0(
          "fdcf_",
          style_id,
          "_wgs84_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".png"
        )
      },
      content = function(file) {
        style_id <- input$fdcf_style

        if (is.null(style_id)) {
          style_id <- fdcf_default_style
        }

        src <- fdcf_png_files[[style_id]]

        validate(
          need(file.exists(src), "Todavía no existe el PNG seleccionado. Primero procesá una imagen.")
        )

        file.copy(src, file, overwrite = TRUE)
      },
      contentType = "image/png"
    )

    output$download_fire_points_csv <- downloadHandler(
      filename = function() {
        paste0(
          "fdcf_fire_points_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        x <- fire_points_data()

        validate(
          need(!is.null(x) && nrow(x) > 0, "No hay focos de calor para exportar.")
        )

        write.csv(x, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    observeEvent(input$map_screenshot_data, {
      x <- input$map_screenshot_data$data

      if (is.null(x) || !nzchar(x)) {
        showNotification(
          "No se recibió información del screenshot.",
          type = "error",
          duration = 6
        )
        return()
      }

      x <- sub("^data:image/png;base64,", "", x)

      ok <- tryCatch({
        dir.create(dirname(fdcf_map_screenshot_png), showWarnings = FALSE, recursive = TRUE)

        base64enc::base64decode(
          what = x,
          output = fdcf_map_screenshot_png
        )

        TRUE
      }, error = function(e) {
        message("Error guardando screenshot FDCF: ", e$message)
        FALSE
      })

      if (ok && file.exists(fdcf_map_screenshot_png)) {
        showNotification(
          "Screenshot FDCF guardado. Ya podés descargarlo.",
          type = "message",
          duration = 5
        )
      } else {
        showNotification(
          "No se pudo guardar el screenshot FDCF.",
          type = "error",
          duration = 6
        )
      }
    })

    output$download_map_screenshot <- downloadHandler(
      filename = function() {
        paste0(
          "fdcf_map_screenshot_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".png"
        )
      },
      content = function(file) {
        validate(
          need(file.exists(fdcf_map_screenshot_png), "Primero capturá un screenshot del mapa.")
        )

        file.copy(fdcf_map_screenshot_png, file, overwrite = TRUE)
      },
      contentType = "image/png"
    )

    observeEvent(input$refresh_fdcf, {
      status_log <- character()

      add_status <- function(txt, type = c("info", "ok", "error")) {
        type <- match.arg(type)
        txt <- as.character(txt)

        icon <- switch(
          type,
          info = "•",
          ok = "✅",
          error = "❌"
        )

        linea <- paste0(
          fdcf_timestamp_ms(),
          " ",
          icon,
          " ",
          txt
        )

        status_log <<- c(status_log, linea)

        nuevo_texto <- paste(status_log, collapse = "\n")

        status_text(nuevo_texto)

        session$sendCustomMessage(
          type = paste0("fdcf-status-", id),
          message = list(text = nuevo_texto)
        )

        try(session$flushReact(), silent = TRUE)

        message(txt)
      }

      withProgress(message = "Procesando GOES FDCF", value = 0, {
        step <- 0
        total_steps <- 18

        progress_status <- function(txt) {
          step <<- min(step + 1, total_steps)

          incProgress(
            amount = 1 / total_steps,
            detail = txt
          )

          txt_chr <- as.character(txt)
          txt_low <- stringr::str_to_lower(txt_chr)

          ok_patterns <- c(
            "finalizada",
            "finalizado",
            "completo",
            "completa",
            "listo",
            "cargado",
            "cargada",
            "calculadas",
            "calculados",
            "generando png",
            "procesamiento fdcf completo",
            "fDCF cargado"
          )

          is_ok <- any(stringr::str_detect(
            txt_low,
            stringr::str_to_lower(paste(ok_patterns, collapse = "|"))
          ))

          if (is_ok) {
            add_status(txt_chr, type = "ok")
          } else {
            add_status(txt_chr, type = "info")
          }
        }

        info <- tryCatch({
          fdcf_update_image(
            bucket = bucket,
            product = product,
            hours_back = hours_back,
            status_fun = progress_status
          )
        }, error = function(e) {
          add_status(paste("ERROR:", e$message), type = "error")

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

        progress_status("Actualizando tablas, focos y marcadores...")

        current_key(info$key)
        current_path(info$path)
        last_update(Sys.time())
        last_processed(info$processed)
        stats_source_file(basename(info$key))

        general_stats_data(info$processed$general_stats)
        class_table_data(info$processed$class_table)
        fire_table_data(info$processed$fire_table)
        fire_points_data(info$processed$fire_points)

        progress_status("Tablas estadísticas actualizadas.")
        progress_status("Cargando PNG FDCF seleccionado en el mapa...")

        fdcf_send_overlay()

        progress_status("FDCF cargado en el mapa.")

        showNotification(
          paste("FDCF actualizado:", basename(info$key)),
          type = "message",
          duration = 6
        )
      })
    })
  })
}


# ============================================================
# 8) APP PRINCIPAL DE PRUEBA
# ============================================================

ui <- fluidPage(
  style = "padding: 0; margin: 0; background: #000000;",
  mod_fdcf_new_ui("fdcf01")
)

server <- function(input, output, session) {
  mod_fdcf_new_server(
    id = "fdcf01"
  )

  observeEvent(input[["fdcf01-btn_go_home"]], {
    showNotification("Botón ← Launcher presionado", type = "message")

    # Acá podés volver al launcher real, por ejemplo:
    # current_screen("launcher")
    # shinyjs::show("launcher")
    # shinyjs::hide("fdcf01-fdcf_root")
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)

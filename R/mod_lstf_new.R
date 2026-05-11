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
library(viridisLite)
library(plotly)
library(sf)
library(rnaturalearth)
library(base64enc)

# ============================================================
# 0) CONFIGURACIÓN GENERAL
# ============================================================

BUCKET  <- "noaa-goes19"
PRODUCT <- "ABI-L2-LSTF"

NC_DIR <- file.path(tempdir(), "goes_lstf_downloads")
dir.create(NC_DIR, showWarnings = FALSE, recursive = TRUE)

dir.create("www", showWarnings = FALSE, recursive = TRUE)

LST_ORIGINAL_PNG <- "www/lstf_original_goes.png"
LST_WGS84_PNG    <- "www/lstf_global_wgs84.png"
LST_3857_PNG     <- "www/lstf_global_3857.png"

MAP_SCREENSHOT_PNG <- "www/map_screenshot.png"

LST_COLOR_MIN_C <- -60
LST_COLOR_MAX_C <- 60

HIST_MIN_C <- -70
HIST_MAX_C <- 70

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

# Fondo WMS con calles/rutas para WGS84
TERRASTRIS_OSM_WMS <- "https://ows.terrestris.de/osm/service"

message("Carpeta temporal de descarga: ", NC_DIR)


# ============================================================
# 1) FUNCIONES AUXILIARES
# ============================================================

default_status_fun <- function(txt) {
  message(txt)
}

timestamp_ms <- function() {
  format(Sys.time(), "%H:%M:%OS3")
}

lst_palette <- function(n = 256) {
  viridisLite::turbo(n)
}

lst_color_for_values <- function(x, n = 256) {
  x_clamped <- pmin(pmax(x, LST_COLOR_MIN_C), LST_COLOR_MAX_C)

  z <- (x_clamped - LST_COLOR_MIN_C) /
    (LST_COLOR_MAX_C - LST_COLOR_MIN_C)

  idx <- floor(z * (n - 1)) + 1
  idx[is.na(idx)] <- 1

  lst_palette(n)[idx]
}

plotly_colorscale_from_palette <- function(pal) {
  n <- length(pal)

  lapply(seq_along(pal), function(i) {
    list(
      (i - 1) / (n - 1),
      pal[i]
    )
  })
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
  if (provider_name == "none") {
    return(TRUE)
  }

  out <- get_leaflet_provider(provider_name)
  !is.null(out)
}


# ============================================================
# 2) FONDOS POR PROYECCIÓN
# ============================================================

BASE_MAPS_MERCATOR <- c(
  "Oscuro - CartoDB DarkMatter"       = "CartoDB.DarkMatter",
  "Claro - CartoDB Positron"          = "CartoDB.Positron",
  "Voyager - CartoDB"                 = "CartoDB.Voyager",
  "OpenStreetMap"                     = "OpenStreetMap",
  "Topográfico - OpenTopoMap"         = "OpenTopoMap",

  "ESRI Imagen satelital"             = "Esri.WorldImagery",
  "ESRI Calles"                       = "Esri.WorldStreetMap",
  "ESRI Topográfico"                  = "Esri.WorldTopoMap",
  "ESRI Terreno"                      = "Esri.WorldTerrain",
  "ESRI Físico"                       = "Esri.WorldPhysical",
  "ESRI Gris"                         = "Esri.WorldGrayCanvas",
  "ESRI National Geographic"          = "Esri.NatGeoWorldMap",
  "ESRI Relieve sombreado"            = "Esri.WorldShadedRelief",

  "Stadia Alidade Smooth"             = "Stadia.AlidadeSmooth",
  "Stadia Alidade Smooth Dark"        = "Stadia.AlidadeSmoothDark",
  "Stadia Outdoors"                   = "Stadia.Outdoors",
  "Stadia OSM Bright"                 = "Stadia.OSMBright",

  "NASA GIBS VIIRS Night 2012"        = "NASAGIBS.ViirsEarthAtNight2012",
  "NASA GIBS Blue Marble"             = "NASAGIBS.BlueMarble",
  "NASA GIBS Modis Terra TrueColor"   = "NASAGIBS.ModisTerraTrueColorCR",
  "NASA GIBS Modis Aqua TrueColor"    = "NASAGIBS.ModisAquaTrueColorCR"
)

BASE_MAPS_MERCATOR <- BASE_MAPS_MERCATOR[
  vapply(BASE_MAPS_MERCATOR, provider_available, logical(1))
]

if (length(BASE_MAPS_MERCATOR) == 0) {
  BASE_MAPS_MERCATOR <- c("OpenStreetMap" = "OpenStreetMap")
}

BASE_MAPS_WGS84 <- c(
  "NASA GIBS VIIRS luces nocturnas 2012" = "gibs_viirs_night_2012",
  "OSM WMS - terrestris calles/rutas"    = "wms_terrestris_osm",
  "NASA GIBS Blue Marble NG"             = "gibs_blue_marble_ng",
  "Sin fondo geográfico"                 = "none"
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
  gibs_blue_marble_shaded = list(
    layer = "BlueMarble_ShadedRelief",
    format = "image/jpeg",
    transparent = FALSE
  ),
  gibs_blue_marble_ng = list(
    layer = "BlueMarble_NextGeneration",
    format = "image/jpeg",
    transparent = FALSE
  ),
  gibs_viirs_night_2012 = list(
    layer = "VIIRS_CityLights_2012",
    format = "image/jpeg",
    transparent = FALSE
  ),
  gibs_modis_terra_true = list(
    layer = "MODIS_Terra_CorrectedReflectance_TrueColor",
    format = "image/jpeg",
    transparent = FALSE
  ),
  gibs_modis_aqua_true = list(
    layer = "MODIS_Aqua_CorrectedReflectance_TrueColor",
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

  if (projection == "wgs84" && "gibs_viirs_night_2012" %in% choices) {
    return("gibs_viirs_night_2012")
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
        addProviderTiles(
          provider = provider,
          group = "base_tiles"
        )
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
        addProviderTiles(
          provider = provider,
          group = "base_tiles"
        )
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

    if (base_id == "wms_terrestris_osm_gray") {
      proxy <- proxy %>%
        addWMSTiles(
          baseUrl = TERRASTRIS_OSM_WMS,
          layers = "OSM-WMS-GRAY",
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
    message("No se pudieron cargar límites internacionales: ", e$message)
    NULL
  })

  admin1_argentina <- tryCatch({
    rnaturalearth::ne_states(
      country = "Argentina",
      returnclass = "sf"
    )
  }, error = function(e) {
    message("No se pudieron cargar provincias/estados: ", e$message)
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
# 4) FUNCIONES PARA LISTAR ARCHIVOS GOES EN S3
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

get_latest_lstf_key <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  status_fun("Buscando la última imagen disponible...")

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
# 5) DESCARGA ROBUSTA A CARPETA TEMPORAL
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

  status_fun("Descargando archivo NetCDF...")

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

download_latest_lstf <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  latest_key <- get_latest_lstf_key(
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
    stop("No se pudo descargar el último archivo LSTF.")
  }

  list(
    key = latest_key,
    path = latest_path
  )
}


# ============================================================
# 6) RASTER A PNG
# ============================================================

raster_to_png <- function(
    r,
    output_png,
    min_c = LST_COLOR_MIN_C,
    max_c = LST_COLOR_MAX_C,
    opacity = 1,
    flip_vertical = FALSE,
    status_fun = default_status_fun
) {
  status_fun(paste("Convirtiendo raster a PNG:", basename(output_png)))

  m <- terra::as.matrix(r, wide = TRUE)

  if (isTRUE(flip_vertical)) {
    m <- m[nrow(m):1, ]
  }

  m_clamped <- pmin(pmax(m, min_c), max_c)

  z <- (m_clamped - min_c) / (max_c - min_c)
  z[z < 0] <- 0
  z[z > 1] <- 1

  pal <- lst_palette(256)
  rgb <- grDevices::col2rgb(pal) / 255

  idx <- floor(z * 255) + 1
  idx[is.na(idx)] <- 1

  img <- array(0, dim = c(nrow(m), ncol(m), 4))

  img[, , 1] <- matrix(rgb[1, idx], nrow = nrow(m), ncol = ncol(m))
  img[, , 2] <- matrix(rgb[2, idx], nrow = nrow(m), ncol = ncol(m))
  img[, , 3] <- matrix(rgb[3, idx], nrow = nrow(m), ncol = ncol(m))

  img[, , 4] <- ifelse(is.na(m), 0, opacity)

  png::writePNG(img, target = output_png)

  status_fun(paste("PNG creado:", basename(output_png)))

  invisible(output_png)
}


# ============================================================
# 7) ESTADÍSTICAS
# ============================================================

compute_lst_statistics <- function(
    r_stats,
    r_nc,
    nc_filename = NA_character_,
    status_fun = default_status_fun
) {
  status_fun("Calculando detalle general del NetCDF original...")
  status_fun("Calculando detalle de píxeles del NetCDF original...")
  status_fun("Calculando medidas de posición y dispersión...")

  nc_rows <- terra::nrow(r_nc)
  nc_cols <- terra::ncol(r_nc)
  nc_total <- terra::ncell(r_nc)

  nc_res <- terra::res(r_nc)
  wgs84_res <- terra::res(r_stats)

  nc_res_x <- as.numeric(nc_res[1])
  nc_res_y <- as.numeric(nc_res[2])

  wgs84_res_x <- as.numeric(wgs84_res[1])
  wgs84_res_y <- as.numeric(wgs84_res[2])

  general_info <- data.frame(
    Medida = c(
      "Nombre del archivo",
      "Resolución original X, metros",
      "Resolución original Y, metros",
      "Resolución WGS84 X, grados",
      "Resolución WGS84 Y, grados",
      "Píxeles en filas",
      "Píxeles en columnas"
    ),
    Valor = c(
      nc_filename,
      round(nc_res_x, 4),
      round(nc_res_y, 4),
      round(wgs84_res_x, 6),
      round(wgs84_res_y, 6),
      nc_rows,
      nc_cols
    )
  )

  nc_na <- terra::global(
    is.na(r_nc),
    fun = "sum",
    na.rm = TRUE
  )[1, 1]

  nc_na <- as.numeric(nc_na)
  nc_valid <- nc_total - nc_na

  pixel_info <- data.frame(
    Medida = c(
      "Cantidad total de píxeles",
      "Cantidad de píxeles sin datos",
      "Cantidad de píxeles con datos"
    ),
    n = c(
      nc_total,
      nc_na,
      nc_valid
    ),
    Porcentaje = c(
      100,
      round((nc_na / nc_total) * 100, 4),
      round((nc_valid / nc_total) * 100, 4)
    )
  )

  vals <- terra::values(r_stats, mat = FALSE)
  vals <- vals[is.finite(vals)]

  if (length(vals) == 0) {
    empty <- data.frame(
      Medida = character(),
      Valor = numeric()
    )

    return(list(
      general_info = general_info,
      pixel_info = pixel_info,
      position_stats = empty,
      dispersion_stats = empty,
      hist_values = numeric()
    ))
  }

  n_valid <- length(vals)

  qs <- stats::quantile(
    vals,
    probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
    na.rm = TRUE,
    names = FALSE
  )

  position_stats <- data.frame(
    Medida = c(
      "n",
      "Mínimo",
      "Percentil 1",
      "Percentil 5",
      "Primer cuartil",
      "Media",
      "Mediana",
      "Tercer cuartil",
      "Percentil 95",
      "Percentil 99",
      "Máximo"
    ),
    Valor = c(
      n_valid,
      qs[1],
      qs[2],
      qs[3],
      qs[4],
      mean(vals),
      qs[5],
      qs[6],
      qs[7],
      qs[8],
      qs[9]
    )
  )

  dispersion_stats <- data.frame(
    Medida = c(
      "n",
      "Desvío estándar",
      "Varianza",
      "Rango intercuartílico",
      "Rango",
      "Desviación absoluta mediana",
      "Coeficiente de variación"
    ),
    Valor = c(
      n_valid,
      stats::sd(vals),
      stats::var(vals),
      stats::IQR(vals),
      diff(range(vals)),
      stats::mad(vals),
      stats::sd(vals) / abs(mean(vals))
    )
  )

  position_stats$Valor <- round(position_stats$Valor, 4)
  dispersion_stats$Valor <- round(dispersion_stats$Valor, 4)

  set.seed(123)

  hist_values <- if (length(vals) > 300000) {
    sample(vals, 300000)
  } else {
    vals
  }

  list(
    general_info = general_info,
    pixel_info = pixel_info,
    position_stats = position_stats,
    dispersion_stats = dispersion_stats,
    hist_values = hist_values
  )
}


# ============================================================
# 8) NETCDF GOES LSTF -> PNG ORIGINAL, WGS84 Y WEB MERCATOR
# ============================================================

lstf_netcdf_to_pngs <- function(
    nc_file,
    output_original_png = LST_ORIGINAL_PNG,
    output_wgs84_png = LST_WGS84_PNG,
    output_3857_png = LST_3857_PNG,
    width_wgs84 = 3600,
    height_wgs84 = 1800,
    width_3857 = 3600,
    height_3857 = 3400,
    min_c = LST_COLOR_MIN_C,
    max_c = LST_COLOR_MAX_C,
    status_fun = default_status_fun
) {
  if (!file.exists(nc_file)) {
    stop("No existe el NetCDF: ", nc_file)
  }

  nc_file_norm <- normalizePath(nc_file, winslash = "/", mustWork = TRUE)

  status_fun("Leyendo archivo NetCDF...")
  status_fun(basename(nc_file_norm))

  s <- terra::sds(nc_file_norm)

  s_names <- names(s)

  status_fun(paste("Variables encontradas:", paste(s_names, collapse = ", ")))

  i_lst <- which(s_names == "LST")

  if (length(i_lst) == 0) {
    i_lst <- grep("LST", s_names)
  }

  if (length(i_lst) == 0) {
    stop("No se encontró la variable LST dentro del NetCDF.")
  }

  i_lst <- i_lst[1]

  status_fun(paste("Variable seleccionada:", s_names[i_lst]))

  r0 <- s[[i_lst]]

  if (!terra::hasValues(r0)) {
    stop("El raster LST fue encontrado, pero terra no cargó valores.")
  }

  status_fun("Convirtiendo LST de Kelvin a Celsius...")

  r0 <- r0 - 273.15
  r0 <- terra::ifel(r0 < -100 | r0 > 100, NA, r0)

  status_fun("Procesamiento original GOES: generando PNG en resolución/proyección original...")

  raster_to_png(
    r = r0,
    output_png = output_original_png,
    min_c = min_c,
    max_c = max_c,
    opacity = 1,
    flip_vertical = FALSE,
    status_fun = status_fun
  )

  status_fun("Procesamiento WGS84: creando grilla global EPSG:4326 3600 x 1800...")

  template_wgs84 <- terra::rast(
    ncols = width_wgs84,
    nrows = height_wgs84,
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90,
    crs = "EPSG:4326"
  )

  status_fun("Procesamiento WGS84: reproyectando NetCDF original a EPSG:4326...")

  r_wgs84 <- terra::project(
    r0,
    template_wgs84,
    method = "bilinear"
  )

  status_fun("Procesamiento WGS84: generando PNG WGS84 global 3600 x 1800...")

  raster_to_png(
    r = r_wgs84,
    output_png = output_wgs84_png,
    min_c = min_c,
    max_c = max_c,
    opacity = 1,
    flip_vertical = FALSE,
    status_fun = status_fun
  )

  lst_summary <- compute_lst_statistics(
    r_stats = r_wgs84,
    r_nc = r0,
    nc_filename = basename(nc_file_norm),
    status_fun = status_fun
  )

  status_fun("Procesamiento Mercator: creando grilla EPSG:3857...")

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

  status_fun("Procesamiento Mercator: reproyectando NetCDF original a EPSG:3857...")

  r_3857 <- terra::project(
    r0,
    template_3857,
    method = "bilinear"
  )

  status_fun("Procesamiento Mercator: generando PNG WebMercator...")

  raster_to_png(
    r = r_3857,
    output_png = output_3857_png,
    min_c = min_c,
    max_c = max_c,
    opacity = 1,
    flip_vertical = FALSE,
    status_fun = status_fun
  )

  invisible(list(
    original_png = output_original_png,
    original_nrow = terra::nrow(r0),
    original_ncol = terra::ncol(r0),
    wgs84_png = output_wgs84_png,
    mercator_png = output_3857_png,
    r_wgs84 = r_wgs84,
    r_3857 = r_3857,
    general_info = lst_summary$general_info,
    pixel_info = lst_summary$pixel_info,
    position_stats = lst_summary$position_stats,
    dispersion_stats = lst_summary$dispersion_stats,
    hist_values = lst_summary$hist_values
  ))
}


# ============================================================
# 9) ACTUALIZAR UNA SOLA IMAGEN LSTF
# ============================================================

update_lstf_image <- function(
    bucket = BUCKET,
    product = PRODUCT,
    hours_back = 6,
    status_fun = default_status_fun
) {
  status_fun("Iniciando actualización LSTF...")

  latest <- download_latest_lstf(
    bucket = bucket,
    product = product,
    hours_back = hours_back,
    status_fun = status_fun
  )

  status_fun("Archivo NetCDF listo. Iniciando procesamiento...")

  processed <- lstf_netcdf_to_pngs(
    nc_file = latest$path,
    output_original_png = LST_ORIGINAL_PNG,
    output_wgs84_png = LST_WGS84_PNG,
    output_3857_png = LST_3857_PNG,
    width_wgs84 = 3600,
    height_wgs84 = 1800,
    width_3857 = 3600,
    height_3857 = 3400,
    min_c = LST_COLOR_MIN_C,
    max_c = LST_COLOR_MAX_C,
    status_fun = status_fun
  )

  status_fun("Procesamiento completo.")

  list(
    key = latest$key,
    path = latest$path,
    processed = processed
  )
}


# ============================================================
# 10) MÓDULO UI LSTF
# ============================================================

# ============================================================
# 10) MÓDULO UI LSTF
# ============================================================

mod_lstf_new_ui <- function(
    id,
    home_button_left = "20px",
    home_button_bottom = "20px"
) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js"),

      tags$style(HTML(paste0("
        html,
        body {
          background: #000000 !important;
        }

        #", ns("goes_lstf_app"), " {
          position: fixed;
          inset: 0;
          width: 100vw;
          height: 100vh;
          overflow: hidden;
          font-family: Arial, sans-serif;
          background: #000000 !important;
        }

        #", ns("goes_lstf_app"), " #", ns("map"), " {
          width: 100vw !important;
          height: 100vh !important;
        }

        #", ns("goes_lstf_app"), " .leaflet-container {
          background: #000000 !important;
        }

        #", ns("goes_lstf_app"), " .top-toolbar {
          position: absolute;
          z-index: 10000;
          background: rgba(0, 0, 0, 0.72);
          padding: 8px 10px;
          border-radius: 8px;
          display: flex;
          gap: 8px;
          align-items: center;
          font-family: Arial, sans-serif;
        }

        #", ns("goes_lstf_app"), " #", ns("map_toolbar_left"), " {
          top: ", LEFT_TOOLBAR_TOP, ";
          left: ", LEFT_TOOLBAR_LEFT, ";
        }

        #", ns("goes_lstf_app"), " #", ns("map_toolbar_right"), " {
          top: ", RIGHT_TOOLBAR_TOP, ";
          right: ", RIGHT_TOOLBAR_RIGHT, ";
        }

        #", ns("goes_lstf_app"), " .toolbar-btn {
          background: #1f78b4;
          color: white;
          border: none;
          padding: 7px 11px;
          border-radius: 5px;
          cursor: pointer;
          font-size: 13px;
        }

        #", ns("goes_lstf_app"), " .toolbar-btn:hover {
          background: #145a86;
        }

        #", ns("goes_lstf_app"), " .lstf-home-floating {
          position: absolute;
          left: ", home_button_left, ";
          bottom: ", home_button_bottom, ";
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

        #", ns("goes_lstf_app"), " .lstf-home-floating:hover {
          background: rgba(234, 88, 12, 0.98);
          color: white;
        }

        #", ns("goes_lstf_app"), " #", ns("projection_container"), " {
          display: flex !important;
          align-items: center !important;
          height: 30px;
          margin: 0 !important;
          padding: 0 !important;
        }

        #", ns("goes_lstf_app"), " #", ns("projection_container"), " .form-group {
          margin: 0 !important;
          padding: 0 !important;
        }

        #", ns("goes_lstf_app"), " #", ns("projection_container"), " select {
          height: 30px !important;
          min-height: 30px !important;
          padding: 3px 28px 3px 8px !important;
          font-size: 13px !important;
          border-radius: 5px !important;
          border: none !important;
        }

        #", ns("goes_lstf_app"), " #", ns("lstf_toggle_container"), " {
          display: flex !important;
          align-items: center !important;
          height: 30px;
          margin-left: 0;
          padding-left: 0;
          color: white !important;
        }

        #", ns("goes_lstf_app"), " #", ns("lstf_toggle_container"), " .form-group,
        #", ns("goes_lstf_app"), " #", ns("lstf_toggle_container"), " .checkbox {
          margin: 0 !important;
          padding: 0 !important;
        }

        #", ns("goes_lstf_app"), " #", ns("lstf_toggle_container"), " label {
          color: white !important;
          margin: 0 !important;
          padding: 0 !important;
          font-size: 13px !important;
          font-weight: normal !important;
          display: flex !important;
          align-items: center !important;
          gap: 4px;
          cursor: pointer;
        }

        #", ns("goes_lstf_app"), " #", ns("lstf_toggle_container"), " input[type='checkbox'] {
          position: static !important;
          margin: 0 4px 0 0 !important;
          opacity: 1 !important;
          display: inline-block !important;
          width: auto !important;
          height: auto !important;
        }

        #", ns("goes_lstf_app"), " .floating-panel {
          position: absolute;
          z-index: 9999;
          border-radius: 8px;
          font-family: Arial, sans-serif;
          box-shadow: 0 4px 16px rgba(0,0,0,0.35);
        }

        #", ns("goes_lstf_app"), " .panel-header {
          cursor: move;
          user-select: none;
          font-weight: bold;
          padding: 8px 10px;
          border-radius: 8px 8px 0 0;
        }

        #", ns("goes_lstf_app"), " .panel-body {
          padding: 10px 12px;
        }

        #", ns("panel_base"), " {
          top: 62px;
          left: 12px;
          width: 400px;
          max-height: 78vh;
          overflow-y: auto;
          background: rgba(255, 255, 255, 0.96);
          color: black;
        }

        #", ns("panel_base"), " .panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", ns("panel_layers"), " {
          top: 62px;
          left: 430px;
          width: 350px;
          background: rgba(255, 255, 255, 0.96);
          color: black;
        }

        #", ns("panel_layers"), " .panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", ns("panel_process"), " {
          top: 62px;
          right: 12px;
          width: 580px;
          max-width: 580px;
          background: rgba(0, 0, 0, 0.84);
          color: white;
        }

        #", ns("panel_process"), " .panel-header {
          background: rgba(20, 20, 20, 0.95);
        }

        #", ns("panel_process"), " pre {
          color: white;
          background: transparent;
          border: none;
          padding: 0;
          margin: 8px 0 0 0;
          white-space: pre-wrap;
          max-height: 560px;
          overflow-y: auto;
        }

        #", ns("refresh_lstf"), " {
          background: #00a870;
          color: white;
          border: none;
          padding: 7px 11px;
          border-radius: 5px;
          cursor: pointer;
          margin-top: 8px;
        }

        #", ns("refresh_lstf"), ":hover {
          background: #007c54;
        }

        #", ns("panel_stats"), " {
          right: 12px;
          top: 62px;
          width: 850px;
          max-height: calc(100vh - 86px);
          overflow-y: auto;
          background: rgba(255, 255, 255, 0.96);
          color: black;
        }

        #", ns("panel_stats"), " .panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", ns("lst_hist"), " {
          height: 390px !important;
        }

        #", ns("panel_scale"), " {
          right: 14px;
          bottom: 14px;
          width: 760px;
          background: rgba(255, 255, 255, 0.96);
          color: black;
          overflow: hidden;
        }

        #", ns("panel_scale"), " .panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", ns("panel_scale"), " .panel-body {
          padding: 8px 18px 12px 18px;
        }

        #", ns("color_scale_plot"), " {
          width: 100% !important;
          height: 105px !important;
        }

        #", ns("panel_downloads"), " {
          top: 62px;
          right: 610px;
          width: 330px;
          background: rgba(255, 255, 255, 0.96);
          color: black;
        }

        #", ns("panel_downloads"), " .panel-header {
          background: rgba(235, 235, 235, 0.98);
        }

        #", ns("panel_downloads"), " .btn,
        #", ns("panel_downloads"), " button,
        #", ns("panel_downloads"), " a {
          width: 100%;
          margin-bottom: 7px;
        }

        #", ns("take_screenshot"), " {
          background: #7b2cbf;
          color: white;
          border: none;
          padding: 7px 11px;
          border-radius: 5px;
          cursor: pointer;
          width: 100%;
          margin-bottom: 7px;
        }

        #", ns("take_screenshot"), ":hover {
          background: #5a189a;
        }

        #", ns("goes_lstf_app"), " table {
          font-size: 12px;
        }

        #", ns("goes_lstf_app"), " .table-section-title {
          margin-top: 12px;
          margin-bottom: 6px;
          font-weight: bold;
        }

        #", ns("goes_lstf_app"), " .shiny-input-container {
          width: 100%;
        }

        #", ns("goes_lstf_app"), " .small-note {
          font-size: 12px;
          color: #555;
        }

        #", ns("goes_lstf_app"), " .stats-source-box {
          background: rgba(245, 245, 245, 0.98);
          border: 1px solid rgba(0, 0, 0, 0.12);
          border-radius: 6px;
          padding: 8px 10px;
          font-size: 13px;
          margin-bottom: 8px;
        }

        #", ns("goes_lstf_app"), " .stats-source-box small {
          color: #444;
        }
      "))),

      tags$script(HTML(paste0("
        function lstf_getEl_", id, "(id) {
          return document.getElementById(id);
        }

        function lstf_togglePanel_", id, "(id) {
          var el = lstf_getEl_", id, "(id);
          if (!el) return;

          if (el.style.display === 'none') {
            el.style.display = 'block';
          } else {
            el.style.display = 'none';
          }
        }

        function lstf_makePanelDraggable_", id, "(panelId) {
          var panel = lstf_getEl_", id, "(panelId);
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

        function lstf_captureMapScreenshot_", id, "() {
          var mapEl = lstf_getEl_", id, "('", ns("map"), "');

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
        }

        function lstf_addOrUpdateOverlay_", id, "(map, message) {
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

          map.lstfUrl = url;
          map.lstfProjection = projection;
          map.lstfBounds = imageBounds;
          map.lstfNrow = message.nrow;
          map.lstfNcol = message.ncol;

          if (!map.getPane('pngPane')) {
            map.createPane('pngPane');
            map.getPane('pngPane').style.zIndex = 450;
          }

          if (!map.getPane('refPane')) {
            map.createPane('refPane');
            map.getPane('refPane').style.zIndex = 650;
          }

          if (map.lstfOverlay) {
            map.removeLayer(map.lstfOverlay);
            map.lstfOverlay = null;
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
            console.log('LSTF cargado:', url);
          });

          overlay.on('error', function(e) {
            console.error('ERROR cargando LSTF:', url, e);
            alert('No se pudo cargar la imagen LSTF.');
          });

          overlay.addTo(map);
          map.lstfOverlay = overlay;
        }

        document.addEventListener('DOMContentLoaded', function() {
          lstf_makePanelDraggable_", id, "('", ns("panel_base"), "');
          lstf_makePanelDraggable_", id, "('", ns("panel_layers"), "');
          lstf_makePanelDraggable_", id, "('", ns("panel_process"), "');
          lstf_makePanelDraggable_", id, "('", ns("panel_stats"), "');
          lstf_makePanelDraggable_", id, "('", ns("panel_scale"), "');
          lstf_makePanelDraggable_", id, "('", ns("panel_downloads"), "');
        });

        Shiny.addCustomMessageHandler('reload-lstf-", id, "', function(message) {
          var widget = HTMLWidgets.find('#", ns("map"), "');

          if (!widget) {
            console.error('No se encontró el widget Leaflet');
            return;
          }

          var map = widget.getMap();

          if (!map) {
            console.error('No se encontró el objeto Leaflet');
            return;
          }

          lstf_addOrUpdateOverlay_", id, "(map, message);
        });

        Shiny.addCustomMessageHandler('toggle-lstf-", id, "', function(message) {
          var widget = HTMLWidgets.find('#", ns("map"), "');

          if (!widget) {
            console.error('No se encontró el widget Leaflet');
            return;
          }

          var map = widget.getMap();

          if (!map) {
            console.error('No se encontró el objeto Leaflet');
            return;
          }

          if (!map.lstfUrl) {
            return;
          }

          lstf_addOrUpdateOverlay_", id, "(map, {
            url: map.lstfUrl,
            visible: message.visible,
            projection: map.lstfProjection,
            nrow: map.lstfNrow,
            ncol: map.lstfNcol
          });
        });

        Shiny.addCustomMessageHandler('update-status-live-", id, "', function(message) {
          var el = lstf_getEl_", id, "('", ns("lstf_status"), "');

          if (el) {
            el.textContent = message.text;
            el.scrollTop = el.scrollHeight;
          }
        });
      ")))
    ),

    div(
      id = ns("goes_lstf_app"),

      actionButton(
        inputId = ns("btn_go_home"),
        label = "← Launcher",
        class = "lstf-home-floating"
      ),

      div(
        id = ns("map_toolbar_left"),
        class = "top-toolbar",

        div(
          id = ns("projection_container"),
          selectInput(
            inputId = ns("map_projection"),
            label = NULL,
            choices = PROJECTION_CHOICES,
            selected = DEFAULT_PROJECTION,
            width = "150px"
          )
        ),

        tags$button(
          class = "toolbar-btn",
          onclick = paste0("lstf_togglePanel_", id, "('", ns("panel_base"), "')"),
          "Fondos"
        ),

        div(
          id = ns("lstf_toggle_container"),
          checkboxInput(
            inputId = ns("show_lstf"),
            label = "LSTF",
            value = TRUE
          )
        ),

        tags$button(
          class = "toolbar-btn",
          onclick = paste0("lstf_togglePanel_", id, "('", ns("panel_layers"), "')"),
          "Capas"
        )
      ),

      div(
        id = ns("map_toolbar_right"),
        class = "top-toolbar",
        tags$button(class = "toolbar-btn", onclick = paste0("lstf_togglePanel_", id, "('", ns("panel_process"), "')"), "Procesamiento"),
        tags$button(class = "toolbar-btn", onclick = paste0("lstf_togglePanel_", id, "('", ns("panel_stats"), "')"), "Estadísticas"),
        tags$button(class = "toolbar-btn", onclick = paste0("lstf_togglePanel_", id, "('", ns("panel_scale"), "')"), "Escala de colores"),
        tags$button(class = "toolbar-btn", onclick = paste0("lstf_togglePanel_", id, "('", ns("panel_downloads"), "')"), "Descargas")
      ),

      div(
        id = ns("panel_base"),
        class = "floating-panel",
        style = "display: none;",
        div(class = "panel-header", "Fondos de mapa"),
        div(
          class = "panel-body",
          radioButtons(
            inputId = ns("base_map"),
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
        id = ns("panel_layers"),
        class = "floating-panel",
        style = "display: none;",
        div(class = "panel-header", "Capas referenciales"),
        div(
          class = "panel-body",
          checkboxGroupInput(
            inputId = ns("reference_layers"),
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
            "Las capas geográficas se muestran encima de LSTF. En GOES original se desactivan."
          )
        )
      ),

      div(
        id = ns("panel_process"),
        class = "floating-panel",
        div(class = "panel-header", "Procesamiento GOES LSTF"),
        div(
          class = "panel-body",
          actionButton(ns("refresh_lstf"), "Descargar y procesar última imagen"),
          br(),
          br(),
          strong("Estado:"),
          verbatimTextOutput(ns("lstf_status"))
        )
      ),

      div(
        id = ns("panel_stats"),
        class = "floating-panel",
        style = "display: none;",
        div(class = "panel-header", "Estadísticas e histograma LST"),
        div(
          class = "panel-body",

          div(
            class = "stats-source-box",
            strong("Archivo NetCDF original:"),
            br(),
            textOutput(ns("stats_source_file")),
            br(),
            tags$small(
              "Las estadísticas son generadas a partir del archivo original .nc, usando la variable LST convertida de Kelvin a Celsius."
            )
          ),

          br(),

          strong("Histograma LST, °C"),
          plotlyOutput(ns("lst_hist"), height = "390px"),

          div(class = "table-section-title", "Información general del archivo NetCDF original"),
          tableOutput(ns("general_info_table")),

          div(class = "table-section-title", "Detalle de píxeles del archivo NetCDF original"),
          tableOutput(ns("pixel_info_table")),

          div(class = "table-section-title", "Medidas de posición, °C"),
          tableOutput(ns("position_stats_table")),

          div(class = "table-section-title", "Medidas de dispersión, °C"),
          tableOutput(ns("dispersion_stats_table"))
        )
      ),

      div(
        id = ns("panel_scale"),
        class = "floating-panel",
        style = "display: none;",
        div(class = "panel-header", "Escala de colores"),
        div(
          class = "panel-body",
          plotlyOutput(ns("color_scale_plot"), height = "105px")
        )
      ),

      div(
        id = ns("panel_downloads"),
        class = "floating-panel",
        style = "display: none;",
        div(class = "panel-header", "Descargas"),
        div(
          class = "panel-body",

          downloadButton(
            outputId = ns("download_png_wgs84"),
            label = "Descargar PNG WGS84"
          ),

          downloadButton(
            outputId = ns("download_png_mercator"),
            label = "Descargar PNG Mercator"
          ),

          downloadButton(
            outputId = ns("download_png_goes"),
            label = "Descargar PNG GOES original"
          ),

          tags$hr(),

          tags$button(
            id = ns("take_screenshot"),
            type = "button",
            onclick = paste0("lstf_captureMapScreenshot_", id, "();"),
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

      leafletOutput(ns("map"))
    )
  )
}


# ============================================================
# 11) MÓDULO SERVER LSTF
# ============================================================

mod_lstf_new_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    current_key <- reactiveVal(NULL)
    current_path <- reactiveVal(NULL)
    last_update <- reactiveVal(NULL)
    last_processed <- reactiveVal(NULL)
    stats_source_file <- reactiveVal(NULL)

    status_text <- reactiveVal(
      "App iniciada.\nMapa listo.\nPresioná el botón para descargar y procesar la última imagen LSTF."
    )

    general_info_data <- reactiveVal(NULL)
    pixel_info_data <- reactiveVal(NULL)
    position_stats_data <- reactiveVal(NULL)
    dispersion_stats_data <- reactiveVal(NULL)
    hist_data <- reactiveVal(NULL)

    selected_lstf_url <- function(projection, processed) {
      if (is.null(processed)) {
        return(NULL)
      }

      if (projection == "goes") {
        return(paste0("lstf_original_goes.png?v=", as.numeric(Sys.time())))
      }

      if (projection == "wgs84") {
        return(paste0("lstf_global_wgs84.png?v=", as.numeric(Sys.time())))
      }

      paste0("lstf_global_3857.png?v=", as.numeric(Sys.time()))
    }

    output$lstf_status <- renderText({
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

    output$stats_source_file <- renderText({
      x <- stats_source_file()

      if (is.null(x)) {
        "Sin archivo procesado todavía."
      } else {
        x
      }
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
          type = paste0("reload-lstf-", id),
          message = list(
            url = selected_lstf_url(projection_now, processed),
            visible = isTRUE(isolate(input$show_lstf)),
            projection = projection_now,
            nrow = processed$original_nrow,
            ncol = processed$original_ncol
          )
        )

      }, once = TRUE)

    }, ignoreInit = TRUE)

    observeEvent(input$show_lstf, {
      session$sendCustomMessage(
        type = paste0("toggle-lstf-", id),
        message = list(
          visible = isTRUE(input$show_lstf)
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
        paste0("lstf_wgs84_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        validate(
          need(file.exists(LST_WGS84_PNG), "Todavía no existe el PNG WGS84. Primero procesá una imagen.")
        )

        file.copy(LST_WGS84_PNG, file, overwrite = TRUE)
      },
      contentType = "image/png"
    )

    output$download_png_mercator <- downloadHandler(
      filename = function() {
        paste0("lstf_mercator_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        validate(
          need(file.exists(LST_3857_PNG), "Todavía no existe el PNG Mercator. Primero procesá una imagen.")
        )

        file.copy(LST_3857_PNG, file, overwrite = TRUE)
      },
      contentType = "image/png"
    )

    output$download_png_goes <- downloadHandler(
      filename = function() {
        paste0("lstf_goes_original_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        validate(
          need(file.exists(LST_ORIGINAL_PNG), "Todavía no existe el PNG GOES original. Primero procesá una imagen.")
        )

        file.copy(LST_ORIGINAL_PNG, file, overwrite = TRUE)
      },
      contentType = "image/png"
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
        base64enc::base64decode(
          what = x,
          output = MAP_SCREENSHOT_PNG
        )
        TRUE
      }, error = function(e) {
        message("Error guardando screenshot: ", e$message)
        FALSE
      })

      if (ok && file.exists(MAP_SCREENSHOT_PNG)) {
        showNotification(
          "Screenshot guardado. Ya podés descargarlo.",
          type = "message",
          duration = 5
        )
      } else {
        showNotification(
          "No se pudo guardar el screenshot.",
          type = "error",
          duration = 6
        )
      }
    })

    output$download_map_screenshot <- downloadHandler(
      filename = function() {
        paste0("map_screenshot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        validate(
          need(file.exists(MAP_SCREENSHOT_PNG), "Primero capturá un screenshot del mapa.")
        )

        file.copy(MAP_SCREENSHOT_PNG, file, overwrite = TRUE)
      },
      contentType = "image/png"
    )

    output$lst_hist <- renderPlotly({
      vals <- hist_data()

      if (is.null(vals) || length(vals) == 0) {
        plot_ly() %>%
          layout(
            title = "Sin datos todavía",
            xaxis = list(
              title = "Temperatura superficial terrestre, °C",
              range = c(HIST_MIN_C, HIST_MAX_C)
            ),
            yaxis = list(title = "Frecuencia")
          )
      } else {
        vals_plot <- vals[is.finite(vals)]
        vals_plot <- vals_plot[
          vals_plot >= HIST_MIN_C &
            vals_plot <= HIST_MAX_C
        ]

        if (length(vals_plot) == 0) {
          plot_ly() %>%
            layout(
              title = "Sin datos dentro del rango -70 a 70 °C",
              xaxis = list(
                title = "Temperatura superficial terrestre, °C",
                range = c(HIST_MIN_C, HIST_MAX_C)
              ),
              yaxis = list(title = "Frecuencia")
            )
        } else {
          breaks <- seq(HIST_MIN_C, HIST_MAX_C, by = 2)

          h <- hist(
            vals_plot,
            breaks = breaks,
            plot = FALSE,
            include.lowest = TRUE,
            right = FALSE
          )

          mids <- h$mids
          counts <- h$counts
          bar_cols <- lst_color_for_values(mids)

          hist_df <- data.frame(
            xmin = h$breaks[-length(h$breaks)],
            xmax = h$breaks[-1],
            mid = mids,
            count = counts,
            color = bar_cols,
            label = paste0(
              "[",
              h$breaks[-length(h$breaks)],
              ", ",
              h$breaks[-1],
              ") °C"
            )
          )

          plot_ly(
            data = hist_df,
            x = ~mid,
            y = ~count,
            type = "bar",
            marker = list(
              color = ~color,
              line = list(color = "rgba(255,255,255,0)", width = 0)
            ),
            width = 2,
            hovertemplate = paste(
              "Intervalo: %{customdata}<br>",
              "Centro: %{x:.1f} °C<br>",
              "Frecuencia: %{y}<extra></extra>"
            ),
            customdata = ~label
          ) %>%
            layout(
              title = "Histograma LST",
              bargap = 0,
              xaxis = list(
                title = "Temperatura superficial terrestre, °C",
                range = c(HIST_MIN_C, HIST_MAX_C),
                tickmode = "array",
                tickvals = seq(-70, 70, by = 10)
              ),
              yaxis = list(
                title = "Frecuencia",
                rangemode = "tozero"
              ),
              shapes = list(
                list(
                  type = "line",
                  x0 = LST_COLOR_MIN_C,
                  x1 = LST_COLOR_MIN_C,
                  y0 = 0,
                  y1 = 1,
                  yref = "paper",
                  line = list(color = "black", width = 2, dash = "dash")
                ),
                list(
                  type = "line",
                  x0 = LST_COLOR_MAX_C,
                  x1 = LST_COLOR_MAX_C,
                  y0 = 0,
                  y1 = 1,
                  yref = "paper",
                  line = list(color = "black", width = 2, dash = "dash")
                )
              ),
              annotations = list(
                list(
                  x = LST_COLOR_MIN_C,
                  y = 1,
                  yref = "paper",
                  text = "-60",
                  showarrow = FALSE,
                  xanchor = "left",
                  yanchor = "top"
                ),
                list(
                  x = LST_COLOR_MAX_C,
                  y = 1,
                  yref = "paper",
                  text = "60",
                  showarrow = FALSE,
                  xanchor = "right",
                  yanchor = "top"
                )
              ),
              margin = list(l = 70, r = 25, b = 70, t = 55)
            ) %>%
            config(
              displayModeBar = TRUE,
              responsive = TRUE
            )
        }
      }
    })

    output$general_info_table <- renderTable({
      x <- general_info_data()

      if (is.null(x)) {
        data.frame(
          Medida = "Sin datos todavía",
          Valor = ""
        )
      } else {
        x
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$pixel_info_table <- renderTable({
      x <- pixel_info_data()

      if (is.null(x)) {
        data.frame(
          Medida = "Sin datos todavía",
          n = "",
          Porcentaje = ""
        )
      } else {
        x
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$position_stats_table <- renderTable({
      x <- position_stats_data()

      if (is.null(x)) {
        data.frame(
          Medida = "Sin datos todavía",
          Valor = ""
        )
      } else {
        x
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$dispersion_stats_table <- renderTable({
      x <- dispersion_stats_data()

      if (is.null(x)) {
        data.frame(
          Medida = "Sin datos todavía",
          Valor = ""
        )
      } else {
        x
      }
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$color_scale_plot <- renderPlotly({
      pal <- lst_palette(256)

      x <- seq(LST_COLOR_MIN_C, LST_COLOR_MAX_C, length.out = 256)
      z <- matrix(x, nrow = 1)

      tick_vals <- seq(LST_COLOR_MIN_C, LST_COLOR_MAX_C, by = 20)

      plot_ly(
        x = x,
        y = c(""),
        z = z,
        type = "heatmap",
        colorscale = plotly_colorscale_from_palette(pal),
        showscale = FALSE,
        hovertemplate = "%{x:.1f} °C<extra></extra>"
      ) %>%
        layout(
          title = NULL,
          autosize = TRUE,
          xaxis = list(
            title = "",
            range = c(LST_COLOR_MIN_C, LST_COLOR_MAX_C),
            tickmode = "array",
            tickvals = tick_vals,
            ticktext = paste0(tick_vals, " °C"),
            side = "top",
            fixedrange = TRUE,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE,
            ticks = "",
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = "",
            showticklabels = FALSE,
            showgrid = FALSE,
            zeroline = FALSE,
            fixedrange = TRUE
          ),
          annotations = list(
            list(
              x = 0.5,
              y = -0.62,
              xref = "paper",
              yref = "paper",
              text = "Temperatura superficial terrestre, °C",
              showarrow = FALSE,
              font = list(size = 12)
            )
          ),
          margin = list(l = 0, r = 0, t = 32, b = 38),
          height = 105
        ) %>%
        config(
          displayModeBar = FALSE,
          responsive = TRUE
        )
    })

    outputOptions(output, "lst_hist", suspendWhenHidden = FALSE)
    outputOptions(output, "general_info_table", suspendWhenHidden = FALSE)
    outputOptions(output, "pixel_info_table", suspendWhenHidden = FALSE)
    outputOptions(output, "position_stats_table", suspendWhenHidden = FALSE)
    outputOptions(output, "dispersion_stats_table", suspendWhenHidden = FALSE)
    outputOptions(output, "color_scale_plot", suspendWhenHidden = FALSE)

    observeEvent(input$refresh_lstf, {

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
          type = paste0("update-status-live-", id),
          message = list(text = nuevo_texto)
        )

        try(session$flushReact(), silent = TRUE)

        message(txt)
      }

      withProgress(message = "Procesando GOES LSTF", value = 0, {

        step <- 0

        progress_status <- function(txt) {
          step <<- min(step + 1, 22)

          incProgress(
            amount = 1 / 22,
            detail = txt
          )

          add_status(txt)
        }

        info <- tryCatch({
          update_lstf_image(
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

        progress_status("Actualizando histograma Plotly y tablas estadísticas...")

        general_info_data(info$processed$general_info)
        pixel_info_data(info$processed$pixel_info)
        position_stats_data(info$processed$position_stats)
        dispersion_stats_data(info$processed$dispersion_stats)
        hist_data(info$processed$hist_values)

        progress_status("Cargando PNG según proyección seleccionada...")

        current_key(info$key)
        current_path(info$path)
        last_update(Sys.time())
        last_processed(info$processed)
        stats_source_file(basename(info$key))

        projection <- input$map_projection

        if (is.null(projection)) {
          projection <- DEFAULT_PROJECTION
        }

        session$sendCustomMessage(
          type = paste0("reload-lstf-", id),
          message = list(
            url = selected_lstf_url(projection, info$processed),
            visible = isTRUE(input$show_lstf),
            projection = projection,
            nrow = info$processed$original_nrow,
            ncol = info$processed$original_ncol
          )
        )

        progress_status("Imagen cargada en el mapa.")

        showNotification(
          paste("Imagen LSTF actualizada:", basename(info$key)),
          type = "message",
          duration = 6
        )
      })
    })
  })
}

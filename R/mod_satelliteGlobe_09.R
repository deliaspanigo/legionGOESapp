library(shiny)
library(lubridate)
library(jsonlite)
library(glue)

if (!exists("fn_my_folder_package")) {
  fn_my_folder_package <- function() getwd()
}

# ============================================================
# CSS configurable
# ============================================================
earth3d_css <- function(
  gis_left = "15px",
  gis_top = "15px",
  sat_left = "395px",
  sat_top = "15px",
  sim_left = "775px",
  sim_top = "15px",
  floaters_left = "1155px",
  floaters_top = "15px",
  floaters_width = "230px",
  panel_width = "365px",
  utc_left = "1155px",
  utc_top = "65px",
  last_update_left = "1155px",
  last_update_top = "108px",
  next_update_left = "1155px",
  next_update_top = "151px",
  orbit_left = "1155px",
  orbit_top = "205px",
  orbit_width = "500px",
  map_left = "1155px",
  map_top = "510px",
  map_width = "500px"
) {
  glue("\n
    .earth3d-module-root {{
      --gis-panel-left: {gis_left};
      --gis-panel-top: {gis_top};
      --sat-panel-left: {sat_left};
      --sat-panel-top: {sat_top};
      --sim-panel-left: {sim_left};
      --sim-panel-top: {sim_top};
      --floaters-panel-left: {floaters_left};
      --floaters-panel-top: {floaters_top};
      --floaters-panel-width: {floaters_width};
      --panel-width: {panel_width};
      position: relative;
      width: 100%;
      height: 100vh;
      min-height: 650px;
      overflow: hidden;
      background: #050816;
      color: white;
      font-family: Arial, sans-serif;
    }}

    .earth3d-module-root .earth3d-container {{ width: 100%; height: 100%; display: block; }}

    .earth3d-module-root .earth-side-panel {{
      position: absolute;
      top: 15px;
      z-index: 10;
      background: rgba(0,0,0,0.76);
      padding: 15px;
      border-radius: 12px;
      width: var(--panel-width);
      max-height: calc(100vh - 30px);
      overflow-y: auto;
      box-shadow: 0 0 18px rgba(0,0,0,0.4);
    }}

    .earth3d-module-root .earth-gis-panel {{ left: var(--gis-panel-left); top: var(--gis-panel-top); }}
    .earth3d-module-root .earth-sat-panel {{ left: var(--sat-panel-left); top: var(--sat-panel-top); width: var(--panel-width); }}
    .earth3d-module-root .earth-sim-panel {{ left: var(--sim-panel-left); right: auto; top: var(--sim-panel-top); width: var(--panel-width); }}

    .earth3d-module-root .earth-floaters-panel {{
      position: absolute;
      left: var(--floaters-panel-left);
      top: var(--floaters-panel-top);
      width: var(--floaters-panel-width);
      z-index: 35;
      background: rgba(0,0,0,0.76);
      padding: 8px;
      border-radius: 12px;
      box-shadow: 0 0 18px rgba(0,0,0,0.4);
    }}

    .earth3d-module-root .menu-toggle-btn {{
      width: 100%;
      background: #ffcc33 !important;
      color: #111 !important;
      border: none !important;
      border-radius: 8px !important;
      font-weight: bold !important;
      font-size: 17px !important;
      margin-bottom: 10px !important;
      text-align: left !important;
    }}
    .earth3d-module-root .menu-toggle-btn:hover {{ background: #ffd966 !important; color: #000 !important; }}
    .earth3d-module-root .menu-toggle-btn::before {{ content: '▾ '; }}
    .earth3d-module-root .menu-toggle-btn.collapsed::before {{ content: '▸ '; }}

    .earth3d-module-root .small-toggle-btn {{
      width: 100%;
      background: #94a3b8 !important;
      color: #111 !important;
      border: none !important;
      border-radius: 8px !important;
      font-weight: bold !important;
      font-size: 13px !important;
      padding: 6px 8px !important;
      margin: 0 !important;
    }}
    .earth3d-module-root .small-toggle-btn:hover {{ background: #cbd5e1 !important; color: #000 !important; }}

    .earth3d-module-root .floating-time-badge {{
      position: absolute;
      z-index: 30;
      transform: none;
      min-width: 330px;
      padding: 9px 13px;
      border-radius: 12px;
      background: rgba(2, 6, 23, 0.82);
      border: 1px solid rgba(255,255,255,0.24);
      color: #e5e7eb;
      font-size: 13px;
      font-weight: bold;
      box-shadow: 0 8px 24px rgba(0,0,0,0.35);
      backdrop-filter: blur(6px);
      cursor: move;
      user-select: none;
      text-align: center;
    }}
    .earth3d-module-root .floating-time-badge .badge-label {{ color: #93c5fd; margin-right: 6px; }}
    .earth3d-module-root .floating-time-badge .badge-value {{ color: #ffcc33; letter-spacing: 0.4px; }}
    .earth3d-module-root .utc-clock-badge {{ left: {utc_left}; top: {utc_top}; }}
    .earth3d-module-root .last-update-badge {{ left: {last_update_left}; top: {last_update_top}; font-size: 12px; min-width: 390px; }}
    .earth3d-module-root .next-update-badge {{ left: {next_update_left}; top: {next_update_top}; font-size: 12px; min-width: 390px; }}

    .earth3d-module-root .floating-visual-window {{
      position: absolute;
      z-index: 25;
      width: 390px;
      background: rgba(2, 6, 23, 0.86);
      border: 1px solid rgba(255,255,255,0.24);
      border-radius: 14px;
      box-shadow: 0 10px 28px rgba(0,0,0,0.42);
      backdrop-filter: blur(7px);
      overflow: hidden;
      user-select: none;
    }}
    .earth3d-module-root .floating-visual-window .window-title {{
      cursor: move;
      padding: 8px 11px;
      background: rgba(15, 23, 42, 0.92);
      border-bottom: 1px solid rgba(255,255,255,0.16);
      color: #ffcc33;
      font-weight: bold;
      font-size: 13px;
    }}
    .earth3d-module-root .floating-visual-window .window-body {{ padding: 8px; }}
    .earth3d-module-root .floating-visual-window canvas {{
      width: 100%;
      height: auto;
      display: block;
      border-radius: 10px;
      background: rgba(15, 23, 42, 0.85);
    }}
    .earth3d-module-root .orbit-window {{ left: {orbit_left}; top: {orbit_top}; width: {orbit_width}; }}
    .earth3d-module-root .map-window {{ left: {map_left}; top: {map_top}; width: {map_width}; }}

    .earth3d-module-root .earth-side-panel h4 {{ margin-top: 14px; margin-bottom: 8px; color: #ffcc33; font-size: 16px; }}
    .earth3d-module-root .earth-side-panel p {{ margin-bottom: 6px; }}
    .earth3d-module-root .value {{ color: #ffcc33; font-weight: bold; }}
    .earth3d-module-root .red {{ color: #ff5555; font-weight: bold; }}
    .earth3d-module-root .white {{ color: #ffffff; font-weight: bold; }}
    .earth3d-module-root .green {{ color: #50fa7b; font-weight: bold; }}
    .earth3d-module-root .blue {{ color: #7aa2ff; font-weight: bold; }}
    .earth3d-module-root .magenta {{ color: #ff79c6; font-weight: bold; }}
    .earth3d-module-root .orange {{ color: #ff9900; font-weight: bold; }}
    .earth3d-module-root label {{ color: white; }}
    .earth3d-module-root input, .earth3d-module-root select {{ color: black; }}
    .earth3d-module-root .form-group {{ margin-bottom: 8px; }}
    .earth3d-module-root .small-note {{ font-size: 12px; opacity: 0.8; }}
    .earth3d-module-root .geo-sat-note {{ font-size: 12px; color: #cbd5e1; line-height: 1.35; margin-top: 8px; }}
    .earth3d-module-root .manual-time-row {{ display: block; }}
    .earth3d-module-root .manual-time-row .form-group {{ margin-bottom: 8px; }}
    .earth3d-module-root .mode-note {{
      margin-top: 6px;
      padding: 8px 10px;
      border-radius: 10px;
      background: rgba(15, 23, 42, 0.72);
      border: 1px solid rgba(148, 163, 184, 0.22);
      color: #cbd5e1;
      font-size: 12px;
      line-height: 1.35;
    }}
  ")
}

# ============================================================
# UI
# ============================================================
mod_satelliteGlobe_09_ui <- function(id) {
  ns <- NS(id)

  ids <- list(
    root = ns("earth3d_module_root"),
    earth3d = ns("earth3d"),
    utc_clock_badge = ns("utc_clock_badge"),
    last_position_update_badge = ns("last_position_update_badge"),
    next_position_update_badge = ns("next_position_update_badge"),
    orbit_window = ns("orbit_window"),
    orbit_canvas = ns("orbit_canvas"),
    map_window = ns("map_window"),
    map_canvas = ns("map_canvas"),
    gis_options = ns("gis_options"),
    toggle_gis = ns("toggle_gis"),
    sat_options = ns("sat_options"),
    toggle_sat = ns("toggle_sat"),
    sim_options = ns("sim_options"),
    toggle_sim = ns("toggle_sim"),
    toggle_floaters = ns("toggle_floaters"),
    time_source_txt = ns("time_source_txt"),
    utc_txt = ns("utc_txt"),
    lat_txt = ns("lat_txt"),
    lon_txt = ns("lon_txt"),
    solar_time_txt = ns("solar_time_txt"),
    subsolar_utc_zone_txt = ns("subsolar_utc_zone_txt"),
    subsolar_clock_txt = ns("subsolar_clock_txt")
  )

  ids_json <- jsonlite::toJSON(ids, auto_unbox = TRUE)
  now_utc <- lubridate::with_tz(Sys.time(), "UTC")

  tagList(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
      tags$style(HTML(earth3d_css()))
    ),

    div(
      id = ns("earth3d_module_root"),
      class = "earth3d-module-root",

      div(
        class = "earth-side-panel earth-gis-panel",
        actionButton(ns("toggle_gis"), "GIS / Cartografía", class = "menu-toggle-btn collapsed"),
        div(
          id = ns("gis_options"),
          style = "display: none;",
          h4("Referencia visual"),
          selectInput(
            ns("reference_axis"),
            "Eje de referencia",
            choices = c(
              "Eje solar / plano de iluminación" = "solar_axis",
              "Eje de la Tierra vertical" = "earth_axis"
            ),
            selected = "solar_axis"
          ),
          tags$div(class = "geo-sat-note", "Si elegís eje terrestre, toda la escena rota para dejar el eje norte-sur vertical."),
          tags$hr(),
          h4("Textura / mapa"),
          selectInput(ns("earth_texture_file"), "Textura base WGS84 solid", choices = character(0)),
          selectInput(ns("earth_overlay_file"), "Capa WGS84 transparente", choices = c("Ninguna" = "")),
          checkboxInput(ns("show_texture"), "Mostrar textura base", value = TRUE),
          checkboxInput(ns("show_overlay"), "Mostrar capa transparente", value = TRUE),
          checkboxInput(ns("show_blue_sphere"), "Mostrar esfera azul base", value = TRUE),
          checkboxInput(ns("show_graticule"), "Mostrar malla meridianos/paralelos", value = FALSE),
          checkboxInput(ns("show_stars"), "Mostrar estrellas", value = TRUE),
          checkboxInput(ns("show_sun_arrow"), "Mostrar dirección solar", value = TRUE),
          checkboxInput(ns("show_subsolar_point"), "Mostrar punto subsolar", value = TRUE),
          checkboxInput(ns("show_earth_axis"), "Mostrar eje terrestre", value = TRUE),
          checkboxInput(ns("show_polar_terminator_axis"), "Eje al plano de traslación", value = TRUE),
          checkboxInput(ns("show_equator"), "Mostrar ecuador destacado", value = TRUE),
          checkboxInput(ns("show_greenwich"), "Mostrar meridiano de Greenwich", value = TRUE),
          tags$hr(),
          h4("Iluminación"),
          sliderInput(ns("night_light"), "Iluminación del lado oscuro", min = 0, max = 100, value = 12, step = 1),
          sliderInput(ns("day_light"), "Intensidad del lado iluminado", min = 0, max = 200, value = 108, step = 1),
          sliderInput(ns("day_gradient"), "Gradiente luz día centro-borde", min = 0, max = 100, value = 35, step = 1),
          tags$hr(),
          h4("Capas geográficas"),
          checkboxInput(ns("show_utc0"), "Mostrar zona UTC±0", value = FALSE),
          checkboxInput(ns("show_subsolar_timezone"), "Resaltar huso UTC del punto subsolar", value = FALSE),
          tags$hr(),
          h4("Elementos físicos"),
          checkboxInput(ns("show_terminator"), "Mostrar terminador", value = FALSE),
          checkboxInput(ns("show_sun_center_line"), "Mostrar haz solar al centro", value = TRUE),
          checkboxInput(ns("show_earth_center"), "Mostrar centro de la Tierra", value = TRUE),
          checkboxInput(ns("show_equator_plane"), "Mostrar plano ecuatorial interno", value = TRUE),
          checkboxInput(ns("show_greenwich_plane"), "Mostrar plano interno de Greenwich", value = TRUE),
          checkboxInput(ns("show_greenwich_equator_vector"), "Mostrar vector centro → Greenwich/Ecuador", value = TRUE),
          checkboxInput(ns("show_solar_equator_projection"), "Mostrar proyección solar sobre el ecuador", value = TRUE),
          checkboxInput(ns("show_solar_meridian"), "Mostrar meridiano de mediodía solar", value = FALSE),
          tags$hr()
        )
      ),

      div(
        class = "earth-side-panel earth-sat-panel",
        actionButton(ns("toggle_sat"), "Satélites GEO", class = "menu-toggle-btn collapsed"),
        div(
          id = ns("sat_options"),
          style = "display: none;",
          h4("Geoestacionarios locales"),
          checkboxInput(ns("show_geo_sats"), "Mostrar sistema GEO", value = TRUE),
          checkboxInput(ns("show_geo_orbits"), "Mostrar órbitas", value = TRUE),
          checkboxInput(ns("show_geo_footprints"), "Mostrar coberturas", value = TRUE),
          checkboxInput(ns("show_geo_cones"), "Mostrar conos", value = TRUE),
          checkboxInput(ns("show_geo_vectors"), "Mostrar líneas Tierra → satélite", value = TRUE),
          checkboxInput(ns("show_geo_labels"), "Mostrar nombres", value = TRUE),
          tags$hr(),
          h4("GOES-18"),
          checkboxInput(ns("show_geo_goes18_enabled"), "Activar GOES-18", value = FALSE),
          checkboxInput(ns("show_geo_goes18_orbit"), "Órbita GOES-18", value = TRUE),
          checkboxInput(ns("show_geo_goes18_footprint"), "Cobertura GOES-18", value = TRUE),
          checkboxInput(ns("show_geo_goes18_cone"), "Cono GOES-18", value = TRUE),
          checkboxInput(ns("show_geo_goes18_vector"), "Línea GOES-18", value = TRUE),
          checkboxInput(ns("show_geo_goes18_label"), "Nombre GOES-18", value = TRUE),
          h4("GOES-19"),
          checkboxInput(ns("show_geo_goes19_enabled"), "Activar GOES-19", value = FALSE),
          checkboxInput(ns("show_geo_goes19_orbit"), "Órbita GOES-19", value = TRUE),
          checkboxInput(ns("show_geo_goes19_footprint"), "Cobertura GOES-19", value = TRUE),
          checkboxInput(ns("show_geo_goes19_cone"), "Cono GOES-19", value = TRUE),
          checkboxInput(ns("show_geo_goes19_vector"), "Línea GOES-19", value = TRUE),
          checkboxInput(ns("show_geo_goes19_label"), "Nombre GOES-19", value = TRUE),
          tags$hr(),
          tags$div(class = "geo-sat-note", "Estos satélites no usan internet. Se dibujan con longitud fija sobre el ecuador. La distancia orbital es proporcional al radio terrestre: 42164 / 6371 ≈ 6.62 radios terrestres."),
          tags$div(class = "geo-sat-note", tags$b("Incluidos:"), tags$br(), "GOES-19 / GOES-East: 75.2°O", tags$br(), "GOES-18 / GOES-West: 137.0°O")
        )
      ),

      div(
        class = "earth-side-panel earth-sim-panel",
        actionButton(ns("toggle_sim"), "Simulación / Procesamiento", class = "menu-toggle-btn collapsed"),
        div(
          id = ns("sim_options"),
          style = "display: none;",
          h4("Modo de trabajo"),
          radioButtons(
            ns("position_mode"),
            "¿Qué querés hacer?",
            choices = c(
              "Seguimiento: usar una fuente de tiempo y recalcular la posición" = "tracking",
              "Simulación: avanzar con tiempo simulado" = "simulation"
            ),
            selected = "tracking"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'tracking'", ns("position_mode")),
            tags$div(class = "mode-note", "Modo seguimiento: la posición subsolar y la orientación de la Tierra se recalculan usando la fuente de tiempo seleccionada.")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'simulation'", ns("position_mode")),
            tags$div(class = "mode-note", "Modo simulación: no se consulta la hora del sistema para actualizar posiciones; se usa un reloj interno simulado controlado con Play, Pausa y Stop.")
          ),
          tags$hr(),
          h4("Fuente de datos / tiempo"),
          radioButtons(
            ns("time_source"),
            "Fuente de tiempo inicial",
            choices = c(
              "Hora UTC del sistema" = "system",
              "Hora UTC web" = "web",
              "Hora UTC manual" = "manual"
            ),
            selected = "system"
          ),
          tags$div(class = "mode-note", "En seguimiento, esta fuente se usa en cada recálculo. En simulación, esta fuente define la hora inicial desde la que empieza a avanzar el reloj simulado."),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("time_source")),
            tags$div(
              class = "manual-time-row",
              numericInput(ns("manual_year"), "Año", value = year(now_utc), min = 1900, max = 2100, step = 1),
              numericInput(ns("manual_month"), "Mes", value = month(now_utc), min = 1, max = 12, step = 1),
              numericInput(ns("manual_day"), "Día", value = day(now_utc), min = 1, max = 31, step = 1),
              numericInput(ns("manual_hour"), "Hora", value = hour(now_utc), min = 0, max = 23, step = 1),
              numericInput(ns("manual_minute"), "Minuto", value = minute(now_utc), min = 0, max = 59, step = 1),
              numericInput(ns("manual_second"), "Segundo", value = second(now_utc), min = 0, max = 59, step = 1)
            )
          ),
          p("Fuente usada: ", span(id = ns("time_source_txt"), class = "value")),
          p("UTC usado: ", span(id = ns("utc_txt"), class = "value")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'tracking'", ns("position_mode")),
            radioButtons(
              ns("refresh_seconds"),
              "Revisión de posición",
              choices = c(
                "Cada 1 segundo" = "1",
                "Cada 5 segundos" = "5",
                "Cada 10 segundos" = "10",
                "Cada 30 segundos" = "30"
              ),
              selected = "5"
            ),
            actionButton(ns("refresh_now"), "Actualizar ahora")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'simulation'", ns("position_mode")),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 6px; margin-bottom: 8px;",
              actionButton(ns("sim_play"), "▶ Play", style = "background:#22c55e;color:white;font-weight:bold;border:0;border-radius:8px;"),
              actionButton(ns("sim_pause"), "⏸ Pausa", style = "background:#f59e0b;color:#111;font-weight:bold;border:0;border-radius:8px;"),
              actionButton(ns("sim_stop"), "⏹ Stop", style = "background:#ef4444;color:white;font-weight:bold;border:0;border-radius:8px;")
            ),
            radioButtons(
              ns("simulation_speed"),
              "Velocidad del tiempo simulado",
              choices = c(
                "1x tiempo real" = "1",
                "60x: 1 minuto simulado por segundo" = "60",
                "600x: 10 minutos simulados por segundo" = "600",
                "3600x: 1 hora simulada por segundo" = "3600"
              ),
              selected = "600"
            )
          ),
          tags$hr(),
          h4("Sol y punto subsolar"),
          p("Latitud subsolar: ", span(id = ns("lat_txt"), class = "value")),
          p("Longitud subsolar: ", span(id = ns("lon_txt"), class = "value")),
          p("Hora solar allí: ", span(id = ns("solar_time_txt"), class = "value")),
          p("Huso UTC ideal: ", span(id = ns("subsolar_utc_zone_txt"), class = "value")),
          p("Hora civil aproximada: ", span(id = ns("subsolar_clock_txt"), class = "value")),
          p("Inclinación terrestre: ", span("23.44°", class = "value")),
          tags$hr(),
          h4("Referencias físicas"),
          p(span("Rojo:", class = "red"), " eje de rotación norte-sur de la Tierra."),
          p(span("Blanco:", class = "white"), " terminador día/noche y centro terrestre."),
          p(span("Verde:", class = "green"), " plano ecuatorial interno."),
          p(span("Azul:", class = "blue"), " plano interno de Greenwich."),
          p(span("Naranja:", class = "orange"), " proyección solar sobre el ecuador."),
          p(span("Amarillo:", class = "value"), " Sol, punto subsolar, haz solar y mediodía solar.")
        )
      ),

      div(
        class = "earth-floaters-panel",
        actionButton(ns("toggle_floaters"), "Ocultar objetos móviles", class = "small-toggle-btn")
      ),

      div(id = ns("utc_clock_badge"), class = "floating-time-badge utc-clock-badge", span(class = "badge-label", "UTC"), span(class = "badge-value", "--:--:--")),
      div(id = ns("last_position_update_badge"), class = "floating-time-badge last-update-badge", span(class = "badge-label", "Última actualización posición"), span(class = "badge-value", "--")),
      div(id = ns("next_position_update_badge"), class = "floating-time-badge next-update-badge", span(class = "badge-label", "Próxima actualización en"), span(class = "badge-value", "--")),

      div(id = ns("orbit_window"), class = "floating-visual-window orbit-window", div(class = "window-title", "Órbita anual Tierra-Sol"), div(class = "window-body", tags$canvas(id = ns("orbit_canvas"), width = 760, height = 430))),
      div(id = ns("map_window"), class = "floating-visual-window map-window", div(class = "window-title", "Mapa WGS84 día / noche"), div(class = "window-body", tags$canvas(id = ns("map_canvas"), width = 920, height = 460))),
      div(id = ns("earth3d"), class = "earth3d-container")
    ),
    tags$script(HTML(earth3d_js(ids_json, ns)))
  )
}

# ============================================================
# JavaScript
# ============================================================
earth3d_js <- function(ids_json, ns) {
  paste0("\n(function() {\n  const ids = ", ids_json, ";\n\n  let scene, camera, renderer, controls;\n  let earthMesh, earthMaterial, overlayMesh, overlayMaterial, starField;\n  let terminatorLine, subsolarMarker, sunArrow, sunCenterLine, earthCenterMarker;\n  let equatorLine, greenwichLine, utcZoneWestLine, utcZoneEastLine, utcZoneSurface, subsolarTimeZoneSurface;\n  let equatorPlaneMesh, greenwichPlaneMesh, greenwichEquatorVector, solarEquatorProjectionVector;\n  let earthAxisLine, earthAxisLabelN, earthAxisLabelS, polarTerminatorAxisLine;\n  let solarMeridianLine, solarNoonSemiCircle;\n  let graticuleLines = [];\n  let geoSatObjects = [];\n\n  let initialized = false;\n  let movableObjectsVisible = false;\n  let currentEarthAxis = new THREE.Vector3(0, 1, 0);\n  let referenceAxisMode = 'solar_axis';\n  let targetSceneQuaternion = new THREE.Quaternion();\n  let currentSubsolarUtcOffset = null;\n  let nightLightFactor = 0.12;\n  let dayLightFactor = 1.08;\n  let dayGradientFactor = 0.35;\n\n  let simulationMode = 'system';\n  let lastPositionUpdateDate = null;\n  let lastPositionRefreshSeconds = 5;\n  let nextPositionUpdateDate = null;\n  let currentTimeSourceCode = 'system';\n  let lastLivePositionBucket = null;\n  let simulationClockDate = null;\n  let simulationLastRealMs = null;\n  let simulationTimeScale = 600;\n  let lastKnownPositionDate = null;\n\n  const R = 1.0;\n  const internalVectorLength = 0.88;\n  const fixedSunDir = new THREE.Vector3(1, 0, 0).normalize();\n  const GEO_TRUE_RADIUS_RATIO = 42164 / 6371;\n  const GEO_VISUAL_RADIUS = GEO_TRUE_RADIUS_RATIO;\n\n  const geoSatellites = [\n    { key: 'goes19', name: 'GOES-19 / GOES-East', lon: -75.2, color: 0xfacc15 },\n    { key: 'goes18', name: 'GOES-18 / GOES-West', lon: -137.0, color: 0xf472b6 }\n  ];\n\n  let layerVisibility = {\n    show_equator: true, show_greenwich: true, show_graticule: false, show_utc0: false, show_subsolar_timezone: false,\n    show_texture: true, show_blue_sphere: true, show_overlay: true, show_stars: true, show_terminator: false,\n    show_sun_arrow: true, show_subsolar_point: true, show_sun_center_line: true, show_earth_center: true,\n    show_equator_plane: true, show_greenwich_plane: true, show_greenwich_equator_vector: true, show_solar_equator_projection: true,\n    show_solar_meridian: false, show_earth_axis: true, show_polar_terminator_axis: true,\n    show_geo_sats: true, show_geo_orbits: true, show_geo_footprints: true, show_geo_cones: true, show_geo_vectors: true, show_geo_labels: true,\n    show_geo_goes18_enabled: false, show_geo_goes18_orbit: true, show_geo_goes18_footprint: true, show_geo_goes18_cone: true, show_geo_goes18_vector: true, show_geo_goes18_label: true,\n    show_geo_goes19_enabled: false, show_geo_goes19_orbit: true, show_geo_goes19_footprint: true, show_geo_goes19_cone: true, show_geo_goes19_vector: true, show_geo_goes19_label: true\n  };\n\n  function setupToggleButton(buttonId, optionsId) {\n    const btn = document.getElementById(buttonId);\n    const options = document.getElementById(optionsId);\n    if (!btn || !options) return;\n    options.style.display = 'none';\n    btn.classList.add('collapsed');\n    btn.addEventListener('click', function() {\n      if (options.style.display === 'none') {\n        options.style.display = 'block';\n        btn.classList.remove('collapsed');\n      } else {\n        options.style.display = 'none';\n        btn.classList.add('collapsed');\n      }\n    });\n  }\n\n  function setMovableObjectsVisible(visible) {\n    movableObjectsVisible = !!visible;\n    const movableIds = [ids.utc_clock_badge, ids.last_position_update_badge, ids.next_position_update_badge, ids.orbit_window, ids.map_window];\n    for (let i = 0; i < movableIds.length; i++) {\n      const el = document.getElementById(movableIds[i]);\n      if (el) el.style.display = movableObjectsVisible ? '' : 'none';\n    }\n    const btn = document.getElementById(ids.toggle_floaters);\n    if (btn) btn.innerText = movableObjectsVisible ? 'Ocultar objetos móviles' : 'Mostrar objetos móviles';\n  }\n\n  function toggleMovableObjects() { setMovableObjectsVisible(!movableObjectsVisible); }\n\n  function normalizeLon(lon) { while (lon < -180) lon += 360; while (lon > 180) lon -= 360; return lon; }\n  function normalizeDegrees(angle) { let out = angle % 360; if (out < 0) out += 360; return out; }\n\n  function formatUtcDateTime(date) {\n    const yyyy = date.getUTCFullYear();\n    const mm = String(date.getUTCMonth() + 1).padStart(2, '0');\n    const dd = String(date.getUTCDate()).padStart(2, '0');\n    const hh = String(date.getUTCHours()).padStart(2, '0');\n    const mi = String(date.getUTCMinutes()).padStart(2, '0');\n    const ss = String(date.getUTCSeconds()).padStart(2, '0');\n    return yyyy + '-' + mm + '-' + dd + ' ' + hh + ':' + mi + ':' + ss + ' UTC';\n  }\n\n  function parseUtcDateTime(txt) {\n    if (!txt || typeof txt !== 'string') return null;\n    const clean = txt.replace(' UTC', '').replace('T', ' ').trim();\n    const m = clean.match(/^([0-9]{4})-([0-9]{2})-([0-9]{2})[ ]+([0-9]{2}):([0-9]{2}):([0-9]{2})/);\n    if (!m) return null;\n    return new Date(Date.UTC(parseInt(m[1], 10), parseInt(m[2], 10) - 1, parseInt(m[3], 10), parseInt(m[4], 10), parseInt(m[5], 10), parseInt(m[6], 10)));\n  }\n\n  function updateFloatingUtcClock() {\n    const badge = document.getElementById(ids.utc_clock_badge); if (!badge) return;\n    const value = badge.querySelector('.badge-value'); if (value) value.innerText = formatUtcDateTime(new Date());\n  }\n  function updateLastPositionUpdateBadge(dateObj) {\n    const badge = document.getElementById(ids.last_position_update_badge); if (!badge) return;\n    const value = badge.querySelector('.badge-value'); if (value) value.innerText = dateObj ? formatUtcDateTime(dateObj) : '--';\n  }\n  function updateNextPositionUpdateBadge() {\n    const badge = document.getElementById(ids.next_position_update_badge); if (!badge) return;\n    const value = badge.querySelector('.badge-value'); if (!value) return;\n    if (simulationMode === 'running') { value.innerText = 'simulación en curso'; return; }\n    if (simulationMode === 'paused') { value.innerText = 'simulación pausada'; return; }\n    if (!nextPositionUpdateDate) { value.innerText = '--'; return; }\n    const remainingSec = Math.max(0, Math.ceil((nextPositionUpdateDate.getTime() - Date.now()) / 1000));\n    value.innerText = remainingSec + ' s';\n  }\n  function resetNextPositionUpdateCountdown(refreshSeconds) {\n    lastPositionRefreshSeconds = refreshSeconds || lastPositionRefreshSeconds || 5;\n    nextPositionUpdateDate = new Date(Date.now() + lastPositionRefreshSeconds * 1000);\n    updateNextPositionUpdateBadge();\n  }\n\n  function latLonToVector3(latDeg, lonDeg, radius) {\n    radius = radius || 1;\n    const lat = latDeg * Math.PI / 180;\n    const lon = lonDeg * Math.PI / 180;\n    const x = radius * Math.cos(lat) * Math.cos(lon);\n    const y = radius * Math.sin(lat);\n    const z = -radius * Math.cos(lat) * Math.sin(lon);\n    return new THREE.Vector3(x, y, z).normalize().multiplyScalar(radius);\n  }\n\n  function makeLine(p1, p2, color, opacity, depthTest) {\n    opacity = opacity === undefined ? 1 : opacity; depthTest = depthTest === undefined ? true : depthTest;\n    const geometry = new THREE.BufferGeometry().setFromPoints([p1, p2]);\n    const material = new THREE.LineBasicMaterial({ color: color, transparent: true, opacity: opacity, depthTest: depthTest, depthWrite: false });\n    return new THREE.Line(geometry, material);\n  }\n  function makePolyline(points, color, opacity, depthTest) {\n    opacity = opacity === undefined ? 1 : opacity; depthTest = depthTest === undefined ? true : depthTest;\n    const geometry = new THREE.BufferGeometry().setFromPoints(points);\n    const material = new THREE.LineBasicMaterial({ color: color, transparent: true, opacity: opacity, depthTest: depthTest, depthWrite: false });\n    return new THREE.Line(geometry, material);\n  }\n  function removeObject(obj) {\n    if (!obj || !scene) return;\n    scene.remove(obj);\n    if (obj.geometry) obj.geometry.dispose();\n    if (obj.material) obj.material.dispose();\n  }\n  function setArrowDepth(arrow, depthTest) {\n    if (!arrow) return;\n    if (arrow.line && arrow.line.material) { arrow.line.material.depthTest = depthTest; arrow.line.material.depthWrite = false; arrow.line.material.transparent = true; }\n    if (arrow.cone && arrow.cone.material) { arrow.cone.material.depthTest = depthTest; arrow.cone.material.depthWrite = false; arrow.cone.material.transparent = true; }\n  }\n  function removeArrowHelper(arrow) {\n    if (!arrow || !scene) return; scene.remove(arrow);\n    if (arrow.line) { if (arrow.line.geometry) arrow.line.geometry.dispose(); if (arrow.line.material) arrow.line.material.dispose(); }\n    if (arrow.cone) { if (arrow.cone.geometry) arrow.cone.geometry.dispose(); if (arrow.cone.material) arrow.cone.material.dispose(); }\n  }\n\n  function makeTextSprite(text, color, fontSize) {\n    color = color || '#ffffff'; fontSize = fontSize || 72;\n    const canvas = document.createElement('canvas');\n    const context = canvas.getContext('2d');\n    canvas.width = 512; canvas.height = 256;\n    context.clearRect(0, 0, canvas.width, canvas.height);\n    context.font = 'bold ' + fontSize + 'px Arial';\n    context.textAlign = 'center'; context.textBaseline = 'middle'; context.lineWidth = 9;\n    context.strokeStyle = 'rgba(0, 0, 0, 0.90)'; context.strokeText(text, canvas.width / 2, canvas.height / 2);\n    context.fillStyle = color; context.fillText(text, canvas.width / 2, canvas.height / 2);\n    const texture = new THREE.CanvasTexture(canvas); texture.needsUpdate = true;\n    const material = new THREE.SpriteMaterial({ map: texture, transparent: true, depthTest: false, depthWrite: false });\n    const sprite = new THREE.Sprite(material); sprite.scale.set(0.16, 0.08, 0.16); sprite.renderOrder = 3000; return sprite;\n  }\n  function removeSprite(sprite) {\n    if (!sprite || !scene) return; scene.remove(sprite);\n    if (sprite.material) { if (sprite.material.map) sprite.material.map.dispose(); sprite.material.dispose(); }\n  }\n\n  function makeLatLonLine(lat1, lat2, lon, radius, color, opacity) {\n    const points = [];\n    for (let lat = lat1; lat <= lat2; lat += 2) { const p = latLonToVector3(lat, lon, radius); p.applyQuaternion(earthMesh.quaternion); points.push(p); }\n    return makePolyline(points, color, opacity);\n  }\n  function getEarthAxisFromSubsolarLatitude(latDeg) { const delta = latDeg * Math.PI / 180; return new THREE.Vector3(Math.sin(delta), Math.cos(delta), 0).normalize(); }\n\n  function quaternionFromTwoBases(localA, localB, localC, worldA, worldB, worldC) {\n    const mLocal = new THREE.Matrix4(); const mWorld = new THREE.Matrix4();\n    mLocal.makeBasis(localA, localB, localC); mWorld.makeBasis(worldA, worldB, worldC);\n    const qLocal = new THREE.Quaternion().setFromRotationMatrix(mLocal);\n    const qWorld = new THREE.Quaternion().setFromRotationMatrix(mWorld);\n    return qWorld.multiply(qLocal.invert());\n  }\n  function updateReferenceAxisMode() {\n    if (!scene) return;\n    if (referenceAxisMode === 'earth_axis') targetSceneQuaternion.setFromUnitVectors(currentEarthAxis.clone().normalize(), new THREE.Vector3(0, 1, 0));\n    else targetSceneQuaternion.identity();\n  }\n  function updateReferenceAxisTransition() { if (!scene || !targetSceneQuaternion) return; scene.quaternion.slerp(targetSceneQuaternion, 0.08); }\n\n  function orientEarthToSubsolar(lat, lon) {\n    if (!earthMesh) return;\n    const localSubsolar = latLonToVector3(lat, lon, 1.0).normalize();\n    const localNorthAxis = new THREE.Vector3(0, 1, 0);\n    currentEarthAxis = getEarthAxisFromSubsolarLatitude(lat);\n    const localA = localSubsolar.clone();\n    let localB = localNorthAxis.clone().sub(localA.clone().multiplyScalar(localNorthAxis.dot(localA)));\n    if (localB.length() < 0.001) localB = new THREE.Vector3(0, 0, 1); else localB.normalize();\n    const localC = new THREE.Vector3().crossVectors(localA, localB).normalize();\n    const worldA = fixedSunDir.clone().normalize();\n    let worldB = currentEarthAxis.clone().sub(worldA.clone().multiplyScalar(currentEarthAxis.dot(worldA)));\n    if (worldB.length() < 0.001) worldB = new THREE.Vector3(0, 1, 0); else worldB.normalize();\n    const worldC = new THREE.Vector3().crossVectors(worldA, worldB).normalize();\n    const q = quaternionFromTwoBases(localA, localB, localC, worldA, worldB, worldC);\n    earthMesh.quaternion.copy(q); if (overlayMesh) overlayMesh.quaternion.copy(q);\n    updateGeographicLines(); updateGraticule(); updateAxes(); updateSolarMeridian(); updateEquatorPlane(); updateGreenwichPlane(); updateGreenwichEquatorVector(); updateSolarEquatorProjectionVector(); createGeoSatellites(); updateReferenceAxisMode(); applyLayerVisibility();\n  }\n\n  function createLonBandSurface(lonWest, lonEast, radius, color, opacity) {\n    const vertices = []; const indices = []; const latStep = 5; const lonStep = 2.5; const lats = []; const lons = [];\n    for (let lat = -90; lat <= 90; lat += latStep) lats.push(lat);\n    for (let lon = lonWest; lon <= lonEast; lon += lonStep) lons.push(lon);\n    if (lons[lons.length - 1] < lonEast) lons.push(lonEast);\n    for (let i = 0; i < lats.length; i++) { for (let j = 0; j < lons.length; j++) { const p = latLonToVector3(lats[i], normalizeLon(lons[j]), radius); p.applyQuaternion(earthMesh.quaternion); vertices.push(p.x, p.y, p.z); } }\n    const cols = lons.length;\n    for (let i = 0; i < lats.length - 1; i++) { for (let j = 0; j < lons.length - 1; j++) { const a = i * cols + j; const b = a + 1; const c = a + cols; const d = c + 1; indices.push(a, c, b); indices.push(b, c, d); } }\n    const geometry = new THREE.BufferGeometry(); geometry.setAttribute('position', new THREE.Float32BufferAttribute(vertices, 3)); geometry.setIndex(indices); geometry.computeVertexNormals();\n    const material = new THREE.MeshBasicMaterial({ color: color, transparent: true, opacity: opacity, side: THREE.DoubleSide, depthWrite: false });\n    return new THREE.Mesh(geometry, material);\n  }\n  function clearSubsolarTimeZoneSurface() { removeObject(subsolarTimeZoneSurface); subsolarTimeZoneSurface = null; }\n  function createSubsolarUTCZoneSurface(subsolarUtcOffset) {\n    clearSubsolarTimeZoneSurface();\n    let centerLon = subsolarUtcOffset * 15; let lonWest = Math.max(-180, centerLon - 7.5); let lonEast = Math.min(180, centerLon + 7.5);\n    subsolarTimeZoneSurface = createLonBandSurface(lonWest, lonEast, 1.027, 0xffcc00, 0.26); scene.add(subsolarTimeZoneSurface); applyLayerVisibility();\n  }\n  function createUTCZoneSurface() { removeObject(utcZoneSurface); utcZoneSurface = createLonBandSurface(-7.5, 7.5, 1.023, 0x3399ff, 0.18); scene.add(utcZoneSurface); }\n  function updateGeographicLines() {\n    if (!earthMesh) return;\n    removeObject(equatorLine); removeObject(greenwichLine); removeObject(utcZoneWestLine); removeObject(utcZoneEastLine); removeObject(utcZoneSurface);\n    const equatorPoints = [];\n    for (let lon = -180; lon < 180; lon += 2) { const p = latLonToVector3(0, lon, 1.013); p.applyQuaternion(earthMesh.quaternion); equatorPoints.push(p); }\n    equatorPoints.push(equatorPoints[0].clone()); equatorLine = makePolyline(equatorPoints, 0x50fa7b, 1.0); scene.add(equatorLine);\n    greenwichLine = makeLatLonLine(-90, 90, 0, 1.017, 0x7aa2ff, 1.0); scene.add(greenwichLine);\n    utcZoneWestLine = makeLatLonLine(-90, 90, -7.5, 1.018, 0x3399ff, 0.85); scene.add(utcZoneWestLine);\n    utcZoneEastLine = makeLatLonLine(-90, 90, 7.5, 1.018, 0x3399ff, 0.85); scene.add(utcZoneEastLine);\n    createUTCZoneSurface(); if (currentSubsolarUtcOffset !== null) createSubsolarUTCZoneSurface(currentSubsolarUtcOffset); applyLayerVisibility();\n  }\n  function clearGraticule() { for (let i = 0; i < graticuleLines.length; i++) removeObject(graticuleLines[i]); graticuleLines = []; }\n  function updateGraticule() {\n    if (!earthMesh) return; clearGraticule(); const radius = 1.026;\n    for (let lat = -75; lat <= 75; lat += 15) { const points = []; for (let lon = -180; lon < 180; lon += 2) { const p = latLonToVector3(lat, lon, radius); p.applyQuaternion(earthMesh.quaternion); points.push(p); } if (points.length > 0) points.push(points[0].clone()); const line = makePolyline(points, 0xaaaaaa, 0.38); graticuleLines.push(line); scene.add(line); }\n    for (let lon = -180; lon < 180; lon += 15) { const points = []; for (let lat = -90; lat <= 90; lat += 2) { const p = latLonToVector3(lat, lon, radius); p.applyQuaternion(earthMesh.quaternion); points.push(p); } const line = makePolyline(points, 0xaaaaaa, 0.38); graticuleLines.push(line); scene.add(line); }\n    applyLayerVisibility();\n  }\n  function createTerminator() {\n    removeObject(terminatorLine); const normal = fixedSunDir.clone().normalize(); let helper = new THREE.Vector3(0, 1, 0); if (Math.abs(normal.dot(helper)) > 0.95) helper = new THREE.Vector3(0, 0, 1);\n    const u = new THREE.Vector3().crossVectors(normal, helper).normalize(); const v = new THREE.Vector3().crossVectors(normal, u).normalize(); const points = [];\n    for (let i = 0; i < 360; i++) { const a = i * Math.PI / 180; points.push(new THREE.Vector3().addScaledVector(u, Math.cos(a) * 1.009).addScaledVector(v, Math.sin(a) * 1.009)); }\n    points.push(points[0].clone()); terminatorLine = makePolyline(points, 0xffffff, 0.95); scene.add(terminatorLine); applyLayerVisibility();\n  }\n  function updateAxes() {\n    if (!earthMesh) return; removeObject(earthAxisLine); removeObject(polarTerminatorAxisLine); removeSprite(earthAxisLabelN); removeSprite(earthAxisLabelS);\n    const earthAxis = currentEarthAxis.clone().normalize(); earthAxisLine = makeLine(earthAxis.clone().multiplyScalar(-1.50), earthAxis.clone().multiplyScalar(1.50), 0xff3333, 1.0); scene.add(earthAxisLine);\n    earthAxisLabelN = makeTextSprite('N', '#ff5555', 84); earthAxisLabelS = makeTextSprite('S', '#ff5555', 84);\n    earthAxisLabelN.position.copy(earthAxis.clone().multiplyScalar(1.63)); earthAxisLabelS.position.copy(earthAxis.clone().multiplyScalar(-1.63)); scene.add(earthAxisLabelN); scene.add(earthAxisLabelS);\n    let polarTermAxis = earthAxis.clone().sub(fixedSunDir.clone().multiplyScalar(earthAxis.dot(fixedSunDir))); if (polarTermAxis.length() < 0.001) polarTermAxis = new THREE.Vector3(0, 0, 1); else polarTermAxis.normalize();\n    polarTerminatorAxisLine = makeLine(polarTermAxis.clone().multiplyScalar(-1.44), polarTermAxis.clone().multiplyScalar(1.44), 0xff79c6, 1.0); scene.add(polarTerminatorAxisLine); applyLayerVisibility();\n  }\n  function updateSolarMeridian() {\n    if (!earthMesh) return; removeObject(solarMeridianLine); removeObject(solarNoonSemiCircle);\n    const earthAxis = currentEarthAxis.clone().normalize(); const meridianNormal = new THREE.Vector3().crossVectors(earthAxis, fixedSunDir).normalize(); if (meridianNormal.length() < 0.001) return;\n    const u = fixedSunDir.clone().normalize(); const v = new THREE.Vector3().crossVectors(meridianNormal, u).normalize(); const fullPoints = [];\n    for (let i = 0; i < 360; i++) { const a = i * Math.PI / 180; fullPoints.push(new THREE.Vector3().addScaledVector(u, Math.cos(a) * 1.023).addScaledVector(v, Math.sin(a) * 1.023)); }\n    fullPoints.push(fullPoints[0].clone()); solarMeridianLine = makePolyline(fullPoints, 0xffcc00, 0.72); scene.add(solarMeridianLine);\n    const semiPoints = []; for (let i = -90; i <= 90; i++) { const a = i * Math.PI / 180; semiPoints.push(new THREE.Vector3().addScaledVector(u, Math.cos(a) * 1.045).addScaledVector(v, Math.sin(a) * 1.045)); }\n    solarNoonSemiCircle = makePolyline(semiPoints, 0xffcc00, 1.0); scene.add(solarNoonSemiCircle); applyLayerVisibility();\n  }\n  function createSubsolarMarker() { removeObject(subsolarMarker); subsolarMarker = new THREE.Mesh(new THREE.SphereGeometry(0.038, 32, 32), new THREE.MeshBasicMaterial({ color: 0xffcc00, transparent: true, opacity: 1, depthTest: true, depthWrite: false })); subsolarMarker.position.copy(fixedSunDir.clone().multiplyScalar(1.075)); subsolarMarker.renderOrder = 1800; scene.add(subsolarMarker); applyLayerVisibility(); }\n  function createEarthCenterMarker() { removeObject(earthCenterMarker); earthCenterMarker = new THREE.Mesh(new THREE.SphereGeometry(0.028, 32, 32), new THREE.MeshBasicMaterial({ color: 0xffffff, transparent: true, opacity: 1, depthTest: true, depthWrite: false })); earthCenterMarker.position.set(0, 0, 0); scene.add(earthCenterMarker); applyLayerVisibility(); }\n  function createSunCenterLine() { removeObject(sunCenterLine); sunCenterLine = makeLine(fixedSunDir.clone().multiplyScalar(1), new THREE.Vector3(0, 0, 0), 0xffcc00, 1, true); scene.add(sunCenterLine); applyLayerVisibility(); }\n  function updateEquatorPlane() { removeObject(equatorPlaneMesh); if (!earthMesh) return; equatorPlaneMesh = new THREE.Mesh(new THREE.CircleGeometry(0.995, 160), new THREE.MeshBasicMaterial({ color: 0x50fa7b, transparent: true, opacity: 0.18, side: THREE.DoubleSide, depthTest: true, depthWrite: false })); const qLocal = new THREE.Quaternion(); qLocal.setFromAxisAngle(new THREE.Vector3(1, 0, 0), Math.PI / 2); equatorPlaneMesh.quaternion.copy(earthMesh.quaternion).multiply(qLocal); scene.add(equatorPlaneMesh); applyLayerVisibility(); }\n  function updateGreenwichPlane() { removeObject(greenwichPlaneMesh); if (!earthMesh) return; greenwichPlaneMesh = new THREE.Mesh(new THREE.CircleGeometry(0.992, 160), new THREE.MeshBasicMaterial({ color: 0x7aa2ff, transparent: true, opacity: 0.16, side: THREE.DoubleSide, depthTest: true, depthWrite: false })); greenwichPlaneMesh.quaternion.copy(earthMesh.quaternion); scene.add(greenwichPlaneMesh); applyLayerVisibility(); }\n  function updateGreenwichEquatorVector() { removeArrowHelper(greenwichEquatorVector); greenwichEquatorVector = null; if (!earthMesh) return; const end = latLonToVector3(0, 0, internalVectorLength); end.applyQuaternion(earthMesh.quaternion); greenwichEquatorVector = new THREE.ArrowHelper(end.clone().normalize(), new THREE.Vector3(0, 0, 0), internalVectorLength, 0x7aa2ff, 0.08, 0.045); setArrowDepth(greenwichEquatorVector, true); scene.add(greenwichEquatorVector); applyLayerVisibility(); }\n  function updateSolarEquatorProjectionVector() { removeArrowHelper(solarEquatorProjectionVector); solarEquatorProjectionVector = null; const earthAxis = currentEarthAxis.clone().normalize(); let proj = fixedSunDir.clone().sub(earthAxis.clone().multiplyScalar(fixedSunDir.dot(earthAxis))); if (proj.length() < 0.0001) return; proj.normalize(); solarEquatorProjectionVector = new THREE.ArrowHelper(proj, new THREE.Vector3(0, 0, 0), internalVectorLength, 0xff9900, 0.08, 0.045); setArrowDepth(solarEquatorProjectionVector, true); scene.add(solarEquatorProjectionVector); applyLayerVisibility(); }\n  function createSunArrow() { if (sunArrow) scene.remove(sunArrow); sunArrow = new THREE.ArrowHelper(fixedSunDir.clone().multiplyScalar(-1), fixedSunDir.clone().multiplyScalar(2.5), 0.85, 0xffcc00, 0.16, 0.08); setArrowDepth(sunArrow, true); scene.add(sunArrow); applyLayerVisibility(); }\n\n  function disposeGeoObject(obj) { if (!obj || !scene) return; scene.remove(obj); obj.traverse(function(child) { if (child.geometry) child.geometry.dispose(); if (child.material) { if (Array.isArray(child.material)) child.material.forEach(function(mat) { if (mat.map) mat.map.dispose(); mat.dispose(); }); else { if (child.material.map) child.material.map.dispose(); child.material.dispose(); } } }); }\n  function destinationPoint(lat, lon, bearingDeg, angularDistanceRad) { const lat1 = lat * Math.PI / 180; const lon1 = lon * Math.PI / 180; const brng = bearingDeg * Math.PI / 180; const d = angularDistanceRad; const lat2 = Math.asin(Math.sin(lat1) * Math.cos(d) + Math.cos(lat1) * Math.sin(d) * Math.cos(brng)); const lon2 = lon1 + Math.atan2(Math.sin(brng) * Math.sin(d) * Math.cos(lat1), Math.cos(d) - Math.sin(lat1) * Math.sin(lat2)); return { lat: lat2 * 180 / Math.PI, lon: normalizeLon(lon2 * 180 / Math.PI) }; }\n  function clearGeoSatellites() { for (let i = 0; i < geoSatObjects.length; i++) { const obj = geoSatObjects[i]; disposeGeoObject(obj.marker); disposeGeoObject(obj.orbit); disposeGeoObject(obj.footprint); disposeGeoObject(obj.vectorLine); disposeGeoObject(obj.cone); removeSprite(obj.label); } geoSatObjects = []; }\n  function createGeoOrbit(color) { const points = []; for (let lon = -180; lon <= 180; lon += 2) { const p = latLonToVector3(0, lon, GEO_VISUAL_RADIUS); p.applyQuaternion(earthMesh.quaternion); points.push(p); } points.push(points[0].clone()); return makePolyline(points, color, 0.42, true); }\n  function getGeoFootprintWorldPoints(lon) { const points = []; const surfaceRadius = 1.034; const angularRadius = Math.acos(1 / GEO_TRUE_RADIUS_RATIO); for (let bearing = 0; bearing <= 360; bearing += 3) { const pLL = destinationPoint(0, lon, bearing, angularRadius); const p = latLonToVector3(pLL.lat, pLL.lon, surfaceRadius); p.applyQuaternion(earthMesh.quaternion); points.push(p); } return points; }\n  function createGeoFootprint(lon, color) { return makePolyline(getGeoFootprintWorldPoints(lon), color, 0.9, true); }\n  function createGeoCone(lon, color, satPoint) { const ringPoints = getGeoFootprintWorldPoints(lon); const positions = [satPoint.x, satPoint.y, satPoint.z]; const indices = []; for (let i = 0; i < ringPoints.length; i++) positions.push(ringPoints[i].x, ringPoints[i].y, ringPoints[i].z); for (let i = 1; i < ringPoints.length; i++) { const next = i === ringPoints.length - 1 ? 1 : i + 1; indices.push(0, i, next); } const geometry = new THREE.BufferGeometry(); geometry.setAttribute('position', new THREE.Float32BufferAttribute(positions, 3)); geometry.setIndex(indices); geometry.computeVertexNormals(); const material = new THREE.MeshBasicMaterial({ color: color, transparent: true, opacity: 0.10, side: THREE.DoubleSide, depthWrite: false }); return new THREE.Mesh(geometry, material); }\n  function createGeoSatelliteMarker(color) { const group = new THREE.Group(); const body = new THREE.Mesh(new THREE.BoxGeometry(0.10, 0.07, 0.07), new THREE.MeshBasicMaterial({ color: color, transparent: true, opacity: 1, depthWrite: false })); const panelMaterial = new THREE.MeshBasicMaterial({ color: 0x1e3a8a, transparent: true, opacity: 0.95, depthWrite: false }); const panel1 = new THREE.Mesh(new THREE.BoxGeometry(0.16, 0.012, 0.07), panelMaterial); panel1.position.x = -0.14; const panel2 = new THREE.Mesh(new THREE.BoxGeometry(0.16, 0.012, 0.07), panelMaterial); panel2.position.x = 0.14; const halo = new THREE.Mesh(new THREE.SphereGeometry(0.055, 24, 24), new THREE.MeshBasicMaterial({ color: color, transparent: true, opacity: 0.65, depthWrite: false })); group.add(body); group.add(panel1); group.add(panel2); group.add(halo); return group; }\n  function orientGeoSatelliteMarker(marker, satLon, satPoint) { if (!marker || !earthMesh) return; const p0 = latLonToVector3(0, satLon, GEO_VISUAL_RADIUS); const p1 = latLonToVector3(0, satLon + 0.1, GEO_VISUAL_RADIUS); p0.applyQuaternion(earthMesh.quaternion); p1.applyQuaternion(earthMesh.quaternion); const tangent = p1.clone().sub(p0).normalize(); const orbitNormal = currentEarthAxis.clone().normalize(); const radial = satPoint.clone().normalize(); let xAxis = tangent; let yAxis = orbitNormal; let zAxis = new THREE.Vector3().crossVectors(xAxis, yAxis).normalize(); if (zAxis.dot(radial) < 0) { zAxis.multiplyScalar(-1); xAxis.multiplyScalar(-1); } const basis = new THREE.Matrix4(); basis.makeBasis(xAxis, yAxis, zAxis); marker.quaternion.setFromRotationMatrix(basis); }\n  function createGeoSatellites() { if (!scene || !earthMesh) return; clearGeoSatellites(); for (let i = 0; i < geoSatellites.length; i++) { const sat = geoSatellites[i]; const surfacePoint = latLonToVector3(0, sat.lon, 1.03); surfacePoint.applyQuaternion(earthMesh.quaternion); const satPoint = latLonToVector3(0, sat.lon, GEO_VISUAL_RADIUS); satPoint.applyQuaternion(earthMesh.quaternion); const marker = createGeoSatelliteMarker(sat.color); marker.position.copy(satPoint); orientGeoSatelliteMarker(marker, sat.lon, satPoint); scene.add(marker); const orbit = createGeoOrbit(sat.color); scene.add(orbit); const footprint = createGeoFootprint(sat.lon, sat.color); scene.add(footprint); const cone = createGeoCone(sat.lon, sat.color, satPoint); scene.add(cone); const vectorLine = makeLine(surfacePoint, satPoint, sat.color, 0.85, true); scene.add(vectorLine); const label = makeTextSprite(sat.name, '#' + sat.color.toString(16).padStart(6, '0'), 58); label.position.copy(satPoint.clone().multiplyScalar(1.07)); label.scale.set(0.32, 0.16, 0.32); scene.add(label); geoSatObjects.push({ key: sat.key, marker: marker, orbit: orbit, footprint: footprint, cone: cone, vectorLine: vectorLine, label: label }); } applyGeoSatelliteVisibility(); }\n  function getGeoSatVisibility(key, elementName) { const specificKey = 'show_geo_' + key + '_' + elementName; if (Object.prototype.hasOwnProperty.call(layerVisibility, specificKey)) return !!layerVisibility[specificKey]; return true; }\n  function applyGeoSatelliteVisibility() { for (let i = 0; i < geoSatObjects.length; i++) { const obj = geoSatObjects[i]; const satEnabled = layerVisibility.show_geo_sats && getGeoSatVisibility(obj.key, 'enabled'); if (obj.marker) obj.marker.visible = satEnabled; if (obj.orbit) obj.orbit.visible = satEnabled && layerVisibility.show_geo_orbits && getGeoSatVisibility(obj.key, 'orbit'); if (obj.footprint) obj.footprint.visible = satEnabled && layerVisibility.show_geo_footprints && getGeoSatVisibility(obj.key, 'footprint'); if (obj.cone) obj.cone.visible = satEnabled && layerVisibility.show_geo_cones && getGeoSatVisibility(obj.key, 'cone'); if (obj.vectorLine) obj.vectorLine.visible = satEnabled && layerVisibility.show_geo_vectors && getGeoSatVisibility(obj.key, 'vector'); if (obj.label) obj.label.visible = satEnabled && layerVisibility.show_geo_labels && getGeoSatVisibility(obj.key, 'label'); } }\n  function applyLayerVisibility() {\n    if (equatorLine) equatorLine.visible = layerVisibility.show_equator; if (greenwichLine) greenwichLine.visible = layerVisibility.show_greenwich; for (let i = 0; i < graticuleLines.length; i++) graticuleLines[i].visible = layerVisibility.show_graticule;\n    if (utcZoneSurface) utcZoneSurface.visible = layerVisibility.show_utc0; if (utcZoneWestLine) utcZoneWestLine.visible = layerVisibility.show_utc0; if (utcZoneEastLine) utcZoneEastLine.visible = layerVisibility.show_utc0; if (subsolarTimeZoneSurface) subsolarTimeZoneSurface.visible = layerVisibility.show_subsolar_timezone;\n    if (terminatorLine) terminatorLine.visible = layerVisibility.show_terminator; if (sunArrow) sunArrow.visible = layerVisibility.show_sun_arrow; if (subsolarMarker) subsolarMarker.visible = layerVisibility.show_subsolar_point; if (sunCenterLine) sunCenterLine.visible = layerVisibility.show_sun_center_line; if (earthCenterMarker) earthCenterMarker.visible = layerVisibility.show_earth_center;\n    if (equatorPlaneMesh) equatorPlaneMesh.visible = layerVisibility.show_equator_plane; if (greenwichPlaneMesh) greenwichPlaneMesh.visible = layerVisibility.show_greenwich_plane; if (greenwichEquatorVector) greenwichEquatorVector.visible = layerVisibility.show_greenwich_equator_vector; if (solarEquatorProjectionVector) solarEquatorProjectionVector.visible = layerVisibility.show_solar_equator_projection;\n    if (solarMeridianLine) solarMeridianLine.visible = layerVisibility.show_solar_meridian; if (solarNoonSemiCircle) solarNoonSemiCircle.visible = layerVisibility.show_solar_meridian; if (earthAxisLine) earthAxisLine.visible = layerVisibility.show_earth_axis; if (earthAxisLabelN) earthAxisLabelN.visible = layerVisibility.show_earth_axis; if (earthAxisLabelS) earthAxisLabelS.visible = layerVisibility.show_earth_axis; if (polarTerminatorAxisLine) polarTerminatorAxisLine.visible = layerVisibility.show_polar_terminator_axis; if (starField) starField.visible = layerVisibility.show_stars;\n    if (earthMaterial && earthMesh) { earthMaterial.uniforms.showTexture.value = layerVisibility.show_texture ? 1.0 : 0.0; earthMesh.visible = layerVisibility.show_texture || layerVisibility.show_blue_sphere; }\n    if (overlayMesh && overlayMaterial) { const hasOverlayMap = overlayMaterial.uniforms.overlayMap.value !== null; overlayMesh.visible = layerVisibility.show_overlay && hasOverlayMap; overlayMaterial.uniforms.showOverlay.value = layerVisibility.show_overlay ? 1.0 : 0.0; }\n    applyGeoSatelliteVisibility();\n  }\n\n  function solveKeplerEquation(meanAnomaly, eccentricity) { let E = meanAnomaly; for (let i = 0; i < 8; i++) E = E - (E - eccentricity * Math.sin(E) - meanAnomaly) / (1 - eccentricity * Math.cos(E)); return E; }\n  function getEarthOrbitPosition(date) { const e = 0.0167; const periodDays = 365.256; const perihelion = Date.UTC(date.getUTCFullYear(), 0, 3, 0, 0, 0); let days = (date.getTime() - perihelion) / 86400000; while (days < 0) days += periodDays; while (days >= periodDays) days -= periodDays; const M = 2 * Math.PI * days / periodDays; const E = solveKeplerEquation(M, e); return { x: Math.cos(E) - e, y: Math.sqrt(1 - e * e) * Math.sin(E), e: e, daysFromPerihelion: days }; }\n  function drawOrbitDayMarker(ctx, cx, cy, a, visualBScale, day, label, color) { const periodDays = 365.256; const M = 2 * Math.PI * day / periodDays; const E = solveKeplerEquation(M, 0.0167); const x = cx + Math.cos(E) * a; const y = cy + Math.sin(E) * a * visualBScale; ctx.beginPath(); ctx.arc(x, y, 6, 0, Math.PI * 2); ctx.fillStyle = color; ctx.fill(); ctx.strokeStyle = 'rgba(2, 6, 23, 0.95)'; ctx.lineWidth = 2; ctx.stroke(); ctx.fillStyle = color; ctx.font = 'bold 13px Arial'; ctx.fillText(label, x + 9, y - 8); }\n  function drawOrbitWindow(date) {\n    const canvas = document.getElementById(ids.orbit_canvas); if (!canvas) return; const ctx = canvas.getContext('2d'); const w = canvas.width; const h = canvas.height; ctx.clearRect(0, 0, w, h);\n    const cx = w * 0.52; const cy = h * 0.54; const a = w * 0.38; const visualBScale = 0.72; const visualE = Math.sqrt(1 - visualBScale * visualBScale); const focusX = cx + a * visualE; const focusY = cy; const otherFocusX = cx - a * visualE;\n    const grd = ctx.createRadialGradient(cx, cy, 10, cx, cy, Math.max(w, h) * 0.7); grd.addColorStop(0, 'rgba(30, 41, 59, 0.95)'); grd.addColorStop(1, 'rgba(2, 6, 23, 0.98)'); ctx.fillStyle = grd; ctx.fillRect(0, 0, w, h);\n    ctx.strokeStyle = 'rgba(148, 163, 184, 0.22)'; ctx.lineWidth = 1; for (let i = 0; i <= 6; i++) { const yy = 55 + i * 50; ctx.beginPath(); ctx.moveTo(24, yy); ctx.lineTo(w - 24, yy); ctx.stroke(); }\n    ctx.beginPath(); ctx.ellipse(cx, cy, a, a * visualBScale, 0, 0, Math.PI * 2); ctx.strokeStyle = 'rgba(147, 197, 253, 0.95)'; ctx.lineWidth = 3; ctx.stroke();\n    ctx.beginPath(); ctx.arc(cx, cy, 4, 0, Math.PI * 2); ctx.fillStyle = 'rgba(255,255,255,0.55)'; ctx.fill(); ctx.fillStyle = 'rgba(255,255,255,0.75)'; ctx.font = '12px Arial'; ctx.fillText('Centro', cx + 8, cy + 18);\n    ctx.beginPath(); ctx.arc(otherFocusX, focusY, 4, 0, Math.PI * 2); ctx.fillStyle = 'rgba(148, 163, 184, 0.7)'; ctx.fill();\n    ctx.beginPath(); ctx.arc(focusX, focusY, 16, 0, Math.PI * 2); ctx.fillStyle = '#ffcc33'; ctx.shadowColor = 'rgba(255, 204, 51, 0.85)'; ctx.shadowBlur = 22; ctx.fill(); ctx.shadowBlur = 0; ctx.strokeStyle = 'rgba(255,255,255,0.85)'; ctx.lineWidth = 2; ctx.stroke();\n    drawOrbitDayMarker(ctx, cx, cy, a, visualBScale, 0, 'Día 0', '#fef08a'); drawOrbitDayMarker(ctx, cx, cy, a, visualBScale, 100, 'Día 100', '#bbf7d0'); drawOrbitDayMarker(ctx, cx, cy, a, visualBScale, 200, 'Día 200', '#bfdbfe'); drawOrbitDayMarker(ctx, cx, cy, a, visualBScale, 300, 'Día 300', '#fbcfe8');\n    const pos = getEarthOrbitPosition(date); const Eearth = solveKeplerEquation(2 * Math.PI * pos.daysFromPerihelion / 365.256, 0.0167); const ex = cx + Math.cos(Eearth) * a; const ey = cy + Math.sin(Eearth) * a * visualBScale;\n    ctx.beginPath(); ctx.moveTo(focusX, focusY); ctx.lineTo(ex, ey); ctx.strokeStyle = 'rgba(255, 204, 51, 0.42)'; ctx.lineWidth = 2; ctx.stroke();\n    ctx.beginPath(); ctx.arc(ex, ey, 12, 0, Math.PI * 2); ctx.fillStyle = '#60a5fa'; ctx.shadowColor = 'rgba(96, 165, 250, 0.75)'; ctx.shadowBlur = 14; ctx.fill(); ctx.shadowBlur = 0; ctx.strokeStyle = '#e0f2fe'; ctx.lineWidth = 2; ctx.stroke();\n    ctx.fillStyle = '#e5e7eb'; ctx.font = 'bold 22px Arial'; ctx.fillText('Órbita terrestre elíptica', 22, 32); ctx.font = '16px Arial'; ctx.fillStyle = '#ffcc33'; ctx.fillText('Sol en un foco', focusX + 22, focusY - 12); ctx.fillStyle = '#93c5fd'; ctx.fillText('Tierra actual', ex + 16, ey - 12); ctx.fillStyle = '#cbd5e1'; ctx.font = '13px Arial'; ctx.fillText('Elipse visualmente exagerada para mostrar claramente los focos', 22, h - 60); ctx.fillText(formatUtcDateTime(date), 22, h - 38); ctx.fillText('Días desde perihelio aprox.: ' + pos.daysFromPerihelion.toFixed(1), 22, h - 18);\n  }\n\n  function mapLonToX(lon, width) { return (normalizeLon(lon) + 180) / 360 * width; }\n  function mapLatToY(lat, height) { return (90 - lat) / 180 * height; }\n  function drawWrappedLonLatPolyline(ctx, lonLatPoints, width, height, strokeStyle, lineWidth) { if (!lonLatPoints || lonLatPoints.length === 0) return; ctx.strokeStyle = strokeStyle; ctx.lineWidth = lineWidth; ctx.setLineDash([]); let previousX = null; let drawing = false; for (let i = 0; i < lonLatPoints.length; i++) { const p = lonLatPoints[i]; const x = mapLonToX(p.lon, width); const y = mapLatToY(p.lat, height); if (previousX !== null && Math.abs(x - previousX) > width * 0.5) { if (drawing) ctx.stroke(); ctx.beginPath(); ctx.moveTo(x, y); drawing = true; } else if (!drawing) { ctx.beginPath(); ctx.moveTo(x, y); drawing = true; } else { ctx.lineTo(x, y); } previousX = x; } if (drawing) ctx.stroke(); }\n  function drawGoes19FullDiskOnMap(ctx, width, height) { const goes19Lon = -75.2; const angularRadius = Math.acos(1 / GEO_TRUE_RADIUS_RATIO); const points = []; for (let bearing = 0; bearing <= 360; bearing += 2) points.push(destinationPoint(0, goes19Lon, bearing, angularRadius)); drawWrappedLonLatPolyline(ctx, points, width, height, 'rgba(250, 204, 21, 0.98)', 3); const centerX = mapLonToX(goes19Lon, width); const centerY = mapLatToY(0, height); ctx.beginPath(); ctx.arc(centerX, centerY, 7, 0, Math.PI * 2); ctx.fillStyle = '#facc15'; ctx.shadowColor = 'rgba(250, 204, 21, 0.9)'; ctx.shadowBlur = 14; ctx.fill(); ctx.shadowBlur = 0; ctx.strokeStyle = '#111827'; ctx.lineWidth = 2; ctx.stroke(); ctx.fillStyle = '#facc15'; ctx.font = 'bold 14px Arial'; ctx.fillText('GOES-19 full disk', centerX + 10, centerY - 10); ctx.font = '12px Arial'; ctx.fillStyle = '#fde68a'; ctx.fillText('Subsatélite 75.2°O', centerX + 10, centerY + 8); }\n  function drawWgs84DayNightMap(subLat, subLon, date) {\n    const canvas = document.getElementById(ids.map_canvas); if (!canvas) return; const ctx = canvas.getContext('2d'); const w = canvas.width; const h = canvas.height; const img = ctx.createImageData(w, h);\n    const subLatRad = subLat * Math.PI / 180; const subLonRad = subLon * Math.PI / 180;\n    for (let y = 0; y < h; y++) { const lat = 90 - 180 * (y / (h - 1)); const latRad = lat * Math.PI / 180; for (let x = 0; x < w; x++) { const lon = -180 + 360 * (x / (w - 1)); const lonRad = lon * Math.PI / 180; const cosZenith = Math.sin(latRad) * Math.sin(subLatRad) + Math.cos(latRad) * Math.cos(subLatRad) * Math.cos(lonRad - subLonRad); const idx = (y * w + x) * 4; const oceanBand = 0.5 + 0.5 * Math.sin((lon + 40) * Math.PI / 38) * Math.cos(latRad * 2.2); const pseudoLand = oceanBand > 0.62 || Math.abs(lat) > 68; let r = pseudoLand ? 43 : 20; let g = pseudoLand ? 92 : 72; let b = pseudoLand ? 55 : 125; if (cosZenith < 0) { const night = Math.min(0.82, 0.45 + Math.abs(cosZenith) * 0.38); r = Math.round(r * (1 - night)); g = Math.round(g * (1 - night)); b = Math.round(b * (1 - night) + 22 * night); } else { const day = Math.min(1, 0.18 + cosZenith * 0.25); r = Math.min(255, Math.round(r * (1 + day))); g = Math.min(255, Math.round(g * (1 + day))); b = Math.min(255, Math.round(b * (1 + day * 0.55))); } if (Math.abs(cosZenith) < 0.012) { r = 255; g = 214; b = 92; } img.data[idx] = r; img.data[idx + 1] = g; img.data[idx + 2] = b; img.data[idx + 3] = 255; } }\n    ctx.putImageData(img, 0, 0); ctx.strokeStyle = 'rgba(255,255,255,0.28)'; ctx.lineWidth = 1; for (let lon = -180; lon <= 180; lon += 30) { const x = (lon + 180) / 360 * w; ctx.beginPath(); ctx.moveTo(x, 0); ctx.lineTo(x, h); ctx.stroke(); } for (let lat = -90; lat <= 90; lat += 30) { const y = (90 - lat) / 180 * h; ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(w, y); ctx.stroke(); }\n    drawGoes19FullDiskOnMap(ctx, w, h); const sx = mapLonToX(subLon, w); const sy = mapLatToY(subLat, h); ctx.beginPath(); ctx.arc(sx, sy, 9, 0, Math.PI * 2); ctx.fillStyle = '#ffcc33'; ctx.shadowColor = 'rgba(255, 204, 51, 0.95)'; ctx.shadowBlur = 16; ctx.fill(); ctx.shadowBlur = 0; ctx.strokeStyle = '#111827'; ctx.lineWidth = 2; ctx.stroke(); ctx.fillStyle = 'rgba(2, 6, 23, 0.72)'; ctx.fillRect(10, 10, 390, 58); ctx.fillStyle = '#e5e7eb'; ctx.font = 'bold 20px Arial'; ctx.fillText('Mapa WGS84 día / noche + GOES-19', 20, 34); ctx.font = '14px Arial'; ctx.fillStyle = '#ffcc33'; ctx.fillText('Subsolar: ' + subLat.toFixed(3) + '°, ' + subLon.toFixed(3) + '°', 20, 56); ctx.fillStyle = '#cbd5e1'; ctx.fillText(formatUtcDateTime(date), w - 250, h - 16);\n  }\n\n  function subsolarPointFromDate(date) {\n    const jd = date.getTime() / 86400000 + 2440587.5; const n = jd - 2451545.0; const T = n / 36525; const L = normalizeDegrees(280.460 + 0.9856474 * n); const gDeg = normalizeDegrees(357.528 + 0.9856003 * n); const g = gDeg * Math.PI / 180; const lambdaDeg = L + 1.915 * Math.sin(g) + 0.020 * Math.sin(2 * g); const lambda = lambdaDeg * Math.PI / 180; const epsilonDeg = 23.439 - 0.0000004 * n; const epsilon = epsilonDeg * Math.PI / 180; let alpha = Math.atan2(Math.cos(epsilon) * Math.sin(lambda), Math.cos(lambda)) * 180 / Math.PI; alpha = normalizeDegrees(alpha); const delta = Math.asin(Math.sin(epsilon) * Math.sin(lambda)) * 180 / Math.PI; const GMST = normalizeDegrees(280.46061837 + 360.98564736629 * (jd - 2451545) + 0.000387933 * T * T - T * T * T / 38710000); const lon = ((alpha - GMST + 540) % 360) - 180; let utcOffset = Math.round(lon / 15); if (utcOffset > 12) utcOffset -= 24; if (utcOffset < -12) utcOffset += 24; const offsetLabel = utcOffset >= 0 ? 'UTC+' + utcOffset : 'UTC' + utcOffset; const localClock = new Date(date.getTime() + utcOffset * 3600000); const hh = String(localClock.getUTCHours()).padStart(2, '0'); const mi = String(localClock.getUTCMinutes()).padStart(2, '0'); const ss = String(localClock.getUTCSeconds()).padStart(2, '0'); return { lat: delta, lon: lon, utc: formatUtcDateTime(date), solar_time: '12:00 solar aprox.', subsolar_utc_zone: offsetLabel, subsolar_clock: hh + ':' + mi + ':' + ss + ' aprox. en ' + offsetLabel, subsolar_utc_offset: utcOffset };\n  }\n  function applySubsolarState(state, timeSourceLabel, markPositionUpdate, resetCountdownSeconds) {\n    if (!state) return; if (markPositionUpdate) { lastPositionUpdateDate = new Date(); updateLastPositionUpdateBadge(lastPositionUpdateDate); } if (resetCountdownSeconds) resetNextPositionUpdateCountdown(resetCountdownSeconds); currentSubsolarUtcOffset = state.subsolar_utc_offset; orientEarthToSubsolar(state.lat, state.lon); document.getElementById(ids.time_source_txt).innerText = timeSourceLabel; document.getElementById(ids.utc_txt).innerText = state.utc; document.getElementById(ids.lat_txt).innerText = state.lat.toFixed(6) + '°'; document.getElementById(ids.lon_txt).innerText = state.lon.toFixed(6) + '°'; document.getElementById(ids.solar_time_txt).innerText = state.solar_time; document.getElementById(ids.subsolar_utc_zone_txt).innerText = state.subsolar_utc_zone; document.getElementById(ids.subsolar_clock_txt).innerText = state.subsolar_clock; createSubsolarUTCZoneSurface(state.subsolar_utc_offset); const drawDate = parseUtcDateTime(state.utc) || new Date(); drawOrbitWindow(drawDate); drawWgs84DayNightMap(state.lat, state.lon, drawDate); applyLayerVisibility();\n  }\n  function updateBrowserLivePositionIfNeeded() {\n    if (!initialized) return; const refreshSeconds = lastPositionRefreshSeconds || 5;\n    if (simulationMode === 'running') { if (!simulationClockDate) simulationClockDate = lastKnownPositionDate ? new Date(lastKnownPositionDate.getTime()) : new Date(); if (simulationLastRealMs === null) simulationLastRealMs = Date.now(); const nowMs = Date.now(); const elapsedMs = nowMs - simulationLastRealMs; simulationLastRealMs = nowMs; simulationClockDate = new Date(simulationClockDate.getTime() + elapsedMs * simulationTimeScale); const state = subsolarPointFromDate(simulationClockDate); applySubsolarState(state, 'Rotación simulada', false, null); updateNextPositionUpdateBadge(); return; }\n    if (simulationMode === 'paused') { updateNextPositionUpdateBadge(); return; } if (currentTimeSourceCode !== 'system') return; const now = new Date(); const bucketKey = Math.floor(now.getTime() / (refreshSeconds * 1000)); if (bucketKey === lastLivePositionBucket) return; lastLivePositionBucket = bucketKey; const state = subsolarPointFromDate(now); applySubsolarState(state, 'Hora UTC del sistema', true, refreshSeconds);\n  }\n\n  function makeDraggableElement(elementId, handleSelector) {\n    const el = document.getElementById(elementId); const root = document.getElementById(ids.root); if (!el || !root) return; const handle = handleSelector ? el.querySelector(handleSelector) : el; if (!handle) return; let dragging = false, offsetX = 0, offsetY = 0;\n    function getPoint(evt) { if (evt.touches && evt.touches.length > 0) return { x: evt.touches[0].clientX, y: evt.touches[0].clientY }; return { x: evt.clientX, y: evt.clientY }; }\n    function startDrag(evt) { const p = getPoint(evt); const rect = el.getBoundingClientRect(); const rootRect = root.getBoundingClientRect(); dragging = true; offsetX = p.x - rect.left; offsetY = p.y - rect.top; el.style.transform = 'none'; el.style.left = (rect.left - rootRect.left) + 'px'; el.style.top = (rect.top - rootRect.top) + 'px'; el.style.right = 'auto'; el.style.bottom = 'auto'; evt.preventDefault(); }\n    function moveDrag(evt) { if (!dragging) return; const p = getPoint(evt); const rootRect = root.getBoundingClientRect(); let x = p.x - rootRect.left - offsetX; let y = p.y - rootRect.top - offsetY; x = Math.max(0, Math.min(rootRect.width - el.offsetWidth, x)); y = Math.max(0, Math.min(rootRect.height - el.offsetHeight, y)); el.style.left = x + 'px'; el.style.top = y + 'px'; evt.preventDefault(); }\n    function endDrag() { dragging = false; }\n    handle.addEventListener('mousedown', startDrag); window.addEventListener('mousemove', moveDrag); window.addEventListener('mouseup', endDrag); handle.addEventListener('touchstart', startDrag, { passive: false }); window.addEventListener('touchmove', moveDrag, { passive: false }); window.addEventListener('touchend', endDrag);\n  }\n\n  function updateEarthTexture(url) { if (!earthMaterial || !url) return; const loader = new THREE.TextureLoader(); loader.crossOrigin = ''; loader.load(url, function(texture) { texture.wrapS = THREE.ClampToEdgeWrapping; texture.wrapT = THREE.ClampToEdgeWrapping; texture.minFilter = THREE.LinearFilter; earthMaterial.uniforms.earthMap.value = texture; earthMaterial.uniforms.earthMap.value.needsUpdate = true; earthMaterial.needsUpdate = true; }, undefined, function(error) { console.error('No se pudo cargar la textura base:', url, error); }); }\n  function updateOverlayTexture(url) { if (!overlayMaterial || !overlayMesh) return; if (!url || url === '') { overlayMaterial.uniforms.overlayMap.value = null; overlayMaterial.needsUpdate = true; overlayMesh.visible = false; return; } const loader = new THREE.TextureLoader(); loader.crossOrigin = ''; loader.load(url, function(texture) { texture.wrapS = THREE.ClampToEdgeWrapping; texture.wrapT = THREE.ClampToEdgeWrapping; texture.minFilter = THREE.LinearFilter; overlayMaterial.uniforms.overlayMap.value = texture; overlayMaterial.uniforms.overlayMap.value.needsUpdate = true; overlayMaterial.needsUpdate = true; overlayMesh.visible = layerVisibility.show_overlay; }, undefined, function(error) { console.error('No se pudo cargar la capa transparente:', url, error); }); }\n\n  function createFarStarField() {\n    const starsGeometry = new THREE.BufferGeometry();\n    const stars = [];\n\n    // Estrellas lejanas.\n    // La Tierra tiene radio 1 y los satélites GEO están cerca de radio 6.62.\n    // Por eso las estrellas empiezan recién en radio 90.\n    const starCount = 1200;\n    const minStarRadius = 90;\n    const maxStarRadius = 160;\n\n    for (let i = 0; i < starCount; i++) {\n      const theta = Math.random() * Math.PI * 2;\n      const phi = Math.acos(2 * Math.random() - 1);\n      const radius = minStarRadius + Math.random() * (maxStarRadius - minStarRadius);\n\n      const x = radius * Math.sin(phi) * Math.cos(theta);\n      const y = radius * Math.sin(phi) * Math.sin(theta);\n      const z = radius * Math.cos(phi);\n\n      stars.push(x, y, z);\n    }\n\n    starsGeometry.setAttribute('position', new THREE.Float32BufferAttribute(stars, 3));\n\n    starField = new THREE.Points(\n      starsGeometry,\n      new THREE.PointsMaterial({\n        color: 0xffffff,\n        size: 0.06,\n        sizeAttenuation: false,\n        depthWrite: false,\n        depthTest: false\n      })\n    );\n\n    starField.renderOrder = -1000;\n    scene.add(starField);\n  }\n\n  function initEarth(initialTextureUrl, initialOverlayUrl) {\n    if (initialized) return; initialized = true; scene = new THREE.Scene(); scene.background = new THREE.Color(0x050816); const container = document.getElementById(ids.earth3d); const width = container.clientWidth || 800; const height = container.clientHeight || 600; camera = new THREE.PerspectiveCamera(45, width / height, 0.1, 1000); camera.position.set(0, 1.25, 6); renderer = new THREE.WebGLRenderer({ antialias: true }); renderer.setSize(width, height); renderer.setPixelRatio(window.devicePixelRatio); container.appendChild(renderer.domElement); controls = new THREE.OrbitControls(camera, renderer.domElement); controls.enableDamping = true; controls.dampingFactor = 0.05; controls.enablePan = false; controls.minDistance = 1.7; controls.maxDistance = 30;\n    const loader = new THREE.TextureLoader(); loader.crossOrigin = ''; const earthTexture = loader.load(initialTextureUrl); const earthVertexShader = 'varying vec2 vUv; varying vec3 vWorldNormal; void main() { vUv = uv; vWorldNormal = normalize(mat3(modelMatrix) * normal); gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }'; const earthFragmentShader = 'uniform sampler2D earthMap; uniform vec3 sunDirection; uniform float showTexture; uniform float nightLightFactor; uniform float dayLightFactor; uniform float dayGradientFactor; varying vec2 vUv; varying vec3 vWorldNormal; void main() { vec3 texColor = texture2D(earthMap, vUv).rgb; vec3 plainColor = vec3(0.12, 0.32, 0.62); vec3 baseColor = mix(plainColor, texColor, showTexture); float light = dot(normalize(vWorldNormal), normalize(sunDirection)); float day = smoothstep(-0.04, 0.08, light); vec3 nightColor = baseColor * vec3(nightLightFactor, nightLightFactor, nightLightFactor * 1.15); float sunCenter = clamp(light, 0.0, 1.0); float dayShape = mix(1.0, pow(sunCenter, 0.55), dayGradientFactor); vec3 dayColor = baseColor * vec3(dayLightFactor, dayLightFactor, dayLightFactor * 0.95) * dayShape; vec3 finalColor = mix(nightColor, dayColor, day); float terminatorGlow = 1.0 - smoothstep(0.0, 0.06, abs(light)); finalColor += vec3(1.0, 0.72, 0.25) * terminatorGlow * 0.10; gl_FragColor = vec4(finalColor, 1.0); }'; earthMaterial = new THREE.ShaderMaterial({ uniforms: { earthMap: { value: earthTexture }, sunDirection: { value: fixedSunDir }, showTexture: { value: 1.0 }, nightLightFactor: { value: nightLightFactor }, dayLightFactor: { value: dayLightFactor }, dayGradientFactor: { value: dayGradientFactor } }, vertexShader: earthVertexShader, fragmentShader: earthFragmentShader }); earthMesh = new THREE.Mesh(new THREE.SphereGeometry(R, 160, 160), earthMaterial); scene.add(earthMesh);\n    const overlayVertexShader = earthVertexShader; const overlayFragmentShader = 'uniform sampler2D overlayMap; uniform vec3 sunDirection; uniform float showOverlay; uniform float nightLightFactor; uniform float dayLightFactor; uniform float dayGradientFactor; varying vec2 vUv; varying vec3 vWorldNormal; void main() { vec4 texColor = texture2D(overlayMap, vUv); if (texColor.a < 0.01 || showOverlay < 0.5) discard; float light = dot(normalize(vWorldNormal), normalize(sunDirection)); float day = smoothstep(-0.04, 0.08, light); vec3 nightColor = texColor.rgb * vec3(nightLightFactor, nightLightFactor, nightLightFactor * 1.15); float sunCenter = clamp(light, 0.0, 1.0); float dayShape = mix(1.0, pow(sunCenter, 0.55), dayGradientFactor); vec3 dayColor = texColor.rgb * vec3(dayLightFactor, dayLightFactor, dayLightFactor * 0.95) * dayShape; vec3 finalColor = mix(nightColor, dayColor, day); gl_FragColor = vec4(finalColor, texColor.a); }'; overlayMaterial = new THREE.ShaderMaterial({ uniforms: { overlayMap: { value: null }, sunDirection: { value: fixedSunDir }, showOverlay: { value: 1.0 }, nightLightFactor: { value: nightLightFactor }, dayLightFactor: { value: dayLightFactor }, dayGradientFactor: { value: dayGradientFactor } }, vertexShader: overlayVertexShader, fragmentShader: overlayFragmentShader, transparent: true, depthWrite: false, side: THREE.DoubleSide }); overlayMesh = new THREE.Mesh(new THREE.SphereGeometry(R * 1.003, 160, 160), overlayMaterial); overlayMesh.quaternion.copy(earthMesh.quaternion); scene.add(overlayMesh); if (initialOverlayUrl && initialOverlayUrl !== '') updateOverlayTexture(initialOverlayUrl); else overlayMesh.visible = false;\n\n    createFarStarField();\n\n    createTerminator(); createSubsolarMarker(); createEarthCenterMarker(); createSunCenterLine(); createSunArrow(); updateGeographicLines(); updateGraticule(); updateAxes(); updateSolarMeridian(); updateEquatorPlane(); updateGreenwichPlane(); updateGreenwichEquatorVector(); updateSolarEquatorProjectionVector(); createGeoSatellites(); applyLayerVisibility(); updateReferenceAxisMode(); drawOrbitWindow(new Date()); drawWgs84DayNightMap(0, 0, new Date()); animate();\n  }\n\n  function updateSunAndEarth(lat, lon, utc, solarTime, subsolarUtcZone, subsolarClock, subsolarUtcOffset, timeSourceLabel, refreshSeconds, timeSourceCode, positionMode) { if (positionMode === 'simulation') return; simulationMode = 'system'; currentTimeSourceCode = timeSourceCode || currentTimeSourceCode || 'system'; lastLivePositionBucket = null; const parsedUtc = parseUtcDateTime(utc); if (parsedUtc) { lastKnownPositionDate = parsedUtc; simulationClockDate = new Date(parsedUtc.getTime()); simulationLastRealMs = Date.now(); } applySubsolarState({ lat: lat, lon: lon, utc: utc, solar_time: solarTime, subsolar_utc_zone: subsolarUtcZone, subsolar_clock: subsolarClock, subsolar_utc_offset: subsolarUtcOffset }, timeSourceLabel, true, refreshSeconds); }\n  function animate() { requestAnimationFrame(animate); updateBrowserLivePositionIfNeeded(); updateReferenceAxisTransition(); controls.update(); renderer.render(scene, camera); }\n\n  window.addEventListener('resize', function() { if (!camera || !renderer) return; const container = document.getElementById(ids.earth3d); if (!container) return; const width = container.clientWidth || 800; const height = container.clientHeight || 600; camera.aspect = width / height; camera.updateProjectionMatrix(); renderer.setSize(width, height); });\n\n  setupToggleButton(ids.toggle_gis, ids.gis_options); setupToggleButton(ids.toggle_sat, ids.sat_options); setupToggleButton(ids.toggle_sim, ids.sim_options);\n  makeDraggableElement(ids.utc_clock_badge, null); makeDraggableElement(ids.last_position_update_badge, null); makeDraggableElement(ids.next_position_update_badge, null); makeDraggableElement(ids.orbit_window, '.window-title'); makeDraggableElement(ids.map_window, '.window-title');\n  updateFloatingUtcClock(); updateNextPositionUpdateBadge(); setInterval(function() { updateFloatingUtcClock(); updateNextPositionUpdateBadge(); }, 1000);\n\n  Shiny.addCustomMessageHandler('", ns("earth_init"), "', function(msg) { initEarth(msg.initialTextureUrl, msg.initialOverlayUrl); });\n  Shiny.addCustomMessageHandler('", ns("sun_update"), "', function(msg) { updateSunAndEarth(msg.lat, msg.lon, msg.utc, msg.solar_time, msg.subsolar_utc_zone, msg.subsolar_clock, msg.subsolar_utc_offset, msg.time_source_label, msg.refresh_seconds, msg.time_source_code, msg.position_mode); });\n  Shiny.addCustomMessageHandler('", ns("autorotate_update"), "', function(msg) { const speedValue = parseFloat(msg.simulationSpeed); if (!isNaN(speedValue) && speedValue > 0) simulationTimeScale = speedValue; const action = msg.action || 'speed'; if (action === 'play') { simulationMode = 'running'; if (!simulationClockDate) simulationClockDate = lastKnownPositionDate ? new Date(lastKnownPositionDate.getTime()) : new Date(); simulationLastRealMs = Date.now(); lastLivePositionBucket = null; updateNextPositionUpdateBadge(); return; } if (action === 'pause') { if (simulationMode === 'running') { simulationMode = 'paused'; simulationLastRealMs = null; lastLivePositionBucket = null; } updateNextPositionUpdateBadge(); return; } if (action === 'stop') { simulationMode = 'system'; simulationClockDate = null; simulationLastRealMs = null; lastLivePositionBucket = null; resetNextPositionUpdateCountdown(lastPositionRefreshSeconds || 5); return; } updateNextPositionUpdateBadge(); });\n  Shiny.addCustomMessageHandler('", ns("night_light_update"), "', function(msg) { nightLightFactor = msg.value / 100.0; if (earthMaterial && earthMaterial.uniforms.nightLightFactor) { earthMaterial.uniforms.nightLightFactor.value = nightLightFactor; earthMaterial.needsUpdate = true; } if (overlayMaterial && overlayMaterial.uniforms.nightLightFactor) { overlayMaterial.uniforms.nightLightFactor.value = nightLightFactor; overlayMaterial.needsUpdate = true; } });\n  Shiny.addCustomMessageHandler('", ns("day_light_update"), "', function(msg) { dayLightFactor = msg.dayLight / 100.0; dayGradientFactor = msg.dayGradient / 100.0; if (earthMaterial) { if (earthMaterial.uniforms.dayLightFactor) earthMaterial.uniforms.dayLightFactor.value = dayLightFactor; if (earthMaterial.uniforms.dayGradientFactor) earthMaterial.uniforms.dayGradientFactor.value = dayGradientFactor; earthMaterial.needsUpdate = true; } if (overlayMaterial) { if (overlayMaterial.uniforms.dayLightFactor) overlayMaterial.uniforms.dayLightFactor.value = dayLightFactor; if (overlayMaterial.uniforms.dayGradientFactor) overlayMaterial.uniforms.dayGradientFactor.value = dayGradientFactor; overlayMaterial.needsUpdate = true; } });\n  Shiny.addCustomMessageHandler('", ns("layer_visibility"), "', function(msg) { layerVisibility = msg; applyLayerVisibility(); });\n  Shiny.addCustomMessageHandler('", ns("earth_texture_update"), "', function(msg) { updateEarthTexture(msg.url); });\n  Shiny.addCustomMessageHandler('", ns("earth_overlay_update"), "', function(msg) { updateOverlayTexture(msg.url); });\n  Shiny.addCustomMessageHandler('", ns("reference_axis_update"), "', function(msg) { referenceAxisMode = msg.mode || 'solar_axis'; updateReferenceAxisMode(); });\n  Shiny.addCustomMessageHandler('", ns("toggle_floaters_update"), "', function(msg) { toggleMovableObjects(); });\n})();\n")
}
# ============================================================
# PARCHE earth3d_js
# 1) Objetos móviles inician ocultos
# 2) Estrellas solo de fondo lejano, no cerca de la Tierra
# ============================================================

earth3d_js_original <- earth3d_js

earth3d_js <- function(ids_json, ns) {
  js <- earth3d_js_original(ids_json, ns)

  # ------------------------------------------------------------
  # 1) Objetos móviles ocultos por defecto
  # ------------------------------------------------------------
  js <- gsub(
    "let movableObjectsVisible = true;",
    "let movableObjectsVisible = false;",
    js,
    fixed = TRUE
  )

  # ------------------------------------------------------------
  # 2) Reemplazar creación de estrellas cercanas por estrellas lejanas
  # ------------------------------------------------------------
  old_stars <- paste0(
    "const starsGeometry = new THREE.BufferGeometry(); const stars = []; ",
    "for (let i = 0; i < 1200; i++) stars.push(THREE.MathUtils.randFloatSpread(100), THREE.MathUtils.randFloatSpread(100), THREE.MathUtils.randFloatSpread(100)); ",
    "starsGeometry.setAttribute('position', new THREE.Float32BufferAttribute(stars, 3)); ",
    "starField = new THREE.Points(starsGeometry, new THREE.PointsMaterial({ color: 0xffffff, size: 0.04 })); ",
    "scene.add(starField);"
  )

  new_stars <- paste0(
    "const starsGeometry = new THREE.BufferGeometry();\n",
    "    const stars = [];\n",
    "    for (let i = 0; i < 1200; i++) {\n",
    "      const dir = new THREE.Vector3(\n",
    "        THREE.MathUtils.randFloatSpread(2),\n",
    "        THREE.MathUtils.randFloatSpread(2),\n",
    "        THREE.MathUtils.randFloatSpread(2)\n",
    "      );\n",
    "      if (dir.length() < 0.0001) dir.set(1, 0, 0);\n",
    "      dir.normalize();\n",
    "      const radius = THREE.MathUtils.randFloat(75, 120);\n",
    "      stars.push(dir.x * radius, dir.y * radius, dir.z * radius);\n",
    "    }\n",
    "    starsGeometry.setAttribute('position', new THREE.Float32BufferAttribute(stars, 3));\n",
    "    starField = new THREE.Points(\n",
    "      starsGeometry,\n",
    "      new THREE.PointsMaterial({\n",
    "        color: 0xffffff,\n",
    "        size: 0.035,\n",
    "        sizeAttenuation: false,\n",
    "        depthTest: false,\n",
    "        depthWrite: false\n",
    "      })\n",
    "    );\n",
    "    starField.renderOrder = -1000;\n",
    "    scene.add(starField);"
  )

  js <- gsub(
    old_stars,
    new_stars,
    js,
    fixed = TRUE
  )

  # ------------------------------------------------------------
  # 3) Forzar que los objetos móviles arranquen ocultos
  # ------------------------------------------------------------
  old_init_floaters <- paste0(
    "updateFloatingUtcClock(); updateNextPositionUpdateBadge(); ",
    "setInterval(function() { updateFloatingUtcClock(); updateNextPositionUpdateBadge(); }, 1000);"
  )

  new_init_floaters <- paste0(
    "updateFloatingUtcClock();\n",
    "  updateNextPositionUpdateBadge();\n",
    "  setMovableObjectsVisible(false);\n",
    "  setInterval(function() { updateFloatingUtcClock(); updateNextPositionUpdateBadge(); }, 1000);"
  )

  js <- gsub(
    old_init_floaters,
    new_init_floaters,
    js,
    fixed = TRUE
  )

  js
}
# ============================================================
# Server
# ============================================================
mod_satelliteGlobe_09_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    find_bg_layers_dir <- function() {
      candidates <- c(
        file.path(fn_my_folder_package(), "www", "bg_layers"),
        file.path(getwd(), "www", "bg_layers"),
        file.path(getwd(), "inst", "www", "bg_layers")
      )
      candidates <- candidates[nzchar(candidates)]
      found <- candidates[file.exists(candidates)]
      if (length(found) == 0) return(NA_character_)
      found[1]
    }

    get_web_utc_time <- function() {
      tryCatch({
        x <- jsonlite::fromJSON("https://worldtimeapi.org/api/timezone/Etc/UTC")
        utc_txt <- x$utc_datetime
        utc_txt <- sub("\\.[0-9]+", "", utc_txt)
        utc_txt <- sub("Z$", "", utc_txt)
        utc_txt <- sub("\\+00:00$", "", utc_txt)
        utc_txt <- gsub("T", " ", utc_txt)
        t <- as.POSIXct(utc_txt, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
        if (is.na(t)) stop("No se pudo interpretar la hora web.")
        list(time = t, label = "Hora UTC web")
      }, error = function(e) {
        list(time = Sys.time(), label = "Hora UTC web no disponible; usando hora del sistema")
      })
    }

    make_manual_utc_time <- function(year, month, day, hour, minute, second) {
      txt <- sprintf(
        "%04d-%02d-%02d %02d:%02d:%02d",
        as.integer(year), as.integer(month), as.integer(day),
        as.integer(hour), as.integer(minute), as.integer(second)
      )
      t <- as.POSIXct(txt, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
      if (is.na(t)) Sys.time() else t
    }

    subsolar_point <- function(time_utc = Sys.time()) {
      t <- as.POSIXct(time_utc, tz = "UTC")
      jd <- as.numeric(t) / 86400 + 2440587.5
      n <- jd - 2451545.0
      T <- n / 36525
      L <- (280.460 + 0.9856474 * n) %% 360
      g <- (357.528 + 0.9856003 * n) %% 360
      lambda <- L + 1.915 * sin(g * pi / 180) + 0.020 * sin(2 * g * pi / 180)
      epsilon <- 23.439 - 0.0000004 * n
      alpha <- atan2(cos(epsilon * pi / 180) * sin(lambda * pi / 180), cos(lambda * pi / 180)) * 180 / pi
      alpha <- alpha %% 360
      delta <- asin(sin(epsilon * pi / 180) * sin(lambda * pi / 180)) * 180 / pi
      GMST <- (280.46061837 + 360.98564736629 * (jd - 2451545) + 0.000387933 * T^2 - T^3 / 38710000) %% 360
      lon <- ((alpha - GMST + 540) %% 360) - 180
      utc_offset <- round(lon / 15)
      if (utc_offset > 12) utc_offset <- utc_offset - 24
      if (utc_offset < -12) utc_offset <- utc_offset + 24
      offset_label <- ifelse(utc_offset >= 0, paste0("UTC+", utc_offset), paste0("UTC", utc_offset))
      local_clock <- t + lubridate::hours(utc_offset)
      list(
        lat = delta,
        lon = lon,
        utc = format(t, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
        local_solar_time = "12:00 solar aprox.",
        utc_offset = utc_offset,
        utc_offset_label = offset_label,
        subsolar_clock_label = paste0(format(local_clock, "%H:%M:%S", tz = "UTC"), " aprox. en ", offset_label)
      )
    }

    bg_layers_dir <- find_bg_layers_dir()
    if (is.na(bg_layers_dir) || !dir.exists(bg_layers_dir)) {
      stop("No encuentro la carpeta bg_layers. Revisá que exista inst/www/bg_layers o www/bg_layers.")
    }

    resource_prefix <- paste0("bg_layers_", gsub("[^A-Za-z0-9_]", "_", session$ns("")))
    addResourcePath(resource_prefix, bg_layers_dir)

    wgs84_solid_dir <- file.path(bg_layers_dir, "bg_01_wgs84_01_solid")
    wgs84_transparent_dir <- file.path(bg_layers_dir, "bg_01_wgs84_02_transparent")

    get_wgs84_solid_files <- function() {
      if (!dir.exists(wgs84_solid_dir)) return(character(0))
      list.files(wgs84_solid_dir, pattern = "\\.(png|jpg|jpeg|webp)$", ignore.case = TRUE, full.names = FALSE)
    }

    get_wgs84_transparent_files <- function() {
      if (!dir.exists(wgs84_transparent_dir)) return(character(0))
      list.files(wgs84_transparent_dir, pattern = "\\.(png|jpg|jpeg|webp)$", ignore.case = TRUE, full.names = FALSE)
    }

    wgs84_solid_files <- get_wgs84_solid_files()
    wgs84_transparent_files <- get_wgs84_transparent_files()

    default_earth_texture <- if (length(wgs84_solid_files) >= 4) {
      wgs84_solid_files[4]
    } else if (length(wgs84_solid_files) > 0) {
      wgs84_solid_files[1]
    } else {
      NA_character_
    }

    default_overlay_texture <- if (length(wgs84_transparent_files) > 0) wgs84_transparent_files[1] else NA_character_

    initial_texture_url <- if (!is.na(default_earth_texture)) {
      file.path(resource_prefix, "bg_01_wgs84_01_solid", default_earth_texture)
    } else {
      "https://threejs.org/examples/textures/land_ocean_ice_cloud_2048.jpg"
    }

    initial_overlay_url <- if (!is.na(default_overlay_texture)) {
      file.path(resource_prefix, "bg_01_wgs84_02_transparent", default_overlay_texture)
    } else {
      ""
    }

    initial_texture_url <- gsub("\\\\", "/", initial_texture_url)
    initial_overlay_url <- gsub("\\\\", "/", initial_overlay_url)

    updateSelectInput(session, "earth_texture_file", choices = wgs84_solid_files, selected = default_earth_texture)
    updateSelectInput(
      session,
      "earth_overlay_file",
      choices = c("Ninguna" = "", wgs84_transparent_files),
      selected = ifelse(is.na(default_overlay_texture), "", default_overlay_texture)
    )

    selected_utc_time <- reactive({
      time_source <- input$time_source
      if (is.null(time_source) || length(time_source) == 0) time_source <- "system"
      if (identical(time_source, "system")) return(list(time = Sys.time(), label = "Hora UTC del sistema"))
      if (identical(time_source, "web")) return(get_web_utc_time())
      if (identical(time_source, "manual")) {
        return(list(
          time = make_manual_utc_time(
            input$manual_year,
            input$manual_month,
            input$manual_day,
            input$manual_hour,
            input$manual_minute,
            input$manual_second
          ),
          label = "Hora UTC manual"
        ))
      }
      list(time = Sys.time(), label = "Hora UTC del sistema")
    })

    get_position_mode <- function() {
      mode <- input$position_mode
      if (is.null(mode) || length(mode) == 0 || !(mode %in% c("tracking", "simulation"))) return("tracking")
      mode
    }

    get_refresh_seconds <- function() {
      x <- suppressWarnings(as.numeric(input$refresh_seconds))
      if (is.null(x) || length(x) == 0 || is.na(x) || !(x %in% c(1, 5, 10, 30))) return(5)
      x
    }

    send_sun_update <- function() {
      position_mode <- isolate(get_position_mode())
      time_source_code <- isolate(input$time_source)
      if (is.null(time_source_code) || length(time_source_code) == 0) time_source_code <- "system"
      time_info <- isolate(selected_utc_time())
      refresh_seconds <- isolate(get_refresh_seconds())
      s <- subsolar_point(time_info$time)
      session$sendCustomMessage(
        session$ns("sun_update"),
        list(
          lat = s$lat,
          lon = s$lon,
          utc = s$utc,
          solar_time = s$local_solar_time,
          subsolar_utc_zone = s$utc_offset_label,
          subsolar_clock = s$subsolar_clock_label,
          subsolar_utc_offset = s$utc_offset,
          time_source_label = time_info$label,
          refresh_seconds = refresh_seconds,
          time_source_code = time_source_code,
          position_mode = position_mode
        )
      )
    }

    session$onFlushed(function() {
      session$sendCustomMessage(
        session$ns("earth_init"),
        list(initialTextureUrl = initial_texture_url, initialOverlayUrl = initial_overlay_url)
      )
      send_sun_update()
    }, once = TRUE)

    observe({
      position_mode <- get_position_mode()
      time_source <- input$time_source
      refresh_seconds <- get_refresh_seconds()
      if (is.null(time_source) || length(time_source) == 0) time_source <- "system"
      if (identical(position_mode, "tracking") && !identical(time_source, "manual")) {
        invalidateLater(refresh_seconds * 1000, session)
        send_sun_update()
      }
    })

    send_simulation_control <- function(action) {
      speed <- suppressWarnings(as.numeric(input$simulation_speed))
      if (is.null(speed) || length(speed) == 0 || is.na(speed) || speed <= 0) speed <- 600
      session$sendCustomMessage(session$ns("autorotate_update"), list(action = action, simulationSpeed = speed))
    }

    observeEvent(input$refresh_now, {
      updateRadioButtons(session, "position_mode", selected = "tracking")
      updateRadioButtons(session, "time_source", selected = "system")
      send_simulation_control("stop")
      send_sun_update()
    }, ignoreInit = TRUE)

    observeEvent(input$sim_play, {
      updateRadioButtons(session, "position_mode", selected = "simulation")
      send_simulation_control("play")
    }, ignoreInit = TRUE)

    observeEvent(input$sim_pause, {
      updateRadioButtons(session, "position_mode", selected = "simulation")
      send_simulation_control("pause")
    }, ignoreInit = TRUE)

    observeEvent(input$sim_stop, {
      updateRadioButtons(session, "position_mode", selected = "tracking")
      updateRadioButtons(session, "time_source", selected = "system")
      send_simulation_control("stop")
      send_sun_update()
    }, ignoreInit = TRUE)

    observeEvent(input$simulation_speed, {
      send_simulation_control("speed")
    }, ignoreInit = TRUE)

    observeEvent(input$toggle_floaters, {
      session$sendCustomMessage(session$ns("toggle_floaters_update"), list())
    }, ignoreInit = TRUE)

    observe({
      session$sendCustomMessage(session$ns("night_light_update"), list(value = input$night_light))
    })

    observe({
      mode <- input$reference_axis
      if (is.null(mode) || length(mode) == 0 || !(mode %in% c("solar_axis", "earth_axis"))) mode <- "solar_axis"
      session$sendCustomMessage(session$ns("reference_axis_update"), list(mode = mode))
    })

    observe({
      session$sendCustomMessage(session$ns("day_light_update"), list(dayLight = input$day_light, dayGradient = input$day_gradient))
    })

    observe({
      session$sendCustomMessage(
        session$ns("layer_visibility"),
        list(
          show_equator = isTRUE(input$show_equator),
          show_greenwich = isTRUE(input$show_greenwich),
          show_graticule = isTRUE(input$show_graticule),
          show_utc0 = isTRUE(input$show_utc0),
          show_subsolar_timezone = isTRUE(input$show_subsolar_timezone),
          show_texture = isTRUE(input$show_texture),
          show_blue_sphere = isTRUE(input$show_blue_sphere),
          show_overlay = isTRUE(input$show_overlay),
          show_stars = isTRUE(input$show_stars),
          show_terminator = isTRUE(input$show_terminator),
          show_sun_arrow = isTRUE(input$show_sun_arrow),
          show_subsolar_point = isTRUE(input$show_subsolar_point),
          show_sun_center_line = isTRUE(input$show_sun_center_line),
          show_earth_center = isTRUE(input$show_earth_center),
          show_equator_plane = isTRUE(input$show_equator_plane),
          show_greenwich_plane = isTRUE(input$show_greenwich_plane),
          show_greenwich_equator_vector = isTRUE(input$show_greenwich_equator_vector),
          show_solar_equator_projection = isTRUE(input$show_solar_equator_projection),
          show_solar_meridian = isTRUE(input$show_solar_meridian),
          show_earth_axis = isTRUE(input$show_earth_axis),
          show_polar_terminator_axis = isTRUE(input$show_polar_terminator_axis),
          show_geo_sats = isTRUE(input$show_geo_sats),
          show_geo_orbits = isTRUE(input$show_geo_orbits),
          show_geo_footprints = isTRUE(input$show_geo_footprints),
          show_geo_cones = isTRUE(input$show_geo_cones),
          show_geo_vectors = isTRUE(input$show_geo_vectors),
          show_geo_labels = isTRUE(input$show_geo_labels),
          show_geo_goes18_enabled = isTRUE(input$show_geo_goes18_enabled),
          show_geo_goes18_orbit = isTRUE(input$show_geo_goes18_orbit),
          show_geo_goes18_footprint = isTRUE(input$show_geo_goes18_footprint),
          show_geo_goes18_cone = isTRUE(input$show_geo_goes18_cone),
          show_geo_goes18_vector = isTRUE(input$show_geo_goes18_vector),
          show_geo_goes18_label = isTRUE(input$show_geo_goes18_label),
          show_geo_goes19_enabled = isTRUE(input$show_geo_goes19_enabled),
          show_geo_goes19_orbit = isTRUE(input$show_geo_goes19_orbit),
          show_geo_goes19_footprint = isTRUE(input$show_geo_goes19_footprint),
          show_geo_goes19_cone = isTRUE(input$show_geo_goes19_cone),
          show_geo_goes19_vector = isTRUE(input$show_geo_goes19_vector),
          show_geo_goes19_label = isTRUE(input$show_geo_goes19_label)
        )
      )
    })

    observeEvent(input$earth_texture_file, {
      req(input$earth_texture_file)
      texture_url <- file.path(resource_prefix, "bg_01_wgs84_01_solid", input$earth_texture_file)
      texture_url <- gsub("\\\\", "/", texture_url)
      session$sendCustomMessage(session$ns("earth_texture_update"), list(url = texture_url))
    }, ignoreInit = TRUE)

    observeEvent(input$earth_overlay_file, {
      overlay_file <- input$earth_overlay_file
      overlay_url <- if (is.null(overlay_file) || overlay_file == "") "" else file.path(resource_prefix, "bg_01_wgs84_02_transparent", overlay_file)
      overlay_url <- gsub("\\\\", "/", overlay_url)
      session$sendCustomMessage(session$ns("earth_overlay_update"), list(url = overlay_url))
    }, ignoreInit = TRUE)
  })
}

# ============================================================
# App completa
# ============================================================
# ui <- fluidPage(
#   tags$style(HTML("body { margin: 0; } .container-fluid { padding: 0; }")),
#   mod_satelliteGlobe_08_ui("earth")
# )
#
# server <- function(input, output, session) {
#   mod_satelliteGlobe_08_server("earth")
# }
#
# shinyApp(ui, server)

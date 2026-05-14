# R/mod_satellite_globe.R

library(shiny)

mod_satelliteGlobe_01_ui <- function(id) {
  ns <- NS(id)

  root_id <- ns("satellite_globe_root")

  tagList(
    tags$head(
      tags$style(HTML(satellite_globe_css(root_id))),

      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/satellite.js@5.0.0/dist/satellite.min.js")
    ),

    div(
      id = root_id,
      class = "satellite-globe-module",

      div(
        class = "sg-app-container",

        div(id = ns("globe_container"), class = "sg-globe-container"),
        div(id = ns("label_layer"), class = "sg-label-layer"),

        tags$button(
          id = ns("menu_toggle"),
          class = "sg-menu-toggle",
          type = "button",
          "×"
        ),

        actionButton(
          inputId = ns("btn_go_home"),
          label = "← Launcher",
          class = "sg-home-floating"
        ),

        div(
          id = ns("panel"),
          class = "sg-panel",

          h3("Globo 3D + satélites en tiempo real"),

          p("Posición orbital calculada con TLEs online de CelesTrak + SGP4. La distancia Tierra-satélite está escalada físicamente. El tamaño visual de los satélites está exagerado para poder verlos."),

          div(id = ns("clock"), class = "sg-clock", "UTC: --"),

          tags$button(
            id = ns("toggle_animation"),
            class = "sg-button",
            type = "button",
            "Pausar animación"
          ),

          tags$button(
            id = ns("reload_tle"),
            class = "sg-button",
            type = "button",
            "Recargar TLE online"
          ),

          div(class = "section-title", "Visualización"),

          div(
            class = "visual-control",
            tags$label("Grosor de órbitas"),
            tags$input(
              id = ns("orbit_width"),
              type = "range",
              min = "1",
              max = "8",
              step = "1",
              value = "2"
            )
          ),

          div(
            class = "visual-control",
            tags$label("Modo de iluminación"),
            tags$select(
              id = ns("lighting_mode"),
              class = "sg-select",
              tags$option(
                value = "real_only",
                selected = "selected",
                "Solo Sol real"
              ),
              tags$option(
                value = "real_plus_fill",
                "Solar real + estética suave"
              )
            )
          ),

          div(
            class = "visual-control",
            tags$label("Modo de referencia visual"),
            tags$select(
              id = ns("reference_mode"),
              class = "sg-select",
              tags$option(
                value = "tilted",
                selected = "selected",
                "Plano solar / eclíptico"
              ),
              tags$option(
                value = "equatorial",
                "Plano ecuatorial terrestre"
              )
            )
          ),

          div(
            id = ns("reference_note"),
            class = "sg-reference-note",
            "Referencia: plano solar/eclíptico. Tierra inclinada respecto del plano de iluminación."
          ),

          div(class = "section-title", "Matriz de visualización"),

          tags$table(
            class = "visibility-matrix",
            tags$thead(
              tags$tr(
                tags$th("Satélite"),
                tags$th("Todo"),
                tags$th("Sat"),
                tags$th("Órbita"),
                tags$th("Cobertura"),
                tags$th("Nombre")
              )
            ),
            tags$tbody(
              satellite_row(ns, "Sentinel2A", "Sentinel-2A", "#ff4d4d"),
              satellite_row(ns, "Sentinel2B", "Sentinel-2B", "#ff9f43"),
              satellite_row(ns, "Landsat8", "Landsat 8", "#00d2d3"),
              satellite_row(ns, "Landsat9", "Landsat 9", "#54a0ff"),
              satellite_row(ns, "GOES16", "GOES-16", "#a78bfa"),
              satellite_row(ns, "GOES18", "GOES-18", "#f472b6"),
              satellite_row(ns, "GOES19", "GOES-19", "#facc15")
            )
          ),

          div(
            id = ns("status"),
            class = "sg-status",
            "Cargando TLEs online..."
          ),

          tags$table(
            class = "sat-table",
            tags$thead(
              tags$tr(
                tags$th("Satélite"),
                tags$th("Tipo"),
                tags$th("Lat"),
                tags$th("Lon"),
                tags$th("Alt km"),
                tags$th("Dist. km"),
                tags$th("Radio km"),
                tags$th("Área km²"),
                tags$th("% Tierra")
              )
            ),
            tags$tbody(id = ns("sat_table_body"))
          ),

          div(
            class = "small-note",
            "Cobertura controla huella + cono al mismo tiempo. Área: aproximación circular instantánea. En Sentinel/Landsat se usa el ancho de barrido real aproximado para la tabla, aunque la huella visual está agrandada para poder verse. En GOES se usa el área visible desde órbita geoestacionaria."
          )
        )
      )
    ),

    tags$script(HTML(satellite_globe_js(
      root_id = root_id,
      globe_id = ns("globe_container"),
      label_layer_id = ns("label_layer"),
      panel_id = ns("panel"),
      menu_toggle_id = ns("menu_toggle"),
      toggle_animation_id = ns("toggle_animation"),
      reload_tle_id = ns("reload_tle"),
      orbit_width_id = ns("orbit_width"),
      lighting_mode_id = ns("lighting_mode"),
      reference_mode_id = ns("reference_mode"),
      reference_note_id = ns("reference_note"),
      status_id = ns("status"),
      clock_id = ns("clock"),
      table_body_id = ns("sat_table_body"),
      ns_prefix = ns("")
    )))
  )
}


mod_satelliteGlobe_01_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Motor JavaScript.
  })
}


satellite_row <- function(ns, key, label, color) {
  tags$tr(
    tags$td(
      span(class = "dot", style = paste0("background:", color, ";")),
      paste0(" ", label)
    ),
    tags$td(tags$input(
      type = "checkbox",
      id = ns(paste0("vis_", key, "_all"))
    )),
    tags$td(tags$input(
      type = "checkbox",
      id = ns(paste0("vis_", key, "_sat"))
    )),
    tags$td(tags$input(
      type = "checkbox",
      id = ns(paste0("vis_", key, "_orbit"))
    )),
    tags$td(tags$input(
      type = "checkbox",
      id = ns(paste0("vis_", key, "_coverage"))
    )),
    tags$td(tags$input(
      type = "checkbox",
      id = ns(paste0("vis_", key, "_label"))
    ))
  )
}


satellite_globe_css <- function(root_id) {
  css <- r"(
    #__ROOT_ID__ {
      position: relative;
      width: 100%;
      height: 100vh;
      min-height: 600px;
      overflow: hidden;
      background:
        radial-gradient(circle at center,
          rgba(16, 25, 54, 0.95) 0%,
          rgba(5, 8, 22, 0.98) 55%,
          #02030a 100%);
      color: white;
      font-family: Arial, sans-serif;
    }

    #__ROOT_ID__ .sg-app-container {
      position: relative;
      width: 100%;
      height: 100%;
      overflow: hidden;
    }

    #__ROOT_ID__ .sg-globe-container {
      width: 100%;
      height: 100%;
    }

    #__ROOT_ID__ .sg-label-layer {
      position: absolute;
      left: 0;
      top: 0;
      width: 100%;
      height: 100%;
      pointer-events: none;
      z-index: 4;
    }

    #__ROOT_ID__ .sat-label {
      position: absolute;
      z-index: 5;
      padding: 3px 7px;
      border-radius: 8px;
      background: rgba(2, 6, 23, 0.78);
      border: 1px solid rgba(255,255,255,0.24);
      color: white;
      font-size: 11px;
      font-weight: bold;
      pointer-events: none;
      transform: translate(-50%, -120%);
      white-space: nowrap;
      text-shadow: 0 1px 3px rgba(0,0,0,0.85);
    }

    #__ROOT_ID__ .sg-panel {
      position: absolute;
      top: 20px;
      left: 20px;
      z-index: 10;
      background: rgba(5, 8, 22, 0.86);
      border: 1px solid rgba(255,255,255,0.18);
      border-radius: 16px;
      padding: 16px 18px;
      width: 620px;
      max-height: 92vh;
      overflow-y: auto;
      backdrop-filter: blur(8px);
      box-shadow: 0 10px 30px rgba(0,0,0,0.35);
      transition: transform 0.35s ease, opacity 0.35s ease;
    }

    #__ROOT_ID__ .sg-panel.hidden-panel {
      transform: translateX(-660px);
      opacity: 0;
      pointer-events: none;
    }

    #__ROOT_ID__ .sg-menu-toggle {
      position: absolute;
      top: 20px;
      left: 655px;
      z-index: 20;
      width: auto;
      min-width: 48px;
      border: 1px solid rgba(255,255,255,0.25);
      border-radius: 12px;
      padding: 10px 14px;
      background: rgba(37, 99, 235, 0.9);
      color: white;
      cursor: pointer;
      font-weight: bold;
      box-shadow: 0 10px 30px rgba(0,0,0,0.35);
      backdrop-filter: blur(8px);
      transition: left 0.35s ease, background 0.25s ease;
    }

    #__ROOT_ID__ .sg-menu-toggle:hover {
      background: rgba(29, 78, 216, 0.95);
    }

    #__ROOT_ID__ .sg-menu-toggle.panel-hidden {
      left: 20px;
    }

    #__ROOT_ID__ .sg-home-floating {
      position: absolute;
      left: 20px;
      bottom: 20px;
      z-index: 20;
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
    }

    #__ROOT_ID__ .sg-home-floating:hover {
      background: rgba(234, 88, 12, 0.98);
      color: white;
    }

    #__ROOT_ID__ .sg-panel h3 {
      margin-top: 0;
      margin-bottom: 10px;
      font-size: 18px;
    }

    #__ROOT_ID__ .sg-panel p {
      font-size: 12px;
      line-height: 1.35;
      color: #cbd5e1;
      margin-bottom: 12px;
    }

    #__ROOT_ID__ .sg-button {
      width: 100%;
      border: 0;
      border-radius: 10px;
      padding: 10px;
      background: #2563eb;
      color: white;
      cursor: pointer;
      font-weight: bold;
      margin-top: 6px;
    }

    #__ROOT_ID__ .sg-button:hover {
      background: #1d4ed8;
    }

    #__ROOT_ID__ .sg-status,
    #__ROOT_ID__ .sg-clock {
      margin-top: 8px;
      padding: 8px;
      border-radius: 10px;
      background: rgba(15, 23, 42, 0.78);
      border: 1px solid rgba(148, 163, 184, 0.22);
      font-size: 11px;
      color: #e5e7eb;
    }

    #__ROOT_ID__ .sg-clock {
      color: #93c5fd;
      font-weight: bold;
      font-size: 12px;
    }

    #__ROOT_ID__ .visual-control {
      margin-top: 8px;
      font-size: 12px;
    }

    #__ROOT_ID__ .visual-control label {
      display: block;
      margin-bottom: 4px;
      color: #e2e8f0;
    }

    #__ROOT_ID__ .visual-control input[type=range] {
      width: 100%;
    }

    #__ROOT_ID__ .sg-select {
      width: 100%;
      border-radius: 10px;
      border: 1px solid rgba(148, 163, 184, 0.35);
      padding: 8px 10px;
      background: rgba(15, 23, 42, 0.92);
      color: white;
      font-size: 12px;
    }

    #__ROOT_ID__ .sg-reference-note {
      margin-top: 7px;
      padding: 7px 9px;
      border-radius: 10px;
      background: rgba(15, 23, 42, 0.62);
      border: 1px solid rgba(148, 163, 184, 0.16);
      color: #cbd5e1;
      font-size: 11px;
      line-height: 1.35;
    }

    #__ROOT_ID__ .dot {
      width: 10px;
      height: 10px;
      border-radius: 999px;
      display: inline-block;
    }

    #__ROOT_ID__ .visibility-matrix {
      width: 100%;
      border-collapse: collapse;
      font-size: 10.5px;
      margin-top: 8px;
    }

    #__ROOT_ID__ .visibility-matrix th,
    #__ROOT_ID__ .visibility-matrix td {
      border-bottom: 1px solid rgba(148, 163, 184, 0.22);
      padding: 4px 3px;
      text-align: center;
      white-space: nowrap;
    }

    #__ROOT_ID__ .visibility-matrix th:first-child,
    #__ROOT_ID__ .visibility-matrix td:first-child {
      text-align: left;
    }

    #__ROOT_ID__ .visibility-matrix th {
      color: #93c5fd;
    }

    #__ROOT_ID__ .sat-table {
      margin-top: 10px;
      width: 100%;
      border-collapse: collapse;
      font-size: 10.2px;
    }

    #__ROOT_ID__ .sat-table th,
    #__ROOT_ID__ .sat-table td {
      border-bottom: 1px solid rgba(148, 163, 184, 0.22);
      padding: 4px 3px;
      text-align: left;
      white-space: nowrap;
    }

    #__ROOT_ID__ .sat-table th {
      color: #93c5fd;
    }

    #__ROOT_ID__ .small-note {
      color: #94a3b8;
      font-size: 11px;
      line-height: 1.3;
      margin-top: 8px;
    }

    #__ROOT_ID__ .section-title {
      margin-top: 12px;
      margin-bottom: 4px;
      font-size: 13px;
      color: #bfdbfe;
      font-weight: bold;
    }

    @media (max-width: 760px) {
      #__ROOT_ID__ .sg-panel {
        width: calc(100vw - 70px);
      }

      #__ROOT_ID__ .sg-menu-toggle {
        left: calc(100vw - 70px);
      }

      #__ROOT_ID__ .sg-panel.hidden-panel {
        transform: translateX(calc(-100vw));
      }

      #__ROOT_ID__ .sg-menu-toggle.panel-hidden {
        left: 20px;
      }
    }
  )"

  gsub("__ROOT_ID__", root_id, css, fixed = TRUE)
}


satellite_globe_js <- function(
    root_id,
    globe_id,
    label_layer_id,
    panel_id,
    menu_toggle_id,
    toggle_animation_id,
    reload_tle_id,
    orbit_width_id,
    lighting_mode_id,
    reference_mode_id,
    reference_note_id,
    status_id,
    clock_id,
    table_body_id,
    ns_prefix
) {
  js <- r"(
    (function() {
      const ROOT_ID = '__ROOT_ID__';
      const GLOBE_ID = '__GLOBE_ID__';
      const LABEL_LAYER_ID = '__LABEL_LAYER_ID__';
      const PANEL_ID = '__PANEL_ID__';
      const MENU_TOGGLE_ID = '__MENU_TOGGLE_ID__';
      const TOGGLE_ANIMATION_ID = '__TOGGLE_ANIMATION_ID__';
      const RELOAD_TLE_ID = '__RELOAD_TLE_ID__';
      const ORBIT_WIDTH_ID = '__ORBIT_WIDTH_ID__';
      const LIGHTING_MODE_ID = '__LIGHTING_MODE_ID__';
      const REFERENCE_MODE_ID = '__REFERENCE_MODE_ID__';
      const REFERENCE_NOTE_ID = '__REFERENCE_NOTE_ID__';
      const STATUS_ID = '__STATUS_ID__';
      const CLOCK_ID = '__CLOCK_ID__';
      const TABLE_BODY_ID = '__TABLE_BODY_ID__';
      const NS_PREFIX = '__NS_PREFIX__';

      function byId(id) {
        return document.getElementById(id);
      }

      function nsId(id) {
        return NS_PREFIX + id;
      }

      let scene, camera, renderer, controls;
      let referenceGroup, earthGroup;
      let ambientLight, sunLight, blueFillLight;
      let satellitesData = [];
      let animationRunning = true;

      let lastSatelliteUpdate = 0;
      let lastTableUpdate = 0;

      const SAT_UPDATE_MS = 250;
      const TABLE_UPDATE_MS = 1000;

      const EARTH_RADIUS_KM = 6371.0;
      const EARTH_OBLIQUITY_DEG = 23.439281;

      const earthRadiusScene = 2.0;
      const kmToScene = earthRadiusScene / EARTH_RADIUS_KM;

      let currentReferenceAngle = THREE.MathUtils.degToRad(EARTH_OBLIQUITY_DEG);
      let targetReferenceAngle = currentReferenceAngle;

      const tleURLs = [
        {
          name: 'resource',
          url: 'https://celestrak.org/NORAD/elements/gp.php?GROUP=resource&FORMAT=tle'
        },
        {
          name: 'weather',
          url: 'https://celestrak.org/NORAD/elements/gp.php?GROUP=weather&FORMAT=tle'
        }
      ];

      const desiredSatellites = [
        {
          key: 'Sentinel2A',
          displayName: 'Sentinel-2A',
          searchName: 'SENTINEL-2A',
          type: 'LEO',
          swathKm: 290,
          color: 0xff4d4d,
          tleGroup: 'resource'
        },
        {
          key: 'Sentinel2B',
          displayName: 'Sentinel-2B',
          searchName: 'SENTINEL-2B',
          type: 'LEO',
          swathKm: 290,
          color: 0xff9f43,
          tleGroup: 'resource'
        },
        {
          key: 'Landsat8',
          displayName: 'Landsat 8',
          searchName: 'LANDSAT 8',
          type: 'LEO',
          swathKm: 185,
          color: 0x00d2d3,
          tleGroup: 'resource'
        },
        {
          key: 'Landsat9',
          displayName: 'Landsat 9',
          searchName: 'LANDSAT 9',
          type: 'LEO',
          swathKm: 185,
          color: 0x54a0ff,
          tleGroup: 'resource'
        },
        {
          key: 'GOES16',
          displayName: 'GOES-16',
          searchName: 'GOES 16',
          type: 'GEO',
          swathKm: null,
          color: 0xa78bfa,
          tleGroup: 'weather'
        },
        {
          key: 'GOES18',
          displayName: 'GOES-18',
          searchName: 'GOES 18',
          type: 'GEO',
          swathKm: null,
          color: 0xf472b6,
          tleGroup: 'weather'
        },
        {
          key: 'GOES19',
          displayName: 'GOES-19',
          searchName: 'GOES 19',
          type: 'GEO',
          swathKm: null,
          color: 0xfacc15,
          tleGroup: 'weather'
        }
      ];

      function init() {
        const container = byId(GLOBE_ID);

        if (!container) {
          return;
        }

        if (container.dataset.initialized === 'true') {
          return;
        }

        container.dataset.initialized = 'true';

        scene = new THREE.Scene();

        referenceGroup = new THREE.Group();
        referenceGroup.rotation.x = currentReferenceAngle;
        scene.add(referenceGroup);

        const w = container.clientWidth || window.innerWidth;
        const h = container.clientHeight || window.innerHeight;

        camera = new THREE.PerspectiveCamera(
          45,
          w / h,
          0.1,
          1000
        );

        camera.position.set(0, 2.2, 10);

        renderer = new THREE.WebGLRenderer({
          antialias: true,
          alpha: true
        });

        renderer.setPixelRatio(window.devicePixelRatio);
        renderer.setSize(w, h);
        container.appendChild(renderer.domElement);

        controls = new THREE.OrbitControls(camera, renderer.domElement);
        controls.enableDamping = true;
        controls.dampingFactor = 0.05;
        controls.rotateSpeed = 0.45;
        controls.zoomSpeed = 0.8;
        controls.minDistance = 2.6;
        controls.maxDistance = 60;

        addLights();
        createEarth();
        updateReferenceMode();
        updateSunLighting(new Date());

        const menuButton = byId(MENU_TOGGLE_ID);
        if (menuButton) {
          menuButton.addEventListener('click', function() {
            const panel = byId(PANEL_ID);
            const button = byId(MENU_TOGGLE_ID);

            if (!panel || !button) {
              return;
            }

            panel.classList.toggle('hidden-panel');
            button.classList.toggle('panel-hidden');

            if (panel.classList.contains('hidden-panel')) {
              button.innerText = '☰';
            } else {
              button.innerText = '×';
            }
          });
        }

        const toggleButton = byId(TOGGLE_ANIMATION_ID);
        if (toggleButton) {
          toggleButton.addEventListener('click', function() {
            animationRunning = !animationRunning;
            this.innerText = animationRunning ? 'Pausar animación' : 'Activar animación';
          });
        }

        const reloadButton = byId(RELOAD_TLE_ID);
        if (reloadButton) {
          reloadButton.addEventListener('click', function() {
            loadTLEs();
          });
        }

        const lightingModeInput = byId(LIGHTING_MODE_ID);
        if (lightingModeInput) {
          lightingModeInput.addEventListener('change', function() {
            updateSunLighting(new Date());
          });
        }

        const referenceModeInput = byId(REFERENCE_MODE_ID);
        if (referenceModeInput) {
          referenceModeInput.addEventListener('change', function() {
            updateReferenceMode();
            updateSunLighting(new Date());
            updateAllSatelliteLabels();
          });
        }

        desiredSatellites.forEach(function(spec) {
          const elementNames = ['sat', 'orbit', 'coverage', 'label'];
          const allEl = byId(nsId('vis_' + spec.key + '_all'));

          if (allEl) {
            allEl.addEventListener('change', function() {
              elementNames.forEach(function(elementName) {
                const el = byId(nsId('vis_' + spec.key + '_' + elementName));

                if (el) {
                  el.checked = allEl.checked;
                }
              });

              updateVisibility();
              updateTable();
              updateAllSatelliteLabels();
            });
          }

          elementNames.forEach(function(elementName) {
            const el = byId(nsId('vis_' + spec.key + '_' + elementName));

            if (el) {
              el.addEventListener('change', function() {
                if (allEl) {
                  allEl.checked = elementNames.every(function(name) {
                    const child = byId(nsId('vis_' + spec.key + '_' + name));
                    return child ? child.checked : false;
                  });
                }

                updateVisibility();
                updateTable();
                updateAllSatelliteLabels();
              });
            }
          });
        });

        const orbitWidthInput = byId(ORBIT_WIDTH_ID);
        if (orbitWidthInput) {
          orbitWidthInput.addEventListener('input', function() {
            rebuildOrbitLines();
          });
        }

        window.addEventListener('resize', onWindowResize);

        loadTLEs();
        animate();
      }

      function setStatus(msg) {
        const status = byId(STATUS_ID);

        if (status) {
          status.innerText = msg;
        }
      }

      function disposeObject3D(obj) {
        if (!obj) return;

        obj.traverse(function(child) {
          if (child.geometry) {
            child.geometry.dispose();
          }

          if (child.material) {
            if (Array.isArray(child.material)) {
              child.material.forEach(function(mat) {
                mat.dispose();
              });
            } else {
              child.material.dispose();
            }
          }
        });
      }

      async function loadTLEs() {
        setStatus('Descargando TLEs desde CelesTrak...');

        try {
          const tleByGroup = {};

          for (const group of tleURLs) {
            const response = await fetch(group.url, { cache: 'no-store' });

            if (!response.ok) {
              throw new Error(group.name + ': HTTP ' + response.status);
            }

            const text = await response.text();

            tleByGroup[group.name] = text
              .split('\n')
              .map(x => x.trim())
              .filter(x => x.length > 0);
          }

          clearSatellites();

          desiredSatellites.forEach(function(spec) {
            const lines = tleByGroup[spec.tleGroup] || [];
            const found = findTLE(lines, spec.searchName);

            if (found) {
              createSatelliteFromTLE(spec, found.name, found.line1, found.line2);
            } else {
              console.warn('No encontrado:', spec.searchName);
            }
          });

          if (satellitesData.length === 0) {
            setStatus('No se encontraron TLEs. Revisa conexión o nombres disponibles en CelesTrak.');
          } else {
            setStatus(
              'TLEs cargados online: ' +
              satellitesData.length +
              ' satélites. Última carga: ' +
              new Date().toLocaleTimeString()
            );
          }

          updateVisibility();
          updateTable();
          updateAllSatelliteLabels();

        } catch (err) {
          console.error(err);
          setStatus('Error cargando TLE online: ' + err.message);
        }
      }

      function findTLE(lines, targetName) {
        const target = targetName.toUpperCase();

        for (let i = 0; i < lines.length - 2; i++) {
          const name = lines[i].toUpperCase();

          if (
            name.includes(target) &&
            lines[i + 1].startsWith('1 ') &&
            lines[i + 2].startsWith('2 ')
          ) {
            return {
              name: lines[i],
              line1: lines[i + 1],
              line2: lines[i + 2]
            };
          }
        }

        return null;
      }

      function clearSatellites() {
        satellitesData.forEach(function(sat) {
          referenceGroup.remove(sat.mesh);
          referenceGroup.remove(sat.orbitLine);

          disposeObject3D(sat.mesh);
          disposeObject3D(sat.orbitLine);

          if (sat.footprintObject) {
            earthGroup.remove(sat.footprintObject);
            disposeObject3D(sat.footprintObject);
          }

          if (sat.coneObject) {
            referenceGroup.remove(sat.coneObject);
            disposeObject3D(sat.coneObject);
          }

          if (sat.label && sat.label.parentNode) {
            sat.label.parentNode.removeChild(sat.label);
          }
        });

        satellitesData = [];

        const tbody = byId(TABLE_BODY_ID);
        if (tbody) {
          tbody.innerHTML = '';
        }
      }

      function createSatelliteFromTLE(spec, tleName, line1, line2) {
        const satrec = satellite.twoline2satrec(line1, line2);

        const mesh = createSatelliteMesh(spec.color, spec.type);
        referenceGroup.add(mesh);

        const orbitLine = createOrbitLineFromTLE(satrec, spec.color, spec.type);
        referenceGroup.add(orbitLine);

        const footprintObject = new THREE.Group();
        earthGroup.add(footprintObject);

        const coneObject = new THREE.Group();
        referenceGroup.add(coneObject);

        const label = document.createElement('div');
        label.className = 'sat-label';
        label.innerText = spec.displayName;
        label.style.color = '#' + spec.color.toString(16).padStart(6, '0');

        const labelLayer = byId(LABEL_LAYER_ID);
        if (labelLayer) {
          labelLayer.appendChild(label);
        }

        satellitesData.push({
          key: spec.key,
          displayName: spec.displayName,
          tleName: tleName,
          type: spec.type,
          swathKm: spec.swathKm,
          color: spec.color,
          satrec: satrec,
          mesh: mesh,
          orbitLine: orbitLine,
          footprintObject: footprintObject,
          coneObject: coneObject,
          label: label,
          lat: null,
          lon: null,
          altKm: null,
          surfaceDistanceKm: null,
          orbitalRadiusKm: null
        });
      }

      function addLights() {
        ambientLight = new THREE.AmbientLight(0xffffff, 0.06);
        scene.add(ambientLight);

        sunLight = new THREE.DirectionalLight(0xffffff, 2.15);
        sunLight.position.set(5, 3, 5);
        sunLight.target.position.set(0, 0, 0);
        scene.add(sunLight);
        scene.add(sunLight.target);

        blueFillLight = new THREE.PointLight(0x60a5fa, 0.00, 30);
        blueFillLight.position.set(-5, -2, -4);
        scene.add(blueFillLight);
      }

      function getJulianDate(date) {
        return date.getTime() / 86400000 + 2440587.5;
      }

      function normalizeAngleDeg(angle) {
        let out = angle % 360;
        if (out < 0) out += 360;
        return out;
      }

      function getSunEciUnitVector(date) {
        const jd = getJulianDate(date);
        const n = jd - 2451545.0;

        const meanLongitudeDeg = normalizeAngleDeg(280.460 + 0.9856474 * n);
        const meanAnomalyDeg = normalizeAngleDeg(357.528 + 0.9856003 * n);

        const g = THREE.MathUtils.degToRad(meanAnomalyDeg);

        const eclipticLongitudeDeg =
          meanLongitudeDeg +
          1.915 * Math.sin(g) +
          0.020 * Math.sin(2 * g);

        const lambda = THREE.MathUtils.degToRad(
          normalizeAngleDeg(eclipticLongitudeDeg)
        );

        const epsilon = THREE.MathUtils.degToRad(
          23.439 - 0.0000004 * n
        );

        const x = Math.cos(lambda);
        const y = Math.cos(epsilon) * Math.sin(lambda);
        const z = Math.sin(epsilon) * Math.sin(lambda);

        return new THREE.Vector3(x, y, z).normalize();
      }

      function eciUnitToSceneVector(v) {
        return new THREE.Vector3(
          v.x,
          -v.z,
          -v.y
        ).normalize();
      }

      function updateSunLighting(date) {
        if (!sunLight || !ambientLight || !blueFillLight) {
          return;
        }

        const sunEci = getSunEciUnitVector(date);
        let sunScene = eciUnitToSceneVector(sunEci);

        if (Math.abs(currentReferenceAngle) > 0.00001) {
          const q = new THREE.Quaternion();

          q.setFromEuler(
            new THREE.Euler(
              currentReferenceAngle,
              0,
              0
            )
          );

          sunScene.applyQuaternion(q);
        }

        sunScene.normalize();

        sunLight.position.copy(
          sunScene.clone().multiplyScalar(20)
        );

        sunLight.target.position.set(0, 0, 0);
        sunLight.target.updateMatrixWorld();

        const lightingModeInput = byId(LIGHTING_MODE_ID);
        const lightingMode = lightingModeInput ? lightingModeInput.value : 'real_only';

        if (lightingMode === 'real_only') {
          ambientLight.intensity = 0.06;
          sunLight.intensity = 2.15;
          blueFillLight.intensity = 0.00;
        } else {
          ambientLight.intensity = 0.28;
          sunLight.intensity = 1.75;
          blueFillLight.intensity = 0.25;
        }
      }

      function updateReferenceMode() {
        if (!referenceGroup) {
          return;
        }

        const referenceModeInput = byId(REFERENCE_MODE_ID);
        const referenceMode = referenceModeInput ? referenceModeInput.value : 'tilted';
        const referenceNote = byId(REFERENCE_NOTE_ID);

        if (referenceMode === 'tilted') {
          targetReferenceAngle = THREE.MathUtils.degToRad(EARTH_OBLIQUITY_DEG);

          if (referenceNote) {
            referenceNote.innerText =
              'Referencia: plano solar/eclíptico. La Tierra se muestra inclinada aproximadamente 23.44° respecto del plano de iluminación.';
          }
        } else {
          targetReferenceAngle = 0;

          if (referenceNote) {
            referenceNote.innerText =
              'Referencia: plano ecuatorial terrestre. La Tierra se muestra derecha, útil para latitud, longitud, órbitas y coberturas.';
          }
        }
      }

      function updateReferenceTransition() {
        if (!referenceGroup) {
          return;
        }

        const diff = targetReferenceAngle - currentReferenceAngle;

        if (Math.abs(diff) < 0.0001) {
          currentReferenceAngle = targetReferenceAngle;
        } else {
          currentReferenceAngle += diff * 0.08;
        }

        referenceGroup.rotation.x = currentReferenceAngle;
        referenceGroup.updateMatrixWorld(true);
      }

      function createEarth() {
        earthGroup = new THREE.Group();
        referenceGroup.add(earthGroup);

        const textureLoader = new THREE.TextureLoader();
        textureLoader.crossOrigin = '';

        const earthTexture = textureLoader.load(
          'https://raw.githubusercontent.com/turban/webgl-earth/master/images/2_no_clouds_4k.jpg',
          function(texture) {
            console.log('Textura de la Tierra cargada correctamente.');
          },
          undefined,
          function(err) {
            console.warn('No se pudo cargar la textura de la Tierra.', err);
          }
        );

        const earthGeometry = new THREE.SphereGeometry(
          earthRadiusScene,
          96,
          96
        );

        const earthMaterial = new THREE.MeshPhongMaterial({
          map: earthTexture,
          color: 0xffffff,
          shininess: 18
        });

        const earth = new THREE.Mesh(earthGeometry, earthMaterial);
        earthGroup.add(earth);

        const atmosphereGeometry = new THREE.SphereGeometry(
          earthRadiusScene * 1.018,
          96,
          96
        );

        const atmosphereMaterial = new THREE.MeshPhongMaterial({
          color: 0x4da3ff,
          transparent: true,
          opacity: 0.12,
          shininess: 20
        });

        const atmosphere = new THREE.Mesh(
          atmosphereGeometry,
          atmosphereMaterial
        );

        earthGroup.add(atmosphere);

        createGridLines();
        createEarthAxis();
      }

      function createEarthAxis() {
        const axisLength = earthRadiusScene * 2.9;

        const points = [
          new THREE.Vector3(0, -axisLength / 2, 0),
          new THREE.Vector3(0, axisLength / 2, 0)
        ];

        const geometry = new THREE.BufferGeometry().setFromPoints(points);

        const material = new THREE.LineBasicMaterial({
          color: 0xffffff,
          transparent: true,
          opacity: 0.9
        });

        const axisLine = new THREE.Line(geometry, material);
        earthGroup.add(axisLine);

        const poleMaterial = new THREE.MeshBasicMaterial({
          color: 0xffffff
        });

        const northPole = new THREE.Mesh(
          new THREE.SphereGeometry(0.045, 16, 16),
          poleMaterial
        );

        northPole.position.set(0, earthRadiusScene * 1.08, 0);
        earthGroup.add(northPole);

        const southPole = new THREE.Mesh(
          new THREE.SphereGeometry(0.045, 16, 16),
          poleMaterial
        );

        southPole.position.set(0, -earthRadiusScene * 1.08, 0);
        earthGroup.add(southPole);

        createAxisLabel('N', new THREE.Vector3(0, earthRadiusScene * 1.25, 0));
        createAxisLabel('S', new THREE.Vector3(0, -earthRadiusScene * 1.25, 0));
      }

      function createAxisLabel(text, position) {
        const canvas = document.createElement('canvas');
        canvas.width = 128;
        canvas.height = 128;

        const ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, 128, 128);

        ctx.fillStyle = 'rgba(255, 255, 255, 0.95)';
        ctx.font = 'bold 72px Arial';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(text, 64, 64);

        const texture = new THREE.CanvasTexture(canvas);

        const material = new THREE.SpriteMaterial({
          map: texture,
          transparent: true
        });

        const sprite = new THREE.Sprite(material);
        sprite.position.copy(position);
        sprite.scale.set(0.28, 0.28, 0.28);

        earthGroup.add(sprite);
      }

      function latLonToVector3(lat, lon, radius) {
        const phi = THREE.MathUtils.degToRad(90 - lat);
        const theta = THREE.MathUtils.degToRad(lon + 180);

        const x = -radius * Math.sin(phi) * Math.cos(theta);
        const z = radius * Math.sin(phi) * Math.sin(theta);
        const y = radius * Math.cos(phi);

        return new THREE.Vector3(x, y, z);
      }

      function eciKmToSceneVector(positionEciKm) {
        return new THREE.Vector3(
          positionEciKm.x * kmToScene,
          positionEciKm.z * kmToScene,
          -positionEciKm.y * kmToScene
        );
      }

      function createGridLines() {
        const gridMaterial = new THREE.LineBasicMaterial({
          color: 0xffffff,
          transparent: true,
          opacity: 0.13
        });

        for (let lat = -60; lat <= 60; lat += 30) {
          const points = [];
          const latRad = THREE.MathUtils.degToRad(lat);
          const r = earthRadiusScene * Math.cos(latRad);
          const y = earthRadiusScene * Math.sin(latRad);

          for (let i = 0; i <= 360; i += 4) {
            const lonRad = THREE.MathUtils.degToRad(i);
            points.push(
              new THREE.Vector3(
                r * Math.cos(lonRad),
                y,
                r * Math.sin(lonRad)
              )
            );
          }

          const geometry = new THREE.BufferGeometry().setFromPoints(points);
          earthGroup.add(new THREE.Line(geometry, gridMaterial));
        }

        for (let lon = 0; lon < 180; lon += 30) {
          const meridian = new THREE.Group();
          const points = [];

          for (let lat = -90; lat <= 90; lat += 4) {
            const latRad = THREE.MathUtils.degToRad(lat);

            points.push(
              new THREE.Vector3(
                earthRadiusScene * Math.cos(latRad),
                earthRadiusScene * Math.sin(latRad),
                0
              )
            );
          }

          const geometry = new THREE.BufferGeometry().setFromPoints(points);
          const line = new THREE.Line(geometry, gridMaterial);

          meridian.add(line);
          meridian.rotation.y = THREE.MathUtils.degToRad(lon);
          earthGroup.add(meridian);
        }
      }

      function createSatelliteMesh(color, type) {
        const group = new THREE.Group();

        const scale = type === 'GEO' ? 1.5 : 1.0;

        const body = new THREE.Mesh(
          new THREE.BoxGeometry(0.075 * scale, 0.052 * scale, 0.052 * scale),
          new THREE.MeshStandardMaterial({
            color: color,
            metalness: 0.45,
            roughness: 0.35
          })
        );

        group.add(body);

        const panelMaterial = new THREE.MeshStandardMaterial({
          color: 0x1e3a8a,
          metalness: 0.2,
          roughness: 0.25
        });

        const panel1 = new THREE.Mesh(
          new THREE.BoxGeometry(0.16 * scale, 0.01 * scale, 0.075 * scale),
          panelMaterial
        );

        panel1.position.x = -0.13 * scale;
        group.add(panel1);

        const panel2 = new THREE.Mesh(
          new THREE.BoxGeometry(0.16 * scale, 0.01 * scale, 0.075 * scale),
          panelMaterial
        );

        panel2.position.x = 0.13 * scale;
        group.add(panel2);

        const halo = new THREE.Mesh(
          new THREE.SphereGeometry(0.04 * scale, 16, 16),
          new THREE.MeshBasicMaterial({
            color: color,
            transparent: true,
            opacity: 0.95
          })
        );

        group.add(halo);

        return group;
      }

      function createOrbitLineFromTLE(satrec, color, type) {
        const points = [];
        const now = new Date();

        const startMin = type === 'GEO' ? -720 : -55;
        const endMin = type === 'GEO' ? 720 : 60;
        const stepMin = type === 'GEO' ? 30 : 2;

        for (let minutes = startMin; minutes <= endMin; minutes += stepMin) {
          const date = new Date(now.getTime() + minutes * 60 * 1000);
          const pv = satellite.propagate(satrec, date);

          if (pv && pv.position) {
            points.push(eciKmToSceneVector(pv.position));
          }
        }

        if (points.length < 2) {
          return new THREE.Group();
        }

        const widthInput = byId(ORBIT_WIDTH_ID);
        const widthValue = widthInput ? parseFloat(widthInput.value) : 2;

        const radius = 0.0025 * widthValue;

        const curve = new THREE.CatmullRomCurve3(points);

        const geometry = new THREE.TubeGeometry(
          curve,
          Math.max(32, points.length * 3),
          radius,
          8,
          false
        );

        const material = new THREE.MeshBasicMaterial({
          color: color,
          transparent: true,
          opacity: type === 'GEO' ? 0.24 : 0.42
        });

        return new THREE.Mesh(geometry, material);
      }

      function rebuildOrbitLines() {
        satellitesData.forEach(function(sat) {
          if (sat.orbitLine) {
            referenceGroup.remove(sat.orbitLine);
            disposeObject3D(sat.orbitLine);
          }

          sat.orbitLine = createOrbitLineFromTLE(
            sat.satrec,
            sat.color,
            sat.type
          );

          referenceGroup.add(sat.orbitLine);
        });

        updateVisibility();
      }

      function getSatelliteState(satrec, date) {
        const pv = satellite.propagate(satrec, date);

        if (!pv || !pv.position) {
          return null;
        }

        const gmst = satellite.gstime(date);
        const gd = satellite.eciToGeodetic(pv.position, gmst);

        const lat = satellite.degreesLat(gd.latitude);
        const lon = satellite.degreesLong(gd.longitude);
        const altKm = gd.height;

        return {
          positionEciKm: pv.position,
          lat: lat,
          lon: lon,
          altKm: altKm,
          surfaceDistanceKm: altKm,
          orbitalRadiusKm: EARTH_RADIUS_KM + altKm
        };
      }

      function normalizeLon(lon) {
        let out = lon;
        while (out > 180) out -= 360;
        while (out < -180) out += 360;
        return out;
      }

      function destinationPoint(lat, lon, bearingDeg, angularDistanceRad) {
        const lat1 = THREE.MathUtils.degToRad(lat);
        const lon1 = THREE.MathUtils.degToRad(lon);
        const brng = THREE.MathUtils.degToRad(bearingDeg);
        const d = angularDistanceRad;

        const lat2 = Math.asin(
          Math.sin(lat1) * Math.cos(d) +
          Math.cos(lat1) * Math.sin(d) * Math.cos(brng)
        );

        const lon2 = lon1 + Math.atan2(
          Math.sin(brng) * Math.sin(d) * Math.cos(lat1),
          Math.cos(d) - Math.sin(lat1) * Math.sin(lat2)
        );

        return {
          lat: THREE.MathUtils.radToDeg(lat2),
          lon: normalizeLon(THREE.MathUtils.radToDeg(lon2))
        };
      }

      function clearGroup(group) {
        while (group.children.length > 0) {
          const child = group.children[0];
          group.remove(child);
          disposeObject3D(child);
        }
      }

      function getFootprintAngularRadiusReal(sat) {
        if (sat.altKm === null) {
          return null;
        }

        if (sat.type === 'GEO') {
          const r = EARTH_RADIUS_KM + sat.altKm;
          return Math.acos(EARTH_RADIUS_KM / r);
        }

        return (sat.swathKm / 2) / EARTH_RADIUS_KM;
      }

      function getFootprintAreaKm2(sat) {
        const theta = getFootprintAngularRadiusReal(sat);

        if (theta === null) {
          return null;
        }

        return 2 * Math.PI * EARTH_RADIUS_KM * EARTH_RADIUS_KM * (1 - Math.cos(theta));
      }

      function getEarthSurfaceAreaKm2() {
        return 4 * Math.PI * EARTH_RADIUS_KM * EARTH_RADIUS_KM;
      }

      function getFootprintPercentEarth(sat) {
        const area = getFootprintAreaKm2(sat);

        if (area === null) {
          return null;
        }

        return 100 * area / getEarthSurfaceAreaKm2();
      }

      function updateFootprint(sat, satLocalPosition) {
        clearGroup(sat.footprintObject);
        clearGroup(sat.coneObject);

        if (sat.lat === null || sat.lon === null || sat.altKm === null) {
          return;
        }

        let angularRadiusReal;

        if (sat.type === 'GEO') {
          const r = EARTH_RADIUS_KM + sat.altKm;
          angularRadiusReal = Math.acos(EARTH_RADIUS_KM / r);
        } else {
          angularRadiusReal = (sat.swathKm / 2) / EARTH_RADIUS_KM;
        }

        const minVisualAngularRadius =
          sat.type === 'LEO'
            ? THREE.MathUtils.degToRad(2.2)
            : 0;

        const angularRadius = Math.max(
          angularRadiusReal,
          minVisualAngularRadius
        );

        const surfaceRadius = earthRadiusScene * 1.022;

        const centerLocal = latLonToVector3(
          sat.lat,
          sat.lon,
          surfaceRadius
        );

        const ringLocalPoints = [];
        const fillPositions = [];
        const fillIndices = [];

        fillPositions.push(centerLocal.x, centerLocal.y, centerLocal.z);

        const stepDeg = sat.type === 'GEO' ? 4 : 8;

        for (let b = 0; b < 360; b += stepDeg) {
          const p = destinationPoint(
            sat.lat,
            sat.lon,
            b,
            angularRadius
          );

          const v = latLonToVector3(
            p.lat,
            p.lon,
            surfaceRadius
          );

          ringLocalPoints.push(v);
          fillPositions.push(v.x, v.y, v.z);
        }

        for (let i = 1; i <= ringLocalPoints.length; i++) {
          const next = i === ringLocalPoints.length ? 1 : i + 1;
          fillIndices.push(0, i, next);
        }

        const fillGeometry = new THREE.BufferGeometry();
        fillGeometry.setAttribute(
          'position',
          new THREE.Float32BufferAttribute(fillPositions, 3)
        );
        fillGeometry.setIndex(fillIndices);
        fillGeometry.computeVertexNormals();

        const fillMaterial = new THREE.MeshBasicMaterial({
          color: sat.color,
          transparent: true,
          opacity: sat.type === 'GEO' ? 0.13 : 0.32,
          side: THREE.DoubleSide,
          depthWrite: false
        });

        const fillMesh = new THREE.Mesh(fillGeometry, fillMaterial);
        sat.footprintObject.add(fillMesh);

        const curve = new THREE.CatmullRomCurve3(
          ringLocalPoints,
          true
        );

        const ringGeometry = new THREE.TubeGeometry(
          curve,
          Math.max(32, ringLocalPoints.length * 3),
          sat.type === 'GEO' ? 0.007 : 0.014,
          8,
          true
        );

        const ringMaterial = new THREE.MeshBasicMaterial({
          color: sat.color,
          transparent: true,
          opacity: sat.type === 'GEO' ? 0.88 : 0.98,
          depthWrite: false
        });

        const ringMesh = new THREE.Mesh(ringGeometry, ringMaterial);
        sat.footprintObject.add(ringMesh);

        const ringReferencePoints = ringLocalPoints.map(function(p) {
          const worldPoint = earthGroup.localToWorld(p.clone());
          return referenceGroup.worldToLocal(worldPoint);
        });

        const conePositions = [];
        const coneIndices = [];

        conePositions.push(
          satLocalPosition.x,
          satLocalPosition.y,
          satLocalPosition.z
        );

        ringReferencePoints.forEach(function(p) {
          conePositions.push(p.x, p.y, p.z);
        });

        for (let i = 1; i <= ringReferencePoints.length; i++) {
          const next = i < ringReferencePoints.length ? i + 1 : 1;
          coneIndices.push(0, i, next);
        }

        const coneGeometry = new THREE.BufferGeometry();
        coneGeometry.setAttribute(
          'position',
          new THREE.Float32BufferAttribute(conePositions, 3)
        );
        coneGeometry.setIndex(coneIndices);
        coneGeometry.computeVertexNormals();

        const coneMaterial = new THREE.MeshBasicMaterial({
          color: sat.color,
          transparent: true,
          opacity: sat.type === 'GEO' ? 0.10 : 0.20,
          side: THREE.DoubleSide,
          depthWrite: false
        });

        const coneMesh = new THREE.Mesh(coneGeometry, coneMaterial);
        sat.coneObject.add(coneMesh);
      }

      function isElementVisible(sat, elementName) {
        const el = byId(nsId('vis_' + sat.key + '_' + elementName));
        return el ? el.checked : false;
      }

      function updateSatellites(date) {
        satellitesData.forEach(function(sat) {
          const anyVisible =
            isElementVisible(sat, 'sat') ||
            isElementVisible(sat, 'orbit') ||
            isElementVisible(sat, 'coverage') ||
            isElementVisible(sat, 'label');

          if (!anyVisible) {
            sat.mesh.visible = false;
            sat.orbitLine.visible = false;
            sat.footprintObject.visible = false;

            if (sat.coneObject) {
              sat.coneObject.visible = false;
            }

            if (sat.label) {
              sat.label.style.display = 'none';
            }

            return;
          }

          const state = getSatelliteState(sat.satrec, date);

          if (!state) {
            return;
          }

          sat.lat = state.lat;
          sat.lon = state.lon;
          sat.altKm = state.altKm;
          sat.surfaceDistanceKm = state.surfaceDistanceKm;
          sat.orbitalRadiusKm = state.orbitalRadiusKm;

          const xyz = eciKmToSceneVector(state.positionEciKm);

          sat.mesh.position.copy(xyz);
          sat.mesh.lookAt(0, 0, 0);
          sat.mesh.rotateY(Math.PI / 2);

          updateFootprint(sat, xyz);

          const coverageVisible = isElementVisible(sat, 'coverage');

          sat.mesh.visible = isElementVisible(sat, 'sat');
          sat.orbitLine.visible = isElementVisible(sat, 'orbit');
          sat.footprintObject.visible = coverageVisible;

          if (sat.coneObject) {
            sat.coneObject.visible = coverageVisible;
          }
        });
      }

      function updateVisibility() {
        satellitesData.forEach(function(sat) {
          const coverageVisible = isElementVisible(sat, 'coverage');

          sat.mesh.visible = isElementVisible(sat, 'sat');
          sat.orbitLine.visible = isElementVisible(sat, 'orbit');
          sat.footprintObject.visible = coverageVisible;

          if (sat.coneObject) {
            sat.coneObject.visible = coverageVisible;
          }

          if (sat.label && !isElementVisible(sat, 'label')) {
            sat.label.style.display = 'none';
          }
        });
      }

      function isSatelliteOccludedByEarth(sat) {
        const cameraPos = camera.position.clone();

        const satPos = new THREE.Vector3();
        sat.mesh.getWorldPosition(satPos);

        const earthCenter = new THREE.Vector3();
        earthGroup.getWorldPosition(earthCenter);

        const earthRadius = earthRadiusScene * 1.03;

        const direction = satPos.clone().sub(cameraPos);
        const distanceToSat = direction.length();

        if (distanceToSat <= 0) {
          return false;
        }

        direction.normalize();

        const ray = new THREE.Ray(cameraPos, direction);

        const sphere = new THREE.Sphere(earthCenter, earthRadius);
        const intersection = new THREE.Vector3();

        const hit = ray.intersectSphere(sphere, intersection);

        if (!hit) {
          return false;
        }

        const distanceToHit = intersection.distanceTo(cameraPos);

        return distanceToHit < distanceToSat;
      }

      function updateSatelliteLabel(sat) {
        if (!sat.label) return;

        const showThisLabel = isElementVisible(sat, 'label');

        if (!showThisLabel) {
          sat.label.style.display = 'none';
          return;
        }

        if (isSatelliteOccludedByEarth(sat)) {
          sat.label.style.display = 'none';
          return;
        }

        const vector = new THREE.Vector3();
        sat.mesh.getWorldPosition(vector);
        vector.project(camera);

        if (vector.z < -1 || vector.z > 1) {
          sat.label.style.display = 'none';
          return;
        }

        const container = byId(GLOBE_ID);

        if (!container) {
          sat.label.style.display = 'none';
          return;
        }

        const rect = container.getBoundingClientRect();

        const x = (vector.x * 0.5 + 0.5) * rect.width;
        const y = (-vector.y * 0.5 + 0.5) * rect.height;

        sat.label.style.display = 'block';
        sat.label.style.left = x + 'px';
        sat.label.style.top = y + 'px';
      }

      function updateAllSatelliteLabels() {
        satellitesData.forEach(function(sat) {
          updateSatelliteLabel(sat);
        });
      }

      function updateTable() {
        const tbody = byId(TABLE_BODY_ID);

        if (!tbody) {
          return;
        }

        tbody.innerHTML = '';

        satellitesData.forEach(function(sat) {
          const anyVisible =
            isElementVisible(sat, 'sat') ||
            isElementVisible(sat, 'orbit') ||
            isElementVisible(sat, 'coverage') ||
            isElementVisible(sat, 'label');

          if (!anyVisible) {
            return;
          }

          const areaKm2 = getFootprintAreaKm2(sat);
          const percentEarth = getFootprintPercentEarth(sat);

          const tr = document.createElement('tr');

          const name = document.createElement('td');
          name.innerText = sat.displayName;

          const type = document.createElement('td');
          type.innerText = sat.type;

          const lat = document.createElement('td');
          lat.innerText = sat.lat === null ? '—' : sat.lat.toFixed(2);

          const lon = document.createElement('td');
          lon.innerText = sat.lon === null ? '—' : sat.lon.toFixed(2);

          const alt = document.createElement('td');
          alt.innerText = sat.altKm === null ? '—' : sat.altKm.toFixed(0);

          const dist = document.createElement('td');
          dist.innerText = sat.surfaceDistanceKm === null ? '—' : sat.surfaceDistanceKm.toFixed(0);

          const radius = document.createElement('td');
          radius.innerText = sat.orbitalRadiusKm === null ? '—' : sat.orbitalRadiusKm.toFixed(0);

          const area = document.createElement('td');
          area.innerText = areaKm2 === null ? '—' : areaKm2.toLocaleString(undefined, {
            maximumFractionDigits: 0
          });

          const percent = document.createElement('td');
          percent.innerText = percentEarth === null ? '—' : percentEarth.toFixed(4) + '%';

          tr.appendChild(name);
          tr.appendChild(type);
          tr.appendChild(lat);
          tr.appendChild(lon);
          tr.appendChild(alt);
          tr.appendChild(dist);
          tr.appendChild(radius);
          tr.appendChild(area);
          tr.appendChild(percent);

          tbody.appendChild(tr);
        });
      }

      function updateEarthRotation(date) {
        const gmst = satellite.gstime(date);
        earthGroup.rotation.y = gmst;
      }

      function updateClock(date) {
        const clock = byId(CLOCK_ID);

        if (!clock) {
          return;
        }

        clock.innerText =
          'UTC: ' +
          date.toISOString().replace('T', ' ').substring(0, 19);
      }

      function animate(timestamp) {
        requestAnimationFrame(animate);

        const now = new Date();

        updateClock(now);
        updateReferenceTransition();
        updateSunLighting(now);

        if (animationRunning) {
          updateEarthRotation(now);

          if (!lastSatelliteUpdate || timestamp - lastSatelliteUpdate > SAT_UPDATE_MS) {
            updateSatellites(now);
            lastSatelliteUpdate = timestamp;
          }

          if (!lastTableUpdate || timestamp - lastTableUpdate > TABLE_UPDATE_MS) {
            updateTable();
            lastTableUpdate = timestamp;
          }
        }

        if (controls) {
          controls.update();
        }

        updateAllSatelliteLabels();

        if (renderer && scene && camera) {
          renderer.render(scene, camera);
        }
      }

      function onWindowResize() {
        const container = byId(GLOBE_ID);

        if (!container || !camera || !renderer) {
          return;
        }

        const w = container.clientWidth || window.innerWidth;
        const h = container.clientHeight || window.innerHeight;

        camera.aspect = w / h;
        camera.updateProjectionMatrix();
        renderer.setSize(w, h);
        updateAllSatelliteLabels();
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
      } else {
        init();
      }
    })();
  )"

  replacements <- list(
    "__ROOT_ID__" = root_id,
    "__GLOBE_ID__" = globe_id,
    "__LABEL_LAYER_ID__" = label_layer_id,
    "__PANEL_ID__" = panel_id,
    "__MENU_TOGGLE_ID__" = menu_toggle_id,
    "__TOGGLE_ANIMATION_ID__" = toggle_animation_id,
    "__RELOAD_TLE_ID__" = reload_tle_id,
    "__ORBIT_WIDTH_ID__" = orbit_width_id,
    "__LIGHTING_MODE_ID__" = lighting_mode_id,
    "__REFERENCE_MODE_ID__" = reference_mode_id,
    "__REFERENCE_NOTE_ID__" = reference_note_id,
    "__STATUS_ID__" = status_id,
    "__CLOCK_ID__" = clock_id,
    "__TABLE_BODY_ID__" = table_body_id,
    "__NS_PREFIX__" = ns_prefix
  )

  for (nm in names(replacements)) {
    js <- gsub(nm, replacements[[nm]], js, fixed = TRUE)
  }

  js
}

# app.R
# Shiny + Three.js + satellite.js
# Globo 3D interactivo con Sentinel, Landsat y GOES en tiempo real.
#
# Requisitos:
# install.packages("shiny")
#
# Ejecutar:
# shiny::runApp("app.R")

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$title("Globo 3D con satélites en tiempo real"),

    tags$style(HTML("
      html, body {
        margin: 0;
        padding: 0;
        width: 100%;
        height: 100%;
        overflow: hidden;
        background: #050816;
        color: white;
        font-family: Arial, sans-serif;
      }

      .container-fluid {
        padding: 0 !important;
      }

      #app-container {
        position: relative;
        width: 100vw;
        height: 100vh;
        overflow: hidden;
        background:
          radial-gradient(circle at center,
            rgba(16, 25, 54, 0.95) 0%,
            rgba(5, 8, 22, 0.98) 55%,
            #02030a 100%);
      }

      #globe-container {
        width: 100%;
        height: 100%;
      }

      #label-layer {
        position: absolute;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        pointer-events: none;
        z-index: 4;
      }

      .sat-label {
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

      #panel {
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

      #panel.hidden-panel {
        transform: translateX(-660px);
        opacity: 0;
        pointer-events: none;
      }

      #menuToggle {
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

      #menuToggle:hover {
        background: rgba(29, 78, 216, 0.95);
      }

      #menuToggle.panel-hidden {
        left: 20px;
      }

      #panel h3 {
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 18px;
      }

      #panel p {
        font-size: 12px;
        line-height: 1.35;
        color: #cbd5e1;
        margin-bottom: 12px;
      }

      button {
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

      button:hover {
        background: #1d4ed8;
      }

      #status, #clock {
        margin-top: 8px;
        padding: 8px;
        border-radius: 10px;
        background: rgba(15, 23, 42, 0.78);
        border: 1px solid rgba(148, 163, 184, 0.22);
        font-size: 11px;
        color: #e5e7eb;
      }

      #clock {
        color: #93c5fd;
        font-weight: bold;
        font-size: 12px;
      }

      .visual-control {
        margin-top: 8px;
        font-size: 12px;
      }

      .visual-control label {
        display: block;
        margin-bottom: 4px;
        color: #e2e8f0;
      }

      .visual-control input[type=range] {
        width: 100%;
      }

      .sat-check {
        display: flex;
        align-items: center;
        gap: 7px;
        font-size: 12px;
      }

      .sat-check input {
        margin: 0;
      }

      .dot {
        width: 10px;
        height: 10px;
        border-radius: 999px;
        display: inline-block;
      }

      #visibilityMatrix {
        width: 100%;
        border-collapse: collapse;
        font-size: 10.5px;
        margin-top: 8px;
      }

      #visibilityMatrix th,
      #visibilityMatrix td {
        border-bottom: 1px solid rgba(148, 163, 184, 0.22);
        padding: 4px 3px;
        text-align: center;
        white-space: nowrap;
      }

      #visibilityMatrix th:first-child,
      #visibilityMatrix td:first-child {
        text-align: left;
      }

      #visibilityMatrix th {
        color: #93c5fd;
      }

      #satTable {
        margin-top: 10px;
        width: 100%;
        border-collapse: collapse;
        font-size: 10.2px;
      }

      #satTable th, #satTable td {
        border-bottom: 1px solid rgba(148, 163, 184, 0.22);
        padding: 4px 3px;
        text-align: left;
        white-space: nowrap;
      }

      #satTable th {
        color: #93c5fd;
      }

      .small-note {
        color: #94a3b8;
        font-size: 11px;
        line-height: 1.3;
        margin-top: 8px;
      }

      .section-title {
        margin-top: 12px;
        margin-bottom: 4px;
        font-size: 13px;
        color: #bfdbfe;
        font-weight: bold;
      }

      @media (max-width: 760px) {
        #panel {
          width: calc(100vw - 70px);
        }

        #menuToggle {
          left: calc(100vw - 70px);
        }

        #panel.hidden-panel {
          transform: translateX(calc(-100vw));
        }

        #menuToggle.panel-hidden {
          left: 20px;
        }
      }
    ")),

    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/satellite.js@5.0.0/dist/satellite.min.js")
  ),

  div(
    id = "app-container",

    div(id = "globe-container"),
    div(id = "label-layer"),

    tags$button(
      id = "menuToggle",
      "×"
    ),

    div(
      id = "panel",

      h3("Globo 3D + satélites en tiempo real"),

      p("Posición orbital calculada con TLEs online de CelesTrak + SGP4. La distancia Tierra-satélite está escalada físicamente. El tamaño visual de los satélites está exagerado para poder verlos."),

      div(id = "clock", "UTC: --"),

      tags$button(id = "toggleAnimation", "Pausar animación"),
      tags$button(id = "reloadTLE", "Recargar TLE online"),

      div(class = "section-title", "Visualización"),

      div(
        class = "visual-control",
        tags$label("Grosor de órbitas"),
        tags$input(
          id = "orbitWidth",
          type = "range",
          min = "1",
          max = "8",
          step = "1",
          value = "2"
        )
      ),

      div(
        class = "sat-check",
        style = "margin-top: 8px;",
        tags$input(
          type = "checkbox",
          id = "showLabels",
          checked = "checked"
        ),
        "Mostrar nombres de satélites"
      ),

      div(class = "section-title", "Matriz de visualización"),

      tags$table(
        id = "visibilityMatrix",
        tags$thead(
          tags$tr(
            tags$th("Satélite"),
            tags$th("Sat"),
            tags$th("Órbita"),
            tags$th("Cobertura"),
            tags$th("Nombre")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(span(class = "dot", style = "background:#ff4d4d;"), " Sentinel-2A"),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2A_sat", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2A_orbit", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2A_coverage", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2A_label", checked = "checked"))
          ),
          tags$tr(
            tags$td(span(class = "dot", style = "background:#ff9f43;"), " Sentinel-2B"),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2B_sat", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2B_orbit", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2B_coverage", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Sentinel2B_label", checked = "checked"))
          ),
          tags$tr(
            tags$td(span(class = "dot", style = "background:#00d2d3;"), " Landsat 8"),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat8_sat", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat8_orbit", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat8_coverage", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat8_label", checked = "checked"))
          ),
          tags$tr(
            tags$td(span(class = "dot", style = "background:#54a0ff;"), " Landsat 9"),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat9_sat", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat9_orbit", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat9_coverage", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_Landsat9_label", checked = "checked"))
          ),
          tags$tr(
            tags$td(span(class = "dot", style = "background:#a78bfa;"), " GOES-16"),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES16_sat", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES16_orbit", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES16_coverage", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES16_label", checked = "checked"))
          ),
          tags$tr(
            tags$td(span(class = "dot", style = "background:#f472b6;"), " GOES-18"),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES18_sat", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES18_orbit", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES18_coverage", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES18_label", checked = "checked"))
          ),
          tags$tr(
            tags$td(span(class = "dot", style = "background:#facc15;"), " GOES-19"),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES19_sat", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES19_orbit", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES19_coverage", checked = "checked")),
            tags$td(tags$input(type = "checkbox", id = "vis_GOES19_label", checked = "checked"))
          )
        )
      ),

      div(id = "status", "Cargando TLEs online..."),

      tags$table(
        id = "satTable",
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
        tags$tbody(id = "satTableBody")
      ),

      div(
        class = "small-note",
        "Cobertura controla huella + cono al mismo tiempo. Área: aproximación circular instantánea. En Sentinel/Landsat se usa el ancho de barrido real aproximado para la tabla, aunque la huella visual está agrandada para poder verse. En GOES se usa el área visible desde órbita geoestacionaria."
      )
    )
  ),

  tags$script(HTML("
    let scene, camera, renderer, controls;
    let earthGroup;
    let satellitesData = [];
    let animationRunning = true;

    let lastSatelliteUpdate = 0;
    let lastTableUpdate = 0;

    const SAT_UPDATE_MS = 250;
    const TABLE_UPDATE_MS = 1000;

    const EARTH_RADIUS_KM = 6371.0;

    const earthRadiusScene = 2.0;
    const kmToScene = earthRadiusScene / EARTH_RADIUS_KM;

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
      const container = document.getElementById('globe-container');

      scene = new THREE.Scene();

      camera = new THREE.PerspectiveCamera(
        45,
        window.innerWidth / window.innerHeight,
        0.1,
        1000
      );

      camera.position.set(0, 2.2, 10);

      renderer = new THREE.WebGLRenderer({
        antialias: true,
        alpha: true
      });

      renderer.setPixelRatio(window.devicePixelRatio);
      renderer.setSize(window.innerWidth, window.innerHeight);
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

      document.getElementById('menuToggle').addEventListener('click', function() {
        const panel = document.getElementById('panel');
        const button = document.getElementById('menuToggle');

        panel.classList.toggle('hidden-panel');
        button.classList.toggle('panel-hidden');

        if (panel.classList.contains('hidden-panel')) {
          button.innerText = '☰';
        } else {
          button.innerText = '×';
        }
      });

      document.getElementById('toggleAnimation').addEventListener('click', function() {
        animationRunning = !animationRunning;
        this.innerText = animationRunning ? 'Pausar animación' : 'Activar animación';
      });

      document.getElementById('reloadTLE').addEventListener('click', function() {
        loadTLEs();
      });

      desiredSatellites.forEach(function(spec) {
        ['sat', 'orbit', 'coverage', 'label'].forEach(function(elementName) {
          const el = document.getElementById('vis_' + spec.key + '_' + elementName);

          if (el) {
            el.addEventListener('change', function() {
              updateVisibility();
              updateTable();
              updateAllSatelliteLabels();
            });
          }
        });
      });

      const showLabelsInput = document.getElementById('showLabels');
      if (showLabelsInput) {
        showLabelsInput.addEventListener('change', function() {
          updateAllSatelliteLabels();
        });
      }

      const orbitWidthInput = document.getElementById('orbitWidth');
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
      document.getElementById('status').innerText = msg;
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
            .split('\\n')
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
        scene.remove(sat.mesh);
        scene.remove(sat.orbitLine);

        disposeObject3D(sat.mesh);
        disposeObject3D(sat.orbitLine);

        if (sat.footprintObject) {
          earthGroup.remove(sat.footprintObject);
          disposeObject3D(sat.footprintObject);
        }

        if (sat.coneObject) {
          scene.remove(sat.coneObject);
          disposeObject3D(sat.coneObject);
        }

        if (sat.label && sat.label.parentNode) {
          sat.label.parentNode.removeChild(sat.label);
        }
      });

      satellitesData = [];
      document.getElementById('satTableBody').innerHTML = '';
    }

    function createSatelliteFromTLE(spec, tleName, line1, line2) {
      const satrec = satellite.twoline2satrec(line1, line2);

      const mesh = createSatelliteMesh(spec.color, spec.type);
      scene.add(mesh);

      const orbitLine = createOrbitLineFromTLE(satrec, spec.color, spec.type);
      scene.add(orbitLine);

      const footprintObject = new THREE.Group();
      earthGroup.add(footprintObject);

      const coneObject = new THREE.Group();
      scene.add(coneObject);

      const label = document.createElement('div');
      label.className = 'sat-label';
      label.innerText = spec.displayName;
      label.style.color = '#' + spec.color.toString(16).padStart(6, '0');
      document.getElementById('label-layer').appendChild(label);

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
      const ambient = new THREE.AmbientLight(0xffffff, 0.70);
      scene.add(ambient);

      const sun = new THREE.DirectionalLight(0xffffff, 1.35);
      sun.position.set(5, 3, 5);
      scene.add(sun);

      const blueFill = new THREE.PointLight(0x60a5fa, 0.8, 30);
      blueFill.position.set(-5, -2, -4);
      scene.add(blueFill);
    }

    function createEarth() {
      earthGroup = new THREE.Group();
      scene.add(earthGroup);

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

      const widthInput = document.getElementById('orbitWidth');
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
          scene.remove(sat.orbitLine);
          disposeObject3D(sat.orbitLine);
        }

        sat.orbitLine = createOrbitLineFromTLE(
          sat.satrec,
          sat.color,
          sat.type
        );

        scene.add(sat.orbitLine);
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

    function updateFootprint(sat, satWorldPosition) {
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

      const ringWorldPoints = ringLocalPoints.map(function(p) {
        return earthGroup.localToWorld(p.clone());
      });

      const conePositions = [];
      const coneIndices = [];

      conePositions.push(
        satWorldPosition.x,
        satWorldPosition.y,
        satWorldPosition.z
      );

      ringWorldPoints.forEach(function(p) {
        conePositions.push(p.x, p.y, p.z);
      });

      for (let i = 1; i <= ringWorldPoints.length; i++) {
        const next = i < ringWorldPoints.length ? i + 1 : 1;
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
      const el = document.getElementById('vis_' + sat.key + '_' + elementName);
      return el ? el.checked : true;
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
      const satPos = sat.mesh.position.clone();

      const earthCenter = new THREE.Vector3(0, 0, 0);
      const earthRadius = earthRadiusScene * 1.03;

      const direction = satPos.clone().sub(cameraPos);
      const distanceToSat = direction.length();
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

      const showLabelsInput = document.getElementById('showLabels');
      const showGlobalLabels = showLabelsInput ? showLabelsInput.checked : true;

      const showThisLabel = isElementVisible(sat, 'label');

      if (!showGlobalLabels || !showThisLabel) {
        sat.label.style.display = 'none';
        return;
      }

      if (isSatelliteOccludedByEarth(sat)) {
        sat.label.style.display = 'none';
        return;
      }

      const vector = sat.mesh.position.clone();
      vector.project(camera);

      if (vector.z < -1 || vector.z > 1) {
        sat.label.style.display = 'none';
        return;
      }

      const x = (vector.x * 0.5 + 0.5) * window.innerWidth;
      const y = (-vector.y * 0.5 + 0.5) * window.innerHeight;

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
      const tbody = document.getElementById('satTableBody');
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
      const clock = document.getElementById('clock');
      clock.innerText =
        'UTC: ' +
        date.toISOString().replace('T', ' ').substring(0, 19);
    }

    function animate(timestamp) {
      requestAnimationFrame(animate);

      const now = new Date();

      updateClock(now);

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

      controls.update();
      updateAllSatelliteLabels();
      renderer.render(scene, camera);
    }

    function onWindowResize() {
      camera.aspect = window.innerWidth / window.innerHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(window.innerWidth, window.innerHeight);
      updateAllSatelliteLabels();
    }

    document.addEventListener('DOMContentLoaded', init);
  "))
)

server <- function(input, output, session) {}

shinyApp(ui, server)

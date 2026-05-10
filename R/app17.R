# app.R
# Shiny + Three.js + satellite.js
# Globo 3D interactivo con posiciones orbitales online desde CelesTrak.
#
# Requisitos:
# install.packages("shiny")
#
# Ejecutar:
# shiny::runApp("app.R")
#
# Importante:
# - Necesita internet.
# - Descarga TLEs actuales desde CelesTrak.
# - Propaga posiciones con satellite.js usando SGP4.
# - La Tierra rota con día sideral.
# - Puedes mover el mundo con el mouse.

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$title("Globo 3D con satélites reales"),

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
        background: radial-gradient(circle at center, #101936 0%, #050816 65%, #02030a 100%);
      }

      #globe-container {
        width: 100%;
        height: 100%;
      }

      #panel {
        position: absolute;
        top: 20px;
        left: 20px;
        z-index: 10;
        background: rgba(5, 8, 22, 0.82);
        border: 1px solid rgba(255,255,255,0.18);
        border-radius: 16px;
        padding: 16px 18px;
        width: 390px;
        backdrop-filter: blur(8px);
        box-shadow: 0 10px 30px rgba(0,0,0,0.35);
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

      .control-row {
        margin: 12px 0;
      }

      label {
        display: block;
        font-size: 13px;
        margin-bottom: 5px;
        color: #e2e8f0;
      }

      input[type=range] {
        width: 100%;
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

      .legend {
        display: grid;
        gap: 6px;
        margin-top: 10px;
        font-size: 12px;
      }

      .legend-item {
        display: flex;
        align-items: center;
        gap: 8px;
      }

      .dot {
        width: 10px;
        height: 10px;
        border-radius: 999px;
        display: inline-block;
      }

      #status {
        margin-top: 8px;
        padding: 8px;
        border-radius: 10px;
        background: rgba(15, 23, 42, 0.78);
        border: 1px solid rgba(148, 163, 184, 0.22);
        font-size: 11px;
        color: #e5e7eb;
      }

      #satTable {
        margin-top: 10px;
        width: 100%;
        border-collapse: collapse;
        font-size: 11px;
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

      #timeFactorText {
        color: #93c5fd;
        font-weight: bold;
      }

      .small-note {
        color: #94a3b8;
        font-size: 11px;
        line-height: 1.3;
        margin-top: 8px;
      }
    ")),

    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/satellite.js@5.0.0/dist/satellite.min.js")
  ),

  div(
    id = "app-container",

    div(id = "globe-container"),

    div(
      id = "panel",

      h3("Globo 3D + satélites reales"),

      p("La posición se calcula online desde TLEs de CelesTrak y se propaga con SGP4 en JavaScript. Puedes mover la Tierra con el mouse."),

      div(
        class = "control-row",
        tags$label("Factor de tiempo"),
        tags$input(
          id = "timeFactor",
          type = "range",
          min = "1",
          max = "3600",
          step = "1",
          value = "120"
        ),
        div("1 segundo real = ", span(id = "timeFactorText", "120"), " segundos simulados")
      ),

      tags$button(id = "toggleAnimation", "Pausar animación"),
      tags$button(id = "reloadTLE", "Recargar TLE online"),

      div(
        class = "legend",
        div(class = "legend-item", span(class = "dot", style = "background:#ff4d4d;"), "Sentinel-2A"),
        div(class = "legend-item", span(class = "dot", style = "background:#ff9f43;"), "Sentinel-2B"),
        div(class = "legend-item", span(class = "dot", style = "background:#00d2d3;"), "Landsat 8"),
        div(class = "legend-item", span(class = "dot", style = "background:#54a0ff;"), "Landsat 9")
      ),

      div(id = "status", "Cargando TLEs online..."),

      tags$table(
        id = "satTable",
        tags$thead(
          tags$tr(
            tags$th("Satélite"),
            tags$th("Lat"),
            tags$th("Lon"),
            tags$th("Alt km")
          )
        ),
        tags$tbody(id = "satTableBody")
      ),

      div(
        class = "small-note",
        "Nota: las posiciones dependen del TLE disponible online. La visualización es 3D y la escala orbital usa el radio terrestre + altitud calculada."
      )
    )
  ),

  tags$script(HTML("
    let scene, camera, renderer, controls;
    let earthGroup, stars;
    let satellitesData = [];
    let animationRunning = true;
    let lastTimestamp = null;
    let simDate = new Date();

    const EARTH_RADIUS_KM = 6371.0;
    const EARTH_SIDEREAL_DAY_SEC = 86164.0905;

    const earthRadius = 2.0;
    const kmToScene = earthRadius / EARTH_RADIUS_KM;

    const tleURL = 'https://celestrak.org/NORAD/elements/gp.php?GROUP=resource&FORMAT=tle';

    const desiredSatellites = [
      {
        displayName: 'Sentinel-2A',
        searchName: 'SENTINEL-2A',
        color: 0xff4d4d
      },
      {
        displayName: 'Sentinel-2B',
        searchName: 'SENTINEL-2B',
        color: 0xff9f43
      },
      {
        displayName: 'Landsat 8',
        searchName: 'LANDSAT 8',
        color: 0x00d2d3
      },
      {
        displayName: 'Landsat 9',
        searchName: 'LANDSAT 9',
        color: 0x54a0ff
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

      camera.position.set(0, 1.4, 7);

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
      controls.maxDistance = 15;

      addLights();
      createEarth();
      createStars();

      document.getElementById('toggleAnimation').addEventListener('click', function() {
        animationRunning = !animationRunning;
        this.innerText = animationRunning ? 'Pausar animación' : 'Activar animación';
      });

      document.getElementById('reloadTLE').addEventListener('click', function() {
        loadTLEs();
      });

      document.getElementById('timeFactor').addEventListener('input', function() {
        document.getElementById('timeFactorText').innerText = this.value;
      });

      window.addEventListener('resize', onWindowResize);

      loadTLEs();
      requestAnimationFrame(animate);
    }

    function setStatus(msg) {
      document.getElementById('status').innerText = msg;
    }

    async function loadTLEs() {
      setStatus('Descargando TLEs desde CelesTrak...');

      try {
        const response = await fetch(tleURL, { cache: 'no-store' });

        if (!response.ok) {
          throw new Error('HTTP ' + response.status);
        }

        const text = await response.text();
        const lines = text
          .split('\\n')
          .map(x => x.trim())
          .filter(x => x.length > 0);

        clearSatellites();

        desiredSatellites.forEach(function(spec) {
          const found = findTLE(lines, spec.searchName);

          if (found) {
            createSatelliteFromTLE(spec, found.name, found.line1, found.line2);
          } else {
            console.warn('No encontrado:', spec.searchName);
          }
        });

        if (satellitesData.length === 0) {
          setStatus('No se encontraron TLEs. Revisa conexión o grupo CelesTrak.');
        } else {
          setStatus(
            'TLEs cargados online: ' +
            satellitesData.length +
            ' satélites. Última carga: ' +
            new Date().toLocaleTimeString()
          );
        }

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
      });

      satellitesData = [];
      document.getElementById('satTableBody').innerHTML = '';
    }

    function createSatelliteFromTLE(spec, tleName, line1, line2) {
      const satrec = satellite.twoline2satrec(line1, line2);

      const mesh = createSatelliteMesh(spec.color);
      scene.add(mesh);

      const orbitLine = createOrbitLineFromTLE(satrec, spec.color);
      scene.add(orbitLine);

      satellitesData.push({
        displayName: spec.displayName,
        tleName: tleName,
        color: spec.color,
        satrec: satrec,
        mesh: mesh,
        orbitLine: orbitLine,
        lat: null,
        lon: null,
        altKm: null
      });
    }

    function addLights() {
      const ambient = new THREE.AmbientLight(0xffffff, 0.65);
      scene.add(ambient);

      const sun = new THREE.DirectionalLight(0xffffff, 1.25);
      sun.position.set(5, 3, 5);
      scene.add(sun);

      const blueFill = new THREE.PointLight(0x60a5fa, 0.8, 30);
      blueFill.position.set(-5, -2, -4);
      scene.add(blueFill);
    }

    function createEarth() {
      earthGroup = new THREE.Group();
      scene.add(earthGroup);

      const earthGeometry = new THREE.SphereGeometry(earthRadius, 128, 128);

      const earthMaterial = new THREE.MeshPhongMaterial({
        color: 0x1e88e5,
        shininess: 18
      });

      const earth = new THREE.Mesh(earthGeometry, earthMaterial);
      earthGroup.add(earth);

      addContinentsApproximation();

      const atmosphereGeometry = new THREE.SphereGeometry(earthRadius * 1.018, 128, 128);

      const atmosphereMaterial = new THREE.MeshPhongMaterial({
        color: 0x4da3ff,
        transparent: true,
        opacity: 0.16,
        shininess: 20
      });

      const atmosphere = new THREE.Mesh(atmosphereGeometry, atmosphereMaterial);
      earthGroup.add(atmosphere);

      createGridLines();
    }

    function latLonToVector3(lat, lon, radius) {
      const phi = THREE.MathUtils.degToRad(90 - lat);
      const theta = THREE.MathUtils.degToRad(lon + 180);

      const x = -radius * Math.sin(phi) * Math.cos(theta);
      const z = radius * Math.sin(phi) * Math.sin(theta);
      const y = radius * Math.cos(phi);

      return new THREE.Vector3(x, y, z);
    }

    function addLandBlob(pointsLatLon, color) {
      const points = [];

      pointsLatLon.forEach(function(p) {
        points.push(latLonToVector3(p[0], p[1], earthRadius * 1.006));
      });

      const geometry = new THREE.BufferGeometry().setFromPoints(points);

      const material = new THREE.LineBasicMaterial({
        color: color,
        transparent: true,
        opacity: 0.9
      });

      const line = new THREE.LineLoop(geometry, material);
      earthGroup.add(line);
    }

    function addContinentsApproximation() {
      const landColor = 0x36d399;

      addLandBlob([[70,-165],[60,-130],[50,-125],[35,-120],[20,-100],[10,-85],[25,-75],[45,-70],[60,-95],[70,-130]], landColor);
      addLandBlob([[12,-80],[5,-75],[-10,-70],[-25,-65],[-45,-70],[-55,-60],[-35,-45],[-15,-35],[0,-50],[10,-65]], landColor);
      addLandBlob([[70,-10],[60,30],[55,80],[45,120],[25,110],[10,80],[20,45],[35,25],[45,5],[60,-5]], landColor);
      addLandBlob([[35,-15],[20,5],[5,20],[-15,25],[-35,18],[-35,35],[-10,45],[10,40],[30,30],[35,5]], landColor);
      addLandBlob([[-10,110],[-20,120],[-30,135],[-38,145],[-30,155],[-18,145]], landColor);
      addLandBlob([[75,-50],[70,-35],[60,-40],[60,-60],[70,-70]], landColor);
      addLandBlob([[-70,-180],[-75,-90],[-72,0],[-75,90],[-70,180],[-82,0]], landColor);
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
        const r = earthRadius * Math.cos(latRad);
        const y = earthRadius * Math.sin(latRad);

        for (let i = 0; i <= 360; i += 2) {
          const lonRad = THREE.MathUtils.degToRad(i);
          points.push(new THREE.Vector3(r * Math.cos(lonRad), y, r * Math.sin(lonRad)));
        }

        const geometry = new THREE.BufferGeometry().setFromPoints(points);
        earthGroup.add(new THREE.Line(geometry, gridMaterial));
      }

      for (let lon = 0; lon < 180; lon += 30) {
        const meridian = new THREE.Group();
        const points = [];

        for (let lat = -90; lat <= 90; lat += 2) {
          const latRad = THREE.MathUtils.degToRad(lat);
          points.push(new THREE.Vector3(earthRadius * Math.cos(latRad), earthRadius * Math.sin(latRad), 0));
        }

        const geometry = new THREE.BufferGeometry().setFromPoints(points);
        const line = new THREE.Line(geometry, gridMaterial);

        meridian.add(line);
        meridian.rotation.y = THREE.MathUtils.degToRad(lon);
        earthGroup.add(meridian);
      }
    }

    function createStars() {
      const starGeometry = new THREE.BufferGeometry();
      const starCount = 1400;
      const positions = [];

      for (let i = 0; i < starCount; i++) {
        positions.push(
          THREE.MathUtils.randFloatSpread(120),
          THREE.MathUtils.randFloatSpread(120),
          THREE.MathUtils.randFloatSpread(120)
        );
      }

      starGeometry.setAttribute('position', new THREE.Float32BufferAttribute(positions, 3));

      const starMaterial = new THREE.PointsMaterial({
        color: 0xffffff,
        size: 0.035,
        transparent: true,
        opacity: 0.75
      });

      stars = new THREE.Points(starGeometry, starMaterial);
      scene.add(stars);
    }

    function createSatelliteMesh(color) {
      const group = new THREE.Group();

      const body = new THREE.Mesh(
        new THREE.BoxGeometry(0.075, 0.052, 0.052),
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
        new THREE.BoxGeometry(0.16, 0.01, 0.075),
        panelMaterial
      );

      panel1.position.x = -0.13;
      group.add(panel1);

      const panel2 = new THREE.Mesh(
        new THREE.BoxGeometry(0.16, 0.01, 0.075),
        panelMaterial
      );

      panel2.position.x = 0.13;
      group.add(panel2);

      const halo = new THREE.Mesh(
        new THREE.SphereGeometry(0.04, 24, 24),
        new THREE.MeshBasicMaterial({
          color: color,
          transparent: true,
          opacity: 0.95
        })
      );

      group.add(halo);

      return group;
    }

    function createOrbitLineFromTLE(satrec, color) {
      const points = [];
      const now = new Date();

      for (let minutes = 0; minutes <= 110; minutes += 1) {
        const date = new Date(now.getTime() + minutes * 60 * 1000);
        const pos = getSatelliteGeodetic(satrec, date);

        if (pos) {
          const radius = (EARTH_RADIUS_KM + pos.altKm) * kmToScene;
          points.push(latLonToVector3(pos.lat, pos.lon, radius));
        }
      }

      const geometry = new THREE.BufferGeometry().setFromPoints(points);

      const material = new THREE.LineBasicMaterial({
        color: color,
        transparent: true,
        opacity: 0.5
      });

      return new THREE.Line(geometry, material);
    }

    function getSatelliteGeodetic(satrec, date) {
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
        lat: lat,
        lon: lon,
        altKm: altKm
      };
    }

    function updateSatellites(date) {
      satellitesData.forEach(function(sat) {
        const pos = getSatelliteGeodetic(sat.satrec, date);

        if (!pos) {
          return;
        }

        sat.lat = pos.lat;
        sat.lon = pos.lon;
        sat.altKm = pos.altKm;

        const radius = (EARTH_RADIUS_KM + pos.altKm) * kmToScene;
        const xyz = latLonToVector3(pos.lat, pos.lon, radius);

        sat.mesh.position.copy(xyz);
        sat.mesh.lookAt(0, 0, 0);
        sat.mesh.rotateY(Math.PI / 2);
      });

      updateTable();
    }

    function updateTable() {
      const tbody = document.getElementById('satTableBody');
      tbody.innerHTML = '';

      satellitesData.forEach(function(sat) {
        const tr = document.createElement('tr');

        const name = document.createElement('td');
        name.innerText = sat.displayName;

        const lat = document.createElement('td');
        lat.innerText = sat.lat === null ? '—' : sat.lat.toFixed(2);

        const lon = document.createElement('td');
        lon.innerText = sat.lon === null ? '—' : sat.lon.toFixed(2);

        const alt = document.createElement('td');
        alt.innerText = sat.altKm === null ? '—' : sat.altKm.toFixed(0);

        tr.appendChild(name);
        tr.appendChild(lat);
        tr.appendChild(lon);
        tr.appendChild(alt);

        tbody.appendChild(tr);
      });
    }

    function updateEarthRotation(deltaSimSec) {
      const earthAngularVelocity = 2 * Math.PI / EARTH_SIDEREAL_DAY_SEC;
      earthGroup.rotation.y += earthAngularVelocity * deltaSimSec;
    }

    function animate(timestamp) {
      requestAnimationFrame(animate);

      if (lastTimestamp === null) {
        lastTimestamp = timestamp;
      }

      const deltaRealSec = (timestamp - lastTimestamp) / 1000;
      lastTimestamp = timestamp;

      const timeFactor = parseFloat(document.getElementById('timeFactor').value);
      const deltaSimSec = animationRunning ? deltaRealSec * timeFactor : 0;

      simDate = new Date(simDate.getTime() + deltaSimSec * 1000);

      if (animationRunning) {
        updateEarthRotation(deltaSimSec);
        updateSatellites(simDate);
      }

      if (stars) {
        stars.rotation.y += 0.00004;
      }

      controls.update();
      renderer.render(scene, camera);
    }

    function onWindowResize() {
      camera.aspect = window.innerWidth / window.innerHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(window.innerWidth, window.innerHeight);
    }

    document.addEventListener('DOMContentLoaded', init);
  "))
)

server <- function(input, output, session) {}

shinyApp(ui, server)

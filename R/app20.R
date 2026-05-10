# app.R
# Shiny + Three.js + satellite.js
# Globo 3D interactivo con Sentinel, Landsat y GOES en tiempo real.
#
# Requisitos:
# install.packages("shiny")
#
# Ejecutar:
# shiny::runApp("app.R")
#
# Necesita internet:
# - Three.js desde CDN
# - OrbitControls desde CDN
# - satellite.js desde CDN
# - TLEs online desde CelesTrak
# - Textura de la Tierra online

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
        background: rgba(5, 8, 22, 0.86);
        border: 1px solid rgba(255,255,255,0.18);
        border-radius: 16px;
        padding: 16px 18px;
        width: 520px;
        max-height: 92vh;
        overflow-y: auto;
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

      .sat-controls {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 5px 10px;
        margin-top: 10px;
        font-size: 12px;
      }

      .sat-check {
        display: flex;
        align-items: center;
        gap: 7px;
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

      #satTable {
        margin-top: 10px;
        width: 100%;
        border-collapse: collapse;
        font-size: 10.5px;
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

      h3("Globo 3D + satélites en tiempo real"),

      p("Posición orbital calculada con TLEs online de CelesTrak + SGP4. La distancia Tierra-satélite está escalada físicamente. El tamaño visual de los satélites está exagerado para poder verlos."),

      div(id = "clock", "UTC: --"),

      tags$button(id = "toggleAnimation", "Pausar animación"),
      tags$button(id = "reloadTLE", "Recargar TLE online"),

      div(class = "section-title", "Activar / desactivar satélites"),

      div(
        class = "sat-controls",

        div(
          class = "sat-check",
          tags$input(type = "checkbox", id = "chk_Sentinel2A", checked = "checked"),
          span(class = "dot", style = "background:#ff4d4d;"),
          "Sentinel-2A"
        ),

        div(
          class = "sat-check",
          tags$input(type = "checkbox", id = "chk_Sentinel2B", checked = "checked"),
          span(class = "dot", style = "background:#ff9f43;"),
          "Sentinel-2B"
        ),

        div(
          class = "sat-check",
          tags$input(type = "checkbox", id = "chk_Landsat8", checked = "checked"),
          span(class = "dot", style = "background:#00d2d3;"),
          "Landsat 8"
        ),

        div(
          class = "sat-check",
          tags$input(type = "checkbox", id = "chk_Landsat9", checked = "checked"),
          span(class = "dot", style = "background:#54a0ff;"),
          "Landsat 9"
        ),

        div(
          class = "sat-check",
          tags$input(type = "checkbox", id = "chk_GOES16", checked = "checked"),
          span(class = "dot", style = "background:#a78bfa;"),
          "GOES-16"
        ),

        div(
          class = "sat-check",
          tags$input(type = "checkbox", id = "chk_GOES18", checked = "checked"),
          span(class = "dot", style = "background:#f472b6;"),
          "GOES-18"
        ),

        div(
          class = "sat-check",
          tags$input(type = "checkbox", id = "chk_GOES19", checked = "checked"),
          span(class = "dot", style = "background:#facc15;"),
          "GOES-19"
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
            tags$th("Dist. superficie km"),
            tags$th("Radio orbital km")
          )
        ),
        tags$tbody(id = "satTableBody")
      ),

      div(
        class = "small-note",
        "Huella: para Sentinel y Landsat se dibuja el ancho de barrido aproximado; para GOES se dibuja la cobertura visible desde órbita geoestacionaria."
      )
    )
  ),

  tags$script(HTML("
    let scene, camera, renderer, controls;
    let earthGroup, stars;
    let satellitesData = [];
    let animationRunning = true;

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
        displayName: 'Sentinel-2A',
        searchName: 'SENTINEL-2A',
        checkboxId: 'chk_Sentinel2A',
        type: 'LEO',
        swathKm: 290,
        color: 0xff4d4d,
        tleGroup: 'resource'
      },
      {
        displayName: 'Sentinel-2B',
        searchName: 'SENTINEL-2B',
        checkboxId: 'chk_Sentinel2B',
        type: 'LEO',
        swathKm: 290,
        color: 0xff9f43,
        tleGroup: 'resource'
      },
      {
        displayName: 'Landsat 8',
        searchName: 'LANDSAT 8',
        checkboxId: 'chk_Landsat8',
        type: 'LEO',
        swathKm: 185,
        color: 0x00d2d3,
        tleGroup: 'resource'
      },
      {
        displayName: 'Landsat 9',
        searchName: 'LANDSAT 9',
        checkboxId: 'chk_Landsat9',
        type: 'LEO',
        swathKm: 185,
        color: 0x54a0ff,
        tleGroup: 'resource'
      },
      {
        displayName: 'GOES-16',
        searchName: 'GOES 16',
        checkboxId: 'chk_GOES16',
        type: 'GEO',
        swathKm: null,
        color: 0xa78bfa,
        tleGroup: 'weather'
      },
      {
        displayName: 'GOES-18',
        searchName: 'GOES 18',
        checkboxId: 'chk_GOES18',
        type: 'GEO',
        swathKm: null,
        color: 0xf472b6,
        tleGroup: 'weather'
      },
      {
        displayName: 'GOES-19',
        searchName: 'GOES 19',
        checkboxId: 'chk_GOES19',
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
      controls.maxDistance = 28;

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

      desiredSatellites.forEach(function(spec) {
        const el = document.getElementById(spec.checkboxId);
        if (el) {
          el.addEventListener('change', function() {
            updateVisibility();
          });
        }
      });

      window.addEventListener('resize', onWindowResize);

      loadTLEs();
      animate();
    }

    function setStatus(msg) {
      document.getElementById('status').innerText = msg;
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
        scene.remove(sat.trailObject);

        if (sat.footprintObject) {
          earthGroup.remove(sat.footprintObject);
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

      const trailObject = new THREE.Group();
      scene.add(trailObject);

      const footprintObject = new THREE.Group();
      earthGroup.add(footprintObject);

      satellitesData.push({
        displayName: spec.displayName,
        tleName: tleName,
        checkboxId: spec.checkboxId,
        type: spec.type,
        swathKm: spec.swathKm,
        color: spec.color,
        satrec: satrec,
        mesh: mesh,
        orbitLine: orbitLine,
        trailObject: trailObject,
        trailPoints: [],
        footprintObject: footprintObject,
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
        128,
        128
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
        128,
        128
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
        opacity: 0.16
      });

      for (let lat = -60; lat <= 60; lat += 30) {
        const points = [];
        const latRad = THREE.MathUtils.degToRad(lat);
        const r = earthRadiusScene * Math.cos(latRad);
        const y = earthRadiusScene * Math.sin(latRad);

        for (let i = 0; i <= 360; i += 2) {
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

        for (let lat = -90; lat <= 90; lat += 2) {
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

      starGeometry.setAttribute(
        'position',
        new THREE.Float32BufferAttribute(positions, 3)
      );

      const starMaterial = new THREE.PointsMaterial({
        color: 0xffffff,
        size: 0.035,
        transparent: true,
        opacity: 0.75
      });

      stars = new THREE.Points(starGeometry, starMaterial);
      scene.add(stars);
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
        new THREE.SphereGeometry(0.04 * scale, 24, 24),
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
      const stepMin = type === 'GEO' ? 20 : 1;

      for (let minutes = startMin; minutes <= endMin; minutes += stepMin) {
        const date = new Date(now.getTime() + minutes * 60 * 1000);
        const pv = satellite.propagate(satrec, date);

        if (pv && pv.position) {
          points.push(eciKmToSceneVector(pv.position));
        }
      }

      const geometry = new THREE.BufferGeometry().setFromPoints(points);

      const material = new THREE.LineBasicMaterial({
        color: color,
        transparent: true,
        opacity: type === 'GEO' ? 0.28 : 0.50
      });

      return new THREE.Line(geometry, material);
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

        if (child.geometry) child.geometry.dispose();
        if (child.material) child.material.dispose();
      }
    }

    function updateFootprint(sat) {
      clearGroup(sat.footprintObject);

      if (sat.lat === null || sat.lon === null || sat.altKm === null) {
        return;
      }

      let angularRadius;

      if (sat.type === 'GEO') {
        const r = EARTH_RADIUS_KM + sat.altKm;
        angularRadius = Math.acos(EARTH_RADIUS_KM / r);
      } else {
        angularRadius = (sat.swathKm / 2) / EARTH_RADIUS_KM;
      }

      const ringPoints = [];
      const fillPoints = [];

      const center = latLonToVector3(
        sat.lat,
        sat.lon,
        earthRadiusScene * 1.012
      );

      fillPoints.push(center);

      for (let b = 0; b <= 360; b += 4) {
        const p = destinationPoint(sat.lat, sat.lon, b, angularRadius);
        const v = latLonToVector3(
          p.lat,
          p.lon,
          earthRadiusScene * 1.012
        );

        ringPoints.push(v);
        fillPoints.push(v);
      }

      const ringGeometry = new THREE.BufferGeometry().setFromPoints(ringPoints);

      const ringMaterial = new THREE.LineBasicMaterial({
        color: sat.color,
        transparent: true,
        opacity: sat.type === 'GEO' ? 0.75 : 0.95
      });

      const ring = new THREE.LineLoop(ringGeometry, ringMaterial);
      sat.footprintObject.add(ring);

      const fillGeometry = new THREE.BufferGeometry().setFromPoints(fillPoints);

      const fillMaterial = new THREE.PointsMaterial({
        color: sat.color,
        size: sat.type === 'GEO' ? 0.015 : 0.02,
        transparent: true,
        opacity: sat.type === 'GEO' ? 0.12 : 0.28
      });

      const fill = new THREE.Points(fillGeometry, fillMaterial);
      sat.footprintObject.add(fill);
    }

    function updateTrail(sat, point) {
      sat.trailPoints.push(point.clone());

      const maxPoints = sat.type === 'GEO' ? 220 : 160;

      if (sat.trailPoints.length > maxPoints) {
        sat.trailPoints.shift();
      }

      scene.remove(sat.trailObject);

      const trailGroup = new THREE.Group();

      if (sat.trailPoints.length >= 2) {
        const curve = new THREE.CatmullRomCurve3(sat.trailPoints);
        const tubeRadius = sat.type === 'GEO' ? 0.012 : 0.018;

        const geometry = new THREE.TubeGeometry(
          curve,
          Math.max(8, sat.trailPoints.length * 2),
          tubeRadius,
          8,
          false
        );

        const material = new THREE.MeshBasicMaterial({
          color: sat.color,
          transparent: true,
          opacity: sat.type === 'GEO' ? 0.32 : 0.48
        });

        const tube = new THREE.Mesh(geometry, material);
        trailGroup.add(tube);
      }

      sat.trailObject = trailGroup;
      scene.add(sat.trailObject);
    }

    function updateSatellites(date) {
      satellitesData.forEach(function(sat) {
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

        updateTrail(sat, xyz);
        updateFootprint(sat);
      });

      updateVisibility();
      updateTable();
    }

    function updateVisibility() {
      satellitesData.forEach(function(sat) {
        const checkbox = document.getElementById(sat.checkboxId);
        const visible = checkbox ? checkbox.checked : true;

        sat.mesh.visible = visible;
        sat.orbitLine.visible = visible;
        sat.trailObject.visible = visible;
        sat.footprintObject.visible = visible;
      });
    }

    function updateTable() {
      const tbody = document.getElementById('satTableBody');
      tbody.innerHTML = '';

      satellitesData.forEach(function(sat) {
        const checkbox = document.getElementById(sat.checkboxId);
        const visible = checkbox ? checkbox.checked : true;

        if (!visible) {
          return;
        }

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

        tr.appendChild(name);
        tr.appendChild(type);
        tr.appendChild(lat);
        tr.appendChild(lon);
        tr.appendChild(alt);
        tr.appendChild(dist);
        tr.appendChild(radius);

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

    function animate() {
      requestAnimationFrame(animate);

      const now = new Date();

      updateClock(now);

      if (animationRunning) {
        updateEarthRotation(now);
        updateSatellites(now);
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

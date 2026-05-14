library(shiny)
library(lubridate)
library(jsonlite)

# ============================================================
# MÓDULO UI
# ============================================================

mod_satelliteGlobe_02_ui <- function(id) {
  ns <- NS(id)

  ids <- list(
    earth3d = ns("earth3d"),
    gis_options = ns("gis_options"),
    toggle_gis = ns("toggle_gis"),
    sim_options = ns("sim_options"),
    toggle_sim = ns("toggle_sim"),
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

      tags$style(HTML("
        html, body {
          margin: 0;
          padding: 0;
          overflow: hidden;
          background: #050816;
          color: white;
          font-family: Arial, sans-serif;
        }

        .container-fluid { padding: 0; }

        .earth3d-container {
          width: 100vw;
          height: 100vh;
          display: block;
        }

        .earth-side-panel {
          position: absolute;
          top: 15px;
          z-index: 10;
          background: rgba(0,0,0,0.76);
          padding: 15px;
          border-radius: 12px;
          width: 365px;
          max-height: calc(100vh - 30px);
          overflow-y: auto;
          box-shadow: 0 0 18px rgba(0,0,0,0.4);
        }

        .earth-gis-panel { left: 15px; }
        .earth-sim-panel { right: 15px; }

        .earth-side-panel h3 {
          margin-top: 0;
          font-size: 20px;
        }

        .earth-side-panel h4 {
          margin-top: 14px;
          margin-bottom: 8px;
          color: #ffcc33;
          font-size: 16px;
        }

        .earth-side-panel p { margin-bottom: 6px; }

        .value { color: #ffcc33; font-weight: bold; }
        .red { color: #ff5555; font-weight: bold; }
        .white { color: #ffffff; font-weight: bold; }
        .green { color: #50fa7b; font-weight: bold; }
        .blue { color: #7aa2ff; font-weight: bold; }
        .magenta { color: #ff79c6; font-weight: bold; }
        .gray { color: #aaaaaa; font-weight: bold; }
        .orange { color: #ff9900; font-weight: bold; }

        label { color: white; }
        input, select { color: black; }

        .form-group { margin-bottom: 8px; }

        .small-note {
          font-size: 12px;
          opacity: 0.8;
        }

        .manual-time-row {
          display: grid;
          grid-template-columns: 1fr 1fr 1fr;
          column-gap: 8px;
        }

        .menu-toggle-btn {
          width: 100%;
          background: #ffcc33 !important;
          color: #111 !important;
          border: none !important;
          border-radius: 8px !important;
          font-weight: bold !important;
          font-size: 17px !important;
          margin-bottom: 10px !important;
          text-align: left !important;
        }

        .menu-toggle-btn:hover {
          background: #ffd966 !important;
          color: #000 !important;
        }

        .menu-toggle-btn::before {
          content: '▾ ';
        }

        .menu-toggle-btn.collapsed::before {
          content: '▸ ';
        }
      "))
    ),

    div(
      class = "earth-side-panel earth-gis-panel",

      actionButton(
        ns("toggle_gis"),
        "GIS / Cartografía",
        class = "menu-toggle-btn collapsed"
      ),

      div(
        id = ns("gis_options"),
        style = "display: none;",


        h4("Textura / mapa"),

        selectInput(
          ns("earth_texture_file"),
          "Textura base WGS84 solid",
          choices = character(0)
        ),

        selectInput(
          ns("earth_overlay_file"),
          "Capa WGS84 transparente",
          choices = c("Ninguna" = "")
        ),

        checkboxInput(ns("show_texture"), "Mostrar textura base", value = TRUE),
        checkboxInput(ns("show_overlay"), "Mostrar capa transparente", value = TRUE),
        checkboxInput(ns("show_blue_sphere"), "Mostrar esfera azul base", value = TRUE),
        checkboxInput(ns("show_graticule"), "Mostrar malla meridianos/paralelos", value = FALSE),
        checkboxInput(ns("show_stars"), "Mostrar estrellas", value = TRUE),
        checkboxInput(ns("show_sun_arrow"), "Mostrar dirección solar", value = TRUE),
        checkboxInput(ns("show_subsolar_point"), "Mostrar punto subsolar", value = TRUE),
        checkboxInput(ns("show_earth_axis"), "Mostrar eje terrestre", value = TRUE),
        checkboxInput(ns("show_polar_terminator_axis"), "Eje al plano de traslacion", value = TRUE),
        checkboxInput(ns("show_equator"), "Mostrar ecuador destacado", value = TRUE),
        checkboxInput(ns("show_greenwich"), "Mostrar meridiano de Greenwich", value = TRUE),

        tags$hr(),

        h4("Iluminación"),

        sliderInput(
          ns("night_light"),
          "Iluminación del lado oscuro",
          min = 0,
          max = 100,
          value = 12,
          step = 1
        ),

        sliderInput(
          ns("day_light"),
          "Intensidad del lado iluminado",
          min = 0,
          max = 200,
          value = 108,
          step = 1
        ),

        sliderInput(
          ns("day_gradient"),
          "Gradiente luz día centro-borde",
          min = 0,
          max = 100,
          value = 35,
          step = 1
        ),

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
      class = "earth-side-panel earth-sim-panel",

      actionButton(
        ns("toggle_sim"),
        "Simulación / Procesamiento",
        class = "menu-toggle-btn collapsed"
      ),

      div(
        id = ns("sim_options"),
        style = "display: none;",

        h4("Tiempo / Animación"),

        radioButtons(
          ns("time_source"),
          "Fuente de tiempo",
          choices = c(
            "Hora UTC del sistema" = "system",
            "Hora UTC web" = "web",
            "Hora UTC manual" = "manual"
          ),
          selected = "system"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("time_source")),

          tags$div(
            class = "manual-time-row",
            numericInput(ns("manual_year"), "Año", value = year(now_utc), min = 1900, max = 2100, step = 1),
            numericInput(ns("manual_month"), "Mes", value = month(now_utc), min = 1, max = 12, step = 1),
            numericInput(ns("manual_day"), "Día", value = day(now_utc), min = 1, max = 31, step = 1)
          ),

          tags$div(
            class = "manual-time-row",
            numericInput(ns("manual_hour"), "Hora", value = hour(now_utc), min = 0, max = 23, step = 1),
            numericInput(ns("manual_minute"), "Minuto", value = minute(now_utc), min = 0, max = 59, step = 1),
            numericInput(ns("manual_second"), "Segundo", value = second(now_utc), min = 0, max = 59, step = 1)
          )
        ),

        p("Fuente usada: ", span(id = ns("time_source_txt"), class = "value")),
        p("UTC usado: ", span(id = ns("utc_txt"), class = "value")),

        numericInput(ns("refresh"), "Actualizar cada segundos", value = 30, min = 5, max = 600, step = 5),
        actionButton(ns("refresh_now"), "Actualizar ahora"),
        checkboxInput(ns("auto_rotate"), "Rotación automática visual", value = FALSE),

        tags$small(
          class = "small-note",
          "La rotación automática es visual. Para representar exactamente la fecha/hora elegida, conviene dejarla apagada."
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

        p(span("Rojo:", class = "red"), " eje de rotación norte-sur de la Tierra. N/S indican los extremos del eje."),
        p(span("Blanco:", class = "white"), " terminador día/noche y centro terrestre."),
        p(span("Verde:", class = "green"), " plano ecuatorial interno."),
        p(span("Azul:", class = "blue"), " plano interno de Greenwich y vector centro → Greenwich/Ecuador."),
        p(span("Naranja:", class = "orange"), " proyección solar sobre el ecuador."),
        p(span("Magenta:", class = "magenta"), " eje del terminador cercano a los polos reales."),
        p(span("Amarillo:", class = "value"), " Sol, punto subsolar, haz solar y mediodía solar.")
      )
    ),

    div(id = ns("earth3d"), class = "earth3d-container"),

    tags$script(HTML(paste0("
      (function() {
        const ids = ", ids_json, ";

        let scene, camera, renderer, controls;
        let earthMesh, terminatorLine, subsolarMarker, sunArrow;
        let earthCenterMarker;
        let sunCenterLine;
        let equatorPlaneMesh;
        let greenwichPlaneMesh;
        let greenwichEquatorVector;
        let solarEquatorProjectionVector;
        let earthAxisLine;
        let earthAxisLabelN;
        let earthAxisLabelS;
        let polarTerminatorAxisLine;
        let equatorLine;
        let greenwichLine;
        let utcZoneWestLine;
        let utcZoneEastLine;
        let utcZoneSurface;
        let solarMeridianLine;
        let solarNoonSemiCircle;
        let subsolarTimeZoneSurface = null;
        let graticuleLines = [];
        let earthMaterial;
        let overlayMesh;
        let overlayMaterial;
        let starField;
        let autoRotate = false;
        let initialized = false;
        let currentEarthAxis = new THREE.Vector3(0, 1, 0);
        let currentSubsolarUtcOffset = null;
        let nightLightFactor = 0.12;
        let dayLightFactor = 1.08;
        let dayGradientFactor = 0.35;

        let layerVisibility = {
          show_equator: true,
          show_greenwich: true,
          show_graticule: true,
          show_utc0: true,
          show_subsolar_timezone: true,
          show_texture: true,
          show_blue_sphere: true,
          show_overlay: true,
          show_stars: true,
          show_terminator: true,
          show_sun_arrow: true,
          show_subsolar_point: true,
          show_sun_center_line: true,
          show_earth_center: true,
          show_equator_plane: true,
          show_greenwich_plane: true,
          show_greenwich_equator_vector: true,
          show_solar_equator_projection: true,
          show_solar_meridian: true,
          show_earth_axis: true,
          show_polar_terminator_axis: true
        };

        const R = 1.0;
        const internalVectorLength = 0.88;
        const fixedSunDir = new THREE.Vector3(1, 0, 0).normalize();

        function setupToggleButton(buttonId, optionsId) {
          const btn = document.getElementById(buttonId);
          const options = document.getElementById(optionsId);

          if (!btn || !options) return;

          options.style.display = 'none';
          btn.classList.add('collapsed');

          btn.addEventListener('click', function() {
            if (options.style.display === 'none') {
              options.style.display = 'block';
              btn.classList.remove('collapsed');
            } else {
              options.style.display = 'none';
              btn.classList.add('collapsed');
            }
          });
        }

        setupToggleButton(ids.toggle_gis, ids.gis_options);
        setupToggleButton(ids.toggle_sim, ids.sim_options);

        function latLonToVector3(latDeg, lonDeg, radius = 1) {
          const lat = latDeg * Math.PI / 180;
          const lon = lonDeg * Math.PI / 180;
          const x = radius * Math.cos(lat) * Math.cos(lon);
          const y = radius * Math.sin(lat);
          const z = -radius * Math.cos(lat) * Math.sin(lon);
          return new THREE.Vector3(x, y, z).normalize().multiplyScalar(radius);
        }

        function setArrowDepth(arrow, depthTest = true) {
          if (!arrow) return;
          if (arrow.line && arrow.line.material) {
            arrow.line.material.depthTest = depthTest;
            arrow.line.material.depthWrite = false;
            arrow.line.material.transparent = true;
          }
          if (arrow.cone && arrow.cone.material) {
            arrow.cone.material.depthTest = depthTest;
            arrow.cone.material.depthWrite = false;
            arrow.cone.material.transparent = true;
          }
        }

        function removeArrowHelper(arrow) {
          if (!arrow) return;
          scene.remove(arrow);
          if (arrow.line) {
            if (arrow.line.geometry) arrow.line.geometry.dispose();
            if (arrow.line.material) arrow.line.material.dispose();
          }
          if (arrow.cone) {
            if (arrow.cone.geometry) arrow.cone.geometry.dispose();
            if (arrow.cone.material) arrow.cone.material.dispose();
          }
        }

        function makeLine(p1, p2, color, opacity = 1.0, depthTest = true) {
          const geometry = new THREE.BufferGeometry().setFromPoints([p1, p2]);
          const material = new THREE.LineBasicMaterial({
            color: color,
            transparent: true,
            opacity: opacity,
            depthTest: depthTest,
            depthWrite: false
          });
          return new THREE.Line(geometry, material);
        }

        function makePolyline(points, color, opacity = 1.0, depthTest = true) {
          const geometry = new THREE.BufferGeometry().setFromPoints(points);
          const material = new THREE.LineBasicMaterial({
            color: color,
            transparent: true,
            opacity: opacity,
            depthTest: depthTest,
            depthWrite: false
          });
          return new THREE.Line(geometry, material);
        }

        function removeObject(obj) {
          if (obj) {
            scene.remove(obj);
            if (obj.geometry) obj.geometry.dispose();
            if (obj.material) obj.material.dispose();
          }
        }

        function makeTextSprite(text, color = '#ffffff', fontSize = 72) {
          const canvas = document.createElement('canvas');
          const context = canvas.getContext('2d');
          canvas.width = 256;
          canvas.height = 256;
          context.clearRect(0, 0, canvas.width, canvas.height);
          context.font = 'bold ' + fontSize + 'px Arial';
          context.textAlign = 'center';
          context.textBaseline = 'middle';
          context.lineWidth = 9;
          context.strokeStyle = 'rgba(0, 0, 0, 0.90)';
          context.strokeText(text, canvas.width / 2, canvas.height / 2);
          context.fillStyle = color;
          context.fillText(text, canvas.width / 2, canvas.height / 2);
          const texture = new THREE.CanvasTexture(canvas);
          texture.needsUpdate = true;
          const material = new THREE.SpriteMaterial({
            map: texture,
            transparent: true,
            depthTest: false,
            depthWrite: false
          });
          const sprite = new THREE.Sprite(material);
          sprite.scale.set(0.16, 0.16, 0.16);
          sprite.renderOrder = 3000;
          return sprite;
        }

        function removeSprite(sprite) {
          if (!sprite) return;
          scene.remove(sprite);
          if (sprite.material) {
            if (sprite.material.map) sprite.material.map.dispose();
            sprite.material.dispose();
          }
        }

        function makeLatLonLine(lat1, lat2, lon, radius, color, opacity = 1.0) {
          const points = [];
          for (let lat = lat1; lat <= lat2; lat += 2) {
            const p = latLonToVector3(lat, lon, radius);
            p.applyQuaternion(earthMesh.quaternion);
            points.push(p);
          }
          return makePolyline(points, color, opacity);
        }

        function normalizeLon(lon) {
          while (lon < -180) lon += 360;
          while (lon > 180) lon -= 360;
          return lon;
        }

        function applyLayerVisibility() {
          if (equatorLine) equatorLine.visible = layerVisibility.show_equator;
          if (greenwichLine) greenwichLine.visible = layerVisibility.show_greenwich;
          for (let i = 0; i < graticuleLines.length; i++) {
            graticuleLines[i].visible = layerVisibility.show_graticule;
          }
          if (utcZoneSurface) utcZoneSurface.visible = layerVisibility.show_utc0;
          if (utcZoneWestLine) utcZoneWestLine.visible = layerVisibility.show_utc0;
          if (utcZoneEastLine) utcZoneEastLine.visible = layerVisibility.show_utc0;
          if (subsolarTimeZoneSurface) subsolarTimeZoneSurface.visible = layerVisibility.show_subsolar_timezone;
          if (terminatorLine) terminatorLine.visible = layerVisibility.show_terminator;
          if (sunArrow) sunArrow.visible = layerVisibility.show_sun_arrow;
          if (subsolarMarker) subsolarMarker.visible = layerVisibility.show_subsolar_point;
          if (sunCenterLine) sunCenterLine.visible = layerVisibility.show_sun_center_line;
          if (earthCenterMarker) earthCenterMarker.visible = layerVisibility.show_earth_center;
          if (equatorPlaneMesh) equatorPlaneMesh.visible = layerVisibility.show_equator_plane;
          if (greenwichPlaneMesh) greenwichPlaneMesh.visible = layerVisibility.show_greenwich_plane;
          if (greenwichEquatorVector) greenwichEquatorVector.visible = layerVisibility.show_greenwich_equator_vector;
          if (solarEquatorProjectionVector) solarEquatorProjectionVector.visible = layerVisibility.show_solar_equator_projection;
          if (solarMeridianLine) solarMeridianLine.visible = layerVisibility.show_solar_meridian;
          if (solarNoonSemiCircle) solarNoonSemiCircle.visible = layerVisibility.show_solar_meridian;
          if (earthAxisLine) earthAxisLine.visible = layerVisibility.show_earth_axis;
          if (earthAxisLabelN) earthAxisLabelN.visible = layerVisibility.show_earth_axis;
          if (earthAxisLabelS) earthAxisLabelS.visible = layerVisibility.show_earth_axis;
          if (polarTerminatorAxisLine) polarTerminatorAxisLine.visible = layerVisibility.show_polar_terminator_axis;
          if (starField) starField.visible = layerVisibility.show_stars;
          if (earthMaterial && earthMesh) {
            earthMaterial.uniforms.showTexture.value = layerVisibility.show_texture ? 1.0 : 0.0;
            earthMesh.visible = layerVisibility.show_texture || layerVisibility.show_blue_sphere;
          }
          if (overlayMesh && overlayMaterial) {
            const hasOverlayMap = overlayMaterial.uniforms.overlayMap.value !== null;
            overlayMesh.visible = layerVisibility.show_overlay && hasOverlayMap;
            overlayMaterial.uniforms.showOverlay.value = layerVisibility.show_overlay ? 1.0 : 0.0;
          }
        }

        function getEarthAxisFromSubsolarLatitude(latDeg) {
          const delta = latDeg * Math.PI / 180;
          return new THREE.Vector3(Math.sin(delta), Math.cos(delta), 0).normalize();
        }

        function quaternionFromTwoBases(localA, localB, localC, worldA, worldB, worldC) {
          const mLocal = new THREE.Matrix4();
          const mWorld = new THREE.Matrix4();
          mLocal.makeBasis(localA, localB, localC);
          mWorld.makeBasis(worldA, worldB, worldC);
          const qLocal = new THREE.Quaternion().setFromRotationMatrix(mLocal);
          const qWorld = new THREE.Quaternion().setFromRotationMatrix(mWorld);
          return qWorld.multiply(qLocal.invert());
        }

        function orientEarthToSubsolar(lat, lon) {
          if (!earthMesh) return;
          const localSubsolar = latLonToVector3(lat, lon, 1.0).normalize();
          const localNorthAxis = new THREE.Vector3(0, 1, 0);
          currentEarthAxis = getEarthAxisFromSubsolarLatitude(lat);
          const localA = localSubsolar.clone();
          let localB = localNorthAxis.clone().sub(localA.clone().multiplyScalar(localNorthAxis.dot(localA)));
          if (localB.length() < 0.001) {
            localB = new THREE.Vector3(0, 0, 1);
          } else {
            localB.normalize();
          }
          const localC = new THREE.Vector3().crossVectors(localA, localB).normalize();
          const worldA = fixedSunDir.clone().normalize();
          let worldB = currentEarthAxis.clone().sub(worldA.clone().multiplyScalar(currentEarthAxis.dot(worldA)));
          if (worldB.length() < 0.001) {
            worldB = new THREE.Vector3(0, 1, 0);
          } else {
            worldB.normalize();
          }
          const worldC = new THREE.Vector3().crossVectors(worldA, worldB).normalize();
          const q = quaternionFromTwoBases(localA, localB, localC, worldA, worldB, worldC);
          earthMesh.quaternion.copy(q);
          if (overlayMesh) overlayMesh.quaternion.copy(q);
          updateGeographicLines();
          updateGraticule();
          updateAxes();
          updateSolarMeridian();
          updateEquatorPlane();
          updateGreenwichPlane();
          updateGreenwichEquatorVector();
          updateSolarEquatorProjectionVector();
          applyLayerVisibility();
        }

        function createLonBandSurface(lonWest, lonEast, radius, color, opacity) {
          const vertices = [];
          const indices = [];
          const latStep = 5;
          const lonStep = 2.5;
          const lats = [];
          const lons = [];
          for (let lat = -90; lat <= 90; lat += latStep) lats.push(lat);
          for (let lon = lonWest; lon <= lonEast; lon += lonStep) lons.push(lon);
          if (lons[lons.length - 1] < lonEast) lons.push(lonEast);
          for (let i = 0; i < lats.length; i++) {
            for (let j = 0; j < lons.length; j++) {
              const p = latLonToVector3(lats[i], normalizeLon(lons[j]), radius);
              p.applyQuaternion(earthMesh.quaternion);
              vertices.push(p.x, p.y, p.z);
            }
          }
          const cols = lons.length;
          for (let i = 0; i < lats.length - 1; i++) {
            for (let j = 0; j < lons.length - 1; j++) {
              const a = i * cols + j;
              const b = a + 1;
              const c = a + cols;
              const d = c + 1;
              indices.push(a, c, b);
              indices.push(b, c, d);
            }
          }
          const geometry = new THREE.BufferGeometry();
          geometry.setAttribute('position', new THREE.Float32BufferAttribute(vertices, 3));
          geometry.setIndex(indices);
          geometry.computeVertexNormals();
          const material = new THREE.MeshBasicMaterial({
            color: color,
            transparent: true,
            opacity: opacity,
            side: THREE.DoubleSide,
            depthWrite: false
          });
          return new THREE.Mesh(geometry, material);
        }

        function clearSubsolarTimeZoneSurface() {
          removeObject(subsolarTimeZoneSurface);
          subsolarTimeZoneSurface = null;
        }

        function createSubsolarUTCZoneSurface(subsolarUtcOffset) {
          clearSubsolarTimeZoneSurface();
          let centerLon = subsolarUtcOffset * 15;
          let lonWest = centerLon - 7.5;
          let lonEast = centerLon + 7.5;
          if (lonWest < -180) lonWest = -180;
          if (lonEast > 180) lonEast = 180;
          subsolarTimeZoneSurface = createLonBandSurface(lonWest, lonEast, 1.027, 0xffcc00, 0.26);
          scene.add(subsolarTimeZoneSurface);
          applyLayerVisibility();
        }

        function createUTCZoneSurface() {
          removeObject(utcZoneSurface);
          utcZoneSurface = createLonBandSurface(-7.5, 7.5, 1.023, 0x3399ff, 0.18);
          scene.add(utcZoneSurface);
        }

        function updateGeographicLines() {
          if (!earthMesh) return;
          removeObject(equatorLine);
          removeObject(greenwichLine);
          removeObject(utcZoneWestLine);
          removeObject(utcZoneEastLine);
          removeObject(utcZoneSurface);
          const equatorPoints = [];
          for (let lon = -180; lon < 180; lon += 2) {
            const p = latLonToVector3(0, lon, 1.013);
            p.applyQuaternion(earthMesh.quaternion);
            equatorPoints.push(p);
          }
          equatorPoints.push(equatorPoints[0].clone());
          equatorLine = makePolyline(equatorPoints, 0x50fa7b, 1.0);
          scene.add(equatorLine);
          greenwichLine = makeLatLonLine(-90, 90, 0, 1.017, 0x7aa2ff, 1.0);
          scene.add(greenwichLine);
          utcZoneWestLine = makeLatLonLine(-90, 90, -7.5, 1.018, 0x3399ff, 0.85);
          scene.add(utcZoneWestLine);
          utcZoneEastLine = makeLatLonLine(-90, 90, 7.5, 1.018, 0x3399ff, 0.85);
          scene.add(utcZoneEastLine);
          createUTCZoneSurface();
          if (currentSubsolarUtcOffset !== null) createSubsolarUTCZoneSurface(currentSubsolarUtcOffset);
          applyLayerVisibility();
        }

        function clearGraticule() {
          for (let i = 0; i < graticuleLines.length; i++) removeObject(graticuleLines[i]);
          graticuleLines = [];
        }

        function updateGraticule() {
          if (!earthMesh) return;
          clearGraticule();
          const radius = 1.026;
          for (let lat = -75; lat <= 75; lat += 15) {
            const points = [];
            for (let lon = -180; lon < 180; lon += 2) {
              const p = latLonToVector3(lat, lon, radius);
              p.applyQuaternion(earthMesh.quaternion);
              points.push(p);
            }
            if (points.length > 0) points.push(points[0].clone());
            const line = makePolyline(points, 0xaaaaaa, 0.38);
            graticuleLines.push(line);
            scene.add(line);
          }
          for (let lon = -180; lon < 180; lon += 15) {
            const points = [];
            for (let lat = -90; lat <= 90; lat += 2) {
              const p = latLonToVector3(lat, lon, radius);
              p.applyQuaternion(earthMesh.quaternion);
              points.push(p);
            }
            const line = makePolyline(points, 0xaaaaaa, 0.38);
            graticuleLines.push(line);
            scene.add(line);
          }
          applyLayerVisibility();
        }

        function createTerminator() {
          removeObject(terminatorLine);
          const normal = fixedSunDir.clone().normalize();
          let helper = new THREE.Vector3(0, 1, 0);
          if (Math.abs(normal.dot(helper)) > 0.95) helper = new THREE.Vector3(0, 0, 1);
          const u = new THREE.Vector3().crossVectors(normal, helper).normalize();
          const v = new THREE.Vector3().crossVectors(normal, u).normalize();
          const points = [];
          const radius = 1.009;
          for (let i = 0; i < 360; i++) {
            const a = i * Math.PI / 180;
            const p = new THREE.Vector3().addScaledVector(u, Math.cos(a) * radius).addScaledVector(v, Math.sin(a) * radius);
            points.push(p);
          }
          points.push(points[0].clone());
          terminatorLine = makePolyline(points, 0xffffff, 0.95);
          scene.add(terminatorLine);
          applyLayerVisibility();
        }

        function updateAxes() {
          if (!earthMesh) return;
          removeObject(earthAxisLine);
          removeObject(polarTerminatorAxisLine);
          removeSprite(earthAxisLabelN);
          removeSprite(earthAxisLabelS);
          earthAxisLabelN = null;
          earthAxisLabelS = null;
          const earthAxis = currentEarthAxis.clone().normalize();
          earthAxisLine = makeLine(earthAxis.clone().multiplyScalar(-1.50), earthAxis.clone().multiplyScalar(1.50), 0xff3333, 1.0);
          scene.add(earthAxisLine);
          earthAxisLabelN = makeTextSprite('N', '#ff5555', 84);
          earthAxisLabelS = makeTextSprite('S', '#ff5555', 84);
          earthAxisLabelN.position.copy(earthAxis.clone().multiplyScalar(1.63));
          earthAxisLabelS.position.copy(earthAxis.clone().multiplyScalar(-1.63));
          scene.add(earthAxisLabelN);
          scene.add(earthAxisLabelS);
          let polarTermAxis = earthAxis.clone().sub(fixedSunDir.clone().multiplyScalar(earthAxis.dot(fixedSunDir)));
          if (polarTermAxis.length() < 0.001) {
            polarTermAxis = new THREE.Vector3(0, 0, 1);
          } else {
            polarTermAxis.normalize();
          }
          polarTerminatorAxisLine = makeLine(polarTermAxis.clone().multiplyScalar(-1.44), polarTermAxis.clone().multiplyScalar(1.44), 0xff79c6, 1.0);
          scene.add(polarTerminatorAxisLine);
          applyLayerVisibility();
        }

        function updateSolarMeridian() {
          if (!earthMesh) return;
          removeObject(solarMeridianLine);
          removeObject(solarNoonSemiCircle);
          const earthAxis = currentEarthAxis.clone().normalize();
          const meridianNormal = new THREE.Vector3().crossVectors(earthAxis, fixedSunDir).normalize();
          if (meridianNormal.length() < 0.001) return;
          const u = fixedSunDir.clone().normalize();
          const v = new THREE.Vector3().crossVectors(meridianNormal, u).normalize();
          const fullPoints = [];
          for (let i = 0; i < 360; i++) {
            const a = i * Math.PI / 180;
            const p = new THREE.Vector3().addScaledVector(u, Math.cos(a) * 1.023).addScaledVector(v, Math.sin(a) * 1.023);
            fullPoints.push(p);
          }
          fullPoints.push(fullPoints[0].clone());
          solarMeridianLine = makePolyline(fullPoints, 0xffcc00, 0.72);
          scene.add(solarMeridianLine);
          const semiPoints = [];
          for (let i = -90; i <= 90; i++) {
            const a = i * Math.PI / 180;
            const p = new THREE.Vector3().addScaledVector(u, Math.cos(a) * 1.045).addScaledVector(v, Math.sin(a) * 1.045);
            semiPoints.push(p);
          }
          solarNoonSemiCircle = makePolyline(semiPoints, 0xffcc00, 1.0);
          scene.add(solarNoonSemiCircle);
          applyLayerVisibility();
        }

        function createSubsolarMarker() {
          if (subsolarMarker) {
            scene.remove(subsolarMarker);
            subsolarMarker.geometry.dispose();
            subsolarMarker.material.dispose();
          }
          const geometry = new THREE.SphereGeometry(0.038, 32, 32);
          const material = new THREE.MeshBasicMaterial({
            color: 0xffcc00,
            transparent: true,
            opacity: 1.0,
            depthTest: true,
            depthWrite: false
          });
          subsolarMarker = new THREE.Mesh(geometry, material);
          subsolarMarker.position.copy(fixedSunDir.clone().multiplyScalar(1.075));
          subsolarMarker.renderOrder = 1800;
          scene.add(subsolarMarker);
          applyLayerVisibility();
        }

        function createEarthCenterMarker() {
          if (earthCenterMarker) {
            scene.remove(earthCenterMarker);
            earthCenterMarker.geometry.dispose();
            earthCenterMarker.material.dispose();
          }
          const geometry = new THREE.SphereGeometry(0.028, 32, 32);
          const material = new THREE.MeshBasicMaterial({
            color: 0xffffff,
            transparent: true,
            opacity: 1.0,
            depthTest: true,
            depthWrite: false
          });
          earthCenterMarker = new THREE.Mesh(geometry, material);
          earthCenterMarker.position.set(0, 0, 0);
          scene.add(earthCenterMarker);
          applyLayerVisibility();
        }

        function createSunCenterLine() {
          removeObject(sunCenterLine);
          const p1 = fixedSunDir.clone().multiplyScalar(1.00);
          const p2 = new THREE.Vector3(0, 0, 0);
          sunCenterLine = makeLine(p1, p2, 0xffcc00, 1.0, true);
          scene.add(sunCenterLine);
          applyLayerVisibility();
        }

        function updateEquatorPlane() {
          removeObject(equatorPlaneMesh);
          if (!earthMesh) return;
          const geometry = new THREE.CircleGeometry(0.995, 160);
          const material = new THREE.MeshBasicMaterial({
            color: 0x50fa7b,
            transparent: true,
            opacity: 0.18,
            side: THREE.DoubleSide,
            depthTest: true,
            depthWrite: false
          });
          equatorPlaneMesh = new THREE.Mesh(geometry, material);
          const qLocal = new THREE.Quaternion();
          qLocal.setFromAxisAngle(new THREE.Vector3(1, 0, 0), Math.PI / 2);
          equatorPlaneMesh.quaternion.copy(earthMesh.quaternion).multiply(qLocal);
          equatorPlaneMesh.position.set(0, 0, 0);
          scene.add(equatorPlaneMesh);
          applyLayerVisibility();
        }

        function updateGreenwichPlane() {
          removeObject(greenwichPlaneMesh);
          if (!earthMesh) return;
          const geometry = new THREE.CircleGeometry(0.992, 160);
          const material = new THREE.MeshBasicMaterial({
            color: 0x7aa2ff,
            transparent: true,
            opacity: 0.16,
            side: THREE.DoubleSide,
            depthTest: true,
            depthWrite: false
          });
          greenwichPlaneMesh = new THREE.Mesh(geometry, material);
          greenwichPlaneMesh.quaternion.copy(earthMesh.quaternion);
          greenwichPlaneMesh.position.set(0, 0, 0);
          scene.add(greenwichPlaneMesh);
          applyLayerVisibility();
        }

        function updateGreenwichEquatorVector() {
          removeArrowHelper(greenwichEquatorVector);
          greenwichEquatorVector = null;
          if (!earthMesh) return;
          const end = latLonToVector3(0, 0, internalVectorLength);
          end.applyQuaternion(earthMesh.quaternion);
          const dir = end.clone().normalize();
          const origin = new THREE.Vector3(0, 0, 0);
          greenwichEquatorVector = new THREE.ArrowHelper(dir, origin, internalVectorLength, 0x7aa2ff, 0.08, 0.045);
          setArrowDepth(greenwichEquatorVector, true);
          scene.add(greenwichEquatorVector);
          applyLayerVisibility();
        }

        function updateSolarEquatorProjectionVector() {
          removeArrowHelper(solarEquatorProjectionVector);
          solarEquatorProjectionVector = null;
          const earthAxis = currentEarthAxis.clone().normalize();
          let proj = fixedSunDir.clone().sub(earthAxis.clone().multiplyScalar(fixedSunDir.dot(earthAxis)));
          if (proj.length() < 0.0001) return;
          proj.normalize();
          const origin = new THREE.Vector3(0, 0, 0);
          solarEquatorProjectionVector = new THREE.ArrowHelper(proj, origin, internalVectorLength, 0xff9900, 0.08, 0.045);
          setArrowDepth(solarEquatorProjectionVector, true);
          scene.add(solarEquatorProjectionVector);
          applyLayerVisibility();
        }

        function createSunArrow() {
          if (sunArrow) scene.remove(sunArrow);
          const origin = fixedSunDir.clone().multiplyScalar(2.5);
          const direction = fixedSunDir.clone().multiplyScalar(-1);
          sunArrow = new THREE.ArrowHelper(direction, origin, 0.85, 0xffcc00, 0.16, 0.08);
          setArrowDepth(sunArrow, true);
          scene.add(sunArrow);
          applyLayerVisibility();
        }

        function updateEarthTexture(url) {
          if (!earthMaterial || !url) return;
          const loader = new THREE.TextureLoader();
          loader.crossOrigin = '';
          loader.load(
            url,
            function(texture) {
              texture.wrapS = THREE.ClampToEdgeWrapping;
              texture.wrapT = THREE.ClampToEdgeWrapping;
              texture.minFilter = THREE.LinearFilter;
              earthMaterial.uniforms.earthMap.value = texture;
              earthMaterial.uniforms.earthMap.value.needsUpdate = true;
              earthMaterial.needsUpdate = true;
            },
            undefined,
            function(error) {
              console.error('No se pudo cargar la textura base:', url, error);
            }
          );
        }

        function updateOverlayTexture(url) {
          if (!overlayMaterial || !overlayMesh) return;
          if (!url || url === '') {
            overlayMaterial.uniforms.overlayMap.value = null;
            overlayMaterial.needsUpdate = true;
            overlayMesh.visible = false;
            return;
          }
          const loader = new THREE.TextureLoader();
          loader.crossOrigin = '';
          loader.load(
            url,
            function(texture) {
              texture.wrapS = THREE.ClampToEdgeWrapping;
              texture.wrapT = THREE.ClampToEdgeWrapping;
              texture.minFilter = THREE.LinearFilter;
              overlayMaterial.uniforms.overlayMap.value = texture;
              overlayMaterial.uniforms.overlayMap.value.needsUpdate = true;
              overlayMaterial.needsUpdate = true;
              overlayMesh.visible = layerVisibility.show_overlay;
            },
            undefined,
            function(error) {
              console.error('No se pudo cargar la capa transparente:', url, error);
            }
          );
        }

        function initEarth(initialTextureUrl, initialOverlayUrl) {
          if (initialized) return;
          initialized = true;

          scene = new THREE.Scene();
          scene.background = new THREE.Color(0x050816);
          camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 1000);
          camera.position.set(0, 0.65, 5.2);
          renderer = new THREE.WebGLRenderer({ antialias: true });
          renderer.setSize(window.innerWidth, window.innerHeight);
          renderer.setPixelRatio(window.devicePixelRatio);
          document.getElementById(ids.earth3d).appendChild(renderer.domElement);
          controls = new THREE.OrbitControls(camera, renderer.domElement);
          controls.enableDamping = true;
          controls.dampingFactor = 0.05;
          controls.enablePan = false;

          const loader = new THREE.TextureLoader();
          loader.crossOrigin = '';
          const earthTexture = loader.load(initialTextureUrl);

          const earthVertexShader = `
            varying vec2 vUv;
            varying vec3 vWorldNormal;
            void main() {
              vUv = uv;
              vWorldNormal = normalize(mat3(modelMatrix) * normal);
              gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
            }
          `;

          const earthFragmentShader = `
            uniform sampler2D earthMap;
            uniform vec3 sunDirection;
            uniform float showTexture;
            uniform float nightLightFactor;
            uniform float dayLightFactor;
            uniform float dayGradientFactor;
            varying vec2 vUv;
            varying vec3 vWorldNormal;
            void main() {
              vec3 texColor = texture2D(earthMap, vUv).rgb;
              vec3 plainColor = vec3(0.12, 0.32, 0.62);
              vec3 baseColor = mix(plainColor, texColor, showTexture);
              float light = dot(normalize(vWorldNormal), normalize(sunDirection));
              float day = smoothstep(-0.04, 0.08, light);
              vec3 nightColor = baseColor * vec3(nightLightFactor, nightLightFactor, nightLightFactor * 1.15);
              float sunCenter = clamp(light, 0.0, 1.0);
              float dayShape = mix(1.0, pow(sunCenter, 0.55), dayGradientFactor);
              vec3 dayColor = baseColor * vec3(dayLightFactor, dayLightFactor, dayLightFactor * 0.95) * dayShape;
              vec3 finalColor = mix(nightColor, dayColor, day);
              float terminatorGlow = 1.0 - smoothstep(0.0, 0.06, abs(light));
              finalColor += vec3(1.0, 0.72, 0.25) * terminatorGlow * 0.10;
              gl_FragColor = vec4(finalColor, 1.0);
            }
          `;

          earthMaterial = new THREE.ShaderMaterial({
            uniforms: {
              earthMap: { value: earthTexture },
              sunDirection: { value: fixedSunDir },
              showTexture: { value: 1.0 },
              nightLightFactor: { value: nightLightFactor },
              dayLightFactor: { value: dayLightFactor },
              dayGradientFactor: { value: dayGradientFactor }
            },
            vertexShader: earthVertexShader,
            fragmentShader: earthFragmentShader
          });

          const earthGeometry = new THREE.SphereGeometry(R, 160, 160);
          earthMesh = new THREE.Mesh(earthGeometry, earthMaterial);
          scene.add(earthMesh);

          const overlayVertexShader = `
            varying vec2 vUv;
            varying vec3 vWorldNormal;
            void main() {
              vUv = uv;
              vWorldNormal = normalize(mat3(modelMatrix) * normal);
              gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
            }
          `;

          const overlayFragmentShader = `
            uniform sampler2D overlayMap;
            uniform vec3 sunDirection;
            uniform float showOverlay;
            uniform float nightLightFactor;
            uniform float dayLightFactor;
            uniform float dayGradientFactor;
            varying vec2 vUv;
            varying vec3 vWorldNormal;
            void main() {
              vec4 texColor = texture2D(overlayMap, vUv);
              if (texColor.a < 0.01 || showOverlay < 0.5) discard;
              float light = dot(normalize(vWorldNormal), normalize(sunDirection));
              float day = smoothstep(-0.04, 0.08, light);
              vec3 nightColor = texColor.rgb * vec3(nightLightFactor, nightLightFactor, nightLightFactor * 1.15);
              float sunCenter = clamp(light, 0.0, 1.0);
              float dayShape = mix(1.0, pow(sunCenter, 0.55), dayGradientFactor);
              vec3 dayColor = texColor.rgb * vec3(dayLightFactor, dayLightFactor, dayLightFactor * 0.95) * dayShape;
              vec3 finalColor = mix(nightColor, dayColor, day);
              gl_FragColor = vec4(finalColor, texColor.a);
            }
          `;

          const overlayGeometry = new THREE.SphereGeometry(R * 1.003, 160, 160);
          overlayMaterial = new THREE.ShaderMaterial({
            uniforms: {
              overlayMap: { value: null },
              sunDirection: { value: fixedSunDir },
              showOverlay: { value: 1.0 },
              nightLightFactor: { value: nightLightFactor },
              dayLightFactor: { value: dayLightFactor },
              dayGradientFactor: { value: dayGradientFactor }
            },
            vertexShader: overlayVertexShader,
            fragmentShader: overlayFragmentShader,
            transparent: true,
            depthWrite: false,
            side: THREE.DoubleSide
          });

          overlayMesh = new THREE.Mesh(overlayGeometry, overlayMaterial);
          overlayMesh.quaternion.copy(earthMesh.quaternion);
          scene.add(overlayMesh);

          if (initialOverlayUrl && initialOverlayUrl !== '') {
            updateOverlayTexture(initialOverlayUrl);
          } else {
            overlayMesh.visible = false;
          }

          const starsGeometry = new THREE.BufferGeometry();
          const stars = [];
          for (let i = 0; i < 1200; i++) {
            stars.push(THREE.MathUtils.randFloatSpread(100), THREE.MathUtils.randFloatSpread(100), THREE.MathUtils.randFloatSpread(100));
          }
          starsGeometry.setAttribute('position', new THREE.Float32BufferAttribute(stars, 3));
          const starsMaterial = new THREE.PointsMaterial({ color: 0xffffff, size: 0.04 });
          starField = new THREE.Points(starsGeometry, starsMaterial);
          scene.add(starField);

          createTerminator();
          createSubsolarMarker();
          createEarthCenterMarker();
          createSunCenterLine();
          createSunArrow();
          updateGeographicLines();
          updateGraticule();
          updateAxes();
          updateSolarMeridian();
          updateEquatorPlane();
          updateGreenwichPlane();
          updateGreenwichEquatorVector();
          updateSolarEquatorProjectionVector();
          applyLayerVisibility();
          animate();
        }

        function updateSunAndEarth(lat, lon, utc, solarTime, subsolarUtcZone, subsolarClock, subsolarUtcOffset, timeSourceLabel) {
          currentSubsolarUtcOffset = subsolarUtcOffset;
          orientEarthToSubsolar(lat, lon);
          document.getElementById(ids.time_source_txt).innerText = timeSourceLabel;
          document.getElementById(ids.utc_txt).innerText = utc;
          document.getElementById(ids.lat_txt).innerText = lat.toFixed(3) + '°';
          document.getElementById(ids.lon_txt).innerText = lon.toFixed(3) + '°';
          document.getElementById(ids.solar_time_txt).innerText = solarTime;
          document.getElementById(ids.subsolar_utc_zone_txt).innerText = subsolarUtcZone;
          document.getElementById(ids.subsolar_clock_txt).innerText = subsolarClock;
          createSubsolarUTCZoneSurface(subsolarUtcOffset);
          applyLayerVisibility();
        }

        function animate() {
          requestAnimationFrame(animate);
          if (autoRotate && earthMesh) {
            const dq = new THREE.Quaternion();
            dq.setFromAxisAngle(currentEarthAxis.clone().normalize(), 0.001);
            earthMesh.quaternion.premultiply(dq);
            if (overlayMesh) overlayMesh.quaternion.premultiply(dq);
            updateGeographicLines();
            updateGraticule();
            updateAxes();
            updateSolarMeridian();
            updateEquatorPlane();
            updateGreenwichPlane();
            updateGreenwichEquatorVector();
            updateSolarEquatorProjectionVector();
            if (currentSubsolarUtcOffset !== null) createSubsolarUTCZoneSurface(currentSubsolarUtcOffset);
            applyLayerVisibility();
          }
          controls.update();
          renderer.render(scene, camera);
        }

        window.addEventListener('resize', function() {
          if (!camera || !renderer) return;
          camera.aspect = window.innerWidth / window.innerHeight;
          camera.updateProjectionMatrix();
          renderer.setSize(window.innerWidth, window.innerHeight);
        });

        Shiny.addCustomMessageHandler('", ns("earth_init"), "', function(msg) {
          initEarth(msg.initialTextureUrl, msg.initialOverlayUrl);
        });

        Shiny.addCustomMessageHandler('", ns("sun_update"), "', function(msg) {
          updateSunAndEarth(
            msg.lat,
            msg.lon,
            msg.utc,
            msg.solar_time,
            msg.subsolar_utc_zone,
            msg.subsolar_clock,
            msg.subsolar_utc_offset,
            msg.time_source_label
          );
        });

        Shiny.addCustomMessageHandler('", ns("autorotate_update"), "', function(msg) {
          autoRotate = msg.autoRotate;
        });

        Shiny.addCustomMessageHandler('", ns("night_light_update"), "', function(msg) {
          nightLightFactor = msg.value / 100.0;
          if (earthMaterial && earthMaterial.uniforms.nightLightFactor) {
            earthMaterial.uniforms.nightLightFactor.value = nightLightFactor;
            earthMaterial.needsUpdate = true;
          }
          if (overlayMaterial && overlayMaterial.uniforms.nightLightFactor) {
            overlayMaterial.uniforms.nightLightFactor.value = nightLightFactor;
            overlayMaterial.needsUpdate = true;
          }
        });

        Shiny.addCustomMessageHandler('", ns("day_light_update"), "', function(msg) {
          dayLightFactor = msg.dayLight / 100.0;
          dayGradientFactor = msg.dayGradient / 100.0;
          if (earthMaterial) {
            if (earthMaterial.uniforms.dayLightFactor) earthMaterial.uniforms.dayLightFactor.value = dayLightFactor;
            if (earthMaterial.uniforms.dayGradientFactor) earthMaterial.uniforms.dayGradientFactor.value = dayGradientFactor;
            earthMaterial.needsUpdate = true;
          }
          if (overlayMaterial) {
            if (overlayMaterial.uniforms.dayLightFactor) overlayMaterial.uniforms.dayLightFactor.value = dayLightFactor;
            if (overlayMaterial.uniforms.dayGradientFactor) overlayMaterial.uniforms.dayGradientFactor.value = dayGradientFactor;
            overlayMaterial.needsUpdate = true;
          }
        });

        Shiny.addCustomMessageHandler('", ns("layer_visibility"), "', function(msg) {
          layerVisibility = msg;
          applyLayerVisibility();
        });

        Shiny.addCustomMessageHandler('", ns("earth_texture_update"), "', function(msg) {
          updateEarthTexture(msg.url);
        });

        Shiny.addCustomMessageHandler('", ns("earth_overlay_update"), "', function(msg) {
          updateOverlayTexture(msg.url);
        });
      })();
    ")))
  )
}

# ============================================================
# MÓDULO SERVER
# ============================================================

mod_satelliteGlobe_02_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    find_bg_layers_dir <- function() {
      candidates <- c(
        file.path(getwd(), "inst", "www", "bg_layers"),
        file.path(getwd(), "www", "bg_layers"),
        system.file("www", "bg_layers", package = "legionGOESapp")
      )
      candidates <- candidates[nzchar(candidates)]
      candidates[file.exists(candidates)][1]
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
        as.integer(year),
        as.integer(month),
        as.integer(day),
        as.integer(hour),
        as.integer(minute),
        as.integer(second)
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
        declination = delta,
        tilt = 23.44,
        utc = format(t, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
        local_solar_time = "12:00 solar aprox.",
        utc_offset = utc_offset,
        utc_offset_label = offset_label,
        subsolar_clock_time = format(local_clock, "%H:%M:%S", tz = "UTC"),
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

    default_overlay_texture <- if (length(wgs84_transparent_files) > 0) {
      wgs84_transparent_files[1]
    } else {
      NA_character_
    }

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

    updateSelectInput(
      session,
      "earth_texture_file",
      choices = wgs84_solid_files,
      selected = default_earth_texture
    )

    updateSelectInput(
      session,
      "earth_overlay_file",
      choices = c("Ninguna" = "", wgs84_transparent_files),
      selected = ifelse(is.na(default_overlay_texture), "", default_overlay_texture)
    )

    selected_utc_time <- reactive({
      if (identical(input$time_source, "system")) {
        return(list(time = Sys.time(), label = "Hora UTC del sistema"))
      }
      if (identical(input$time_source, "web")) {
        return(get_web_utc_time())
      }
      if (identical(input$time_source, "manual")) {
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

    send_sun_update <- function() {
      time_info <- isolate(selected_utc_time())
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
          time_source_label = time_info$label
        )
      )
    }

    session$onFlushed(function() {
      session$sendCustomMessage(
        session$ns("earth_init"),
        list(
          initialTextureUrl = initial_texture_url,
          initialOverlayUrl = initial_overlay_url
        )
      )
      send_sun_update()
    }, once = TRUE)

    observe({
      input$refresh_now
      if (!identical(input$time_source, "manual")) {
        invalidateLater(input$refresh * 1000, session)
      }
      send_sun_update()
    })

    observe({
      session$sendCustomMessage(session$ns("autorotate_update"), list(autoRotate = isTRUE(input$auto_rotate)))
    })

    observe({
      session$sendCustomMessage(session$ns("night_light_update"), list(value = input$night_light))
    })

    observe({
      session$sendCustomMessage(
        session$ns("day_light_update"),
        list(dayLight = input$day_light, dayGradient = input$day_gradient)
      )
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
          show_polar_terminator_axis = isTRUE(input$show_polar_terminator_axis)
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
      overlay_url <- if (is.null(overlay_file) || overlay_file == "") {
        ""
      } else {
        file.path(resource_prefix, "bg_01_wgs84_02_transparent", overlay_file)
      }
      overlay_url <- gsub("\\\\", "/", overlay_url)
      session$sendCustomMessage(session$ns("earth_overlay_update"), list(url = overlay_url))
    }, ignoreInit = TRUE)
  })
}

# ============================================================
# APP COMPLETA DE EJEMPLO
# ============================================================

ui <- fluidPage(
  mod_satelliteGlobe_02_ui("earth")
)

server <- function(input, output, session) {
  mod_satelliteGlobe_02_server("earth")
}

shinyApp(ui, server)

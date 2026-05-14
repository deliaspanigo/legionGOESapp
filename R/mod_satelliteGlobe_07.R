# app.R

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),

    tags$style(HTML("
      html, body {
        margin: 0;
        padding: 0;
        overflow: hidden;
        background: #02030a;
      }

      .app-root {
        position: relative;
        width: 100vw;
        height: 100vh;
        background: radial-gradient(circle at center, #111827 0%, #02030a 75%);
        color: white;
        font-family: Arial, sans-serif;
      }

      #scene_container {
        width: 100%;
        height: 100%;
      }

      .control-panel {
        position: absolute;
        top: 20px;
        left: 20px;
        z-index: 10;
        background: rgba(5, 8, 22, 0.88);
        border: 1px solid rgba(255,255,255,0.18);
        border-radius: 14px;
        padding: 16px 18px;
        width: 360px;
        max-height: calc(100vh - 40px);
        overflow-y: auto;
        backdrop-filter: blur(8px);
        box-shadow: 0 10px 30px rgba(0,0,0,0.35);
      }

      .control-panel h3 {
        margin-top: 0;
        margin-bottom: 12px;
        font-size: 18px;
      }

      .control-panel h4 {
        margin-top: 16px;
        margin-bottom: 8px;
        font-size: 14px;
        color: #bfdbfe;
        border-top: 1px solid rgba(255,255,255,0.16);
        padding-top: 10px;
      }

      .control-panel label {
        display: block;
        margin-top: 10px;
        font-size: 14px;
        color: #e5e7eb;
      }

      .reset-button {
        width: 100%;
        margin-top: 16px;
        padding: 9px 10px;
        border: 0;
        border-radius: 10px;
        background: #2563eb;
        color: white;
        font-weight: bold;
        cursor: pointer;
      }

      .reset-button:hover {
        background: #1d4ed8;
      }

      .small-note {
        margin-top: 12px;
        font-size: 11px;
        color: #9ca3af;
        line-height: 1.35;
      }
    "))
  ),

  div(
    class = "app-root",

    div(id = "scene_container"),

    div(
      class = "control-panel",

      h3("Tierra - Sol"),

      h4("Tierra"),

      tags$label(
        tags$input(id = "show_earth_solid_surface", type = "checkbox", checked = "checked"),
        " Mostrar superficie sólida de la Tierra"
      ),

      tags$label(
        tags$input(id = "show_earth_surface_grid", type = "checkbox", checked = "checked"),
        " Mostrar malla de la Tierra"
      ),

      h4("Referencias"),

      tags$label(
        tags$input(id = "show_optical_axis", type = "checkbox", checked = "checked"),
        " Mostrar eje óptico"
      ),

      tags$label(
        tags$input(id = "show_solar_point", type = "checkbox", checked = "checked"),
        " Mostrar punto solar"
      ),

      tags$label(
        tags$input(id = "show_solar_axis", type = "checkbox", checked = "checked"),
        " Mostrar eje solar"
      ),

      tags$label(
        tags$input(id = "show_earth_ns_axis", type = "checkbox", checked = "checked"),
        " Mostrar eje Norte-Sur terrestre"
      ),

      tags$button(
        id = "reset_view",
        type = "button",
        class = "reset-button",
        "Volver a posición inicial"
      ),

      div(
        class = "small-note",
        "Eje óptico: une el centro del Sol con el centro de la Tierra.",
        tags$br(),
        "La luz viene desde el Sol con rayos paralelos al eje óptico.",
        tags$br(),
        "Punto solar: intersección del eje óptico con la superficie terrestre.",
        tags$br(),
        "Eje solar: perpendicular al eje óptico y pasa por el centro de la Tierra.",
        tags$br(),
        "Eje Norte-Sur terrestre: definido con la inclinación axial de la Tierra.",
        tags$br(),
        "La malla de la Tierra usa como referencia el eje Norte-Sur inclinado."
      )
    )
  ),

  tags$script(HTML("
    (function() {
      let scene, camera, renderer, controls;

      let earthMesh;
      let earthSolidSurfaceMesh;
      let earthSurfaceGridGroup;

      let sunMesh;

      let opticalAxisGroup;
      let solarPointGroup;
      let solarAxisGroup;
      let earthNSAxisGroup;

      let ambientLight;
      let sunLight;
      let sunLightTarget;

      const earthRadius = 2.0;
      const sunRadius = 0.45;

      /*
        Sistema geométrico simple:

        Tierra:
          centro en (0, 0, 0)

        Sol:
          centro en (+6, 0, 0)

        Eje óptico:
          dirección +X si se mira desde Tierra hacia Sol.
          La luz viaja en sentido contrario: desde Sol hacia Tierra.

        Punto solar:
          sobre superficie terrestre en (+earthRadius, 0, 0)

        Eje solar:
          perpendicular al eje óptico.
          Como el eje óptico es X, usamos Y.

        Eje Norte-Sur terrestre:
          inclinado respecto del eje solar.
      */

      const earthCenter = new THREE.Vector3(0, 0, 0);
      const sunCenter = new THREE.Vector3(6, 0, 0);

      const opticalDirection = new THREE.Vector3(1, 0, 0);

      /*
        Eje solar:
        perpendicular al eje óptico.
      */
      const solarAxisDirection = new THREE.Vector3(0, 1, 0);

      /*
        Eje Norte-Sur terrestre inclinado.

        Parte del eje solar Y y se inclina 23.439281°
        rotándolo alrededor de Z.
      */
      const earthTiltDeg = 23.439281;
      const earthTiltRad = THREE.MathUtils.degToRad(earthTiltDeg);

      const earthNSAxisDirection = new THREE.Vector3(
        -Math.sin(earthTiltRad),
         Math.cos(earthTiltRad),
         0
      ).normalize();

      const initialCameraPosition = new THREE.Vector3(6.5, 3.0, 7.0);
      const initialControlsTarget = new THREE.Vector3(0, 0, 0);

      function byId(id) {
        return document.getElementById(id);
      }

      function init() {
        const container = byId('scene_container');

        scene = new THREE.Scene();

        const w = container.clientWidth || window.innerWidth;
        const h = container.clientHeight || window.innerHeight;

        camera = new THREE.PerspectiveCamera(45, w / h, 0.1, 1000);
        camera.position.copy(initialCameraPosition);
        camera.lookAt(initialControlsTarget);

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
        controls.minDistance = 3;
        controls.maxDistance = 18;
        controls.target.copy(initialControlsTarget);
        controls.update();

        addLights();

        createEarth();
        createEarthSolidSurface();
        createEarthSurfaceGrid();

        createSun();

        createOpticalAxis();
        createSolarPoint();
        createSolarAxis();
        createEarthNSAxis();

        setupCheckboxes();
        setupResetButton();

        window.addEventListener('resize', onWindowResize);

        animate();
      }

      function addLights() {
        /*
          Luz ambiente baja:
          permite ver un poco el lado oscuro,
          pero conserva claramente el efecto día/noche.
        */
        ambientLight = new THREE.AmbientLight(0xffffff, 0.18);
        scene.add(ambientLight);

        /*
          DirectionalLight genera rayos paralelos.
          La dirección queda definida por:
            posición de la luz -> target

          Acá:
            posición = centro del Sol
            target   = centro de la Tierra

          Por lo tanto, los rayos son paralelos al eje óptico
          y viajan desde el Sol hacia la Tierra.
        */
        sunLight = new THREE.DirectionalLight(0xffffff, 1.85);
        sunLight.position.copy(sunCenter);

        sunLightTarget = new THREE.Object3D();
        sunLightTarget.position.copy(earthCenter);

        scene.add(sunLight);
        scene.add(sunLightTarget);

        sunLight.target = sunLightTarget;
      }

      function createEarth() {
        const geometry = new THREE.SphereGeometry(earthRadius, 96, 96);

        const material = new THREE.MeshPhongMaterial({
          color: 0x2563eb,
          shininess: 18,
          transparent: false,
          opacity: 1.0
        });

        earthMesh = new THREE.Mesh(geometry, material);
        earthMesh.position.copy(earthCenter);

        scene.add(earthMesh);
      }

      function createEarthSolidSurface() {
        /*
          Capa sólida translúcida apenas por encima de la Tierra.
          Sirve para visualizar la superficie como una cáscara.
        */
        const geometry = new THREE.SphereGeometry(earthRadius * 1.006, 96, 96);

        const material = new THREE.MeshPhongMaterial({
          color: 0x22d3ee,
          shininess: 10,
          transparent: true,
          opacity: 0.28,
          depthTest: true,
          depthWrite: false,
          side: THREE.FrontSide
        });

        earthSolidSurfaceMesh = new THREE.Mesh(geometry, material);
        earthSolidSurfaceMesh.renderOrder = 2;
        earthSolidSurfaceMesh.visible = true;

        scene.add(earthSolidSurfaceMesh);
      }

      function createEarthSurfaceGrid() {
        earthSurfaceGridGroup = new THREE.Group();
        scene.add(earthSurfaceGridGroup);

        /*
          La malla se define originalmente con su eje polar en Y.
          Como el eje Norte-Sur terrestre está inclinado respecto de Y,
          rotamos toda la malla para que sus polos coincidan con el eje N-S.
        */
        earthSurfaceGridGroup.rotation.z = earthTiltRad;

        const gridRadius = earthRadius * 1.018;

        const majorMaterial = new THREE.LineBasicMaterial({
          color: 0xe0f2fe,
          transparent: true,
          opacity: 0.95,
          depthTest: true,
          depthWrite: false
        });

        const minorMaterial = new THREE.LineBasicMaterial({
          color: 0x93c5fd,
          transparent: true,
          opacity: 0.55,
          depthTest: true,
          depthWrite: false
        });

        /*
          Líneas de latitud.
          El Ecuador de esta malla queda perpendicular al eje N-S inclinado.
        */
        for (let lat = -75; lat <= 75; lat += 15) {
          const material = lat === 0 ? majorMaterial : minorMaterial;
          const line = createLatitudeLine(lat, gridRadius, material);
          line.renderOrder = 3;
          earthSurfaceGridGroup.add(line);
        }

        /*
          Líneas de longitud.
          Los meridianos convergen en los polos definidos por el eje N-S inclinado.
        */
        for (let lon = -180; lon < 180; lon += 15) {
          const material = lon === 0 ? majorMaterial : minorMaterial;
          const line = createMeridianLine(lon, gridRadius, material);
          line.renderOrder = 3;
          earthSurfaceGridGroup.add(line);
        }

        earthSurfaceGridGroup.visible = true;
      }

      function latLonToVector3(lat, lon, radius) {
        const phi = THREE.MathUtils.degToRad(90 - lat);
        const theta = THREE.MathUtils.degToRad(lon + 180);

        const x = -radius * Math.sin(phi) * Math.cos(theta);
        const z =  radius * Math.sin(phi) * Math.sin(theta);
        const y =  radius * Math.cos(phi);

        return new THREE.Vector3(x, y, z);
      }

      function createLatitudeLine(latDeg, radius, material) {
        const points = [];

        for (let lon = -180; lon <= 180; lon += 2) {
          points.push(latLonToVector3(latDeg, lon, radius));
        }

        const geometry = new THREE.BufferGeometry().setFromPoints(points);
        return new THREE.Line(geometry, material);
      }

      function createMeridianLine(lonDeg, radius, material) {
        const points = [];

        for (let lat = -90; lat <= 90; lat += 2) {
          points.push(latLonToVector3(lat, lonDeg, radius));
        }

        const geometry = new THREE.BufferGeometry().setFromPoints(points);
        return new THREE.Line(geometry, material);
      }

      function createSun() {
        const geometry = new THREE.SphereGeometry(sunRadius, 64, 64);

        const material = new THREE.MeshBasicMaterial({
          color: 0xfacc15,
          transparent: true,
          opacity: 1.0
        });

        sunMesh = new THREE.Mesh(geometry, material);
        sunMesh.position.copy(sunCenter);

        scene.add(sunMesh);

        const glowGeometry = new THREE.SphereGeometry(sunRadius * 1.9, 64, 64);

        const glowMaterial = new THREE.MeshBasicMaterial({
          color: 0xfacc15,
          transparent: true,
          opacity: 0.18,
          depthWrite: false
        });

        const glowMesh = new THREE.Mesh(glowGeometry, glowMaterial);
        glowMesh.position.copy(sunCenter);

        scene.add(glowMesh);
      }

      function createOpticalAxis() {
        opticalAxisGroup = new THREE.Group();
        scene.add(opticalAxisGroup);

        const points = [
          earthCenter.clone(),
          sunCenter.clone()
        ];

        const geometry = new THREE.BufferGeometry().setFromPoints(points);

        const material = new THREE.LineBasicMaterial({
          color: 0xfacc15,
          transparent: true,
          opacity: 0.95,
          depthTest: true,
          depthWrite: false
        });

        const line = new THREE.Line(geometry, material);
        opticalAxisGroup.add(line);

        opticalAxisGroup.visible = true;
      }

      function createSolarPoint() {
        solarPointGroup = new THREE.Group();
        scene.add(solarPointGroup);

        const pointPosition = opticalDirection
          .clone()
          .multiplyScalar(earthRadius * 1.035);

        const geometry = new THREE.SphereGeometry(0.09, 32, 32);

        const material = new THREE.MeshBasicMaterial({
          color: 0xfff7ed,
          transparent: true,
          opacity: 1.0,
          depthTest: true,
          depthWrite: false
        });

        const point = new THREE.Mesh(geometry, material);
        point.position.copy(pointPosition);

        solarPointGroup.add(point);

        solarPointGroup.visible = true;
      }

      function createSolarAxis() {
        solarAxisGroup = new THREE.Group();
        scene.add(solarAxisGroup);

        const axisLength = earthRadius * 3.2;

        const start = solarAxisDirection
          .clone()
          .multiplyScalar(-axisLength / 2);

        const end = solarAxisDirection
          .clone()
          .multiplyScalar(axisLength / 2);

        const geometry = new THREE.BufferGeometry().setFromPoints([
          start,
          end
        ]);

        const material = new THREE.LineBasicMaterial({
          color: 0x38bdf8,
          transparent: true,
          opacity: 0.9,
          depthTest: true,
          depthWrite: false
        });

        const line = new THREE.Line(geometry, material);
        solarAxisGroup.add(line);

        createAxisLabel(
          'Eje solar',
          end.clone().multiplyScalar(1.08),
          solarAxisGroup,
          'rgba(56,189,248,0.98)',
          0.40
        );

        solarAxisGroup.visible = true;
      }

      function createEarthNSAxis() {
        earthNSAxisGroup = new THREE.Group();
        scene.add(earthNSAxisGroup);

        const axisLength = earthRadius * 3.0;

        const start = earthNSAxisDirection
          .clone()
          .multiplyScalar(-axisLength / 2);

        const end = earthNSAxisDirection
          .clone()
          .multiplyScalar(axisLength / 2);

        const geometry = new THREE.BufferGeometry().setFromPoints([
          start,
          end
        ]);

        const material = new THREE.LineBasicMaterial({
          color: 0xffffff,
          transparent: true,
          opacity: 0.95,
          depthTest: true,
          depthWrite: false
        });

        const line = new THREE.Line(geometry, material);
        earthNSAxisGroup.add(line);

        createAxisLabel(
          'N-S',
          end.clone().multiplyScalar(1.08),
          earthNSAxisGroup,
          'rgba(255,255,255,0.98)',
          0.42
        );

        earthNSAxisGroup.visible = true;
      }

      function createAxisLabel(text, position, parentGroup, color, scale) {
        const canvas = document.createElement('canvas');
        canvas.width = 512;
        canvas.height = 128;

        const ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, 512, 128);

        ctx.fillStyle = color || 'rgba(255,255,255,0.95)';
        ctx.font = 'bold 48px Arial';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(text, 256, 64);

        const texture = new THREE.CanvasTexture(canvas);

        const material = new THREE.SpriteMaterial({
          map: texture,
          transparent: true,
          depthTest: true,
          depthWrite: false
        });

        const sprite = new THREE.Sprite(material);
        sprite.position.copy(position);
        sprite.scale.set(scale * 3.0, scale * 0.75, scale);

        parentGroup.add(sprite);
      }

      function setupCheckboxes() {
        const earthSolidSurfaceInput = byId('show_earth_solid_surface');
        const earthSurfaceGridInput = byId('show_earth_surface_grid');

        const opticalAxisInput = byId('show_optical_axis');
        const solarPointInput = byId('show_solar_point');
        const solarAxisInput = byId('show_solar_axis');
        const earthNSAxisInput = byId('show_earth_ns_axis');

        if (earthSolidSurfaceInput) {
          earthSolidSurfaceMesh.visible = earthSolidSurfaceInput.checked;

          earthSolidSurfaceInput.addEventListener('change', function() {
            earthSolidSurfaceMesh.visible = earthSolidSurfaceInput.checked;
          });
        }

        if (earthSurfaceGridInput) {
          earthSurfaceGridGroup.visible = earthSurfaceGridInput.checked;

          earthSurfaceGridInput.addEventListener('change', function() {
            earthSurfaceGridGroup.visible = earthSurfaceGridInput.checked;
          });
        }

        if (opticalAxisInput) {
          opticalAxisGroup.visible = opticalAxisInput.checked;

          opticalAxisInput.addEventListener('change', function() {
            opticalAxisGroup.visible = opticalAxisInput.checked;
          });
        }

        if (solarPointInput) {
          solarPointGroup.visible = solarPointInput.checked;

          solarPointInput.addEventListener('change', function() {
            solarPointGroup.visible = solarPointInput.checked;
          });
        }

        if (solarAxisInput) {
          solarAxisGroup.visible = solarAxisInput.checked;

          solarAxisInput.addEventListener('change', function() {
            solarAxisGroup.visible = solarAxisInput.checked;
          });
        }

        if (earthNSAxisInput) {
          earthNSAxisGroup.visible = earthNSAxisInput.checked;

          earthNSAxisInput.addEventListener('change', function() {
            earthNSAxisGroup.visible = earthNSAxisInput.checked;
          });
        }
      }

      function setupResetButton() {
        const resetButton = byId('reset_view');

        if (!resetButton) {
          return;
        }

        resetButton.addEventListener('click', function() {
          camera.position.copy(initialCameraPosition);
          controls.target.copy(initialControlsTarget);
          camera.lookAt(initialControlsTarget);
          controls.update();
        });
      }

      function onWindowResize() {
        const container = byId('scene_container');
        const w = container.clientWidth || window.innerWidth;
        const h = container.clientHeight || window.innerHeight;

        camera.aspect = w / h;
        camera.updateProjectionMatrix();
        renderer.setSize(w, h);
      }

      function animate() {
        requestAnimationFrame(animate);

        if (controls) {
          controls.update();
        }

        renderer.render(scene, camera);
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
      } else {
        init();
      }
    })();
  "))
)

server <- function(input, output, session) {
}

shinyApp(ui, server)

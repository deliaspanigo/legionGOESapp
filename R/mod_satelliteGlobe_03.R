# app.R

library(shiny)
library(jsonlite)

# devtools::load_all()

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

get_satellite_globe_www_path <- function() {
  if (exists("fn_my_folder_package", mode = "function")) {
    return(file.path(fn_my_folder_package(), "www"))
  }

  candidate_1 <- file.path(getwd(), "inst", "www")
  candidate_2 <- file.path(getwd(), "www")

  if (dir.exists(candidate_1)) {
    return(candidate_1)
  }

  if (dir.exists(candidate_2)) {
    return(candidate_2)
  }

  stop(
    "No encuentro la carpeta www. Probé: ",
    candidate_1,
    " y ",
    candidate_2
  )
}

# ------------------------------------------------------------
# Módulo UI
# ------------------------------------------------------------

mod_satelliteGlobe_02_ui <- function(id) {
  ns <- NS(id)

  tagList(
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

        .sat-globe-root {
          position: relative;
          width: 100vw;
          height: 100vh;
          background: radial-gradient(circle at center, #16213e 0%, #02030a 70%);
          color: white;
          font-family: Arial, sans-serif;
        }

        .sat-globe-container {
          width: 100%;
          height: 100%;
        }

        .sat-globe-control-panel {
          position: absolute;
          top: 20px;
          left: 20px;
          z-index: 10;
          background: rgba(5, 8, 22, 0.86);
          border: 1px solid rgba(255,255,255,0.18);
          border-radius: 14px;
          padding: 16px 18px;
          width: 390px;
          max-height: calc(100vh - 40px);
          overflow-y: auto;
          backdrop-filter: blur(8px);
          box-shadow: 0 10px 30px rgba(0,0,0,0.35);
        }

        .sat-globe-control-panel h3 {
          margin-top: 0;
          margin-bottom: 12px;
          font-size: 18px;
        }

        .sat-globe-control-panel h4 {
          margin-top: 16px;
          margin-bottom: 8px;
          font-size: 14px;
          color: #bfdbfe;
          border-top: 1px solid rgba(255,255,255,0.16);
          padding-top: 10px;
        }

        .sat-globe-control-panel label {
          display: block;
          margin-top: 10px;
          font-size: 14px;
          color: #e5e7eb;
        }

        .sat-globe-control-panel .form-group {
          margin-bottom: 10px;
        }

        .sat-globe-control-panel select {
          width: 100%;
          background: #111827;
          color: white;
          border: 1px solid rgba(255,255,255,0.25);
          border-radius: 8px;
          padding: 6px;
        }

        .sat-globe-utc-box {
          margin-top: 10px;
          margin-bottom: 10px;
          padding: 8px 10px;
          border-radius: 10px;
          background: rgba(15, 23, 42, 0.9);
          border: 1px solid rgba(125, 211, 252, 0.35);
          color: #e0f2fe;
          font-size: 13px;
          font-family: Consolas, monospace;
        }

        .sat-globe-reset-button {
          width: 100%;
          margin-top: 14px;
          padding: 9px 10px;
          border: 0;
          border-radius: 10px;
          background: #2563eb;
          color: white;
          font-weight: bold;
          cursor: pointer;
        }

        .sat-globe-reset-button:hover {
          background: #1d4ed8;
        }

        .sat-globe-small-note {
          margin-top: 10px;
          font-size: 11px;
          color: #9ca3af;
          line-height: 1.35;
          word-break: break-word;
        }
      ")),

      tags$script(HTML("
        (function() {
          const instances = {};

          function byId(id) {
            return document.getElementById(id);
          }

          if (window.satelliteGlobe02HandlerInstalled) {
            return;
          }

          window.satelliteGlobe02HandlerInstalled = true;

          Shiny.addCustomMessageHandler('satelliteGlobe02-init', function(config) {
            if (instances[config.container_id]) {
              return;
            }

            instances[config.container_id] = createSatelliteGlobe02(config);
          });

          function createSatelliteGlobe02(config) {
            let scene, camera, renderer, controls;

            let worldGroup;
            let earthGroup;

            let earthBaseMesh;
            let earthBaseMaterial;

            let earthOverlayMesh;
            let earthOverlayMaterial;

            let shellMesh;
            let surfaceGridGroup;
            let centerPoint;

            let axisGroup;
            let solarAxisGroup;

            let equatorLine;
            let greenwichLine;

            let equatorialPlaneGroup;
            let greenwichPlaneGroup;
            let solarPlaneGroup;

            const earthRadius = 2.0;

            const axialTiltDeg = 23.439281;
            const axialTiltRad = THREE.MathUtils.degToRad(axialTiltDeg);

            const initialCameraPosition = new THREE.Vector3(7.0, 0.0, 0.0);
            const initialControlsTarget = new THREE.Vector3(0, 0, 0);

            let resetAnimation = null;

            let targetWorldRotationX = 0;
            const referenceTransitionSpeed = 0.055;

            init();

            function init() {
              const container = byId(config.container_id);

              if (!container) {
                console.error('No existe el contenedor:', config.container_id);
                return;
              }

              scene = new THREE.Scene();

              worldGroup = new THREE.Group();
              scene.add(worldGroup);

              const w = container.clientWidth || window.innerWidth;
              const h = container.clientHeight || window.innerHeight;

              camera = new THREE.PerspectiveCamera(45, w / h, 0.1, 1000);
              camera.position.copy(initialCameraPosition);
              camera.up.set(0, 1, 0);
              camera.lookAt(initialControlsTarget);

              renderer = new THREE.WebGLRenderer({
                antialias: true,
                alpha: true
              });

              renderer.setPixelRatio(window.devicePixelRatio);
              renderer.setSize(w, h);
              renderer.sortObjects = true;

              container.appendChild(renderer.domElement);

              controls = new THREE.OrbitControls(camera, renderer.domElement);
              controls.enableRotate = true;
              controls.enablePan = false;
              controls.enableZoom = true;
              controls.enableDamping = true;
              controls.dampingFactor = 0.05;
              controls.rotateSpeed = 0.45;
              controls.zoomSpeed = 0.8;
              controls.minDistance = 3;
              controls.maxDistance = 12;
              controls.target.copy(initialControlsTarget);
              controls.update();

              addLights();

              createEarth();
              createInteriorObjects();
              createSurfaceShell();
              createSurfaceGrid();
              createSurfaceReferenceLines();

              createSolarAxis();
              createSolarPlane();

              setupCheckboxes();
              setupTextureSelectors();
              setupResetButton();
              setupVerticalAxisReferenceControl();

              window.addEventListener('resize', onWindowResize);

              animate();
            }

            function addLights() {
              scene.add(new THREE.AmbientLight(0xffffff, 1.35));
            }

            function createEarth() {
              earthGroup = new THREE.Group();
              worldGroup.add(earthGroup);

              const baseGeometry = new THREE.SphereGeometry(earthRadius, 128, 128);

              earthBaseMaterial = new THREE.MeshBasicMaterial({
                color: 0xffffff,
                transparent: false,
                opacity: 1.0,
                depthWrite: true,
                depthTest: true,
                side: THREE.FrontSide
              });

              earthBaseMesh = new THREE.Mesh(baseGeometry, earthBaseMaterial);
              earthBaseMesh.renderOrder = 1;
              earthGroup.add(earthBaseMesh);

              const overlayGeometry = new THREE.SphereGeometry(earthRadius * 1.003, 128, 128);

              earthOverlayMaterial = new THREE.MeshBasicMaterial({
                color: 0xffffff,
                transparent: true,
                opacity: 1.0,
                alphaTest: 0.05,
                depthWrite: true,
                depthTest: true,
                side: THREE.FrontSide
              });

              earthOverlayMesh = new THREE.Mesh(overlayGeometry, earthOverlayMaterial);
              earthOverlayMesh.renderOrder = 2;
              earthOverlayMesh.visible = false;
              earthGroup.add(earthOverlayMesh);

              loadBaseTexture(config.base_texture_url);

              if (config.overlay_texture_url) {
                loadOverlayTexture(config.overlay_texture_url);
              }
            }

            function createInteriorObjects() {
              createCenterPoint();
              createNorthSouthAxis();
              createEquatorialPlane();
              createGreenwichPlane();
            }

            function createCenterPoint() {
              const geometry = new THREE.SphereGeometry(0.075, 32, 32);

              const material = new THREE.MeshBasicMaterial({
                color: 0xfff7ed,
                transparent: true,
                opacity: 1.0,
                depthTest: true,
                depthWrite: false
              });

              centerPoint = new THREE.Mesh(geometry, material);
              centerPoint.position.set(0, 0, 0);
              centerPoint.renderOrder = 3;
              earthGroup.add(centerPoint);
            }

            function createNorthSouthAxis() {
              axisGroup = new THREE.Group();
              earthGroup.add(axisGroup);

              const axisLength = earthRadius * 2.8;

              const points = [
                new THREE.Vector3(0, -axisLength / 2, 0),
                new THREE.Vector3(0,  axisLength / 2, 0)
              ];

              const geometry = new THREE.BufferGeometry().setFromPoints(points);

              const material = new THREE.LineBasicMaterial({
                color: 0xffffff,
                transparent: true,
                opacity: 0.92,
                depthTest: true,
                depthWrite: false
              });

              const axisLine = new THREE.Line(geometry, material);
              axisLine.renderOrder = 4;
              axisGroup.add(axisLine);

              createPoleMarker(
                'N',
                new THREE.Vector3(0, earthRadius * 1.28, 0),
                axisGroup,
                'rgba(255,255,255,0.95)'
              );

              createPoleMarker(
                'S',
                new THREE.Vector3(0, -earthRadius * 1.28, 0),
                axisGroup,
                'rgba(255,255,255,0.95)'
              );
            }

            function createEquatorialPlane() {
              equatorialPlaneGroup = new THREE.Group();
              earthGroup.add(equatorialPlaneGroup);

              const geometry = new THREE.CircleGeometry(earthRadius * 0.985, 180);

              const material = new THREE.MeshBasicMaterial({
                color: 0x22c55e,
                transparent: true,
                opacity: 0.20,
                side: THREE.DoubleSide,
                depthTest: true,
                depthWrite: false
              });

              const plane = new THREE.Mesh(geometry, material);
              plane.rotation.x = Math.PI / 2;
              plane.renderOrder = 3;

              equatorialPlaneGroup.add(plane);
              equatorialPlaneGroup.visible = true;
            }

            function createGreenwichPlane() {
              greenwichPlaneGroup = new THREE.Group();
              earthGroup.add(greenwichPlaneGroup);

              const geometry = new THREE.CircleGeometry(earthRadius * 0.985, 180);

              const material = new THREE.MeshBasicMaterial({
                color: 0xf97316,
                transparent: true,
                opacity: 0.18,
                side: THREE.DoubleSide,
                depthTest: true,
                depthWrite: false
              });

              const plane = new THREE.Mesh(geometry, material);
              plane.renderOrder = 3;

              greenwichPlaneGroup.add(plane);
              greenwichPlaneGroup.visible = false;
            }

            function createSurfaceShell() {
              const geometry = new THREE.SphereGeometry(earthRadius * 0.998, 96, 96);

              const material = new THREE.MeshBasicMaterial({
                color: 0x22d3ee,
                transparent: true,
                opacity: 0.38,
                side: THREE.FrontSide,
                depthTest: true,
                depthWrite: false
              });

              shellMesh = new THREE.Mesh(geometry, material);
              shellMesh.renderOrder = 0;
              shellMesh.visible = false;

              earthGroup.add(shellMesh);
            }

            function createSurfaceGrid() {
              surfaceGridGroup = new THREE.Group();
              earthGroup.add(surfaceGridGroup);

              const gridRadius = earthRadius * 1.009;

              const gridMaterialMajor = new THREE.LineBasicMaterial({
                color: 0xbfd7ff,
                transparent: true,
                opacity: 0.92,
                depthTest: true,
                depthWrite: false
              });

              const gridMaterialMinor = new THREE.LineBasicMaterial({
                color: 0x93c5fd,
                transparent: true,
                opacity: 0.52,
                depthTest: true,
                depthWrite: false
              });

              for (let lat = -75; lat <= 75; lat += 15) {
                const material = lat === 0 ? gridMaterialMajor : gridMaterialMinor;
                const line = createLatitudeLine(lat, material.color.getHex(), gridRadius / earthRadius);
                line.material = material;
                line.renderOrder = 8;
                surfaceGridGroup.add(line);
              }

              for (let lon = -180; lon < 180; lon += 15) {
                const material = lon === 0 ? gridMaterialMajor : gridMaterialMinor;
                const line = createMeridianLine(lon, material.color.getHex(), gridRadius / earthRadius);
                line.material = material;
                line.renderOrder = 8;
                surfaceGridGroup.add(line);
              }

              surfaceGridGroup.visible = true;
            }

            function createSurfaceReferenceLines() {
              equatorLine = createLatitudeLine(0, 0x22c55e, 1.018);
              greenwichLine = createMeridianLine(0, 0xf97316, 1.022);

              equatorLine.renderOrder = 9;
              greenwichLine.renderOrder = 9;

              earthGroup.add(equatorLine);
              earthGroup.add(greenwichLine);

              equatorLine.visible = true;
              greenwichLine.visible = true;
            }

            function createSolarAxis() {
              solarAxisGroup = new THREE.Group();
              worldGroup.add(solarAxisGroup);

              solarAxisGroup.rotation.x = axialTiltRad;

              const axisLength = earthRadius * 3.4;

              const points = [
                new THREE.Vector3(0, -axisLength / 2, 0),
                new THREE.Vector3(0,  axisLength / 2, 0)
              ];

              const geometry = new THREE.BufferGeometry().setFromPoints(points);

              const material = new THREE.LineBasicMaterial({
                color: 0x38bdf8,
                transparent: true,
                opacity: 0.95,
                depthTest: true,
                depthWrite: false
              });

              const axisLine = new THREE.Line(geometry, material);
              axisLine.renderOrder = 7;
              solarAxisGroup.add(axisLine);

              createPoleMarker(
                '☉',
                new THREE.Vector3(0, earthRadius * 1.55, 0),
                solarAxisGroup,
                'rgba(56,189,248,0.98)'
              );

              createPoleMarker(
                '☉',
                new THREE.Vector3(0, -earthRadius * 1.55, 0),
                solarAxisGroup,
                'rgba(56,189,248,0.98)'
              );

              solarAxisGroup.visible = false;
            }

            function createSolarPlane() {
              solarPlaneGroup = new THREE.Group();
              worldGroup.add(solarPlaneGroup);

              solarPlaneGroup.rotation.x = axialTiltRad;

              const plane = createAnnularPlane(
                earthRadius * 1.04,
                earthRadius * 2.3,
                0x38bdf8,
                0.20
              );

              plane.rotation.x = Math.PI / 2;
              plane.renderOrder = 5;

              solarPlaneGroup.add(plane);
              solarPlaneGroup.visible = false;
            }

            function createPoleMarker(text, position, parentGroup, color) {
              const canvas = document.createElement('canvas');
              canvas.width = 128;
              canvas.height = 128;

              const ctx = canvas.getContext('2d');
              ctx.clearRect(0, 0, 128, 128);
              ctx.fillStyle = color || 'rgba(255,255,255,0.95)';
              ctx.font = 'bold 70px Arial';
              ctx.textAlign = 'center';
              ctx.textBaseline = 'middle';
              ctx.fillText(text, 64, 64);

              const texture = new THREE.CanvasTexture(canvas);

              const material = new THREE.SpriteMaterial({
                map: texture,
                transparent: true,
                depthTest: true,
                depthWrite: false
              });

              const sprite = new THREE.Sprite(material);
              sprite.position.copy(position);
              sprite.scale.set(0.32, 0.32, 0.32);
              sprite.renderOrder = 10;

              parentGroup.add(sprite);
            }

            function latLonToVector3(lat, lon, radius) {
              const phi = THREE.MathUtils.degToRad(90 - lat);
              const theta = THREE.MathUtils.degToRad(lon + 180);

              const x = -radius * Math.sin(phi) * Math.cos(theta);
              const z =  radius * Math.sin(phi) * Math.sin(theta);
              const y =  radius * Math.cos(phi);

              return new THREE.Vector3(x, y, z);
            }

            function createLatitudeLine(latDeg, color, radiusFactor) {
              const points = [];
              const radius = earthRadius * radiusFactor;

              for (let lon = -180; lon <= 180; lon += 2) {
                points.push(latLonToVector3(latDeg, lon, radius));
              }

              const geometry = new THREE.BufferGeometry().setFromPoints(points);

              const material = new THREE.LineBasicMaterial({
                color: color,
                transparent: true,
                opacity: 1.0,
                depthTest: true,
                depthWrite: false
              });

              return new THREE.Line(geometry, material);
            }

            function createMeridianLine(lonDeg, color, radiusFactor) {
              const points = [];
              const radius = earthRadius * radiusFactor;

              for (let lat = -90; lat <= 90; lat += 2) {
                points.push(latLonToVector3(lat, lonDeg, radius));
              }

              const geometry = new THREE.BufferGeometry().setFromPoints(points);

              const material = new THREE.LineBasicMaterial({
                color: color,
                transparent: true,
                opacity: 1.0,
                depthTest: true,
                depthWrite: false
              });

              return new THREE.Line(geometry, material);
            }

            function createAnnularPlane(innerRadius, outerRadius, color, opacity) {
              const geometry = new THREE.RingGeometry(innerRadius, outerRadius, 160, 1);

              const material = new THREE.MeshBasicMaterial({
                color: color,
                transparent: true,
                opacity: opacity,
                side: THREE.DoubleSide,
                depthWrite: false,
                depthTest: true
              });

              return new THREE.Mesh(geometry, material);
            }

            function loadTextureIntoMaterial(textureUrl, material, label, onLoaded) {
              if (!textureUrl || !material) {
                return;
              }

              const textureLoader = new THREE.TextureLoader();
              textureLoader.crossOrigin = '';

              textureLoader.load(
                textureUrl,
                function(texture) {
                  texture.anisotropy = renderer.capabilities.getMaxAnisotropy();
                  texture.needsUpdate = true;

                  if (material.map) {
                    material.map.dispose();
                    material.map = null;
                  }

                  material.map = texture;
                  material.needsUpdate = true;

                  if (typeof onLoaded === 'function') {
                    onLoaded(texture);
                  }

                  if (renderer && scene && camera) {
                    renderer.render(scene, camera);
                  }
                },
                undefined,
                function(err) {
                  console.error('No se pudo cargar la textura ' + label + ':', textureUrl, err);
                }
              );
            }

            function loadBaseTexture(textureUrl) {
              loadTextureIntoMaterial(
                textureUrl,
                earthBaseMaterial,
                'base',
                function() {
                  updateBaseVisibility();
                  updateShellAndGridVisibility();
                }
              );
            }

            function loadOverlayTexture(textureUrl) {
              if (!textureUrl) {
                clearOverlayTexture();
                return;
              }

              loadTextureIntoMaterial(
                textureUrl,
                earthOverlayMaterial,
                'overlay',
                function() {
                  updateOverlayVisibility();
                  updateShellAndGridVisibility();
                }
              );
            }

            function clearOverlayTexture() {
              if (!earthOverlayMaterial) {
                return;
              }

              if (earthOverlayMaterial.map) {
                earthOverlayMaterial.map.dispose();
                earthOverlayMaterial.map = null;
              }

              earthOverlayMaterial.needsUpdate = true;

              if (earthOverlayMesh) {
                earthOverlayMesh.visible = false;
              }

              updateShellAndGridVisibility();
            }

            function isBaseVisible() {
              const showBaseInput = byId(config.show_base_bg_id);

              return !!(
                showBaseInput &&
                showBaseInput.checked &&
                earthBaseMaterial &&
                earthBaseMaterial.map
              );
            }

            function isOverlayVisible() {
              const showOverlayInput = byId(config.show_overlay_bg_id);
              const overlaySelect = byId(config.earth_overlay_texture_id);

              return !!(
                showOverlayInput &&
                showOverlayInput.checked &&
                overlaySelect &&
                overlaySelect.value !== '' &&
                earthOverlayMaterial &&
                earthOverlayMaterial.map
              );
            }

            function updateBaseVisibility() {
              if (earthBaseMesh) {
                earthBaseMesh.visible = isBaseVisible();
              }

              updateShellAndGridVisibility();
            }

            function updateOverlayVisibility() {
              if (earthOverlayMesh) {
                earthOverlayMesh.visible = isOverlayVisible();
              }

              updateShellAndGridVisibility();
            }

            function updateShellAndGridVisibility() {
              const shellInput = byId(config.show_surface_shell_id);
              const gridInput = byId(config.show_surface_grid_id);

              const showShell = shellInput && shellInput.checked;
              const showGrid = gridInput && gridInput.checked;

              if (shellMesh) {
                shellMesh.visible = !!showShell;
              }

              if (surfaceGridGroup) {
                surfaceGridGroup.visible = !!showGrid;
              }
            }

            function setupTextureSelectors() {
              document.addEventListener('change', function(event) {
                if (!event.target) {
                  return;
                }

                if (event.target.id === config.earth_base_texture_id) {
                  loadBaseTexture(event.target.value);
                }

                if (event.target.id === config.earth_overlay_texture_id) {
                  const newOverlayUrl = event.target.value;

                  if (newOverlayUrl === '') {
                    clearOverlayTexture();
                  } else {
                    loadOverlayTexture(newOverlayUrl);
                  }
                }

                if (event.target.id === config.show_base_bg_id) {
                  updateBaseVisibility();
                }

                if (event.target.id === config.show_overlay_bg_id) {
                  updateOverlayVisibility();
                }

                if (
                  event.target.id === config.show_surface_shell_id ||
                  event.target.id === config.show_surface_grid_id
                ) {
                  updateShellAndGridVisibility();
                }
              });
            }

            function setupVerticalAxisReferenceControl() {
              const referenceSelect = byId(config.vertical_axis_reference_id);

              if (!referenceSelect) {
                return;
              }

              updateVerticalAxisReferenceTarget();

              referenceSelect.addEventListener('change', function() {
                updateVerticalAxisReferenceTarget();
              });
            }

            function updateVerticalAxisReferenceTarget() {
              const referenceSelect = byId(config.vertical_axis_reference_id);

              if (!referenceSelect) {
                targetWorldRotationX = 0;
                return;
              }

              if (referenceSelect.value === 'solar') {
                targetWorldRotationX = -axialTiltRad;
              } else {
                targetWorldRotationX = 0;
              }
            }

            function updateVerticalAxisReferenceTransition() {
              if (!worldGroup) {
                return;
              }

              worldGroup.rotation.x +=
                (targetWorldRotationX - worldGroup.rotation.x) * referenceTransitionSpeed;
            }

            function setupCheckboxes() {
              const centerPointInput = byId(config.show_center_point_id);
              const axisInput = byId(config.show_axis_id);
              const solarAxisInput = byId(config.show_solar_axis_id);
              const equatorInput = byId(config.show_equator_id);
              const greenwichInput = byId(config.show_greenwich_id);

              const equatorialPlaneInput = byId(config.show_equatorial_plane_id);
              const greenwichPlaneInput = byId(config.show_greenwich_plane_id);

              const solarPlaneInput = byId(config.show_solar_plane_id);

              if (centerPointInput) {
                centerPoint.visible = centerPointInput.checked;
                centerPointInput.addEventListener('change', function() {
                  centerPoint.visible = centerPointInput.checked;
                });
              }

              if (axisInput) {
                axisGroup.visible = axisInput.checked;
                axisInput.addEventListener('change', function() {
                  axisGroup.visible = axisInput.checked;
                });
              }

              if (solarAxisInput) {
                solarAxisGroup.visible = solarAxisInput.checked;
                solarAxisInput.addEventListener('change', function() {
                  solarAxisGroup.visible = solarAxisInput.checked;
                });
              }

              if (equatorInput) {
                equatorLine.visible = equatorInput.checked;
                equatorInput.addEventListener('change', function() {
                  equatorLine.visible = equatorInput.checked;
                });
              }

              if (greenwichInput) {
                greenwichLine.visible = greenwichInput.checked;
                greenwichInput.addEventListener('change', function() {
                  greenwichLine.visible = greenwichInput.checked;
                });
              }

              if (equatorialPlaneInput) {
                equatorialPlaneGroup.visible = equatorialPlaneInput.checked;
                equatorialPlaneInput.addEventListener('change', function() {
                  equatorialPlaneGroup.visible = equatorialPlaneInput.checked;
                });
              }

              if (greenwichPlaneInput) {
                greenwichPlaneGroup.visible = greenwichPlaneInput.checked;
                greenwichPlaneInput.addEventListener('change', function() {
                  greenwichPlaneGroup.visible = greenwichPlaneInput.checked;
                });
              }

              if (solarPlaneInput) {
                solarPlaneGroup.visible = solarPlaneInput.checked;
                solarPlaneInput.addEventListener('change', function() {
                  solarPlaneGroup.visible = solarPlaneInput.checked;
                });
              }

              updateBaseVisibility();
              updateOverlayVisibility();
              updateShellAndGridVisibility();
            }

            function setupResetButton() {
              const resetButton = byId(config.reset_view_id);

              if (!resetButton) {
                return;
              }

              resetButton.addEventListener('click', function() {
                if (earthGroup) {
                  earthGroup.rotation.set(0, 0, 0);
                }

                updateVerticalAxisReferenceTarget();

                resetAnimation = {
                  startTime: performance.now(),
                  duration: 1000,
                  startCameraPosition: camera.position.clone(),
                  startControlsTarget: controls.target.clone(),
                  endCameraPosition: initialCameraPosition.clone(),
                  endControlsTarget: initialControlsTarget.clone()
                };
              });
            }

            function easeInOutCubic(t) {
              return t < 0.5
                ? 4 * t * t * t
                : 1 - Math.pow(-2 * t + 2, 3) / 2;
            }

            function updateResetAnimation() {
              if (!resetAnimation) {
                return;
              }

              const elapsed = performance.now() - resetAnimation.startTime;
              const rawT = Math.min(elapsed / resetAnimation.duration, 1);
              const t = easeInOutCubic(rawT);

              camera.position.lerpVectors(
                resetAnimation.startCameraPosition,
                resetAnimation.endCameraPosition,
                t
              );

              controls.target.lerpVectors(
                resetAnimation.startControlsTarget,
                resetAnimation.endControlsTarget,
                t
              );

              camera.up.set(0, 1, 0);
              camera.lookAt(controls.target);

              if (rawT >= 1) {
                camera.position.copy(resetAnimation.endCameraPosition);
                controls.target.copy(resetAnimation.endControlsTarget);
                camera.up.set(0, 1, 0);
                camera.lookAt(controls.target);
                controls.update();
                resetAnimation = null;
              }
            }

            function onWindowResize() {
              const container = byId(config.container_id);
              const w = container.clientWidth || window.innerWidth;
              const h = container.clientHeight || window.innerHeight;

              camera.aspect = w / h;
              camera.updateProjectionMatrix();
              renderer.setSize(w, h);
            }

            function animate() {
              requestAnimationFrame(animate);

              updateResetAnimation();
              updateVerticalAxisReferenceTransition();

              if (controls) {
                controls.update();
              }

              renderer.render(scene, camera);
            }

            return {
              scene: scene,
              camera: camera,
              renderer: renderer
            };
          }
        })();
      "))
    ),

    div(
      class = "sat-globe-root",

      div(
        id = ns("globe_container"),
        class = "sat-globe-container"
      ),

      div(
        class = "sat-globe-control-panel",

        h3("Planeta Tierra 3D"),

        div(
          class = "sat-globe-utc-box",
          "UTC: ",
          textOutput(ns("utc_time"), inline = TRUE)
        ),

        h4("01 Esfera"),

        tags$label(
          tags$input(id = ns("show_surface_shell"), type = "checkbox", checked = "checked"),
          " Superficie sólida de la esfera"
        ),

        tags$label(
          tags$input(id = ns("show_surface_grid"), type = "checkbox", checked = "checked"),
          " Mallado de la esfera"
        ),

        tags$label(
          tags$input(id = ns("show_center_point"), type = "checkbox", checked = "checked"),
          " Punto central de la esfera"
        ),

        tags$label(
          tags$input(id = ns("show_equatorial_plane"), type = "checkbox", checked = "checked"),
          " Plano ecuatorial interno"
        ),

        tags$label(
          tags$input(id = ns("show_greenwich_plane"), type = "checkbox"),
          " Plano de Greenwich interno"
        ),

        h4("02 Referencias"),

        selectInput(
          inputId = ns("vertical_axis_reference"),
          label = "Eje vertical de visualización",
          choices = c(
            "Eje Norte-Sur terrestre" = "earth",
            "Eje solar" = "solar"
          ),
          selected = "earth",
          selectize = FALSE
        ),

        tags$label(
          tags$input(id = ns("show_axis"), type = "checkbox", checked = "checked"),
          " Eje Norte-Sur"
        ),

        tags$label(
          tags$input(id = ns("show_solar_axis"), type = "checkbox"),
          " Eje solar"
        ),

        tags$label(
          tags$input(id = ns("show_equator"), type = "checkbox", checked = "checked"),
          " Ecuador"
        ),

        tags$label(
          tags$input(id = ns("show_greenwich"), type = "checkbox", checked = "checked"),
          " Meridiano de Greenwich"
        ),

        h4("03 Background"),

        tags$label(
          tags$input(id = ns("show_base_bg"), type = "checkbox", checked = "checked"),
          " Mostrar background base"
        ),

        uiOutput(ns("base_texture_ui")),

        tags$label(
          tags$input(id = ns("show_overlay_bg"), type = "checkbox", checked = "checked"),
          " Mostrar segundo background"
        ),

        uiOutput(ns("overlay_texture_ui")),

        h4("Extra"),

        tags$label(
          tags$input(id = ns("show_solar_plane"), type = "checkbox"),
          " Plano solar"
        ),

        tags$button(
          id = ns("reset_view"),
          type = "button",
          class = "sat-globe-reset-button",
          "Volver a posición inicial"
        ),

        div(
          class = "sat-globe-small-note",
          "La hora UTC se obtiene desde R usando Sys.time().",
          tags$br(),
          "El eje solar ahora está en Referencias, debajo del eje Norte-Sur."
        )
      )
    )
  )
}

# ------------------------------------------------------------
# Módulo Server
# ------------------------------------------------------------

mod_satelliteGlobe_02_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    www_path <- get_satellite_globe_www_path()

    addResourcePath("assets", www_path)

    solid_texture_dir <- file.path(
      www_path,
      "bg_layers",
      "bg_01_wgs84_01_solid"
    )

    transparent_texture_dir <- file.path(
      www_path,
      "bg_layers",
      "bg_01_wgs84_02_transparent"
    )

    if (!dir.exists(solid_texture_dir)) {
      stop("No encuentro la carpeta de texturas sólidas: ", solid_texture_dir)
    }

    if (!dir.exists(transparent_texture_dir)) {
      stop("No encuentro la carpeta de capas transparentes: ", transparent_texture_dir)
    }

    solid_texture_files <- list.files(
      solid_texture_dir,
      pattern = "\\.png$",
      full.names = FALSE
    )

    transparent_texture_files <- list.files(
      transparent_texture_dir,
      pattern = "\\.png$",
      full.names = FALSE
    )

    if (length(solid_texture_files) == 0) {
      stop("No encontré archivos PNG en: ", solid_texture_dir)
    }

    solid_texture_urls <- setNames(
      paste0("assets/bg_layers/bg_01_wgs84_01_solid/", solid_texture_files),
      solid_texture_files
    )

    transparent_texture_urls <- setNames(
      paste0("assets/bg_layers/bg_01_wgs84_02_transparent/", transparent_texture_files),
      transparent_texture_files
    )

    transparent_choices <- c(
      "Sin segundo background" = "",
      transparent_texture_urls
    )

    output$base_texture_ui <- renderUI({
      selectInput(
        inputId = ns("earth_base_texture"),
        label = "Background base",
        choices = solid_texture_urls,
        selected = solid_texture_urls[[1]],
        selectize = FALSE
      )
    })

    output$overlay_texture_ui <- renderUI({
      selectInput(
        inputId = ns("earth_overlay_texture"),
        label = "Segundo background / capa",
        choices = transparent_choices,
        selected = "",
        selectize = FALSE
      )
    })

    output$utc_time <- renderText({
      invalidateLater(1000, session)

      format(
        as.POSIXct(Sys.time(), tz = "UTC"),
        "%Y-%m-%d %H:%M:%S UTC",
        tz = "UTC",
        usetz = FALSE
      )
    })

    initialized <- reactiveVal(FALSE)

    observe({
      req(input$earth_base_texture)

      if (initialized()) {
        return()
      }

      config <- list(
        container_id = ns("globe_container"),

        reset_view_id = ns("reset_view"),

        vertical_axis_reference_id = ns("vertical_axis_reference"),

        show_surface_shell_id = ns("show_surface_shell"),
        show_surface_grid_id = ns("show_surface_grid"),
        show_center_point_id = ns("show_center_point"),
        show_equatorial_plane_id = ns("show_equatorial_plane"),
        show_greenwich_plane_id = ns("show_greenwich_plane"),

        show_axis_id = ns("show_axis"),
        show_solar_axis_id = ns("show_solar_axis"),
        show_equator_id = ns("show_equator"),
        show_greenwich_id = ns("show_greenwich"),

        show_base_bg_id = ns("show_base_bg"),
        earth_base_texture_id = ns("earth_base_texture"),

        show_overlay_bg_id = ns("show_overlay_bg"),
        earth_overlay_texture_id = ns("earth_overlay_texture"),

        show_solar_plane_id = ns("show_solar_plane"),

        base_texture_url = input$earth_base_texture,
        overlay_texture_url = input$earth_overlay_texture %||% ""
      )

      session$sendCustomMessage(
        type = "satelliteGlobe02-init",
        message = config
      )

      initialized(TRUE)
    })
  })
}

# ------------------------------------------------------------
# App de prueba
# ------------------------------------------------------------

ui <- fluidPage(
  mod_satelliteGlobe_02_ui("globe")
)

server <- function(input, output, session) {
  mod_satelliteGlobe_02_server("globe")
}

shinyApp(ui, server)

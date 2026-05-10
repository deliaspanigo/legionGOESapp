# 1. Preparar y Guardar los cambios (incluyendo el DESCRIPTION limpio)
git add .
git commit -m "Release: Version 0.0.2"

# 2. Etiquetar la versión
# Borramos el tag local por si acaso ya se creó con error antes
#git tag -d v0.9.12 2>/dev/null
git tag -d v0.0.2 | Out-Null
git tag -a v0.0.2 -m "Version  0.0.2"

# 3. Subir cambios y etiquetas a GitHub
git push origin main --follow-tags

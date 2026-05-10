# ==============================================================================
# RScience / RMedic - Environment Verification
# ==============================================================================

local({
  # 1. Obtener rutas principales
  r_home <- chartr("\\", "/", R.home())
  working_dir <- getwd()

  # 2. Formatear mensaje de bienvenida
  cat("\n========================================================\n")
  cat("  🚀 LegionGOESapp Environment Initialized\n")
  cat("========================================================\n")

  # Verificar si es Portable (buscando la palabra 'App' o 'Portable' en la ruta)
  is_portable <- grepl("Portable|App", r_home, ignore.case = TRUE)
  status_icon <- if(is_portable) "✅ PORTABLE MODE" else "⚠️ SYSTEM MODE"

  cat(sprintf("  Status:    %s\n", status_icon))
  cat(sprintf("  R Version: %s\n", R.version$version.string))
  cat(sprintf("  R Home:    %s\n", r_home))
  cat(sprintf("  Project:   %s\n", basename(working_dir)))
  cat("--------------------------------------------------------\n")

  # 3. Verificación de librerías (opcional pero muy útil)
  # Muestra cuántas librerías hay disponibles en este R
  #lib_count <- length(installed.packages()[,1])
  #cat(sprintf("  Packages:  %d libraries detected\n", lib_count))
  cat("========================================================\n\n")
})

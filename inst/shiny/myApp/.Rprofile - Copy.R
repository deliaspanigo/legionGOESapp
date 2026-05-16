# ==============================================================================
# RScience / RMedic - Environment Verification
# ==============================================================================

get_os_info <- function() {
  sys <- Sys.info()

  os_name <- unname(sys[["sysname"]])
  os_release <- unname(sys[["release"]])
  os_version <- unname(sys[["version"]])
  machine <- unname(sys[["machine"]])

  os_family <- switch(
    os_name,
    "Windows" = "windows",
    "Linux"   = "linux",
    "Darwin"  = "macos",
    "unknown"
  )

  list(
    family = os_family,
    name = os_name,
    release = os_release,
    version = os_version,
    machine = machine,
    is_windows = os_family == "windows",
    is_linux = os_family == "linux",
    is_macos = os_family == "macos",
    is_64bit = grepl("64", machine, ignore.case = TRUE)
  )
}

local({
  # 1. Important folders
  r_home <- chartr("\\", "/", R.home())
  r_libraries  <- file.path(dirname(R.home()), "R-Libraries")
  working_dir <- getwd()

  # 2. Setting special library folder source
  portable_lib   <- file.path(app_root, "App", "R-Library")
  portable_r_lib <- file.path(app_root, "App", "R-Portable", "library")

  dir.create(portable_lib, recursive = TRUE, showWarnings = FALSE)

  .libPaths(c(
    portable_lib,
    portable_r_lib
  ))

  # 3. Setting specific repo and date
  #  | Version     |       Avairiable from      |          Name            |
  #  | ----------- | ------------------------:  | ------------------------ |
  #  | **R 4.5.2** | **31/10/2025**             | `[Not] Part in a Rumble` |
  #  | **R 4.5.3** | **11/03/2026**             | `Reassured Reassurer`    |

  options(
    repos = c(
      CRAN = "https://packagemanager.posit.co/cran/2026-03-01"  # We choose a date close but not super close to 11/03/2026
    )
  )


  # 3. Welcome message
  cat("\n========================================================\n")
  cat("  🚀 LegionGOESapp Environment Initialized\n")
  cat("========================================================\n")

  # Verificar si es Portable (buscando la palabra 'App' o 'Portable' en la ruta)
  is_portable <- grepl("Portable|App", r_home, ignore.case = TRUE)
  status_icon <- if(is_portable) "✅ PORTABLE MODE" else "⚠️ SYSTEM MODE"

  cat(sprintf("  System Time:    %s\n", Sys.time()))
  cat(sprintf("  System Time UTC:    %s\n", format(Sys.time(), tz = "UTC", usetz = TRUE)))
  cat(sprintf("  System Time UTC:    %s\n", format(Sys.time(), tz = "UTC", usetz = TRUE)))



  cat(sprintf("  Status:    %s\n", status_icon))
  cat(sprintf("  R Version: %s\n", R.version$version.string))
  cat(sprintf("  R Home Folder:    %s\n", r_home))
  cat(sprintf("  R Libraries Folder:    %s\n", r_home))
  cat(sprintf("  Working Directory:   %s\n", basename(working_dir)))
  cat("--------------------------------------------------------\n")

  # 3. Verificación de librerías (opcional pero muy útil)
  # Muestra cuántas librerías hay disponibles en este R
  #lib_count <- length(installed.packages()[,1])
  #cat(sprintf("  Packages:  %d libraries detected\n", lib_count))
  cat("========================================================\n\n")
})

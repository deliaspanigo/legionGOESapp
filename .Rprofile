# ==============================================================================
# .Rprofile - LegionGOESapp portable environment
# ==============================================================================

local({

  # ---------------------------------------------------------------------------
  # OS helpers
  # ---------------------------------------------------------------------------

  get_os_info <- function() {

    sys <- Sys.info()

    os_name <- unname(sys[["sysname"]])
    os_release <- unname(sys[["release"]])
    os_version <- unname(sys[["version"]])
    machine <- unname(sys[["machine"]])
    nodename <- unname(sys[["nodename"]])
    user <- unname(sys[["user"]])

    os_family <- switch(
      os_name,
      "Windows" = "windows",
      "Linux"   = "linux",
      "Darwin"  = "macos",
      "unknown"
    )

    architecture <- R.version$arch

    is_64bit <- grepl("64", machine, ignore.case = TRUE) ||
      grepl("64", architecture, ignore.case = TRUE)

    list(
      family = os_family,
      name = os_name,
      release = os_release,
      version = os_version,
      machine = machine,
      architecture = architecture,
      nodename = nodename,
      user = user,
      is_windows = os_family == "windows",
      is_linux = os_family == "linux",
      is_macos = os_family == "macos",
      is_64bit = is_64bit,
      platform_os_type = .Platform$OS.type,
      platform_file_sep = .Platform$file.sep,
      platform_path_sep = .Platform$path.sep,
      r_platform = R.version$platform,
      r_arch = R.version$arch
    )
  }

  print_os_info <- function() {

    os <- get_os_info()

    cat("--------------------------------------------------------\n")
    cat("🖥️  Operating System\n")
    cat("--------------------------------------------------------\n")
    cat("OS Family:     ", os$family, "\n", sep = "")
    cat("OS Name:       ", os$name, "\n", sep = "")
    cat("OS Release:    ", os$release, "\n", sep = "")
    cat("OS Version:    ", os$version, "\n", sep = "")
    cat("Machine:       ", os$machine, "\n", sep = "")
    cat("Architecture:  ", os$architecture, "\n", sep = "")
    cat("64-bit:        ", os$is_64bit, "\n", sep = "")
    cat("Node Name:     ", os$nodename, "\n", sep = "")
    cat("User:          ", os$user, "\n", sep = "")
    cat("Platform Type: ", os$platform_os_type, "\n", sep = "")
    cat("File Sep:      ", os$platform_file_sep, "\n", sep = "")
    cat("Path Sep:      ", os$platform_path_sep, "\n", sep = "")
    cat("R Platform:    ", os$r_platform, "\n", sep = "")
    cat("R Arch:        ", os$r_arch, "\n", sep = "")
    cat("--------------------------------------------------------\n")
  }

  # ---------------------------------------------------------------------------
  # App paths: R portable + R-Library
  # ---------------------------------------------------------------------------

  r_home <- normalizePath(R.home(), winslash = "/", mustWork = FALSE)

  # Si R.home() es:
  # .../f01_installer_legion/App/R-Portable
  #
  # app_folder será:
  # .../f01_installer_legion/App

  app_folder <- dirname(r_home)

  portable_lib <- file.path(app_folder, "R-Library")
  portable_r_lib <- file.path(app_folder, "R-Portable", "library")

  dir.create(portable_lib, recursive = TRUE, showWarnings = FALSE)

  .libPaths(c(
    portable_lib,
    portable_r_lib
  ))

  my_wd <- getwd()

  Sys.setenv(
    R_LIBS_USER = portable_lib
  )

  # ---------------------------------------------------------------------------
  # User data folders: downloads, processed, cache, logs, config
  # ---------------------------------------------------------------------------

  APP_DATA_NAME <- "LegionGOES_lab_0.0.2"

  get_standard_user_data_dir <- function(app_name = APP_DATA_NAME) {

    os <- Sys.info()[["sysname"]]

    if (os == "Windows") {

      base_dir <- Sys.getenv("LOCALAPPDATA")

      if (!nzchar(base_dir)) {
        userprofile <- Sys.getenv("USERPROFILE")

        if (nzchar(userprofile)) {
          base_dir <- file.path(userprofile, "AppData", "Local")
        } else {
          base_dir <- tempdir()
        }
      }

    } else if (os == "Linux") {

      base_dir <- Sys.getenv("XDG_DATA_HOME")

      if (!nzchar(base_dir)) {
        home <- Sys.getenv("HOME")

        if (nzchar(home)) {
          base_dir <- file.path(home, ".local", "share")
        } else {
          base_dir <- tempdir()
        }
      }

    } else if (os == "Darwin") {

      home <- Sys.getenv("HOME")

      if (nzchar(home)) {
        base_dir <- file.path(
          home,
          "Library",
          "Application Support"
        )
      } else {
        base_dir <- tempdir()
      }

    } else {

      base_dir <- tempdir()
    }

    file.path(base_dir, app_name)
  }

  check_write_permission <- function(path) {

    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    if (!dir.exists(path)) {
      return(FALSE)
    }

    test_file <- file.path(path, ".write_test")

    ok <- tryCatch({
      writeLines("test", test_file)
      file.remove(test_file)
      TRUE
    }, error = function(e) {
      FALSE
    })

    ok
  }

  setup_legion_data_dirs <- function(app_name = APP_DATA_NAME) {

    root <- get_standard_user_data_dir(app_name)

    dir.create(root, recursive = TRUE, showWarnings = FALSE)

    if (!dir.exists(root)) {
      stop("No se pudo crear la carpeta principal de datos: ", root)
    }

    if (!check_write_permission(root)) {
      stop("No hay permiso de escritura en la carpeta de datos: ", root)
    }

    dirs <- list(
      root = root,
      downloads = file.path(root, "downloads"),
      processed = file.path(root, "processed"),
      cache = file.path(root, "cache"),
      logs = file.path(root, "logs"),
      config = file.path(root, "config")
    )

    for (d in dirs) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)

      if (!dir.exists(d)) {
        stop("No se pudo crear la carpeta: ", d)
      }
    }

    lapply(dirs, normalizePath, winslash = "/", mustWork = TRUE)
  }

  LEGION_DIRS <- setup_legion_data_dirs(APP_DATA_NAME)

  Sys.setenv(
    LEGION_DATA_NAME = APP_DATA_NAME,
    LEGION_DATA_DIR = LEGION_DIRS$root,
    LEGION_DOWNLOADS_DIR = LEGION_DIRS$downloads,
    LEGION_PROCESSED_DIR = LEGION_DIRS$processed,
    LEGION_CACHE_DIR = LEGION_DIRS$cache,
    LEGION_LOGS_DIR = LEGION_DIRS$logs,
    LEGION_CONFIG_DIR = LEGION_DIRS$config
  )

  # ---------------------------------------------------------------------------
  # Fixed repository
  # ---------------------------------------------------------------------------

  options(
    repos = c(
      CRAN = "https://packagemanager.posit.co/cran/2026-03-01"
    )
  )

  # ---------------------------------------------------------------------------
  # Startup log
  # ---------------------------------------------------------------------------

  cat("\n========================================================\n")
  cat("🚀 LegionGOESapp Environment Initialized\n")
  cat("========================================================\n")
  cat("Status:     ✅ PORTABLE MODE\n")
  cat("R Version:  ", R.version.string, "\n", sep = "")
  cat("R Home:     ", r_home, "\n", sep = "")
  cat("App Folder: ", app_folder, "\n", sep = "")
  cat("Work Dir:   ", my_wd, "\n", sep = "")
  cat("UTC Time:   ", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n", sep = "")
  cat("Local Time: ", format(Sys.time(), usetz = TRUE), "\n", sep = "")
  cat("Repo:       ", getOption("repos")[["CRAN"]], "\n", sep = "")

  cat("--------------------------------------------------------\n")
  cat("📦 R Libraries\n")
  cat("--------------------------------------------------------\n")
  cat("Portable Lib:   ", portable_lib, "\n", sep = "")
  cat("R Base Lib:     ", portable_r_lib, "\n", sep = "")
  cat("LibPaths:\n")
  print(.libPaths())

  cat("--------------------------------------------------------\n")
  cat("🗂️  LegionGOES User Data\n")
  cat("--------------------------------------------------------\n")
  cat("Data Name:      ", Sys.getenv("LEGION_DATA_NAME"), "\n", sep = "")
  cat("Data Dir:       ", Sys.getenv("LEGION_DATA_DIR"), "\n", sep = "")
  cat("Downloads Dir:  ", Sys.getenv("LEGION_DOWNLOADS_DIR"), "\n", sep = "")
  cat("Processed Dir:  ", Sys.getenv("LEGION_PROCESSED_DIR"), "\n", sep = "")
  cat("Cache Dir:      ", Sys.getenv("LEGION_CACHE_DIR"), "\n", sep = "")
  cat("Logs Dir:       ", Sys.getenv("LEGION_LOGS_DIR"), "\n", sep = "")
  cat("Config Dir:     ", Sys.getenv("LEGION_CONFIG_DIR"), "\n", sep = "")
  cat("========================================================\n")

  print_os_info()

})

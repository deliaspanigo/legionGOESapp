

r_home <- normalizePath(R.home(), winslash = "/", mustWork = FALSE)
app_folder <- dirname(dirname(r_home))
app_folder

devtools::load_all()

folder_run_app <- fn_my_folder_package()
app_shiny_file_path <- file.path(folder_run_app)
app_shiny_file_path

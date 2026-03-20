# ==============================================================================
# System Paths
# ==============================================================================
# Sourced by: a00_data_loader.R, a01_network.R, a02_components.R, etc.
# Sets: output_folder_path  (root directory for all project data on cloud storage)
#
# Paths are resolved per OS. If a path does not exist, execution stops early
# with a clear message so the user can fix the path before proceeding.
# ==============================================================================

os_name <- Sys.info()[["sysname"]]

if (os_name == "Windows") {
  message("OS detected: Windows")
  output_folder_path <- file.path("C:", "Users", "crist", "OneDrive", "Documentos", "output")

} else if (os_name == "Darwin") {
  message("OS detected: macOS")
  output_folder_path <- file.path(
    Sys.getenv("HOME"),
    "Library", "CloudStorage",
    "GoogleDrive-cristianmejia00@gmail.com", "My Drive", "Bibliometrics_Drive"
  )

} else if (os_name == "Linux") {
  message("OS detected: Linux")
  # Typical location when Google Drive is mounted via rclone, google-drive-ocamlfuse,
  # or similar. Adjust if your mount point differs.
  output_folder_path <- file.path(Sys.getenv("HOME"), "google-drive", "Bibliometrics_Drive")

} else {
  stop(glue("Unsupported operating system: '{os_name}'. ",
            "Add a path mapping in zz_utils/00_system_paths.R for this OS."))
}

# Normalise path separators for the current OS
output_folder_path <- normalizePath(output_folder_path, mustWork = FALSE)

# Validate that the directory exists
if (!dir.exists(output_folder_path)) {
  stop(glue("output_folder_path does not exist:\n  {output_folder_path}\n",
            "Check your cloud-storage mount or update zz_utils/00_system_paths.R."))
}

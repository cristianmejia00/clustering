# ==============================================================================
# System Paths
# ==============================================================================
# Sourced by: a00_data_loader.R, a01_network.R, a02_components.R, etc.
# Sets: output_folder_path  (root directory for all project data on cloud storage)
#
# Paths are resolved per OS. If a path does not exist, execution stops early
# with a clear message so the user can fix the path before proceeding.
# ==============================================================================

# Priority:
# 1) BIBLIOMETRICS_DRIVE env var
# 2) .env file at repo root (key: BIBLIOMETRICS_DRIVE)
# 3) OS defaults

get_dotenv_value <- function(key, dotenv_path = ".env") {
  if (!file.exists(dotenv_path)) return("")
  lines <- readLines(dotenv_path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != "" & !startsWith(lines, "#")]
  if (length(lines) == 0) return("")

  pattern <- paste0("^", key, "\\s*=")
  match_idx <- grep(pattern, lines)
  if (length(match_idx) == 0) return("")

  value <- sub(pattern, "", lines[match_idx[1]])
  value <- gsub('^"|"$', "", value)
  value <- gsub("^'|'$", "", value)
  value
}

env_output_path <- Sys.getenv("BIBLIOMETRICS_DRIVE", unset = "")
if (env_output_path == "") {
  env_output_path <- get_dotenv_value("BIBLIOMETRICS_DRIVE", ".env")
}

os_name <- Sys.info()[["sysname"]]

if (env_output_path != "") {
  message("Using BIBLIOMETRICS_DRIVE from environment/.env")
  output_folder_path <- env_output_path

} else if (os_name == "Windows") {
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
  stop(glue::glue("Unsupported operating system: '{os_name}'. ",
                  "Add a path mapping in zz_utils/00_system_paths.R for this OS."))
}

# Normalise path separators for the current OS
output_folder_path <- normalizePath(output_folder_path, mustWork = FALSE)

# Validate that the directory exists
if (!dir.exists(output_folder_path)) {
  stop(glue::glue("output_folder_path does not exist:\n  {output_folder_path}\n",
                  "Set BIBLIOMETRICS_DRIVE in .env or update zz_utils/00_system_paths.R."))
}

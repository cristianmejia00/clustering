#!/usr/bin/env Rscript

# ==============================================================================
# Project setup (native, cross-platform)
#
# Usage:
#   Rscript --vanilla scripts/setup.R
#   Rscript --vanilla scripts/setup.R --validate-only
#   Rscript --vanilla scripts/setup.R --force
# ==============================================================================

find_repo_root <- function() {
  has_configs <- function(path) {
    file.exists(file.path(path, "config_dataset.yml")) &&
      file.exists(file.path(path, "config_analysis.yml"))
  }

  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (has_configs(wd)) {
    return(wd)
  }

  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args, value = TRUE)
  if (length(script_arg) > 0) {
    script_path <- sub("^--file=", "", script_arg[[1]])
    script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
    repo_root <- dirname(script_dir)
    if (has_configs(repo_root)) {
      return(repo_root)
    }
  }

  stop("Run setup from repository root or invoke with: Rscript --vanilla scripts/setup.R")
}

parse_setup_args <- function(args) {
  out <- list(mode = "full", force = FALSE, help = FALSE)

  if ("--help" %in% args || "-h" %in% args) {
    out$help <- TRUE
    return(out)
  }
  if ("--validate-only" %in% args) {
    out$mode <- "validate"
  }
  if ("--force" %in% args) {
    out$force <- TRUE
  }

  out
}

print_help <- function() {
  cat(
    "Usage:\n",
    "  Rscript --vanilla scripts/setup.R\n",
    "  Rscript --vanilla scripts/setup.R --validate-only\n",
    "  Rscript --vanilla scripts/setup.R --force\n\n",
    "Flags:\n",
    "  --validate-only   Run checks only (no installs).\n",
    "  --force           Recreate Python venv and reinstall dependencies.\n",
    "  --help, -h        Show this help.\n",
    sep = ""
  )
}

fail <- function(msg) {
  stop(msg, call. = FALSE)
}

# system2() does not quote arguments; any argument containing spaces or shell
# metacharacters (e.g. inline "python -c" code) must be quoted per platform.
quote_arg <- function(x) {
  if (.Platform$OS.type == "windows") shQuote(x, type = "cmd") else shQuote(x)
}

run_cmd <- function(command, args, label) {
  cat("[setup] ", label, "\n", sep = "")
  status <- system2(command, args = args)
  if (!identical(status, 0L)) {
    fail(paste0(label, " failed with exit code ", status, "."))
  }
}

check_r_version <- function(min_version = "4.4.0") {
  if (getRversion() < min_version) {
    fail(paste0(
      "R ", min_version, "+ required. Current version: ",
      as.character(getRversion()),
      "."
    ))
  }
}

find_system_python <- function() {
  env_python <- Sys.getenv("PYTHON_EXECUTABLE", unset = "")
  if (nzchar(env_python) && file.exists(env_python)) {
    return(env_python)
  }

  resolve_python_from_py_launcher <- function(version_tag) {
    py_launcher <- Sys.which("py")
    if (!nzchar(py_launcher)) {
      return("")
    }

    out <- suppressWarnings(system2(
      py_launcher,
      c(paste0("-", version_tag), "-c", quote_arg("import sys; print(sys.executable)")),
      stdout = TRUE,
      stderr = TRUE
    ))

    if (length(out) == 0) {
      return("")
    }

    candidate <- trimws(out[[1]])
    if (!nzchar(candidate)) {
      return("")
    }

    candidate
  }

  if (.Platform$OS.type == "windows") {
    # Prefer supported versions for scientific wheels in this project.
    for (version_tag in c("3.11", "3.10", "3.12")) {
      candidate <- resolve_python_from_py_launcher(version_tag)
      if (nzchar(candidate)) {
        return(candidate)
      }
    }
  }

  candidates <- if (.Platform$OS.type == "windows") {
    c("python", "python3", "py")
  } else {
    c("python3", "python")
  }

  for (candidate in candidates) {
    path <- Sys.which(candidate)
    if (nzchar(path)) {
      return(path)
    }
  }

  ""
}

check_python_version <- function(py_exec,
                                 min_major = 3L,
                                 min_minor = 10L,
                                 max_major = 3L,
                                 max_minor = 12L) {
  if (!nzchar(py_exec)) {
    fail("Python 3.10-3.12 not found in PATH.")
  }

  version <- suppressWarnings(system2(
    py_exec,
    c("-c", quote_arg("import sys; print(f'{sys.version_info[0]}.{sys.version_info[1]}.{sys.version_info[2]}')")),
    stdout = TRUE,
    stderr = TRUE
  ))

  if (length(version) == 0) {
    fail("Could not determine Python version.")
  }

  parts <- strsplit(version[[1]], "\\.")[[1]]
  if (length(parts) < 2) {
    fail(paste0("Unexpected Python version output: ", version[[1]]))
  }

  major <- suppressWarnings(as.integer(parts[[1]]))
  minor <- suppressWarnings(as.integer(parts[[2]]))

  if (is.na(major) || is.na(minor) ||
      major < min_major || (major == min_major && minor < min_minor)) {
    fail(paste0("Python ", min_major, ".", min_minor, "+ required. Current: ", version[[1]], "."))
  }

  if (major > max_major || (major == max_major && minor > max_minor)) {
    fail(paste0(
      "Python ", major, ".", minor,
      " is currently unsupported for this setup. Use Python 3.10-3.12 (recommended: 3.11)."
    ))
  }

  version[[1]]
}

configure_windows_binary_restore <- function() {
  if (.Platform$OS.type != "windows") {
    return(invisible(FALSE))
  }

  options(repos = c(CRAN = "https://cloud.r-project.org"))
  options(pkgType = "binary")
  Sys.setenv(RENV_CONFIG_PAK_ENABLED = "FALSE")

  cat("[setup] Windows detected: using CRAN binary packages for renv restore\n")
  invisible(TRUE)
}

ensure_renv_restore <- function(force = FALSE) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
  }

  renv::consent(provided = TRUE)
  configure_windows_binary_restore()

  restore_args <- list(
    lockfile = "renv.lock",
    prompt = FALSE,
    repos = getOption("repos")
  )
  if (force) {
    restore_args$rebuild <- TRUE
  }

  tryCatch(
    do.call(renv::restore, restore_args),
    error = function(e) {
      if (.Platform$OS.type == "windows") {
        fail(paste0(
          "renv restore failed on Windows. ",
          "Try installing Rtools 4.5, then rerun setup. ",
          "Original error: ", conditionMessage(e)
        ))
      }
      stop(e)
    }
  )
}

venv_python_path <- function(venv_dir = ".venv") {
  if (.Platform$OS.type == "windows") {
    return(file.path(venv_dir, "Scripts", "python.exe"))
  }

  file.path(venv_dir, "bin", "python")
}

ensure_python_venv <- function(system_python, force = FALSE, venv_dir = ".venv") {
  if (force && dir.exists(venv_dir)) {
    unlink(venv_dir, recursive = TRUE, force = TRUE)
  }

  if (!dir.exists(venv_dir)) {
    run_cmd(system_python, c("-m", "venv", venv_dir), "Creating Python virtual environment")
  }

  py_venv <- venv_python_path(venv_dir)
  if (!file.exists(py_venv)) {
    fail(paste0("Virtual environment Python not found: ", py_venv))
  }

  run_cmd(py_venv, c("-m", "pip", "install", "--upgrade", "pip", "setuptools", "wheel"),
          "Upgrading pip tooling")

  req_files <- c(
    "pipelines/analysis/topic_model/requirements_embeds.txt",
    "pipelines/analysis/topic_model/requirements_new_tm.txt",
    "pipelines/ai/requirements.txt"
  )

  for (req in req_files) {
    if (!file.exists(req)) {
      fail(paste0("Missing requirements file: ", req))
    }
    run_cmd(py_venv, c("-m", "pip", "install", "-r", req), paste0("Installing ", req))
  }

  py_venv
}

validate_python_imports <- function(py_exec) {
  code <- paste(
    "import importlib.util, sys",
    "mods=['yaml','pandas','numpy','sentence_transformers','bertopic','litellm','umap','matplotlib','sklearn','hdbscan']",
    "missing=[m for m in mods if importlib.util.find_spec(m) is None]",
    "print('MISSING:' + ','.join(missing) if missing else 'OK')",
    sep = ";"
  )

  result <- suppressWarnings(system2(py_exec, c("-c", quote_arg(code)), stdout = TRUE, stderr = TRUE))
  if (length(result) == 0) {
    fail("Python import validation did not produce output.")
  }

  line <- tail(result, 1)
  if (!identical(line, "OK")) {
    fail(paste0("Python import validation failed: ", line))
  }
}

validate_config_paths <- function(strict = FALSE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    install.packages("yaml", repos = "https://cloud.r-project.org")
  }

  cfg_dataset <- yaml::read_yaml("config_dataset.yml")
  cfg_analysis <- yaml::read_yaml("config_analysis.yml")

  path_specs <- list(
    list(name = "config_dataset.yml -> metadata.raw_input_directory", value = cfg_dataset$metadata$raw_input_directory),
    list(name = "config_dataset.yml -> metadata.bibliometrics_directory", value = cfg_dataset$metadata$bibliometrics_directory),
    list(name = "config_analysis.yml -> metadata.bibliometrics_directory", value = cfg_analysis$metadata$bibliometrics_directory)
  )

  issues <- character(0)

  for (item in path_specs) {
    value <- item$value
    text <- if (is.null(value) || !is.character(value) || length(value) == 0) "" else trimws(value[[1]])

    if (!nzchar(text)) {
      issues <- c(issues, paste0(item$name, " is empty."))
      next
    }

    if (!dir.exists(text)) {
      issues <- c(issues, paste0(item$name, " does not exist: ", text))
    }
  }

  if (length(issues) > 0) {
    msg <- paste(issues, collapse = "\n")
    if (strict) {
      fail(paste0("Configuration path validation failed:\n", msg))
    }
    warning(paste0("Configuration path warnings:\n", msg), call. = FALSE)
  }

  invisible(length(issues) == 0)
}

print_next_steps <- function() {
  cat(
    "\nNext steps:\n",
    "1) Edit config_dataset.yml and config_analysis.yml paths for your machine.\n",
    "2) Run pipeline stages from repo root, for example:\n",
    "   Rscript --vanilla -e \"source('scripts/run_pipeline.R'); run_pipeline(c('dataset','analysis','reports'))\"\n",
    "3) Add AI/charts when ready:\n",
    "   Rscript --vanilla -e \"source('scripts/run_pipeline.R'); run_pipeline(c('ai','charts'))\"\n",
    sep = ""
  )
}

setup_pipeline <- function(mode = c("full", "validate"), force = FALSE) {
  mode <- match.arg(mode)

  root <- find_repo_root()
  setwd(root)

  cat("[setup] Repository root: ", root, "\n", sep = "")
  cat("[setup] Mode: ", mode, if (force) " (force)" else "", "\n", sep = "")

  check_r_version()

  system_python <- find_system_python()
  py_version <- check_python_version(system_python)
  cat("[setup] Python detected: ", system_python, " (", py_version, ")\n", sep = "")

  venv_python <- venv_python_path()

  if (mode == "full") {
    cat("[setup] Restoring R packages from renv.lock\n")
    ensure_renv_restore(force = force)

    venv_python <- ensure_python_venv(system_python = system_python, force = force)
  } else {
    if (!file.exists(venv_python)) {
      fail("Validate-only mode requires an existing .venv. Run full setup first.")
    }
  }

  validate_python_imports(venv_python)
  validate_config_paths(strict = (mode == "validate"))

  cat("\n[setup] Setup checks completed successfully.\n")
  print_next_steps()

  invisible(TRUE)
}

args <- parse_setup_args(commandArgs(trailingOnly = TRUE))

if (isTRUE(args$help)) {
  print_help()
  quit(status = 0)
}

status <- tryCatch({
  setup_pipeline(mode = args$mode, force = args$force)
  0L
}, error = function(e) {
  message("\n[setup] ERROR: ", conditionMessage(e))
  1L
})

quit(status = status)

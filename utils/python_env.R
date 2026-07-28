# ==============================================================================
# Python environment helpers (cross-platform)
# ==============================================================================

python_candidates_from_venv <- function(venv_dir) {
    if (!nzchar(venv_dir)) {
        return(character(0))
    }

    if (.Platform$OS.type == "windows") {
        return(c(
            file.path(venv_dir, "Scripts", "python.exe"),
            file.path(venv_dir, "python.exe"),
            file.path(venv_dir, "bin", "python")
        ))
    }

    c(
        file.path(venv_dir, "bin", "python3"),
        file.path(venv_dir, "bin", "python")
    )
}

find_python_executable <- function(preferred_venvs = c(".venv", "pipelines/ai/.venv"),
                                   allow_system = TRUE) {
    for (venv_dir in preferred_venvs) {
        candidates <- python_candidates_from_venv(venv_dir)
        for (candidate in candidates) {
            if (file.exists(candidate)) {
                # Keep the venv-relative interpreter path. Resolving symlinks here can
                # escape the virtualenv and lose access to venv-installed packages.
                return(candidate)
            }
        }
    }

    if (allow_system) {
        system_candidates <- c("python3", "python")
        if (.Platform$OS.type == "windows") {
            system_candidates <- c("python", "python3", "py")
        }

        for (cmd in system_candidates) {
            path <- Sys.which(cmd)
            if (nzchar(path)) {
                return(path)
            }
        }
    }

    ""
}

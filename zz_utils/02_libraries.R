# ==============================================================================
# Package Management
# ==============================================================================
# Core packages used by the main pipeline (a00 -> a05) and shared utilities.
# Optional packages (05_llm, 06_quarto, yy_heatmap_sankey, network plots)
# load their own dependencies locally.
# ==============================================================================

# Helper: install a package only if not already installed
ensure_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# --- Core pipeline packages ---
core_packages <- c(
  "data.table",   # Fast file I/O (fread) and data manipulation
  "plyr",         # rbind.fill (load BEFORE dplyr)
  "dplyr",        # Data wrangling (mutate, filter, select, etc.)
  "readr",        # CSV reading/writing (read_csv, read_file)
  "igraph",       # Network analysis (graphs, clustering, components)
  "stringr",      # String operations (str_split, str_replace, etc.)
  "glue",         # String interpolation
  "ggplot2",      # Visualization (used in charts, reports, overlays)
  "ggrepel",      # Non-overlapping text labels on plots
  "plotly",       # Interactive plots (ggplotly, plot_ly)
  "reshape2",     # Data reshaping (melt, dcast)
  "gridExtra",    # Arrange multiple ggplot panels
  "tools",        # File utilities (file_ext, md5sum)
  "DT",           # Interactive data tables (datatable)
  "RJSONIO",      # JSON serialization for config snapshots
  "uuid",         # Unique identifiers (UUIDgenerate)
  "yaml"          # YAML config loading (read_yaml)
)

invisible(lapply(core_packages, ensure_installed))

# --- Load core libraries (order matters: plyr before dplyr) ---
library(data.table)
library(plyr)
library(dplyr)
library(readr)
library(igraph)
library(stringr)
library(glue)
library(ggplot2)
library(ggrepel)
library(plotly)
library(reshape2)
library(gridExtra)
library(tools)
library(DT)
library(RJSONIO)
library(uuid)
library(yaml)


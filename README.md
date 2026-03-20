# Bibliometric Clustering Pipeline

Analysis pipeline for clustering academic articles, news, and patents using citation networks and topic models.

## Quick Start

### 1. Configure

Edit the two YAML config files in the project root:

- **`config_dataset.yml`** — Data source paths, file filtering, column selection, embedding parameters, and network type.
- **`config_analysis.yml`** — Analysis parameters: clustering algorithm, thresholding, topic model options, LLM prompts, and report settings.

### 2. Run the Pipeline

Execute the scripts **in order** from an R session with the working directory set to this repo:

| Step | Script | Purpose |
|------|--------|---------|
| 1 | `a00_data_loader.R` | Load raw data files, clean, filter, and save dataset |
| 2 | `a01_network.R` | Build the citation network |
| 3 | `a02_components.R` | Extract connected components |
| 4 | `a03_clustering.R` | Apply clustering algorithm (Louvain/Infomap/Newman) |
| 5 | `a04_thresholding.R` | Recursive sub-clustering and size thresholds |
| 6 | `a05_reports.R` | Generate all reports, charts, and archives |

Each script sources its dependencies automatically. A JSON snapshot of the active config is archived with each run for reproducibility.

### 3. System Paths

`zz_utils/00_system_paths.R` resolves the cloud-storage root (`output_folder_path`) per OS (macOS, Windows, Linux). Update the paths there if your mount point differs.

## Project Structure

```
config_dataset.yml          # Dataset configuration (YAML)
config_analysis.yml         # Analysis configuration (YAML)
a00–a05_*.R                 # Main pipeline scripts (run in order)
01_data_loading/            # Data loading helpers
02_citation_network/        # Network clustering subroutines
03_reports/                 # Report generators
04_charts/                  # Visualization scripts
05_llm/                     # LLM integration (Claude API)
06_quarto/                  # Quarto document generation
yy_heatmap_sankey/          # Heatmap and Sankey diagrams
zz_utils/                   # Shared utilities and config loader
zz_assets/                  # Static assets (colors, overlays, credentials)
```

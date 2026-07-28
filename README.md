# Bibliometric Clustering Pipeline

Analysis pipeline for clustering academic articles, news, and patents using citation networks and topic models.

There are two ways to use it:

- **[Docker (recommended for most users)](#docker-recommended)** — no R, no Python, no compilers. Install Docker Desktop, download this repository, double-click two files.
- **[Native setup (maintainers / developers)](#native-setup-maintainers)** — installs R and Python environments directly on your machine.

---

## Docker (recommended)

### One-time setup

1. **Install Docker Desktop** from <https://www.docker.com/products/docker-desktop/>. Accept the defaults (on Windows it will enable WSL 2 — say yes, and reboot if asked). Start Docker Desktop and wait until it says it is running.
2. **Download this repository**: on the GitHub page click the green **Code** button → **Download ZIP**, and unzip it somewhere simple, e.g. `C:\clustering` (Windows) or `~/clustering` (Mac).
3. **Windows:** double-click **`1 - Setup.bat`**.
   **Mac/Linux:** in a terminal at the repo folder run `docker compose pull && docker compose run --rm pipeline init`.

   This downloads the ready-made pipeline image (2–3 GB, one time only) and creates a `docker/` folder with everything you will touch.

### Prepare your data

Everything you provide or receive lives in the `docker/` folder next to the launchers:

```text
docker/
  raw_input/<your_folder>/   <- put your Web of Science .txt exports here
  bibliometrics/             <- results appear here
  credentials/               <- API key file for the AI stage (e.g. openai.key)
  config_dataset.yml         <- edit the 4 lines marked at the top
  config_analysis.yml        <- edit the 4 lines marked at the top
```

1. Create a folder inside `docker/raw_input/` (e.g. `my_project`) and copy your Web of Science tab-delimited `.txt` export files into it.
2. Put your API key file into `docker/credentials/` — a plain text file named `openai.key` (or `claude.key` / `google.key`, matching the `llm.provider` you set in `config_analysis.yml`) containing only the key.
3. Open `docker/config_dataset.yml` and `docker/config_analysis.yml` in Notepad (or any text editor) and edit the lines marked **EDIT THESE 4 LINES** at the top of each file. Leave the `/workspace/...` paths untouched — they are container paths that map to your `docker/` folder automatically.

### Run

**Windows:** double-click **`2 - Run Pipeline.bat`** and press Enter to run the full pipeline (or type a subset like `dataset,analysis,reports`).

**Mac/Linux:** `docker compose run --rm pipeline run` (full pipeline) or `docker compose run --rm pipeline run dataset,analysis,reports`.

Results appear in `docker/bibliometrics/<project_folder>/`. The stages run in this order: `dataset` → `analysis` → `reports` → `ai` → `enriched_embeds` → `charts`.

Other helpers:

- **`Validate.bat`** — checks your config files and folders without running anything.
- **`3 - Open Shell.bat`** — opens a terminal inside the container (useful when screen-sharing with the maintainer).

### Docker troubleshooting

- **"Docker is not running"** — start Docker Desktop (whale icon) and wait until it says "running", then retry.
- **Pipeline killed / exit code 137** — the container ran out of memory. In Docker Desktop: Settings → Resources → raise Memory to 8 GB or more, then rerun.
- **AI stage fails with a key/authentication error** — check the key file in `docker/credentials/` is named after your provider (`openai.key`, `claude.key`, or `google.key`) and contains only the key on one line.
- **"No .txt files found"** — confirm your export files are inside `docker/raw_input/<folder>` and that `raw_input_folder_name` in `docker/config_dataset.yml` matches that folder name exactly.
- **Editing configs has no effect** — edit the copies in `docker/`, not `docker_templates/`. The `docker/` copies are the ones the pipeline reads.

### Maintainer: building and publishing the image

The image is built and published automatically to `ghcr.io/cristianmejia00/clustering:latest` by GitHub Actions (`.github/workflows/docker-image.yml`) on every push to `main` (multi-arch: amd64 + arm64). After the first publish, set the GHCR package visibility to **public** once in GitHub → Packages, or colleagues' pulls will fail with 401.

To build locally instead of pulling:

```bash
docker compose -f docker-compose.yml -f docker-compose.build.yml build
```

---

## Native setup (maintainers)

Prerequisites:

- R 4.4+
- Python 3.10–3.12

From repository root, run one setup command:

- **macOS / Linux**: `bash scripts/setup_mac_linux.sh`
- **Windows (PowerShell, recommended)**: `powershell -ExecutionPolicy Bypass -File scripts/setup_windows_safe.ps1`
- **Windows (PowerShell, basic wrapper)**: `powershell -ExecutionPolicy Bypass -File scripts/setup_windows.ps1`

Optional setup modes:

- Validate only (no installs): `Rscript --vanilla scripts/setup.R --validate-only`
- Force reinstall environments: `Rscript --vanilla scripts/setup.R --force`

After setup:

1. Edit `config_dataset.yml` and `config_analysis.yml` with paths for your own machine.
2. Run pipeline stages with `scripts/run_pipeline.R`.

Example:

```bash
Rscript --vanilla -e "source('scripts/run_pipeline.R'); run_pipeline(c('dataset','analysis','reports'))"
Rscript --vanilla -e "source('scripts/run_pipeline.R'); run_pipeline(c('ai','charts'))"
```

### Run by task

Run from an R session with working directory at repo root:

```r
source("scripts/run_pipeline.R")
run_pipeline(c("dataset", "analysis", "reports", "ai", "enriched_embeds", "charts"))  # full pipeline
run_pipeline(c("reports", "ai", "charts"))                                            # rerun from reports
run_pipeline(c("charts"))                                                             # charts only
```

Valid stages: `dataset`, `analysis`, `reports`, `ai`, `enriched_embeds`, `charts`. Supply any subset in any order; prerequisites are enforced (`dataset` → `analysis` → `reports` → `ai` → `charts`).

Individual stage scripts: `scripts/dataset_only.R`, `scripts/analysis_only.R`, `scripts/reports_only.R`, `scripts/ai_only.R`, `scripts/charts_only.R`, `scripts/enriched_embeds_only.R` (re-encodes documents with cluster names prepended to TI+AB for improved UMAP separation; requires AI naming first).

### Dataset artifacts

For each embed profile in `config_dataset.yml` (e.g., `e01`), the dataset routine writes to `<bibliometrics_directory>/<project_folder>/<from_filtered_dataset>/<embed_profile>/`:

- `corpus.csv`
- `embeddings.npy`
- `embeddings_ids.json`
- `embeds_settings.json`

Embeddings are part of the dataset pipeline for all datasets because they are also used for optional cosine-similarity analyses, not only topic modeling.

### Native troubleshooting

- **Windows picks unsupported Python (e.g., 3.13/3.14)** — run the safe wrapper: `powershell -ExecutionPolicy Bypass -File scripts/setup_windows_safe.ps1 --force`
- **`Python not found` or missing Python modules** — `Rscript --vanilla scripts/setup.R --force`
- **`renv/activate.R` missing** — `Rscript --vanilla scripts/setup.R`
- **Configuration path does not exist** — update local paths in the two YAML configs, then `Rscript --vanilla scripts/setup.R --validate-only`
- **`Required topic-model embeddings artifact not found`** — run the `dataset` stage first.

## Project Structure

```text
config_dataset.yml           # Dataset configuration (native runs)
config_analysis.yml          # Analysis configuration (native runs)
1 - Setup.bat                # Windows: one-time Docker setup
2 - Run Pipeline.bat         # Windows: run the pipeline in Docker
3 - Open Shell.bat           # Windows: shell inside the container
Validate.bat                 # Windows: validate configs in Docker
Dockerfile                   # Pipeline image (R + Python + baked model)
docker-compose.yml           # Colleague-facing compose (pulls prebuilt image)
docker-compose.build.yml     # Maintainer override to build locally
docker_templates/            # Config templates copied into docker/ on init
docker/                      # (created on init) your data, configs, results
scripts/                     # Entry points: setup, run_pipeline, per-stage
pipelines/dataset/           # Loader, network build, embeddings
pipelines/analysis/          # citation_network/ and topic_model/
pipelines/reports/           # Report generators
pipelines/ai/                # LLM enrichment (providers, naming)
pipelines/charts/            # Chart generation (R + Python UMAP scatter)
utils/                       # Shared utilities and config loader
assets/                      # Static assets (colors, overlays, credentials)
```

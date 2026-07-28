#!/usr/bin/env bash
set -euo pipefail

WORKDIR="/workspace"
DOCKER_HOME="${WORKDIR}/docker"
TEMPLATE_DIR="${WORKDIR}/docker_templates"

ensure_docker_workspace() {
  mkdir -p "${DOCKER_HOME}/raw_input" "${DOCKER_HOME}/bibliometrics" "${DOCKER_HOME}/credentials"
}

ensure_docker_configs() {
  ensure_docker_workspace

  if [[ ! -f "${DOCKER_HOME}/config_dataset.yml" ]]; then
    cp "${TEMPLATE_DIR}/config_dataset.yml" "${DOCKER_HOME}/config_dataset.yml"
    echo "[docker] Created ${DOCKER_HOME}/config_dataset.yml"
  fi

  if [[ ! -f "${DOCKER_HOME}/config_analysis.yml" ]]; then
    cp "${TEMPLATE_DIR}/config_analysis.yml" "${DOCKER_HOME}/config_analysis.yml"
    echo "[docker] Created ${DOCKER_HOME}/config_analysis.yml"
  fi
}

activate_runtime_configs() {
  ensure_docker_configs
  cp "${DOCKER_HOME}/config_dataset.yml" "${WORKDIR}/config_dataset.yml"
  cp "${DOCKER_HOME}/config_analysis.yml" "${WORKDIR}/config_analysis.yml"
}

print_help() {
  cat <<'EOF'
Docker pipeline commands:
  init                        Create docker configs and data folders.
  setup                       Run full setup (R + Python) inside container.
  validate                    Validate setup and config paths.
  run <comma_stages>          Run stages, e.g. run dataset,analysis,reports.
                              Default: dataset,analysis,reports,ai,enriched_embeds,charts
  shell                       Open an interactive shell.

Examples:
  docker compose run --rm pipeline init
  docker compose run --rm pipeline run
  docker compose run --rm pipeline run dataset,analysis,reports
  docker compose run --rm pipeline run ai,enriched_embeds,charts
EOF
}

cmd="${1:-help}"
shift || true

case "${cmd}" in
  init)
    ensure_docker_configs
    echo "[docker] Ready. Put your raw files under ${DOCKER_HOME}/raw_input"
    echo "[docker] Outputs will be written under ${DOCKER_HOME}/bibliometrics"
    echo "[docker] For the 'ai' stage, put your API key file (e.g. openai.key) in ${DOCKER_HOME}/credentials"
    ;;

  setup)
    activate_runtime_configs
    Rscript --vanilla scripts/setup.R --force
    ;;

  validate)
    activate_runtime_configs
    Rscript --vanilla scripts/setup.R --validate-only
    ;;

  run)
    stages="${1:-dataset,analysis,reports,ai,enriched_embeds,charts}"
    stages="${stages// /}"
    activate_runtime_configs
    Rscript --vanilla -e "source('scripts/run_pipeline.R'); run_pipeline(strsplit('${stages}', ',')[[1]])"
    ;;

  shell)
    activate_runtime_configs
    exec bash
    ;;

  help|--help|-h)
    print_help
    ;;

  *)
    echo "Unknown command: ${cmd}" >&2
    print_help
    exit 1
    ;;
esac

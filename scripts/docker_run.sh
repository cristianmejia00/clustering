#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  cat <<'EOF'
Usage:
  scripts/docker_run.sh build
  scripts/docker_run.sh init
  scripts/docker_run.sh validate
  scripts/docker_run.sh run dataset,analysis,reports
  scripts/docker_run.sh run ai,charts
  scripts/docker_run.sh shell
EOF
  exit 1
fi

cmd="$1"
shift || true

case "$cmd" in
  build)
    docker compose build
    ;;
  init|setup|validate|shell)
    docker compose run --rm pipeline "$cmd" "$@"
    ;;
  run)
    stages="${1:-dataset,analysis,reports,ai,charts}"
    docker compose run --rm pipeline run "$stages"
    ;;
  *)
    echo "Unknown command: $cmd" >&2
    exit 1
    ;;
esac

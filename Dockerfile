FROM rocker/tidyverse:4.5.0

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 \
    python3-venv \
    python3-pip \
    python3-dev \
    build-essential \
    libglpk-dev \
    libgmp-dev \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace
COPY . /workspace

RUN chmod +x scripts/setup_mac_linux.sh scripts/docker_entrypoint.sh

# Install R + Python dependencies in-image so users only need Docker.
RUN Rscript --vanilla scripts/setup.R --force

ENTRYPOINT ["/workspace/scripts/docker_entrypoint.sh"]
CMD ["help"]

FROM rocker/tidyverse:4.5.0

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
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

# --- R dependencies (layer cached until renv.lock changes) ---
COPY renv.lock /workspace/renv.lock
RUN Rscript -e 'install.packages("renv", repos = "https://cloud.r-project.org")' \
    && Rscript -e 'renv::consent(provided = TRUE); renv::restore(lockfile = "/workspace/renv.lock", library = "/usr/local/lib/R/site-library", prompt = FALSE)'

# --- Python dependencies (layer cached until requirements change) ---
# CPU-only torch is installed first so the requirements below do not pull the
# multi-GB CUDA wheels; nothing in this pipeline uses a GPU inside Docker.
COPY pipelines/analysis/topic_model/requirements_embeds.txt \
     pipelines/analysis/topic_model/requirements_new_tm.txt \
     /tmp/reqs/topic_model/
COPY pipelines/ai/requirements.txt /tmp/reqs/ai/
RUN python3 -m venv /workspace/.venv \
    && /workspace/.venv/bin/pip install --no-cache-dir --upgrade pip setuptools wheel \
    && /workspace/.venv/bin/pip install --no-cache-dir torch --index-url https://download.pytorch.org/whl/cpu \
    && /workspace/.venv/bin/pip install --no-cache-dir \
        -r /tmp/reqs/topic_model/requirements_embeds.txt \
        -r /tmp/reqs/topic_model/requirements_new_tm.txt \
        -r /tmp/reqs/ai/requirements.txt \
    && rm -rf /tmp/reqs

# --- Bake the sentence-transformers model so first run needs no download ---
ENV HF_HOME=/opt/hf-cache
RUN /workspace/.venv/bin/python -c "from sentence_transformers import SentenceTransformer; SentenceTransformer('all-MiniLM-L6-v2')"

# --- Code last: cheap layer, rebuilds fast on source changes ---
COPY . /workspace

RUN chmod +x scripts/docker_entrypoint.sh

ENTRYPOINT ["/workspace/scripts/docker_entrypoint.sh"]
CMD ["help"]

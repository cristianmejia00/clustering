#!/usr/bin/env python3
"""Enrich cluster data with LLM-generated names and descriptions.

Called from R via ``system2(python, c("pipelines/ai/enrich_clusters.py", ...))``
or directly from the command line.

Usage
-----
    python pipelines/ai/enrich_clusters.py \
        --rcs       path/to/rcs_merged.csv \
        --dataset   path/to/dataset_for_ai.csv \
        --output-dir path/to/output/ \
        [--top-papers 5]

The script reads ``config_analysis.yml`` from the repository root for:
  - ``llm.provider``  — which provider YAML to load (openai | anthropic | google)
  - ``llm.theme``     — research topic used in prompts
  - ``llm.description`` — topic description used in prompts
  - ``llm.compute``   — list of tasks to execute
  - ``llm.rate_limit_delay`` — seconds between API calls (default: 2)

Resumption
----------
The script writes ``rcs_merged.csv`` after **every cluster**, so a
crash or interruption loses at most one cluster's work.  On re-run it
skips clusters that already have a non-empty ``cluster_name``.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd
import yaml

# Ensure the package directory is importable when called as a script
sys.path.insert(0, str(Path(__file__).parent))

from papers import build_bulk_text, get_cluster_papers
from prompts import load_prompt
from providers import call_llm, init_provider

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_ERROR_STRINGS = {"", "Error: 529", "Error 529"}


def _is_complete(value: object) -> bool:
    """Return True if a cluster_name value represents completed work."""
    if pd.isna(value):
        return False
    return str(value).strip() not in _ERROR_STRINGS


def _clean_name(raw: str) -> str:
    """Strip surrounding quotes and whitespace from an LLM-generated name."""
    name = re.sub(r'^.*?"', "", raw)
    name = re.sub(r'".$', "", name)
    name = name.replace('"', "").strip()
    return name


# ---------------------------------------------------------------------------
# Core pipeline
# ---------------------------------------------------------------------------

def _summarize_papers(
    papers: pd.DataFrame,
    dataset: pd.DataFrame,
    topic: str,
    topic_description: str,
    model: str,
    defaults: dict,
    delay: float,
) -> pd.DataFrame:
    """Summarize each paper that doesn't already have a summary."""
    for idx in papers.index:
        existing = str(dataset.loc[dataset["UT"] == papers.at[idx, "UT"], "summary"].values[0])
        if existing and existing != "nan" and len(existing) > 0:
            papers.at[idx, "text"] = existing
            continue

        system, user, settings = load_prompt(
            "summarize_paper",
            topic=topic,
            topic_description=topic_description,
            article_text=papers.at[idx, "text"],
        )
        summary = call_llm(
            system_prompt=system,
            user_prompt=user,
            model=model,
            temperature=settings.get("temperature", defaults.get("temperature", 0.7)),
            max_tokens=settings.get("max_tokens", defaults.get("max_tokens", 200)),
        )
        papers.at[idx, "text"] = summary

        # Persist summary back to dataset so it survives across clusters
        mask = dataset["UT"] == papers.at[idx, "UT"]
        dataset.loc[mask, "summary"] = summary

        time.sleep(delay)

    return papers


def enrich_cluster(
    cluster_code: str,
    rcs: pd.DataFrame,
    dataset: pd.DataFrame,
    topic: str,
    topic_description: str,
    model: str,
    defaults: dict,
    compute_tasks: list[str],
    top_papers: int,
    delay: float,
) -> None:
    """Run all LLM tasks for a single cluster, updating *rcs* in place."""
    mask = rcs["cluster_code"].astype(str) == str(cluster_code)

    # ── Select representative papers ──────────────────────────────────────
    papers = get_cluster_papers(dataset, cluster_code, top=top_papers)
    if papers.empty:
        print(f"  [skip] No papers found for cluster {cluster_code}")
        return

    print(f"  Selected {len(papers)} representative papers")

    # ── Summarize papers (optional) ───────────────────────────────────────
    if "representative_docs_summaries" in compute_tasks:
        print("  Summarizing papers...")
        papers = _summarize_papers(
            papers, dataset, topic, topic_description, model, defaults, delay
        )

    # ── Build bulk text ───────────────────────────────────────────────────
    bulk_text = build_bulk_text(papers)

    # ── Cluster description ───────────────────────────────────────────────
    description = ""
    if "cluster_description" in compute_tasks:
        print("  Generating description...")
        system, user, settings = load_prompt(
            "cluster_description",
            topic=topic,
            topic_description=topic_description,
            cluster_text=bulk_text,
        )
        description = call_llm(
            system_prompt=system,
            user_prompt=user,
            model=model,
            temperature=settings.get("temperature", defaults.get("temperature", 0.2)),
            max_tokens=settings.get("max_tokens", defaults.get("max_tokens", 500)),
        )
        rcs.loc[mask, "description"] = description
        time.sleep(delay)

    # ── Cluster name ──────────────────────────────────────────────────────
    if "cluster_title" in compute_tasks:
        print("  Generating name...")
        system, user, settings = load_prompt(
            "cluster_name",
            topic=topic,
            topic_description=topic_description,
            cluster_description=description,
        )
        raw_name = call_llm(
            system_prompt=system,
            user_prompt=user,
            model=model,
            temperature=settings.get("temperature", defaults.get("temperature", 0.3)),
            max_tokens=settings.get("max_tokens", defaults.get("max_tokens", 60)),
        )
        name = _clean_name(raw_name)
        rcs.loc[mask, "cluster_name"] = name
        print(f"  Name: {name}")
        time.sleep(delay)

    # ── Enhanced description ──────────────────────────────────────────────
    if "cluster_enhanced_description" in compute_tasks:
        print("  Enhancing description...")
        system, user, settings = load_prompt(
            "cluster_enhanced_description",
            topic=topic,
            cluster_description=description,
        )
        enhanced = call_llm(
            system_prompt=system,
            user_prompt=user,
            model=model,
            temperature=settings.get("temperature", defaults.get("temperature", 0.1)),
            max_tokens=settings.get("max_tokens", defaults.get("max_tokens", 500)),
        )
        rcs.loc[mask, "description"] = enhanced
        time.sleep(delay)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Enrich cluster data with LLM-generated names and descriptions."
    )
    parser.add_argument("--rcs", required=True, help="Path to rcs_merged.csv")
    parser.add_argument("--dataset", required=True, help="Path to dataset_for_ai.csv")
    parser.add_argument("--output-dir", required=True, help="Output directory")
    parser.add_argument("--top-papers", type=int, default=None,
                        help="Papers per criterion per cluster (overrides config)")
    args = parser.parse_args()

    # ── Load config ───────────────────────────────────────────────────────
    config_path = Path("config_analysis.yml")
    if not config_path.exists():
        sys.exit("config_analysis.yml not found at repository root.")

    with config_path.open("r", encoding="utf-8") as f:
        config = yaml.safe_load(f)

    llm_cfg = config.get("llm", {})
    provider_name = llm_cfg.get("provider", "openai")
    topic = llm_cfg.get("theme", "")
    topic_description = llm_cfg.get("description", "")
    compute_tasks = llm_cfg.get("compute", [])
    top_papers = args.top_papers or llm_cfg.get("top_papers", 5)
    delay = llm_cfg.get("rate_limit_delay", 2)

    if not compute_tasks:
        print("No LLM compute tasks configured. Nothing to do.")
        return

    # ── Init provider ─────────────────────────────────────────────────────
    print(f"Provider: {provider_name}")
    settings = init_provider(provider_name, repo_root=Path.cwd())
    model = settings["model"]
    defaults = {
        "temperature": settings.get("temperature", 0.1),
        "max_tokens": settings.get("max_tokens", 500),
    }
    print(f"Model: {model}")
    print(f"Tasks: {compute_tasks}")

    # ── Load data ─────────────────────────────────────────────────────────
    rcs_path = Path(args.rcs)
    dataset_path = Path(args.dataset)
    output_dir = Path(args.output_dir)

    rcs = pd.read_csv(rcs_path, dtype={"cluster_code": str})
    dataset = pd.read_csv(dataset_path, dtype={"X_C": str})

    # Ensure required columns exist
    if "cluster_name" not in rcs.columns:
        rcs["cluster_name"] = ""
    if "description" not in rcs.columns:
        rcs["description"] = ""
    if "summary" not in dataset.columns:
        dataset["summary"] = ""

    # Fill NAs to simplify downstream checks
    rcs["cluster_name"] = rcs["cluster_name"].fillna("")
    rcs["description"] = rcs["description"].fillna("")

    cluster_codes = sorted(rcs["cluster_code"].dropna().unique())
    total = len(cluster_codes)
    print(f"\nClusters to process: {total}")

    # ── Process each cluster ──────────────────────────────────────────────
    processed = 0
    skipped = 0
    for i, cluster_code in enumerate(cluster_codes, 1):
        print(f"\n{'='*60}")
        print(f"[{i}/{total}] Cluster: {cluster_code}")

        current_name = rcs.loc[
            rcs["cluster_code"].astype(str) == str(cluster_code), "cluster_name"
        ]
        if not current_name.empty and _is_complete(current_name.iloc[0]):
            print(f"  [skip] Already named: {current_name.iloc[0]}")
            skipped += 1
            continue

        try:
            enrich_cluster(
                cluster_code=cluster_code,
                rcs=rcs,
                dataset=dataset,
                topic=topic,
                topic_description=topic_description,
                model=model,
                defaults=defaults,
                compute_tasks=compute_tasks,
                top_papers=top_papers,
                delay=delay,
            )
            processed += 1
        except Exception as exc:
            print(f"  [error] {exc!r}")
            # Mark as error so next run retries
            mask = rcs["cluster_code"].astype(str) == str(cluster_code)
            rcs.loc[mask, "cluster_name"] = ""

        # ── Write after every cluster for crash recovery ──────────────
        rcs.to_csv(rcs_path, index=False)

    # ── Final outputs ─────────────────────────────────────────────────────
    rcs.to_csv(rcs_path, index=False)

    # Short summary CSV
    summary_cols = [
        c for c in ["cluster_code", "cluster_name", "documents", "PY_Mean", "Z9_Mean", "description"]
        if c in rcs.columns
    ]
    rcs[summary_cols].to_csv(output_dir / "cluster_summary.csv", index=False)

    # Metadata
    meta = {
        "provider": provider_name,
        "model": model,
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "clusters_processed": processed,
        "clusters_skipped": skipped,
        "compute_tasks": compute_tasks,
    }
    with (output_dir / "ai_metadata.json").open("w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)

    print(f"\nDone. Processed: {processed}, Skipped: {skipped}")
    print(f"Outputs: {rcs_path}, {output_dir / 'cluster_summary.csv'}")


if __name__ == "__main__":
    main()

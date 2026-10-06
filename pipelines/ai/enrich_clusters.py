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
        [--subcluster-summary path/to/level1/cluster_summary.csv] \
        [--top-papers 10]

The script reads ``config_analysis.yml`` from the repository root for:
  - ``llm.provider``  — which provider YAML to load (openai | anthropic | google)
  - ``llm.theme``     — research topic used in prompts
  - ``llm.description`` — topic description used in prompts
  - ``llm.compute``   — list of tasks to execute
  - ``llm.top_papers`` / ``llm.sample_titles`` / ``llm.top_keywords`` — context per cluster
  - ``llm.rate_limit_delay`` — seconds between API calls (default: 1)

Each cluster gets one LLM call that sees its distinctive keywords, its
representative papers with abstracts, a sample of further titles and, for
parent clusters, the names of its subclusters, and returns both the name
and the description.

Resumption
----------
The script writes ``rcs_merged.csv`` after **every cluster**, so a
crash or interruption loses at most one cluster's work.  On re-run it
skips clusters that already have a non-empty ``cluster_name``.
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd
import yaml

# Ensure the package directory is importable when called as a script
sys.path.insert(0, str(Path(__file__).parent))

from papers import (
    build_cluster_context,
    clean_code,
    get_cluster_papers,
    parent_code,
    parse_keywords,
)
from prompts import load_prompt
from providers import call_llm, init_provider

CLUSTER_LABEL_SCHEMA = {
    "name": "cluster_label",
    "schema": {
        "type": "object",
        "properties": {
            "description": {"type": "string"},
            "name": {"type": "string"},
        },
        "required": ["description", "name"],
        "additionalProperties": False,
    },
}

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
    """Strip whitespace, surrounding quotes and a trailing period from a name."""
    return str(raw).strip().strip("\"'“”").rstrip(".").strip()


def _load_subclusters(path: str | None) -> pd.DataFrame | None:
    """Load subcluster names (global_name preferred) keyed by parent code."""
    if not path or not Path(path).exists():
        return None
    sub = pd.read_csv(path, dtype={"cluster_code": str})
    names = sub.get("global_name", pd.Series(index=sub.index, dtype=str))
    sub["name"] = names.where(names.notna() & names.astype(str).str.strip().ne(""),
                              sub.get("cluster_name"))
    sub = sub.loc[sub["name"].notna()].copy()
    sub["parent"] = sub["cluster_code"].map(parent_code)
    return sub


# ---------------------------------------------------------------------------
# Core pipeline
# ---------------------------------------------------------------------------

def _summarize_papers(
    papers: pd.DataFrame,
    dataset: pd.DataFrame,
    topic: str,
    topic_description: str,
    settings: dict,
    delay: float,
) -> pd.DataFrame:
    """Replace each abstract with a one-sentence summary (cached in *dataset*)."""
    for idx in papers.index:
        existing = str(dataset.loc[dataset["UT"] == papers.at[idx, "UT"], "summary"].values[0])
        if existing and existing != "nan" and len(existing) > 0:
            papers.at[idx, "text"] = existing
            continue

        system, user, prompt_settings = load_prompt(
            "summarize_paper",
            topic=topic,
            topic_description=topic_description,
            article_text=f"{papers.at[idx, 'TI']}. {papers.at[idx, 'text']}",
        )
        summary = call_llm(
            system_prompt=system,
            user_prompt=user,
            settings=settings,
            temperature=prompt_settings.get("temperature"),
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
    settings: dict,
    compute_tasks: list[str],
    context_cfg: dict,
    subclusters: pd.DataFrame | None,
    delay: float,
) -> None:
    """Run all LLM tasks for a single cluster, updating *rcs* in place."""
    mask = rcs["cluster_code"].astype(str) == str(cluster_code)

    # ── Select representative papers ──────────────────────────────────────
    papers = get_cluster_papers(dataset, cluster_code, top=context_cfg["top_papers"])
    if papers.empty:
        print(f"  [skip] No papers found for cluster {cluster_code}")
        return

    print(f"  Selected {len(papers)} representative papers")

    # ── Summarize papers (optional) ───────────────────────────────────────
    if "representative_docs_summaries" in compute_tasks:
        print("  Summarizing papers...")
        papers = _summarize_papers(
            papers, dataset, topic, topic_description, settings, delay
        )

    if not {"cluster_title", "cluster_description"} & set(compute_tasks):
        return

    # ── Name + description in one call ────────────────────────────────────
    children = None
    if subclusters is not None:
        children = subclusters.loc[subclusters["parent"] == clean_code(cluster_code)]

    context = build_cluster_context(
        dataset,
        cluster_code,
        papers,
        n_titles=context_cfg["sample_titles"],
        n_keywords=context_cfg["top_keywords"],
        seed=context_cfg["seed"],
        subclusters=children,
    )
    system, user, _ = load_prompt(
        "cluster_label",
        topic=topic,
        topic_description=topic_description,
        cluster_context=context,
    )
    print("  Generating name and description...")
    result = call_llm(
        system_prompt=system,
        user_prompt=user,
        settings=settings,
        json_schema=CLUSTER_LABEL_SCHEMA,
    )

    if "cluster_description" in compute_tasks:
        rcs.loc[mask, "description"] = str(result["description"]).strip()
    if "cluster_title" in compute_tasks:
        name = _clean_name(result["name"])
        rcs.loc[mask, "cluster_name"] = name
        print(f"  Name: {name}")
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
    parser.add_argument("--subcluster-summary", default=None,
                        help="Optional path to subcluster cluster_summary.csv; "
                             "their names are shown when naming parent clusters")
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
    delay = llm_cfg.get("rate_limit_delay", 1)
    context_cfg = {
        "top_papers": args.top_papers or llm_cfg.get("top_papers", 10),
        "sample_titles": llm_cfg.get("sample_titles", 40),
        "top_keywords": llm_cfg.get("top_keywords", 15),
        "seed": config.get("params", {}).get("seed", 100),
    }

    if not compute_tasks:
        print("No LLM compute tasks configured. Nothing to do.")
        return

    # ── Init provider ─────────────────────────────────────────────────────
    print(f"Provider: {provider_name}")
    settings = init_provider(provider_name, repo_root=Path.cwd())
    model = settings["model"]
    print(f"Model: {model}")
    print(f"Tasks: {compute_tasks}")
    print(f"Context per cluster: {context_cfg}")

    # ── Load data ─────────────────────────────────────────────────────────
    rcs_path = Path(args.rcs)
    dataset_path = Path(args.dataset)
    output_dir = Path(args.output_dir)

    rcs = pd.read_csv(rcs_path, dtype={"cluster_code": str})
    dataset = pd.read_csv(dataset_path, dtype={"X_C": str})
    dataset["_code"] = dataset["X_C"].map(clean_code)
    if "DE" in dataset.columns:
        dataset["_keywords"] = parse_keywords(dataset["DE"])

    subclusters = _load_subclusters(args.subcluster_summary)
    if subclusters is not None:
        print(f"Subcluster context: {len(subclusters)} subclusters")

    # Ensure required columns exist
    if "cluster_name" not in rcs.columns:
        rcs["cluster_name"] = ""
    if "description" not in rcs.columns:
        rcs["description"] = ""
    if "summary" not in dataset.columns:
        dataset["summary"] = ""

    # Fill NAs to simplify downstream checks
    rcs["cluster_name"] = rcs["cluster_name"].fillna("").astype(str)
    rcs["description"] = rcs["description"].fillna("").astype(str)

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
                settings=settings,
                compute_tasks=compute_tasks,
                context_cfg=context_cfg,
                subclusters=subclusters,
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
        "context": context_cfg,
    }
    with (output_dir / "ai_metadata.json").open("w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)

    print(f"\nDone. Processed: {processed}, Skipped: {skipped}")
    print(f"Outputs: {rcs_path}, {output_dir / 'cluster_summary.csv'}")

    if processed == 0 and skipped == 0:
        sys.exit(1)


if __name__ == "__main__":
    main()

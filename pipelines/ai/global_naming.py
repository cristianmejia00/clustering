#!/usr/bin/env python3
"""Assign globally distinctive cluster names using LLM function calling.

After ``enrich_clusters.py`` assigns per-cluster names in isolation, this
script sees **all** clusters at once and renames them so that every name
is unique and clearly distinguishable.

Usage
-----
    python pipelines/ai/global_naming.py \
        --rcs       path/to/rcs_merged.csv \
        --output-dir path/to/output/ \
        [--subcluster-summary path/to/level1/cluster_summary.csv]

The optional ``--subcluster-summary`` flag provides subcluster names as
additional context when renaming parent-level clusters, so that the parent
names don't duplicate or overlap with their children.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

import pandas as pd
import yaml

sys.path.insert(0, str(Path(__file__).parent))

from providers import call_llm_with_tools, init_provider

# ---------------------------------------------------------------------------
# Prompt helpers
# ---------------------------------------------------------------------------

_PROMPTS_DIR = Path(__file__).parent / "prompts"


def _load_system_prompt(topic: str, topic_description: str) -> str:
    """Load and fill the global_naming system prompt."""
    path = _PROMPTS_DIR / "global_naming.yml"
    with path.open("r", encoding="utf-8") as f:
        data = yaml.safe_load(f)
    return (
        data["system"]
        .replace("{topic}", topic)
        .replace("{topic_description}", topic_description)
    )


def _build_user_message(
    rcs: pd.DataFrame,
    subcluster_summary: pd.DataFrame | None = None,
) -> str:
    """Format all clusters into a numbered list for the LLM."""
    lines: list[str] = []

    if subcluster_summary is not None and not subcluster_summary.empty:
        lines.append("=== SUBCLUSTER CONTEXT (for reference only — do NOT rename these) ===")
        lines.append("The following are the subclusters that belong to the parent clusters "
                      "you will rename. Use them to understand the landscape and ensure "
                      "parent names are broader and non-overlapping with subclusters.\n")
        for _, row in subcluster_summary.iterrows():
            code = row.get("cluster_code", "")
            name = row.get("global_name") or row.get("cluster_name", "")
            lines.append(f"  Subcluster {code}: {name}")
        lines.append("\n=== CLUSTERS TO RENAME ===\n")

    for _, row in rcs.iterrows():
        code = row["cluster_code"].rstrip("-")
        name = row.get("cluster_name", "")
        desc = row.get("description", "")
        lines.append(f"Topic {code}:")
        lines.append(f"  Current name: {name}")
        lines.append(f"  Description: {desc}")
        lines.append("")

    return "\n".join(lines)


def _build_rename_tool(n_topics: int) -> dict:
    """Build the function-calling tool schema."""
    return {
        "type": "function",
        "function": {
            "name": "rename_topics",
            "description": (
                f"Assign a unique, concise, publication-ready name to each of "
                f"the {n_topics} topic clusters. Every name must be distinct."
            ),
            "parameters": {
                "type": "object",
                "properties": {
                    "topics": {
                        "type": "array",
                        "description": f"Exactly {n_topics} renamed topics.",
                        "items": {
                            "type": "object",
                            "properties": {
                                "topic_id": {
                                    "type": "string",
                                    "description": "The original topic/cluster ID.",
                                },
                                "name": {
                                    "type": "string",
                                    "description": (
                                        "A short, unique name for this topic. "
                                        "Must be distinct from every other topic name."
                                    ),
                                },
                            },
                            "required": ["topic_id", "name"],
                        },
                    },
                },
                "required": ["topics"],
            },
        },
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Assign globally distinctive cluster names via LLM function calling."
    )
    parser.add_argument("--rcs", required=True, help="Path to rcs_merged.csv")
    parser.add_argument("--output-dir", required=True, help="Output directory")
    parser.add_argument("--subcluster-summary", default=None,
                        help="Optional path to subcluster cluster_summary.csv for context")
    args = parser.parse_args()

    # ── Load config ──────────────────────────────────────────────────────
    config_path = Path("config_analysis.yml")
    if not config_path.exists():
        sys.exit("config_analysis.yml not found at repository root.")

    with config_path.open("r", encoding="utf-8") as f:
        config = yaml.safe_load(f)

    llm_cfg = config.get("llm", {})
    provider_name = llm_cfg.get("provider", "openai")
    topic = llm_cfg.get("theme", "")
    topic_description = llm_cfg.get("description", "")

    # ── Init provider ────────────────────────────────────────────────────
    print(f"[global_naming] Provider: {provider_name}")
    settings = init_provider(provider_name, repo_root=Path.cwd())
    model = settings["model"]
    print(f"[global_naming] Model: {model}")

    # ── Load data ────────────────────────────────────────────────────────
    rcs_path = Path(args.rcs)
    output_dir = Path(args.output_dir)
    rcs = pd.read_csv(rcs_path, dtype={"cluster_code": str})

    # Filter to clusters that have a name and description
    has_content = rcs["cluster_name"].fillna("").str.strip().ne("") & \
                  rcs["description"].fillna("").str.strip().ne("")
    clusters_to_rename = rcs.loc[has_content].copy()

    n_topics = len(clusters_to_rename)
    if n_topics == 0:
        print("[global_naming] No clusters with names/descriptions to rename. Skipping.")
        return

    print(f"[global_naming] Clusters to rename: {n_topics}")

    # Load optional subcluster context
    subcluster_summary = None
    if args.subcluster_summary and Path(args.subcluster_summary).exists():
        subcluster_summary = pd.read_csv(
            args.subcluster_summary, dtype={"cluster_code": str}
        )
        print(f"[global_naming] Subcluster context: {len(subcluster_summary)} subclusters")

    # ── Build prompt and tool ────────────────────────────────────────────
    system_prompt = _load_system_prompt(topic, topic_description)
    user_message = _build_user_message(clusters_to_rename, subcluster_summary)
    tool = _build_rename_tool(n_topics)

    # Scale max_tokens for large cluster counts
    prompt_settings_path = _PROMPTS_DIR / "global_naming.yml"
    with prompt_settings_path.open("r", encoding="utf-8") as f:
        prompt_cfg = yaml.safe_load(f)
    base_max_tokens = prompt_cfg.get("settings", {}).get("max_tokens", 4096)
    # ~30 tokens per cluster for the JSON response
    max_tokens = max(base_max_tokens, n_topics * 30 + 200)

    # ── Call LLM with function calling ───────────────────────────────────
    print("[global_naming] Calling LLM for global renaming...")
    result = call_llm_with_tools(
        system_prompt=system_prompt,
        user_prompt=user_message,
        model=model,
        tools=[tool],
        temperature=prompt_cfg.get("settings", {}).get("temperature", 0.2),
        max_tokens=max_tokens,
    )

    # ── Parse and apply ──────────────────────────────────────────────────
    # Normalise topic_ids from LLM (strip trailing dashes if any)
    renamed = {
        str(item["topic_id"]).rstrip("-"): item["name"]
        for item in result.get("topics", [])
    }
    print(f"[global_naming] Received {len(renamed)} renamed topics")

    # Build a lookup from clean code → original code for matching
    clean_to_orig = {
        c.rstrip("-"): c
        for c in clusters_to_rename["cluster_code"].astype(str)
    }

    # Validate
    missing = set(clean_to_orig.keys()) - set(renamed.keys())
    if missing:
        print(f"[global_naming] WARNING: Missing names for clusters: {missing}")

    names_list = list(renamed.values())
    duplicates = [n for n in names_list if names_list.count(n) > 1]
    if duplicates:
        print(f"[global_naming] WARNING: Duplicate names detected: {set(duplicates)}")

    # Apply global_name to rcs (ensure string dtype — column may be float if all NaN)
    if "global_name" not in rcs.columns:
        rcs["global_name"] = ""
    else:
        rcs["global_name"] = rcs["global_name"].fillna("").astype(str)
    for clean_code, name in renamed.items():
        orig_code = clean_to_orig.get(clean_code, clean_code)
        mask = rcs["cluster_code"].astype(str) == orig_code
        rcs.loc[mask, "global_name"] = name

    # Print summary
    for clean_code, name in sorted(renamed.items()):
        orig_code = clean_to_orig.get(clean_code, clean_code)
        old = clusters_to_rename.loc[
            clusters_to_rename["cluster_code"].astype(str) == orig_code, "cluster_name"
        ]
        old_name = old.iloc[0] if not old.empty else "?"
        print(f"  {clean_code}: {old_name} → {name}")

    # ── Save ─────────────────────────────────────────────────────────────
    rcs.to_csv(rcs_path, index=False)
    print(f"[global_naming] Updated: {rcs_path}")

    # Update cluster_summary.csv too
    summary_cols = [
        c for c in ["cluster_code", "cluster_name", "global_name", "documents",
                     "PY_Mean", "Z9_Mean", "description"]
        if c in rcs.columns
    ]
    rcs[summary_cols].to_csv(output_dir / "cluster_summary.csv", index=False)
    print(f"[global_naming] Updated: {output_dir / 'cluster_summary.csv'}")


if __name__ == "__main__":
    main()

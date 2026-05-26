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
import re
import sys
from pathlib import Path

import pandas as pd
import yaml

sys.path.insert(0, str(Path(__file__).parent))

from providers import call_llm_with_tools, init_provider

# ---------------------------------------------------------------------------
# ID normalisation
# ---------------------------------------------------------------------------


def _normalize_id(s: str) -> str:
    """Normalise a topic/cluster ID so LLM-reformatted IDs still match.

    Handles cases such as:
      "Topic 9-1"  -> "9-1"
      "cluster_9-1" -> "9-1"
      "9_1"         -> "9-1"
      "9-1-"        -> "9-1"
    """
    s = str(s).strip()
    # Strip common word prefixes the LLM tends to add
    s = re.sub(r'^(?:topic|cluster|subcluster|group)[\s_\-]*', '', s, flags=re.IGNORECASE)
    s = s.strip()
    # Collapse whitespace/underscores to dash
    s = re.sub(r'[\s_]+', '-', s)
    # Strip leading/trailing dashes
    s = s.strip('-')
    return s.lower()


# ---------------------------------------------------------------------------
# Prompt helpers
# ---------------------------------------------------------------------------

_PROMPTS_DIR = Path(__file__).parent / "prompts"


def _fixed_max_tokens_for_model(model: str, fallback: int = 4096) -> int:
    """Return a fixed max_tokens value per model family (no dynamic scaling)."""
    m = (model or "").lower()

    # OpenAI GPT-4.1 family supports long context and high output limits.
    # Keep a fixed cap for global naming to avoid truncating tool-call JSON.
    if m.startswith("gpt-4.1"):
        return 32768

    # GPT-5 family supports higher output limits.
    if m.startswith("gpt-5"):
        return 128000

    # Anthropic Claude 3/3.5/3.7/4 families commonly support up to 8192 output.
    if "claude" in m:
        return 8192

    # Gemini models typically support at least 8192 output.
    if "gemini" in m:
        return 8192

    return fallback


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


def _fix_duplicates(
    renamed: dict[str, str],
    clusters_to_rename: pd.DataFrame,
    system_prompt: str,
    model: str,
    temperature: float,
    max_tokens: int,
) -> dict[str, str]:
    """Second LLM pass: disambiguate any clusters that still share a name."""
    from collections import Counter

    name_counts = Counter(renamed.values())
    dup_names = {n for n, c in name_counts.items() if c > 1}
    if not dup_names:
        return renamed

    dup_codes = [code for code, name in renamed.items() if name in dup_names]
    print(f"[global_naming] Dedup pass: {len(dup_codes)} clusters share "
          f"{len(dup_names)} duplicate name(s)")

    dup_df = clusters_to_rename[
        clusters_to_rename["cluster_code"].astype(str).isin(dup_codes)
    ].copy()

    already_taken = sorted(set(renamed.values()) - dup_names)
    lines: list[str] = [
        "The following clusters currently share a name with at least one other cluster. "
        "Assign a NEW, unique name to each one.\n",
        "Names already used by OTHER clusters (do NOT reuse any of these):",
    ]
    for nm in already_taken:
        lines.append(f"  - {nm}")
    lines.append("\nClusters that need a unique replacement name:\n")
    for _, row in dup_df.iterrows():
        code = str(row["cluster_code"]).rstrip("-")
        name = row.get("cluster_name", "")
        desc = row.get("description", "")
        current_dup = renamed.get(str(row["cluster_code"]), "")
        lines.append(f"Topic {code}:")
        lines.append(f"  Duplicate name to replace: {current_dup}")
        lines.append(f"  Original cluster name: {name}")
        lines.append(f"  Description: {desc}")
        lines.append("")

    tool = _build_rename_tool(len(dup_df))
    result2 = call_llm_with_tools(
        system_prompt=system_prompt,
        user_prompt="\n".join(lines),
        model=model,
        tools=[tool],
        temperature=temperature,
        max_tokens=max_tokens,
    )

    orig_codes_dup = dup_df["cluster_code"].astype(str).tolist()
    norm_to_orig_dup: dict[str, str] = {_normalize_id(c): c for c in orig_codes_dup}
    matched_dedup = 0
    for item in result2.get("topics", []):
        raw_id = str(item.get("topic_id", ""))
        orig_code = norm_to_orig_dup.get(_normalize_id(raw_id))
        if orig_code:
            renamed[orig_code] = item["name"]
            matched_dedup += 1
    print(f"[global_naming] Dedup pass applied {matched_dedup} / {len(dup_codes)} fixes")
    return renamed


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

    # Include clusters that have at least a current name.
    # (Descriptions can be empty for some subclusters.)
    has_content = rcs["cluster_name"].fillna("").str.strip().ne("")
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

    # Fixed max_tokens (no dynamic scaling by number of clusters)
    prompt_settings_path = _PROMPTS_DIR / "global_naming.yml"
    with prompt_settings_path.open("r", encoding="utf-8") as f:
        prompt_cfg = yaml.safe_load(f)
    max_tokens = _fixed_max_tokens_for_model(
        model,
        fallback=prompt_cfg.get("settings", {}).get("max_tokens", 4096),
    )
    print(f"[global_naming] max_tokens (fixed): {max_tokens}")

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

    # ── Save raw API response for debugging ──────────────────────────────
    output_dir.mkdir(parents=True, exist_ok=True)
    api_response_path = output_dir / "global_naming_response.json"
    with api_response_path.open("w", encoding="utf-8") as f:
        json.dump(result, f, ensure_ascii=False, indent=2)
    print(f"[global_naming] Saved raw API response → {api_response_path}")

    # ── Parse and apply ──────────────────────────────────────────────────
    raw_topics = result.get("topics", [])
    print(f"[global_naming] Received {len(raw_topics)} renamed topics from LLM")

    # Log the first few returned topic_ids to aid debugging
    if raw_topics:
        sample = [str(t.get("topic_id", "")) for t in raw_topics[:5]]
        print(f"[global_naming] Sample returned topic_ids: {sample}")

    # Build normalised lookup: norm_code -> original cluster_code
    # Warn if two codes normalise to the same key (would cause silent collisions)
    orig_codes: list[str] = clusters_to_rename["cluster_code"].astype(str).tolist()
    norm_to_orig: dict[str, str] = {}
    for c in orig_codes:
        nk = _normalize_id(c)
        if nk in norm_to_orig:
            print(f"[global_naming] WARNING: norm_to_orig collision: "
                  f"{norm_to_orig[nk]!r} and {c!r} both normalise to {nk!r}")
        norm_to_orig[nk] = c

    # Match LLM results via normalised IDs (first match wins)
    renamed: dict[str, str] = {}          # original_code -> name
    unmatched_llm: list[dict] = []        # LLM items that couldn't be matched by ID
    for item in raw_topics:
        raw_id = str(item.get("topic_id", ""))
        norm_id = _normalize_id(raw_id)
        orig_code = norm_to_orig.get(norm_id)
        if orig_code and orig_code not in renamed:
            renamed[orig_code] = item["name"]
        elif orig_code is None:
            unmatched_llm.append(item)
        # If orig_code already matched, it's a genuine LLM duplicate — skip

    print(f"[global_naming] Matched by ID: {len(renamed)} / {n_topics}")
    if unmatched_llm:
        sample_ids = [str(t.get("topic_id", "")) for t in unmatched_llm[:5]]
        print(f"[global_naming] Unmatched LLM items (sample IDs): {sample_ids}")

    # ── Positional fallback for clusters that still have no name ─────────
    # Pair remaining clusters in prompt-order with remaining LLM items.
    # Skip LLM items whose name is already assigned (avoids duplicate cascade).
    unmatched_orig = [c for c in orig_codes if c not in renamed]
    if unmatched_orig and unmatched_llm:
        print(f"[global_naming] Positional fallback: "
              f"{len(unmatched_llm)} LLM items → {len(unmatched_orig)} unmatched clusters")
        used_names: set[str] = set(renamed.values())
        fallback_assigned = 0
        llm_iter = iter(unmatched_llm)
        for orig_code in unmatched_orig:
            # Advance past LLM items whose name is already taken
            assigned = False
            for item in llm_iter:
                candidate_name = item["name"]
                if candidate_name not in used_names:
                    renamed[orig_code] = candidate_name
                    used_names.add(candidate_name)
                    fallback_assigned += 1
                    assigned = True
                    break
            if not assigned:
                break  # no more usable LLM items
        print(f"[global_naming] After fallback: {len(renamed)} / {n_topics} named")

    # ── Validate ─────────────────────────────────────────────────────────
    still_missing = set(orig_codes) - set(renamed.keys())
    if still_missing:
        print(f"[global_naming] WARNING: Missing names for clusters: {still_missing}")

    names_list = list(renamed.values())
    dup_names = {n for n in set(names_list) if names_list.count(n) > 1}
    if dup_names:
        print(f"[global_naming] WARNING: {len(dup_names)} duplicate name(s) — running dedup pass")
        renamed = _fix_duplicates(
            renamed,
            clusters_to_rename,
            system_prompt,
            model,
            prompt_cfg.get("settings", {}).get("temperature", 0.2),
            max_tokens,
        )

    # Final check
    names_final = list(renamed.values())
    final_dups = {n for n in set(names_final) if names_final.count(n) > 1}
    if final_dups:
        print(f"[global_naming] WARNING: {len(final_dups)} duplicate(s) remain after dedup: "
              f"{final_dups}")
    else:
        print(f"[global_naming] All {len(renamed)} names are unique.")

    # Apply global_name to rcs (ensure string dtype — column may be float if all NaN)
    if "global_name" not in rcs.columns:
        rcs["global_name"] = ""
    else:
        rcs["global_name"] = rcs["global_name"].fillna("").astype(str)
    for orig_code, name in renamed.items():
        mask = rcs["cluster_code"].astype(str) == orig_code
        rcs.loc[mask, "global_name"] = name

    # Print summary
    for orig_code, name in sorted(renamed.items()):
        old = clusters_to_rename.loc[
            clusters_to_rename["cluster_code"].astype(str) == orig_code, "cluster_name"
        ]
        old_name = old.iloc[0] if not old.empty else "?"
        print(f"  {orig_code}: {old_name} → {name}")

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

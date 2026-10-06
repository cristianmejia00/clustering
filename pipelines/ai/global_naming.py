#!/usr/bin/env python3
"""Assign globally distinctive cluster names with one LLM call per level.

After ``enrich_clusters.py`` assigns per-cluster names in isolation, this
script sees **all** clusters at once and renames them so that every name
is unique and clearly distinguishable. Sibling subclusters are listed
together, and parent clusters show their subclusters, so the model can
see where names collide.

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
from collections import Counter
from pathlib import Path

import pandas as pd
import yaml

sys.path.insert(0, str(Path(__file__).parent))

from papers import clean_code, parent_code
from providers import call_llm, init_provider

RENAME_SCHEMA = {
    "name": "rename_topics",
    "schema": {
        "type": "object",
        "properties": {
            "topics": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "topic_id": {"type": "string"},
                        "issue": {"type": "string"},
                        "name": {"type": "string"},
                    },
                    "required": ["topic_id", "issue", "name"],
                    "additionalProperties": False,
                },
            },
        },
        "required": ["topics"],
        "additionalProperties": False,
    },
}

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


_MAX_NAME_CHARS = 60
_STOPWORDS = {"and", "of", "for", "the", "in", "to", "from", "with", "by", "on", "a", "an"}


def _shared_words(names: list[str], min_count: int = 3) -> list[tuple[str, int]]:
    """Content words that recur across several names (a sign of weak contrast)."""
    counts = Counter(
        w
        for name in names
        for w in {t for t in re.findall(r"[a-z0-9][a-z0-9-]*", str(name).lower())
                  if t not in _STOPWORDS}
    )
    return [(w, c) for w, c in counts.most_common() if c >= min_count]


def _build_user_message(
    rcs: pd.DataFrame,
    subcluster_summary: pd.DataFrame | None = None,
) -> str:
    """List all clusters for the LLM, grouping siblings under their parent.

    With *subcluster_summary*, each parent cluster also lists its
    subclusters' names as read-only context.
    """
    children: dict[str, list[str]] = {}
    if subcluster_summary is not None and not subcluster_summary.empty:
        for _, row in subcluster_summary.iterrows():
            gn = row.get("global_name")
            name = gn if isinstance(gn, str) and gn.strip() else row.get("cluster_name", "")
            children.setdefault(parent_code(row["cluster_code"]), []).append(
                f"{clean_code(row['cluster_code'])} {name} ({row.get('documents', '?')} papers)"
            )

    lines: list[str] = [f"Clusters to name: {len(rcs)}"]
    if children:
        lines.append(
            "Each cluster lists its subclusters for context only; do not rename them. "
            "A parent name must be broader than its subclusters' names and cover all "
            "of them, not just the largest."
        )

    parents = rcs["cluster_code"].map(parent_code)
    for parent, group in rcs.groupby(parents, sort=False, dropna=False):
        names = group["cluster_name"].astype(str).tolist()
        if pd.notna(parent):
            lines += ["", f"=== Subclusters of parent cluster {parent} ==="]
        shared = _shared_words(names)
        if shared:
            lines.append(
                "Words repeated across these current names: "
                + ", ".join(f'"{w}" ({c} of {len(names)})' for w, c in shared)
            )
        for _, row in group.iterrows():
            code = clean_code(row["cluster_code"])
            name = str(row.get("cluster_name", ""))
            too_long = (f"  [too long: {len(name)} characters]"
                        if len(name) > _MAX_NAME_CHARS else "")
            lines += [
                "",
                f"Topic {code} ({row.get('documents', '?')} papers)",
                f"  Current name: {name}{too_long}",
                f"  Description: {row.get('description', '')}",
            ]
            if code in children:
                lines.append("  Subclusters: " + "; ".join(children[code]))

    return "\n".join(lines)


def _duplicate_keys(renamed: dict[str, str]) -> set[str]:
    """Names (case-insensitive) assigned to more than one cluster."""
    counts = Counter(n.strip().casefold() for n in renamed.values())
    return {n for n, c in counts.items() if c > 1}


def _fix_duplicates(
    renamed: dict[str, str],
    clusters_to_rename: pd.DataFrame,
    system_prompt: str,
    settings: dict,
) -> dict[str, str]:
    """Second LLM pass: disambiguate any clusters that still share a name."""
    dup_keys = _duplicate_keys(renamed)
    if not dup_keys:
        return renamed

    dup_codes = [code for code, name in renamed.items() if name.strip().casefold() in dup_keys]
    print(f"[global_naming] Dedup pass: {len(dup_codes)} clusters share "
          f"{len(dup_keys)} duplicate name(s)")

    dup_df = clusters_to_rename[
        clusters_to_rename["cluster_code"].astype(str).isin(dup_codes)
    ].copy()

    already_taken = sorted({n for c, n in renamed.items() if c not in dup_codes})
    lines: list[str] = [
        "The following clusters currently share a name with at least one other cluster. "
        "Assign a NEW, unique name to each one.\n",
        "Names already used by OTHER clusters (do NOT reuse any of these):",
    ]
    for nm in already_taken:
        lines.append(f"  - {nm}")
    lines.append("\nClusters that need a unique replacement name:\n")
    for _, row in dup_df.iterrows():
        code = clean_code(row["cluster_code"])
        lines.append(f"Topic {code}:")
        lines.append(f"  Duplicate name to replace: {renamed.get(str(row['cluster_code']), '')}")
        lines.append(f"  Original cluster name: {row.get('cluster_name', '')}")
        lines.append(f"  Description: {row.get('description', '')}")
        lines.append("")

    result2 = call_llm(
        system_prompt=system_prompt,
        user_prompt="\n".join(lines),
        settings=settings,
        json_schema=RENAME_SCHEMA,
    )

    norm_to_orig_dup: dict[str, str] = {_normalize_id(c): c for c in dup_codes}
    matched_dedup = 0
    for item in result2.get("topics", []):
        orig_code = norm_to_orig_dup.get(_normalize_id(str(item.get("topic_id", ""))))
        if orig_code:
            renamed[orig_code] = item["name"]
            matched_dedup += 1
    print(f"[global_naming] Dedup pass applied {matched_dedup} / {len(dup_codes)} fixes")
    return renamed


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Assign globally distinctive cluster names with one LLM call per level."
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

    # ── Build prompt and call LLM ────────────────────────────────────────
    system_prompt = _load_system_prompt(topic, topic_description)
    user_message = _build_user_message(clusters_to_rename, subcluster_summary)

    print("[global_naming] Calling LLM for global renaming...")
    result = call_llm(
        system_prompt=system_prompt,
        user_prompt=user_message,
        settings=settings,
        json_schema=RENAME_SCHEMA,
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

    if _duplicate_keys(renamed):
        print("[global_naming] WARNING: duplicate name(s) — running dedup pass")
        renamed = _fix_duplicates(renamed, clusters_to_rename, system_prompt, settings)

    # Final check
    final_dups = _duplicate_keys(renamed)
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

"""Select representative papers and build the LLM context for each cluster."""

from __future__ import annotations

import math
from collections import Counter

import pandas as pd

# Guard against malformed records; real abstracts are ~1,500 chars (p90 ~2,100)
_MAX_ABSTRACT_CHARS = 4_000


def clean_code(code: object) -> str:
    """Strip the trailing dashes of subcluster codes: ``"1-3---"`` -> ``"1-3"``."""
    return str(code).strip().rstrip("-")


def parent_code(code: object) -> str | None:
    """Return the parent of a subcluster code (``"1-3"`` -> ``"1"``), or None at level 0."""
    parts = clean_code(code).split("-")
    return "-".join(parts[:-1]) if len(parts) > 1 else None


def is_residual(code: object) -> bool:
    """Codes ending in 99 collect the small clusters left out by thresholding."""
    return clean_code(code).split("-")[-1] == "99"


def parse_keywords(series: pd.Series) -> pd.Series:
    """Turn a ``;``-separated keyword column into per-paper sets of normalised keywords."""
    return series.fillna("").astype(str).map(
        lambda s: {" ".join(k.split()).lower() for k in s.split(";") if k.strip()}
    )


def get_cluster_papers(
    dataset: pd.DataFrame,
    cluster_code: str,
    top: int = 10,
) -> pd.DataFrame:
    """Select the most representative papers for a cluster.

    Picks the union of:
      - top-N papers by ``X_E`` (most connected within the cluster: its core)
      - top-N most cited papers (highest ``Z9``: its landmarks)

    Returns at most ``top * 2`` rows ordered by ``X_E`` then ``Z9``, with a
    ``text`` column holding the abstract.
    """
    cols = ["X_C", "UT", "TI", "AB", "Z9", "X_E", "PY"]
    available = [c for c in cols if c in dataset.columns]
    cluster = dataset.loc[dataset["X_C"].astype(str) == str(cluster_code), available].copy()

    if cluster.empty:
        return cluster

    keep = cluster.nlargest(top, "Z9")["UT"]
    if "X_E" in cluster.columns:
        keep = pd.concat([cluster.nlargest(top, "X_E")["UT"], keep]).drop_duplicates()
    selected = cluster.loc[cluster["UT"].isin(keep)].copy()

    sort_cols = [c for c in ["X_E", "Z9"] if c in selected.columns]
    selected = selected.sort_values(sort_cols, ascending=False)
    selected["text"] = selected["AB"].fillna("").astype(str).str.slice(0, _MAX_ABSTRACT_CHARS)
    return selected


def sample_titles(
    dataset: pd.DataFrame,
    cluster_code: str,
    exclude: pd.Series,
    n: int,
    seed: int,
) -> list[str]:
    """Random titles from the rest of the cluster, showing its breadth beyond the hubs."""
    if n <= 0:
        return []
    rest = dataset.loc[
        (dataset["X_C"].astype(str) == str(cluster_code)) & ~dataset["UT"].isin(exclude),
        "TI",
    ].dropna()
    if rest.empty:
        return []
    return rest.sample(n=min(n, len(rest)), random_state=seed).astype(str).tolist()


def distinctive_keywords(
    dataset: pd.DataFrame,
    cluster_code: str,
    n: int,
) -> tuple[list[tuple[str, float]], str]:
    """Author keywords over-represented in the cluster.

    Level-0 clusters are compared with the whole dataset; subclusters with
    the other subclusters of their parent, which is where names collide.
    Expects a ``_keywords`` column from :func:`parse_keywords` and a
    ``_code`` column from :func:`clean_code`.

    Returns ``([(keyword, share_of_cluster_papers), ...], reference_label)``.
    """
    if n <= 0 or "_keywords" not in dataset.columns:
        return [], ""

    code = clean_code(cluster_code)
    parent = parent_code(code)
    if parent is None:
        reference = dataset
        reference_label = "the whole dataset"
    else:
        reference = dataset.loc[dataset["_code"].str.startswith(parent + "-")]
        reference_label = f"the other subclusters of parent cluster {parent}"
    cluster = dataset.loc[dataset["_code"] == code]
    if cluster.empty or reference.empty:
        return [], reference_label

    n_c, n_r = len(cluster), len(reference)
    count_c = Counter(k for kws in cluster["_keywords"] for k in kws)
    count_r = Counter(k for kws in reference["_keywords"] for k in kws)
    min_support = max(2, math.ceil(0.01 * n_c))

    scored = []
    for kw, c in count_c.items():
        p_c, p_r = c / n_c, count_r[kw] / n_r
        # Skip rare keywords and field-wide ones (e.g. the theme itself)
        if c < min_support or p_c / p_r < 1.5:
            continue
        scored.append((p_c * math.log(p_c / p_r), kw, p_c))
    scored.sort(reverse=True)
    return [(kw, p_c) for _, kw, p_c in scored[:n]], reference_label


def build_cluster_context(
    dataset: pd.DataFrame,
    cluster_code: str,
    papers: pd.DataFrame,
    n_titles: int,
    n_keywords: int,
    seed: int,
    subclusters: pd.DataFrame | None = None,
) -> str:
    """Assemble everything the LLM sees about one cluster.

    Sections: size, distinctive keywords, subclusters (parent clusters
    only), representative papers with abstracts, and sampled titles.
    """
    code = clean_code(cluster_code)
    size = int((dataset["X_C"].astype(str) == str(cluster_code)).sum())
    lines = [f"Cluster {code}: {size} papers."]
    if is_residual(code):
        lines.append(
            "Note: this is a residual group that merges several small clusters, "
            "so it may be heterogeneous."
        )

    keywords, reference_label = distinctive_keywords(dataset, cluster_code, n_keywords)
    if keywords:
        lines += [
            "",
            "## Most distinctive author keywords",
            "Share of this cluster's papers using each keyword; ranked by "
            f"over-representation relative to {reference_label}.",
            "; ".join(f"{kw} ({share:.0%})" for kw, share in keywords),
        ]

    if subclusters is not None and not subclusters.empty:
        lines += ["", "## Subclusters of this cluster (already named)"]
        for _, row in subclusters.iterrows():
            lines.append(f"- {clean_code(row['cluster_code'])} ({row['documents']} papers): {row['name']}")

    lines += [
        "",
        "## Representative papers",
        "The most connected papers within the cluster and its most cited papers.",
    ]
    for i, (_, row) in enumerate(papers.iterrows(), 1):
        meta = []
        if pd.notna(row.get("PY")):
            meta.append(str(int(row["PY"])))
        if pd.notna(row.get("Z9")):
            meta.append(f"{int(row['Z9'])} citations")
        lines += ["", f"[{i}] {row['TI']} ({'; '.join(meta)})", str(row["text"])]

    titles = sample_titles(dataset, cluster_code, papers["UT"], n_titles, seed)
    if titles:
        lines += [
            "",
            "## Other titles from this cluster",
            f"A random sample of {len(titles)} further titles, showing the cluster's breadth.",
        ]
        lines += [f"- {t}" for t in titles]

    return "\n".join(lines)

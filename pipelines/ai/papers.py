"""Select representative papers for each cluster."""

from __future__ import annotations

import pandas as pd


def get_cluster_papers(
    dataset: pd.DataFrame,
    cluster_code: str,
    top: int = 5,
) -> pd.DataFrame:
    """Select the most representative papers for a cluster.

    Picks the union of:
      - top-N most connected papers (highest ``X_E``)
      - top-N most cited papers (highest ``Z9``)

    Returns at most ``top * 2`` rows (fewer if there is overlap or if the
    cluster has fewer papers than *top*).

    Parameters
    ----------
    dataset : pd.DataFrame
        Must contain columns: X_C, UT, TI, AB, Z9, X_E.
    cluster_code : str
        Cluster identifier to filter on ``X_C``.
    top : int
        Number of papers to pick per criterion.

    Returns
    -------
    pd.DataFrame
        Subset with an added ``text`` column (TI + " " + AB).
    """
    cols = ["X_C", "UT", "TI", "AB", "Z9", "X_E", "PY"]
    available = [c for c in cols if c in dataset.columns]
    cluster = dataset.loc[dataset["X_C"].astype(str) == str(cluster_code), available].copy()

    if cluster.empty:
        return cluster

    if len(cluster) <= top:
        selected = cluster
    else:
        by_citations = cluster.nlargest(top, "Z9")["UT"]
        if "X_E" in cluster.columns:
            by_connectivity = cluster.nlargest(top, "X_E")["UT"]
            keep = pd.concat([by_connectivity, by_citations]).drop_duplicates()
        else:
            keep = by_citations
        selected = cluster.loc[cluster["UT"].isin(keep)]

    selected = selected.copy()
    ti = selected["TI"].fillna("")
    #ab = selected["AB"].fillna("")
    selected["text"] = ti
    #selected["text"] = ti.str.cat(ab, sep=" ")

    return selected


def build_bulk_text(papers: pd.DataFrame, max_chars: int = 14_000) -> str:
    """Concatenate paper texts with ##### separators, capped at *max_chars*.

    Parameters
    ----------
    papers : pd.DataFrame
        Must have a ``text`` column.
    max_chars : int
        Maximum character length of the returned string (approx 3 500 tokens).
    """
    parts = ["##### " + t for t in papers["text"].head(10)]
    bulk = " ".join(parts)
    return bulk[:max_chars]

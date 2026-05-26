from __future__ import annotations

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd
import yaml
from bertopic import BERTopic
from sklearn.cluster import KMeans
from sklearn.manifold import TSNE
from sklearn.metrics.pairwise import cosine_distances
from sklearn.preprocessing import MinMaxScaler
from umap import UMAP


def _as_str(x: object) -> str:
    return "" if pd.isna(x) else str(x)


def _load_yaml(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def _to_int(value: object, default: int) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _to_float(value: object, default: float) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _calculate_centroid_scores(embeddings: np.ndarray, topics: np.ndarray) -> np.ndarray:
    """Return normalized distance-to-centroid score in [0,1] per topic.

    0 = closest to topic centroid, 1 = farthest from centroid.
    Outlier topic -1 is assigned score 0.
    """
    embeddings = np.asarray(embeddings)
    topics = np.asarray(topics)
    scores = np.zeros(len(embeddings), dtype=float)

    for topic in np.unique(topics):
        if topic == -1:
            continue

        mask = topics == topic
        topic_embeds = embeddings[mask]
        if topic_embeds.size == 0:
            continue

        centroid = np.mean(topic_embeds, axis=0)
        dists = cosine_distances([centroid], topic_embeds)[0]

        if len(dists) > 1 and dists.max() > dists.min():
            scores[mask] = (dists - dists.min()) / (dists.max() - dists.min())
        else:
            scores[mask] = 0.0

    return scores


def _safe_tsne_coords(embeddings: np.ndarray, seed: int) -> np.ndarray:
    n_samples = len(embeddings)
    if n_samples == 0:
        return np.zeros((0, 2), dtype=float)
    if n_samples == 1:
        return np.zeros((1, 2), dtype=float)

    perplexity = min(50, max(5, n_samples - 1))
    tsne_model = TSNE(
        n_components=2,
        perplexity=perplexity,
        learning_rate="auto",
        init="pca",
        metric="cosine",
        random_state=seed,
        n_jobs=-1,
    )
    coords_2d = tsne_model.fit_transform(embeddings)
    scaler = MinMaxScaler(feature_range=(-100, 100))
    return scaler.fit_transform(coords_2d)


def main() -> None:
    parser = argparse.ArgumentParser(description="Run BERTopic pipeline non-interactively")
    parser.add_argument("--config-analysis", default="config_analysis.yml")
    parser.add_argument("--config-dataset", default="config_dataset.yml")
    args = parser.parse_args()

    cfg_analysis = _load_yaml(Path(args.config_analysis))
    _ = _load_yaml(Path(args.config_dataset))

    metadata = cfg_analysis.get("metadata", {})
    params = cfg_analysis.get("params", {})
    topic_cfg = cfg_analysis.get("topic_model", cfg_analysis.get("tmo", {}))

    root = Path(metadata["bibliometrics_directory"])
    project_folder = metadata["project_folder"]
    filtered_folder = metadata["filtered_folder"]
    analysis_id = metadata["analysis_id"]

    embeds_folder = topic_cfg.get("embeds_folder", "e01")
    n_topics = _to_int(topic_cfg.get("n_topics", 0), 0)
    min_topic_size = max(_to_int(topic_cfg.get("min_topic_size", 10), 10), 2)
    others_threshold = _to_float(topic_cfg.get("others_threshold", 0.99), 0.99)
    seed = _to_int(params.get("seed", 100), 100)

    analysis_dir = root / project_folder / analysis_id
    analysis_dir.mkdir(parents=True, exist_ok=True)

    dataset_path = root / project_folder / filtered_folder / "dataset_raw_cleaned.csv"
    embeds_base = root / project_folder / filtered_folder / embeds_folder

    embeddings_path = embeds_base / "embeddings.npy"
    embeddings_ids_path = embeds_base / "embeddings_ids.json"
    corpus_path = embeds_base / "corpus.csv"

    for required_path in [dataset_path, embeddings_path, embeddings_ids_path, corpus_path]:
        if not required_path.exists():
            raise FileNotFoundError(f"Required artifact not found: {required_path}")

    df_dataset = pd.read_csv(dataset_path, encoding="latin-1")
    embeddings_array = np.load(embeddings_path)
    with embeddings_ids_path.open("r", encoding="utf-8") as f:
        embeddings_ids = json.load(f)
    corpus = pd.read_csv(corpus_path).reset_index(drop=True)

    if "text" not in corpus.columns:
        raise ValueError(f"corpus.csv missing required 'text' column: {corpus_path}")

    documents = corpus["text"].astype(str).tolist()

    if len(embeddings_array) != len(documents):
        raise ValueError(
            f"Embeddings/document count mismatch: {len(embeddings_array)} vs {len(documents)}"
        )

    id_col = "uuid" if "uuid" in corpus.columns else ("UT" if "UT" in corpus.columns else None)
    if id_col is None:
        raise ValueError("Neither 'uuid' nor 'UT' column found in corpus.csv")

    corpus_ids = corpus[id_col].astype(str).tolist()
    if corpus_ids != [str(x) for x in embeddings_ids]:
        raise ValueError("embeddings_ids.json is not aligned with corpus.csv order")

    umap_model = UMAP(
        n_neighbors=15,
        n_components=5,
        min_dist=0.0,
        metric="cosine",
        random_state=seed,
    )

    if n_topics == 0:
        from hdbscan.hdbscan_ import HDBSCAN

        cluster_model = HDBSCAN(
            min_cluster_size=min_topic_size,
            metric="euclidean",
            cluster_selection_method="eom",
            prediction_data=True,
        )
    else:
        cluster_model = KMeans(n_clusters=n_topics, random_state=seed, n_init=10)

    topic_model = BERTopic(
        umap_model=umap_model,
        hdbscan_model=cluster_model,
        min_topic_size=min_topic_size,
        n_gram_range=(1, 3),
        language="english",
        calculate_probabilities=True,
        verbose=True,
    )

    topics, _ = topic_model.fit_transform(documents, embeddings_array)

    tm_summary = topic_model.get_topic_info()
    tm_summary.to_csv(analysis_dir / "topic_model_info.csv", index=False)

    tm_params = {k: str(v) for k, v in dict(topic_model.get_params()).items()}
    with (analysis_dir / "topic_model_params.json").open("w", encoding="utf-8") as f:
        json.dump(tm_params, f, ensure_ascii=False, indent=2)

    scores = _calculate_centroid_scores(embeddings_array, np.array(topics))

    raw_document_info = topic_model.get_document_info(
        documents,
        df=corpus,
        metadata={"Score": scores},
    )

    keep_mask = raw_document_info["Topic"] != -1

    if (~keep_mask).any():
        orphans = raw_document_info[~keep_mask]
        orphans.to_csv(analysis_dir / "orphans.csv", index=False)
    dataset_clustering_results = raw_document_info[keep_mask].copy()

    dataset_clustering_results = dataset_clustering_results.reset_index(drop=True)

    if "text" in dataset_clustering_results.columns:
        dataset_clustering_results = dataset_clustering_results.drop(columns=["text"])

    dataset_clustering_results["X_E"] = dataset_clustering_results["Score"]
    dataset_clustering_results["X_C"] = dataset_clustering_results["Topic"] + 1

    cluster_counts = dataset_clustering_results["X_C"].value_counts().sort_values(ascending=False)
    total_rows = len(dataset_clustering_results)
    cumulative = cluster_counts.cumsum() / max(total_rows, 1)
    main_clusters = cluster_counts.index[cumulative <= others_threshold].tolist()

    if len(main_clusters) == 0 and len(cluster_counts) > 0:
        main_clusters = [cluster_counts.index[0]]

    dataset_clustering_results["level0"] = dataset_clustering_results["X_C"].apply(
        lambda x: x if x in main_clusters else 99999
    )
    dataset_clustering_results["cl99"] = dataset_clustering_results["level0"] == 99999
    dataset_clustering_results["cl-99"] = dataset_clustering_results["level0"] == 99999

    if "UT" not in dataset_clustering_results.columns:
        raise ValueError("Topic-model output is missing UT column; required downstream")

    if "uuid" not in dataset_clustering_results.columns and "uuid" in df_dataset.columns:
        uuid_lookup = (
            df_dataset[["UT", "uuid"]]
            .dropna(subset=["UT"])
            .drop_duplicates(subset=["UT"], keep="first")
        )
        dataset_clustering_results = dataset_clustering_results.merge(
            uuid_lookup,
            on="UT",
            how="left",
        )

    valid_embeddings = np.asarray(embeddings_array)[keep_mask.to_numpy()]
    df_clean = dataset_clustering_results.reset_index(drop=True)

    coords_2d = _safe_tsne_coords(valid_embeddings, seed=42)
    df_clean["x_coords_tsne"] = coords_2d[:, 0] if len(coords_2d) else []
    df_clean["y_coords_tsne"] = coords_2d[:, 1] if len(coords_2d) else []

    coord_cols = ["UT", "x_coords_tsne", "y_coords_tsne"]
    if "uuid" in df_clean.columns:
        coord_cols = ["uuid"] + coord_cols

    df_clean[coord_cols].to_csv(analysis_dir / "document_coords_tsne.csv", index=False)

    dataset_clustering_results.to_csv(analysis_dir / "dataset_minimal.csv", index=False)

    try:
        topic_model.save(str(analysis_dir / "topic_model_object.pck"), serialization="pickle")
    except TypeError:
        topic_model.save(str(analysis_dir / "topic_model_object.pck"))

    print(f"Topic-model artifacts saved in: {analysis_dir}")


if __name__ == "__main__":
    main()

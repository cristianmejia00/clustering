from __future__ import annotations

import json
import re
from pathlib import Path

import numpy as np
import pandas as pd
import yaml
from sentence_transformers import SentenceTransformer


def _as_str(x: object) -> str:
    return "" if pd.isna(x) else str(x)


def _clean_text(text: str, profile: dict) -> str:
    value = text
    if profile.get("to_lowercase", False):
        value = value.lower()
    if profile.get("remove_numbers", False):
        value = re.sub(r"\d+", " ", value)
    if profile.get("remove_symbols", False):
        value = re.sub(r"[^a-zA-Z\s]", " ", value)
    value = value.replace("nan ", " ").strip()
    value = re.sub(r"\s+", " ", value)
    return value


def _get_embed_profiles(embeds_cfg: dict) -> list[tuple[str, dict]]:
    profiles = []
    for key, value in embeds_cfg.items():
        if str(key).lower().startswith("e") and isinstance(value, dict):
            profiles.append((key, value))
    if not profiles:
        raise ValueError("No embedding profiles found in config_dataset.yml (expected keys like e01, e02).")
    return profiles


def _read_dotenv_var(key: str, dotenv_path: Path = Path(".env")) -> str:
    if not dotenv_path.exists():
        return ""
    for raw in dotenv_path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if "=" not in line:
            continue
        k, v = line.split("=", 1)
        if k.strip() == key:
            return v.strip().strip('"').strip("'")
    return ""


def main() -> None:
    cfg_path = Path("config_dataset.yml")
    if not cfg_path.exists():
        raise FileNotFoundError("config_dataset.yml not found at repository root.")

    with cfg_path.open("r", encoding="utf-8") as f:
        cfg = yaml.safe_load(f)

    metadata = cfg["metadata"]
    embeds_cfg = cfg["embeds"]

    env_root = _read_dotenv_var("BIBLIOMETRICS_DRIVE")
    if env_root:
        root = Path(env_root)
    else:
        root = Path(metadata["bibliometrics_directory"])

    project = root / metadata["project_folder"]
    filtered_folder = embeds_cfg["from_filtered_dataset"]

    dataset_path = project / filtered_folder / "dataset_raw_cleaned.csv"
    if not dataset_path.exists():
        raise FileNotFoundError(f"Dataset not found: {dataset_path}")

    print(f"Loading dataset: {dataset_path}")
    df = pd.read_csv(dataset_path, encoding="latin-1")

    profiles = _get_embed_profiles(embeds_cfg)
    for profile_name, profile in profiles:
        print(f"\n=== Embeddings profile: {profile_name} ===")
        text_columns = profile.get("text_columns", [])
        if not text_columns:
            raise ValueError(f"Profile {profile_name} has no text_columns.")

        missing_cols = [c for c in text_columns if c not in df.columns]
        if missing_cols:
            raise ValueError(f"Profile {profile_name} missing text columns in dataset: {missing_cols}")

        id_cfg = profile.get("id_column", "UT")
        id_column = id_cfg[0] if isinstance(id_cfg, list) and id_cfg else id_cfg
        if not isinstance(id_column, str):
            raise ValueError(f"Profile {profile_name} has invalid id_column: {id_cfg}")

        if "UT" not in df.columns:
            raise ValueError("Dataset must include 'UT' column for downstream compatibility.")
        if id_column not in df.columns:
            raise ValueError(f"Profile {profile_name} id_column '{id_column}' not found in dataset.")

        data = df.copy()
        for col in text_columns:
            data[col] = data[col].map(_as_str)

        data["text"] = data[text_columns].agg(" ".join, axis=1)
        data["text"] = data["text"].map(lambda x: _clean_text(x, profile))

        corpus_columns = list(dict.fromkeys(["text", "UT", id_column]))
        corpus = data[corpus_columns].dropna().reset_index(drop=True)

        out_dir = project / filtered_folder / profile_name
        out_dir.mkdir(parents=True, exist_ok=True)

        model_name = profile["transformer_model"]
        print(f"Model: {model_name}")
        model = SentenceTransformer(model_name)
        embeddings = model.encode(corpus["text"].tolist(), show_progress_bar=True)

        np.save(out_dir / "embeddings.npy", embeddings)

        id_values = corpus.loc[:, id_column]
        if isinstance(id_values, pd.DataFrame):
            id_values = id_values.iloc[:, 0]
        ids = id_values.astype(str).tolist()

        with (out_dir / "embeddings_ids.json").open("w", encoding="utf-8") as f:
            json.dump(ids, f)

        corpus.to_csv(out_dir / "corpus.csv", index=False)
        with (out_dir / "embeds_settings.json").open("w", encoding="utf-8") as f:
            json.dump(profile, f, indent=2)

        print(f"Saved: {out_dir / 'embeddings.npy'}")
        print(f"Saved: {out_dir / 'embeddings_ids.json'}")
        print(f"Saved: {out_dir / 'corpus.csv'}")

    print("\nAll embedding profiles completed.")


if __name__ == "__main__":
    main()

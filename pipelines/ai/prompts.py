"""Load prompt templates from YAML and fill placeholders."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml

_PROMPTS_DIR = Path(__file__).parent / "prompts"


def load_prompt(
    task_name: str,
    **context: str,
) -> tuple[str, str, dict[str, Any]]:
    """Load a prompt template and fill its placeholders.

    Parameters
    ----------
    task_name : str
        Matches a YAML filename in ``prompts/`` (without extension).
        E.g. ``"cluster_description"``.
    **context : str
        Values for ``{placeholder}`` variables in the template.

    Returns
    -------
    tuple[str, str, dict]
        ``(system_prompt, user_prompt, settings)`` where *settings* contains
        per-prompt overrides like ``temperature`` and ``max_tokens``.

    Raises
    ------
    FileNotFoundError
        If the template YAML does not exist.
    KeyError
        If a required placeholder is missing from *context*.
    """
    path = _PROMPTS_DIR / f"{task_name}.yml"
    if not path.exists():
        available = sorted(p.stem for p in _PROMPTS_DIR.glob("*.yml"))
        raise FileNotFoundError(
            f"No prompt template '{task_name}'. Available: {available}"
        )

    with path.open("r", encoding="utf-8") as f:
        template = yaml.safe_load(f)

    system_prompt = template["system"].format(**context)
    user_prompt = template["user"].format(**context)
    settings = template.get("settings", {})

    return system_prompt, user_prompt, settings

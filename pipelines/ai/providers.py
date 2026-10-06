"""LLM provider abstraction using litellm.

Reads per-provider YAML settings, loads API keys from credential files,
and exposes a single ``call_llm`` function that routes to any supported
backend (OpenAI, Anthropic, Google, etc.).

Request parameters come from the provider YAML, so model quirks live in
config rather than code:

- ``temperature: null``       never send temperature (Claude 5.x, GPT-5, o-series reject it)
- ``max_tokens``              output cap per call; reasoning models spend part of it thinking
- ``structured_output: true`` enforce the JSON schema via ``response_format``;
                              when false the JSON is requested in the prompt and parsed
- ``extra_params``            provider-specific request params passed through as-is
"""

from __future__ import annotations

import json
import os
import re
import time
from pathlib import Path
from typing import Any

import yaml

import litellm

# Silence litellm's verbose logging by default
litellm.suppress_debug_info = True

_PROVIDERS_DIR = Path(__file__).parent / "providers"


def load_provider_settings(provider_name: str) -> dict[str, Any]:
    """Load a provider's YAML settings file."""
    path = _PROVIDERS_DIR / f"{provider_name}.yml"
    if not path.exists():
        available = sorted(p.stem for p in _PROVIDERS_DIR.glob("*.yml"))
        raise FileNotFoundError(
            f"No settings file for provider '{provider_name}'. "
            f"Available: {available}"
        )
    with path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def init_provider(provider_name: str, repo_root: Path | None = None) -> dict[str, Any]:
    """Load settings and set the API key as an environment variable.

    Parameters
    ----------
    provider_name : str
        Name matching a YAML file in ``providers/`` (e.g. "openai").
    repo_root : Path, optional
        Repository root used to resolve the ``api_key_file`` path.
        Defaults to CWD.

    Returns
    -------
    dict
        The full provider settings dict (model, temperature, max_tokens, etc.).
    """
    settings = load_provider_settings(provider_name)
    root = repo_root or Path.cwd()

    key_file = root / settings["api_key_file"]
    if not key_file.exists():
        raise FileNotFoundError(
            f"API key file not found: {key_file}  "
            f"(expected by provider '{provider_name}')"
        )

    api_key = key_file.read_text(encoding="utf-8").strip()
    env_var = settings.get("api_key_env_var", f"{provider_name.upper()}_API_KEY")
    os.environ[env_var] = api_key

    return settings


def _parse_json(text: str) -> dict[str, Any]:
    """Parse a JSON object from a model reply, tolerating code fences or preamble."""
    text = (text or "").strip()
    text = re.sub(r"^```(?:json)?\s*|\s*```$", "", text)
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        start, end = text.find("{"), text.rfind("}")
        if start == -1 or end <= start:
            raise
        return json.loads(text[start:end + 1])


def call_llm(
    system_prompt: str,
    user_prompt: str,
    settings: dict[str, Any],
    temperature: float | None = None,
    json_schema: dict[str, Any] | None = None,
    retries: int = 3,
    retry_delay: float = 5.0,
) -> str | dict[str, Any]:
    """Send a system + user prompt to any LLM via litellm.

    Retries on transient errors (rate limits, server errors) and on replies
    that are empty, truncated, or not valid JSON.

    Parameters
    ----------
    settings : dict
        Provider settings from :func:`init_provider`.
    temperature : float, optional
        Overrides the provider default. Ignored when the provider sets
        ``temperature: null``.
    json_schema : dict, optional
        ``{"name": ..., "schema": {...}}``. When given, the reply is parsed
        and returned as a dict.

    Returns
    -------
    str or dict
        The assistant's text, or the parsed JSON object if *json_schema* is set.
    """
    messages = [
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": user_prompt},
    ]

    params: dict[str, Any] = {
        "model": settings["model"],
        "messages": messages,
        "max_tokens": settings.get("max_tokens", 16000),
        **(settings.get("extra_params") or {}),
    }
    if settings.get("temperature") is not None:
        params["temperature"] = (
            temperature if temperature is not None else settings["temperature"]
        )
    if json_schema and settings.get("structured_output", False):
        params["response_format"] = {
            "type": "json_schema",
            "json_schema": {**json_schema, "strict": True},
        }

    last_error: Exception | None = None
    for attempt in range(1, retries + 1):
        try:
            response = litellm.completion(**params)
            choice = response.choices[0]
            content = choice.message.content or ""
            if choice.finish_reason == "length":
                raise RuntimeError(
                    f"Reply truncated at max_tokens={params['max_tokens']}; "
                    "raise max_tokens in the provider YAML"
                )
            if not content.strip():
                raise RuntimeError(f"Empty reply (finish_reason={choice.finish_reason})")
            return _parse_json(content) if json_schema else content.strip()
        except Exception as exc:
            last_error = exc
            if attempt < retries:
                wait = retry_delay * attempt
                print(f"  [retry {attempt}/{retries}] {exc!r} — waiting {wait:.0f}s")
                time.sleep(wait)

    raise RuntimeError(
        f"LLM call failed after {retries} attempts: {last_error!r}"
    )

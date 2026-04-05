"""LLM provider abstraction using litellm.

Reads per-provider YAML settings, loads API keys from credential files,
and exposes a single ``call_llm`` function that routes to any supported
backend (OpenAI, Anthropic, Google, etc.).
"""

from __future__ import annotations

import json
import os
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


def call_llm(
    system_prompt: str,
    user_prompt: str,
    model: str,
    temperature: float = 0.1,
    max_tokens: int = 500,
    retries: int = 3,
    retry_delay: float = 5.0,
) -> str:
    """Send a system + user prompt to any LLM via litellm.

    Retries on transient errors (rate limits, server errors).

    Returns
    -------
    str
        The assistant's response text.
    """
    messages = [
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": user_prompt},
    ]

    last_error: Exception | None = None
    for attempt in range(1, retries + 1):
        try:
            response = litellm.completion(
                model=model,
                messages=messages,
                temperature=temperature,
                max_tokens=max_tokens,
            )
            return response.choices[0].message.content
        except Exception as exc:
            last_error = exc
            if attempt < retries:
                wait = retry_delay * attempt
                print(f"  [retry {attempt}/{retries}] {exc!r} — waiting {wait:.0f}s")
                time.sleep(wait)

    raise RuntimeError(
        f"LLM call failed after {retries} attempts: {last_error!r}"
    )


def call_llm_with_tools(
    system_prompt: str,
    user_prompt: str,
    model: str,
    tools: list[dict],
    tool_choice: dict | None = None,
    temperature: float = 0.2,
    max_tokens: int = 4096,
    retries: int = 3,
    retry_delay: float = 5.0,
) -> dict[str, Any]:
    """Send a prompt with function-calling tools via litellm.

    Forces the model to call the specified tool and returns the parsed
    JSON arguments.

    Returns
    -------
    dict
        Parsed arguments from the tool call.
    """
    messages = [
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": user_prompt},
    ]

    last_error: Exception | None = None
    for attempt in range(1, retries + 1):
        try:
            response = litellm.completion(
                model=model,
                messages=messages,
                tools=tools,
                tool_choice=tool_choice or {"type": "function", "function": {"name": tools[0]["function"]["name"]}},
                temperature=temperature,
                max_tokens=max_tokens,
            )
            tool_call = response.choices[0].message.tool_calls[0]
            return json.loads(tool_call.function.arguments)
        except Exception as exc:
            last_error = exc
            if attempt < retries:
                wait = retry_delay * attempt
                print(f"  [retry {attempt}/{retries}] {exc!r} — waiting {wait:.0f}s")
                time.sleep(wait)

    raise RuntimeError(
        f"LLM tool call failed after {retries} attempts: {last_error!r}"
    )

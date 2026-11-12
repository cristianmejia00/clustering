# Change log

## 2026-11-12: LLM switched to Claude Sonnet 5.5; crop breeding analysis

- **Model.** `pipelines/ai/providers/anthropic.yml`: `model` changed from `anthropic/claude-opus-5-5` to `anthropic/claude-sonnet-5-5`. Other settings are unchanged and valid for Sonnet 5.5:
  - `temperature: null` (Sonnet 5.5 rejects non-default sampling values)
  - JSON requested in the prompt, not through forced tool use (also rejected)
  - `output_config.effort: medium`
- **Test call.** `providers.call_llm` from `.venv` returned valid JSON in 1.2 s. The API reported `model: claude-sonnet-5-5`.
- **Configs** for `Q26-11-12_oax_crop_breeding` (68,380 OpenAlex works; see `../openalex/log.md`):
  - All folder names were replaced in `config_dataset.yml` and `config_analysis.yml`.
  - `llm.theme: "Crop Breeding"`; `llm.description` lists the root and the four sub-trends.
  - Unchanged: direct citation, largest component (`strategy: top`, `value: 1`), Louvain, threshold 0.90, subclusters for clusters of 270 or more papers.
- **Run** (all stages, exit 0):
  - Largest component: 51,615 of 68,380 works (75.5%), 331,705 links.
  - 18 level-0 clusters (1–17 plus 99) and 202 subclusters. Sonnet named and described all of them; level-0 global naming used the subcluster context.
- **Possibly off-topic subclusters,** from their names (about 1,900 papers, 3.7%), to review:
  - Wild-plant evolution: 8-1 Hybrid Zones and Phylogenomics of Wild Species (556), 8-3 Hybrid Swarms and Reproductive Isolation in Wild Plants (252), 8-6 Polyploid Speciation and Interploidy Gene Flow (217), 8-9 Floral Trait QTL and Pollination Syndrome Evolution (155)
  - Model plants: 10-1 Arabidopsis Natural Variation and Local Adaptation (430), 10-7 Quantitative Disease Resistance in Arabidopsis and Medicago (110)
  - Partly animal: 4-8 Genomic Prediction in Livestock, Forages and Trees (180)

## 2026-11-12: OpenAlex datasets; scatter-plot axis and title

- **OpenAlex citation networks** (`pipelines/dataset/utils/01_data_loading/01z_compute_direct_citation_network.R`).
  - When every `UT` is an OpenAlex work ID (`https://openalex.org/W…`), the `CR` entries (cited OpenAlex IDs) are matched exactly against `UT`.
  - WoS datasets keep the record-key/DOI matching, unchanged: a test on a synthetic WoS dataset gives identical edges.
  - Input files come from `../openalex/map_to_wos.ipynb`.
- **Scatter plots** (`pipelines/charts/cluster_scatterplots.R`).
  - `fig_scatter_clusters_PY_x_Z9_rank` is now titled "Cluster Landscape: Impact vs. Age", not "Impact vs. Recency".
  - Publication-year axes now extend to whole years at both ends, so the outer ticks show.
  - The tick step adapts: 1 year for spans up to 12 years, 2 up to 24, else 5.
  - Before, a level-0 plot with cluster means between 2021.5 and 2023.8 showed only a 2023 tick.

## 2026-10-05: AI cluster naming and description (`ai` stage)

Goal: use stronger models, better prompts and more context per cluster, so that cluster names match the papers they cover and stay distinct from each other.

### Per-cluster naming (`pipelines/ai/enrich_clusters.py`, `pipelines/ai/papers.py`)

- **One call per cluster instead of three.** A single LLM call now returns a JSON object with the description and then the name, so the name is based on the papers. Previously the pipeline wrote a description, derived the name from that description, then rewrote the description ("enhanced description").
- **More context per cluster**, assembled by `build_cluster_context()`:
  - **Representative papers with full title and abstract**: the top-N papers by `X_E` (most connected within the cluster) combined with the top-N by `Z9` (most cited), ordered by `X_E`. Previously at most 10 abstracts were joined and cut at 14,000 characters, often mid-abstract. Now each abstract is capped at 4,000 characters, only to guard against malformed records.
  - **Random sample of further titles** (seeded with `params.seed`), so the name reflects the whole cluster and not only its most central papers.
  - **Most distinctive author keywords (`DE`)**, each shown with its share of the cluster's papers.
    - Ranked by `p_c · log(p_c / p_ref)`, where `p_c` is the keyword's share in the cluster and `p_ref` its share in the reference set.
    - A keyword must appear in at least max(2, 1% of the cluster) papers and be at least 1.5× more common than in the reference set. This drops keywords used across the whole field, such as the theme itself.
    - Reference set: the whole dataset for level-0 clusters, and the parent cluster for subclusters, because sibling names are where collisions happen.
  - **Subcluster names and sizes** for level-0 clusters, read from the level-1 `cluster_summary.csv`.
  - **Residual groups** (codes ending in `99`) are flagged as possibly heterogeneous.
- `representative_docs_summaries` (optional) still works. It replaces abstracts with one-sentence summaries; its temperature dropped from 0.7 to 0.2.
- New CLI flag: `enrich_clusters.py --subcluster-summary <path>`.
- `ai_metadata.json` also records the context settings used.

### Prompts (`pipelines/ai/prompts/`)

- **New `cluster_label.yml`**, which replaces `cluster_description.yml`, `cluster_name.yml` and `cluster_enhanced_description.yml` (all deleted).
  - The model acts as a bibliometrics and science-mapping expert in `{topic}`, instead of a "policy consultant".
  - It is told what a citation-network cluster is, and that every paper already belongs to `{topic}`, so a useful name says what sets the cluster apart.
  - **Description:** one paragraph of 80–130 words covering the shared research problem, objects of study, methods and subtopics. No paper-by-paper listing, no authors or years, no definition of the topic, no "This cluster focuses on…" opener and no filler phrases.
  - **Name:**
    - 2–6 words, at most 60 characters (charts cut labels at 70 characters, including the `code. ` prefix), in Title Case.
    - Names the specific object and angle the cluster shares.
    - Does not repeat the field name or generic field vocabulary, unless the word is part of an established concept.
    - Uses no filler words (Research, Studies, Advances, …), spells out non-standard acronyms, and has no colon, subtitle, quotation marks or final period.
- **`global_naming.yml` rewritten.**
  - Per-cluster descriptions are treated as accurate. The task is to make the names work together as one map.
  - Five rules: faithful, distinct, contrastive among siblings, consistent style, and stable (a name that already meets every rule is kept).
  - The output has an `issue` field before each `name`. The model must state which rule a name breaks, or which cluster it overlaps with, before renaming it. Without this, models tended to confirm every name unchanged.
  - Residual (`99`) groups are named after their dominant themes.
- All prompts stay generic: the topic and its description come from `llm.theme` and `llm.description` in `config_analysis.yml`.

### Global naming (`pipelines/ai/global_naming.py`)

- Uses the same JSON call as the per-cluster step. It no longer forces a function call, which current Claude models reject.
- The prompt is grouped by parent:
  - Sibling subclusters are listed together under "Subclusters of parent cluster N".
  - At level 0, each cluster lists its subclusters as context only; they are not renamed.
- Computed hints for each group:
  - words repeated across at least 3 current names, with counts;
  - names over 60 characters, flagged `[too long]`.
- Duplicate detection is case-insensitive, and the follow-up pass that fixes duplicates uses the same call.
- The per-model `max_tokens` table is gone; `max_tokens` now comes from the provider YAML.
- `global_naming_response.json` keeps the raw reply, including the `issue` notes.

### Provider layer (`pipelines/ai/providers.py`, `pipelines/ai/providers/*.yml`)

- One function, `call_llm(system, user, settings, temperature=None, json_schema=None)`. `call_llm_with_tools` was removed.
- Model-specific request settings live in the provider YAML instead of in code:

  | Key | Effect |
  |---|---|
  | `temperature: null` | Temperature is never sent (required for Claude 5.x, GPT-5 and o-series models). |
  | `max_tokens` | Output cap per call. Reasoning models spend part of it on thinking. |
  | `structured_output` | `true`: the JSON schema is enforced through `response_format` (strict). `false`: JSON is requested in the prompt and parsed, tolerating code fences or preamble. |
  | `extra_params` | Passed to the request as-is, e.g. Claude `output_config.effort`. |

- Retries also cover empty replies, truncated replies (`finish_reason == "length"`) and replies that aren't valid JSON.
- Default models:

  | Provider | Model | Settings |
  |---|---|---|
  | `openai` | `gpt-4.1` (was `gpt-4.1-mini`) | temperature 0.2, structured output on |
  | `anthropic` | `anthropic/claude-opus-5-5` (was `claude-sonnet-4-20250514`) | no temperature, effort `medium`, structured output off |
  | `google` | `gemini/gemini-2.0-flash` (unchanged) | `max_tokens` 500 → 8192 (global naming needs it), structured output off; untested |

- **Why structured output is off for Claude:** litellm 1.83 does not recognise Claude 5.x models. It turns `response_format` into a forced tool call and also sends `temperature`, and these models reject both with a 400 error. litellm also forwards `extra_body` literally, which the API rejects; pass Claude-specific parameters at the top level through `extra_params`.
- **GPT-5 models** must be enabled for the OpenAI API project, otherwise they return 403. Once enabled, set `model` and `temperature: null` in `openai.yml`.

### R runner (`scripts/ai_only.R`)

- `DE` is now written to `dataset_for_ai.csv`.
- Level-0 enrichment now also receives `--subcluster-summary` (previously only global naming did). Levels are still processed deepest first, so subcluster names exist when parents are named.

### Configuration (`config_analysis.yml`, `llm` section)

- New keys:
  - `sample_titles` (default 40);
  - `top_keywords` (default 15).
- Changed defaults:
  - `top_papers` 7 → 10. It is per criterion, so up to 2N papers are sent;
  - `rate_limit_delay` 2 → 1.
- `cluster_enhanced_description` was removed from `compute` and is ignored if present. The remaining tasks are `representative_docs_summaries` (optional), `cluster_title`, `cluster_description` and `global_naming`.

## 2026-10-06: Global naming must not narrow a name to a minor strand

- `global_naming.yml`, rule 1 (faithful): a name must reflect the main body of its description and must not be narrowed to a minor strand. The model is told to read the description's weighting cues ("dominant", "core", "centre" versus "smaller", "further", "peripheral", "also").
- Rule 2 (distinct): separating details must hold for most of the cluster. If only a minor strand separates a broad cluster from a specialised sibling, the broad cluster keeps a broad name and the sibling's name carries the contrast.
- Reason: to separate a broad cluster from a specialised sibling, the global pass sometimes renamed the broad cluster after one of its minor strands.

## Usage notes

- **Resumption:** `enrich_clusters.py` skips clusters that already have a `cluster_name`.
  - Re-running `ai` on a finished project only repeats global naming, one call per level.
  - To regenerate names and descriptions from scratch, re-run `reports` first (it resets `rcs_merged.csv`) or clear `cluster_name`.
- **Global naming always starts from `cluster_name`** and rewrites `global_name`. Results vary somewhat between runs, especially on Claude 5.x, which doesn't accept a temperature setting.
- **Downstream stages** (`charts`, `enriched_embeds`) read `global_name` from `rcs_merged.csv`. Re-run them after editing names by hand.
- **New dataset:** update `llm.theme` and `llm.description`. Switch provider with `llm.provider`.

## Known limitations and follow-ups

- The untracked `docker/config_analysis.yml` template still lists `cluster_enhanced_description` and `top_papers: 7`. This is harmless, but the template should be updated.
- No spelling convention is enforced: names and descriptions can mix US and UK spelling.
- Some prompt wording leans toward materials and waste vocabulary:
  - "waste stream" appears in both prompts;
  - the naming example in `cluster_label.yml` is "Lithium-Ion Battery Recycling".

  The prompts still work for other topics, but this wording could be made field-neutral.
- Anthropic server-side refusal fallbacks are not enabled, because litellm reserves the `fallbacks` parameter name.
- The Google provider is untested.

# ADH politics event-study pipeline (`extension-adh2013/src`)

The pipeline builds CZ-level presidential outcomes from Amlani and Algara (2021), bridges counties to 1990 geography with the FTZ county crosswalk, joins ADH China-shock exposure and baseline controls, validates the panel, estimates event-study specifications, and exports diagnostics, tables, figures, crosswalk support diagnostics, and demonstration maps.

Run from the project root:

```r
source(here::here("src", "run_pipeline.R"))
```

## Default run

The default run is the practical M5 + M2-fallback specification used for diagnostics and drafting:

- `crosswalk_weight = "m5_weight"`
- `crosswalk_missing_weight_policy = "fallback_m2"`
- `require_balanced_panel = TRUE`
- `min_retained_vote_share = 0.98`
- `main_se_type = "cluster_cz"`
- `allow_vcov_repair = FALSE`
- `require_no_vcov_repair_for_main = TRUE`
- `standardize_interacted_controls = TRUE`
- `save_single_rds = FALSE`

Set `CROSSWALK_MISSING_WEIGHT_POLICY="fail"` when you want a strict diagnostic run that stops if selected FTZ weights have zero/undefined support.

## Four-specification crosswalk sensitivity run

To run M5 + fallback M2, M6 + fallback M2, pure M2, and pure M4 in one pass and create the appendix comparison plot:

```r
Sys.setenv(RUN_CROSSWALK_SENSITIVITY = "true")
source(here::here("src", "run_pipeline.R"))
```

The sensitivity plan is:

| Label | `CROSSWALK_WEIGHT` | Missing-weight policy |
|---|---|---|
| M5 + fallback M2 | `m5_weight` | `fallback_m2` |
| M6 + fallback M2 | `m6_weight` | `fallback_m2` |
| Pure M2 | `m2_weight` | `fallback_m2` |
| Pure M4 | `m4_weight` | `fallback_m4` |

For the pure M2/M4 runs, the fallback policy is the same as the selected weight. It is used only to keep non-analysis/unavailable cases from triggering the strict selected-weight failure; it does not substitute M2 into the M4 specification.

The comparison plot is written to:

- `output/figures/fig_crosswalk_specification_comparison.png`
- `output/figures/fig_crosswalk_specification_comparison.pdf`

## Demonstration maps

The demo maps use **1990 county geography** and flag ADH-mainland 1990 counties/CZs that receive **positive bridged vote mass** from source counties where M2, M4, M5, or M6 support is zero/undefined. This simplified demonstration does not require 1970/1980/2000/2010/2020 shapefiles.

Required runtime inputs:

1. the full FTZ county-to-county crosswalk ZIP at
   `ftz2024/crosswalks/CountyToCounty/1990/1990_csv.zip`;
2. a 1990 county shapefile under `spatial-data/counties-1990` (for example
   the Stanford/BTAA `co1990p020.shp` and companion files) or `COUNTY1990_SHAPEFILE` set to a `.shp` file or directory containing one;
3. package `sf` installed.

The map exporter writes full-resolution and simplified-geometry versions:

- `fig_crosswalk_unavailable_weights_1990_counties.*`
- `fig_crosswalk_unavailable_weights_1990_counties_simplified.*`
- `fig_crosswalk_unavailable_weights_1990_czs.*`
- `fig_crosswalk_unavailable_weights_1990_czs_simplified.*`

All PNGs are saved with a white background for portable previewing.

## Large model-output files and GitHub

GitHub blocks regular repository files larger than 100 MiB. The pipeline therefore defaults to `save_single_rds = FALSE` and writes large model objects as compressed chunk directories such as:

```text
output/intermediate/event_study_results_all_specs_aa2021_rep_margin_m5_rds_chunks/
```

Each chunk is capped by `rds_chunk_size_mib` (default: 45 MiB). The single `.rds` files are ignored by `.gitignore`; the chunk directories are intended to be GitHub-safe. Set `SAVE_SINGLE_RDS=true` only for local debugging or if you use Git LFS.

If a large `.rds` has already been committed, remove it from Git history before pushing; adding it to `.gitignore` only prevents future additions.

## Dependency policy

The runner does not install packages. Install the core dependencies listed in `src/dependencies.json`. The `sf` package is required for routine diagnostic maps; `ipumsr` is optional for NHGIS shapefile retrieval.


## Crosswalk sensitivity appendix run

Set `RUN_CROSSWALK_SENSITIVITY=true` to run four appendix specifications in one pass:

- M5 + fallback M2
- M6 + fallback M2
- Pure M2
- M4 + fallback M2

The runner writes `output/diagnostics/crosswalk_sensitivity_run_status.csv` and archives per-spec diagnostics under `output/diagnostics/by_crosswalk/<slug>/`. The M4 specification intentionally uses M2 fallback so that all four sensitivity series use the same balanced ADH mainland CZ panel.

Diagnostic support-map PNGs are saved with full, simplified, and small-raster versions.

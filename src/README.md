# ADH politics event-study pipeline (`replication/src`)

The pipeline builds CZ-level presidential outcomes from Amlani and Algara (2021),
bridges counties to 1990 geography with the FTZ county crosswalk, joins ADH
China-shock exposure and baseline controls, validates the panel, estimates
event-study specifications, and exports diagnostics, tables, and figures.

Run from the replication project root:

```r
source(here::here("src", "run_pipeline.R"))
```

The default run uses a strict crosswalk policy and conservative VCOV policy:

- `crosswalk_missing_weight_policy = "fail"`
- The strict default intentionally stops if selected M5/M6 built-up-support
  weights are zero, undefined, or otherwise invalid. This creates an auditable
  diagnostic run before any fallback is chosen.
- `require_balanced_panel = TRUE`
- `min_retained_vote_share = 0.98`
- `main_se_type = "cluster_cz"`
- `allow_vcov_repair = FALSE`
- `require_no_vcov_repair_for_main = TRUE`

Strict M5/M6 runs are expected to fail for this application because some source
counties have zero or undefined built-up support. The pipeline writes a preflight
M5/M6 support report before building the selected crosswalk.

After reviewing the strict preflight report, run an exploratory fallback build only
with an explicit override, for example:

```r
Sys.setenv(CROSSWALK_MISSING_WEIGHT_POLICY = "fallback_m2")
source(here::here("src", "run_pipeline.R"))
```

Other supported overrides include `CROSSWALK_WEIGHT` (`m5_weight` or
`m6_weight`), `RENORMALIZE_CROSSWALK_WEIGHTS`, `REQUIRE_BALANCED_PANEL`,
`REQUIRE_NO_VCOV_REPAIR_FOR_MAIN`, `ALLOW_VCOV_REPAIR`, and `ALLOW_FAILED_DIAGNOSTICS`. Repaired Conley
output is explicitly labeled provisional and should be treated as robustness
output, not the default main specification.


The runner no longer installs R packages. Install dependencies before running, or
restore them from `src/dependencies.json` / `output/diagnostics/dependency_manifest.json`.

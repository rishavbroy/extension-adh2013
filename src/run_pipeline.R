# extension-adh2013/src/run_pipeline.R

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install it before running the pipeline.", call. = FALSE)
}
library(here)
here::i_am("src/run_pipeline.R")

source(here::here("src", "00_config.R"))
source(here::here("src", "01_helpers.R"))
source(here::here("src", "02_build_analysis_data.R"))
source(here::here("src", "03_estimate_event_study.R"))
source(here::here("src", "04_export_outputs.R"))
source(here::here("src", "05_export_diagnostic_maps.R"))

read_bool_env <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  tolower(value) %in% c("1", "true", "t", "yes", "y")
}

run_single_pipeline <- function(config) {
  config <- finalize_config(config)
  ensure_output_dirs(config)
  write_dependency_manifest(config)

  message("Building analysis data...")
  build_analysis_data(config, stop_on_fatal = TRUE)

  message("Estimating event-study specifications...")
  estimate_event_study(config, stop_on_fatal = TRUE)

  message("Exporting tables and figures...")
  export_event_study_outputs(config)

  message("Exporting diagnostic maps and crosswalk comparisons when inputs are available...")
  export_diagnostic_maps_and_comparisons(config)

  invisible(config)
}

policy_override <- Sys.getenv("CROSSWALK_MISSING_WEIGHT_POLICY", unset = "")
if (nzchar(policy_override)) CONFIG$crosswalk_missing_weight_policy <- policy_override

weight_override <- Sys.getenv("CROSSWALK_WEIGHT", unset = "")
if (nzchar(weight_override)) CONFIG$crosswalk_weight <- weight_override

CONFIG$renormalize_crosswalk_weights <- read_bool_env(
  "RENORMALIZE_CROSSWALK_WEIGHTS", CONFIG$renormalize_crosswalk_weights
)
CONFIG$require_balanced_panel <- read_bool_env(
  "REQUIRE_BALANCED_PANEL", CONFIG$require_balanced_panel
)
CONFIG$require_no_vcov_repair_for_main <- read_bool_env(
  "REQUIRE_NO_VCOV_REPAIR_FOR_MAIN", CONFIG$require_no_vcov_repair_for_main
)
CONFIG$allow_vcov_repair <- read_bool_env("ALLOW_VCOV_REPAIR", CONFIG$allow_vcov_repair)
CONFIG$allow_failed_diagnostics <- read_bool_env(
  "ALLOW_FAILED_DIAGNOSTICS", CONFIG$allow_failed_diagnostics
)
CONFIG$standardize_interacted_controls <- read_bool_env(
  "STANDARDIZE_INTERACTED_CONTROLS", CONFIG$standardize_interacted_controls
)
CONFIG$export_crosswalk_maps <- read_bool_env(
  "EXPORT_CROSSWALK_MAPS", CONFIG$export_crosswalk_maps
)
CONFIG$run_crosswalk_sensitivity <- read_bool_env(
  "RUN_CROSSWALK_SENSITIVITY", CONFIG$run_crosswalk_sensitivity
)
CONFIG$save_single_rds <- read_bool_env(
  "SAVE_SINGLE_RDS", CONFIG$save_single_rds
)

if (isTRUE(CONFIG$run_crosswalk_sensitivity)) {
  message("Running the four-specification crosswalk sensitivity pipeline...")
  run_crosswalk_sensitivity_pipeline(CONFIG)
} else {
  run_single_pipeline(CONFIG)
}

message("Pipeline complete.")

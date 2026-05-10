# extension-adh2013/src/00_config.R

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install it before sourcing the pipeline config.")
}

REPLICATION_DIR <- here::here()
SRC_DIR <- here::here("src")
OUTPUT_DIR <- here::here("output")
INTERMEDIATE_DIR <- here::here("output", "intermediate")
TABLE_DIR <- here::here("output", "tables")
FIGURE_DIR <- here::here("output", "figures")
DIAGNOSTIC_DIR <- here::here("output", "diagnostics")

CONFIG <- list(
  replication_dir = REPLICATION_DIR,
  src_dir = SRC_DIR,
  output_dir = OUTPUT_DIR,
  intermediate_dir = INTERMEDIATE_DIR,
  table_dir = TABLE_DIR,
  figure_dir = FIGURE_DIR,
  diagnostic_dir = DIAGNOSTIC_DIR,

  first_election_year = 1972L,
  diagnostic_first_election_year = 1952L,
  last_election_year = 2020L,
  reference_year = 1988L,

  outcome_var = "rep_margin",
  outcome_label = "Republican presidential vote margin",
  exposure_var = "exposure",
  exposure_label = "ADH China import exposure, 1990-2007",
  exposure_units = "thousand dollars per worker",
  weight_var = "adh_weight",

  crosswalk_weight = "m5_weight",
  crosswalk_missing_weight_policy = "fallback_m2",
  renormalize_crosswalk_weights = FALSE,
  weight_sum_tolerance = 1e-6,
  min_retained_vote_share = 0.98,
  max_abs_vote_identity_error = 1e-6,
  max_missing_cz_year_share = 0,

  require_balanced_panel = TRUE,
  require_no_duplicate_county_years = TRUE,
  require_no_vcov_repair_for_main = TRUE,
  allow_vcov_repair = FALSE,
  allow_failed_diagnostics = FALSE,

  conley_cutoff_km = 500,
  conley_cutoffs_km = c(250, 500, 750, 1000),
  conley_distance = "spherical",
  nw_lag = 1L,
  dk_lag = 1L,
  main_se_type = "cluster_cz",

  standardize_interacted_controls = TRUE,
  export_bandwidth_diagnostic = TRUE,
  export_crosswalk_maps = TRUE,
  run_crosswalk_sensitivity = FALSE,
  run_sixth_extensions = TRUE,
  extension_crosswalk_weight = "m5_weight",
  extension_crosswalk_missing_weight_policy = "fallback_m2",
  save_single_rds = FALSE,
  rds_chunk_size_mib = 45,
  county1990_shapefile_path = Sys.getenv("COUNTY1990_SHAPEFILE", unset = file.path(REPLICATION_DIR, "spatial-data", "counties-1990")),
  nhgis_extract_dir = Sys.getenv("NHGIS_1990_COUNTY_EXTRACT_DIR", unset = file.path(REPLICATION_DIR, "spatial-data", "nhgis_1990_county"))
)

CONFIG$baseline_controls <- c(
  "l_shind_manuf_cbp",
  "l_sh_popedu_c",
  "l_sh_popfborn",
  "l_sh_empl_f",
  "l_sh_routine33",
  "l_task_outsource"
)

CONFIG$interacted_controls <- CONFIG$baseline_controls

CONFIG$core_interacted_controls <- c(
  "l_shind_manuf_cbp",
  "l_sh_popedu_c",
  "l_sh_popfborn"
)

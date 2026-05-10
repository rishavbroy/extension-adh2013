# extension-adh2013/src/01_helpers.R

required_packages <- function(config = NULL) {
  pkgs <- c(
    "dplyr", "fixest", "ggplot2", "haven", "jsonlite", "kableExtra", "knitr",
    "purrr", "readr", "readxl", "rlang", "stringr", "tibble", "tidyr"
  )
  if (!is.null(config) && isTRUE(config$export_crosswalk_maps)) pkgs <- c(pkgs, "sf")
  unique(pkgs)
}

optional_packages <- function() {
  c("ipumsr")
}

configured_shapefile_defaults <- function(config) {
  c(
    file.path(config$replication_dir, "spatial-data", "counties-1990"),
    file.path(config$replication_dir, "spatial-data", "nhgis_1990_county")
  )
}

load_required_packages <- function(config = NULL) {
  pkgs <- required_packages(config)
  missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      ". Install them before running the pipeline, or restore the project dependency manifest.",
      call. = FALSE
    )
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
  options(dplyr.summarise.inform = FALSE)
}

ensure_output_dirs <- function(config) {
  dirs <- c(config$output_dir, config$intermediate_dir, config$table_dir,
            config$figure_dir, config$diagnostic_dir)
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

weight_slug <- function(weight_col) {
  stringr::str_replace(weight_col, "_weight$", "")
}

finalize_config <- function(config) {
  allowed_weights <- c("m2_weight", "m4_weight", "m5_weight", "m6_weight")
  if (!config$crosswalk_weight %in% allowed_weights) {
    stop("CONFIG$crosswalk_weight must be one of: ", paste(allowed_weights, collapse = ", "))
  }

  allowed_policies <- c("fail", "fallback_m4", "fallback_m2", "identity_if_same_fips")
  if (!config$crosswalk_missing_weight_policy %in% allowed_policies) {
    stop(
      "CONFIG$crosswalk_missing_weight_policy must be one of: ",
      paste(allowed_policies, collapse = ", ")
    )
  }

  if (is.null(config$interacted_controls)) {
    config$interacted_controls <- config$baseline_controls
  }
  missing_interacted <- setdiff(config$interacted_controls, config$baseline_controls)
  if (length(missing_interacted) > 0) {
    stop("CONFIG$interacted_controls must be drawn from CONFIG$baseline_controls. Invalid: ",
         paste(missing_interacted, collapse = ", "))
  }

  slug <- weight_slug(config$crosswalk_weight)
  config$crosswalk_weight_slug <- slug
  config$event_years <- seq(config$first_election_year, config$last_election_year, by = 4)
  config$diagnostic_event_years <- seq(config$diagnostic_first_election_year, config$last_election_year, by = 4)
  config$source_decades <- sort(unique(year_to_source_decade(config$diagnostic_event_years)))
  if (is.null(config$main_se_type) || !nzchar(config$main_se_type)) {
    config$main_se_type <- paste0("conley_", config$conley_cutoff_km, "km")
  }

  # Treat common placeholder strings as unset. This avoids silently skipping maps
  # when COUNTY1990_SHAPEFILE was previously set to something like
  # "path/to/nhgis_1990_county.shp".
  if (is.null(config$county1990_shapefile_path) ||
      !nzchar(config$county1990_shapefile_path) ||
      grepl("^path/to", config$county1990_shapefile_path, ignore.case = TRUE)) {
    config$county1990_shapefile_path <- file.path(config$replication_dir, "spatial-data", "counties-1990")
  }
  if (is.null(config$nhgis_extract_dir) || !nzchar(config$nhgis_extract_dir)) {
    config$nhgis_extract_dir <- file.path(config$replication_dir, "spatial-data", "nhgis_1990_county")
  }

  config$analysis_panel_rds <- file.path(
    config$intermediate_dir, paste0("analysis_panel_aa2021_rep_margin_", slug, ".rds")
  )
  config$analysis_panel_full_rds <- file.path(
    config$intermediate_dir, paste0("analysis_panel_full_aa2021_rep_margin_", slug, ".rds")
  )
  config$diagnostic_panel_rds <- file.path(
    config$intermediate_dir, paste0("analysis_panel_aa2021_rep_margin_1952start_", slug, ".rds")
  )
  config$diagnostic_panel_full_rds <- file.path(
    config$intermediate_dir, paste0("analysis_panel_full_aa2021_rep_margin_1952start_", slug, ".rds")
  )
  config$cz_centroids_csv <- file.path(
    config$intermediate_dir, "cz_centroids_1990_population_weighted.csv"
  )
  config$event_study_rds <- file.path(
    config$intermediate_dir, paste0("event_study_results_aa2021_rep_margin_", slug, ".rds")
  )
  config$event_study_csv <- file.path(
    config$intermediate_dir, paste0("event_study_coefficients_aa2021_rep_margin_", slug, ".csv")
  )
  config$all_specs_rds <- file.path(
    config$intermediate_dir, paste0("event_study_results_all_specs_aa2021_rep_margin_", slug, ".rds")
  )
  config$all_specs_csv <- file.path(
    config$intermediate_dir, paste0("event_study_coefficients_all_specs_aa2021_rep_margin_", slug, ".csv")
  )

  config$dependency_manifest_json <- file.path(config$diagnostic_dir, "dependency_manifest.json")
  config$pipeline_manifest_json <- file.path(config$diagnostic_dir, "pipeline_manifest.json")
  config$output_manifest_csv <- file.path(config$diagnostic_dir, "output_manifest.csv")
  config$output_manifest_json <- file.path(config$diagnostic_dir, "output_manifest.json")
  config$validation_checks_csv <- file.path(config$diagnostic_dir, "pipeline_validation_checks.csv")
  config$crosswalk_diagnostics_csv <- file.path(
    config$diagnostic_dir, paste0("ftz_", slug, "_crosswalk_source_diagnostics.csv")
  )
  config$crosswalk_zero_support_csv <- file.path(
    config$diagnostic_dir, paste0("ftz_", slug, "_undefined_or_zero_support_counties.csv")
  )
  config$crosswalk_row_diagnostics_csv <- file.path(
    config$diagnostic_dir, paste0("ftz_", slug, "_crosswalk_row_diagnostics.csv")
  )
  config$crosswalk_issue_vote_report_csv <- file.path(
    config$diagnostic_dir, paste0("ftz_", slug, "_crosswalk_issue_vote_report.csv")
  )
  config$crosswalk_preflight_summary_csv <- file.path(
    config$diagnostic_dir, "ftz_m5_m6_zero_support_preflight_summary.csv"
  )
  config$crosswalk_preflight_counties_csv <- file.path(
    config$diagnostic_dir, "ftz_m5_m6_zero_support_preflight_counties.csv"
  )
  config$all_weight_support_csv <- file.path(
    config$diagnostic_dir, "ftz_all_weight_support_by_source_county.csv"
  )
  config$target_weight_support_county_csv <- file.path(
    config$diagnostic_dir, "ftz_weight_support_by_1990_county.csv"
  )
  config$target_weight_support_cz_csv <- file.path(
    config$diagnostic_dir, "ftz_weight_support_by_cz.csv"
  )
  config$county_bridge_classification_csv <- file.path(
    config$diagnostic_dir, paste0("unmatched_source_county_classification_", slug, ".csv")
  )
  config$mainland_retention_csv <- file.path(
    config$diagnostic_dir, paste0("county_bridge_retention_by_scope_", slug, ".csv")
  )
  config$baseline_control_summary_csv <- file.path(
    config$diagnostic_dir, "baseline_control_summary.csv"
  )
  config$baseline_control_correlation_csv <- file.path(
    config$diagnostic_dir, "baseline_control_correlation_matrix.csv"
  )
  config$interacted_control_variance_csv <- file.path(
    config$diagnostic_dir, "interacted_control_variance_diagnostics.csv"
  )
  config$conley_neighbor_counts_csv <- file.path(
    config$diagnostic_dir, "conley_neighbor_count_by_cutoff.csv"
  )
  config$vcov_rank_csv <- file.path(
    config$diagnostic_dir, "event_study_vcov_rank_by_se_type.csv"
  )
  config$se_warning_diagnostics_csv <- file.path(
    config$diagnostic_dir, "event_study_se_warning_diagnostics.csv"
  )
  config$crosswalk_comparison_csv <- file.path(
    config$diagnostic_dir, "crosswalk_specification_comparison_coefficients.csv"
  )
  config$crosswalk_comparison_png <- file.path(
    config$figure_dir, "fig_crosswalk_specification_comparison.png"
  )
  config$crosswalk_comparison_pdf <- file.path(
    config$figure_dir, "fig_crosswalk_specification_comparison.pdf"
  )
  config$crosswalk_comparison_delta_csv <- file.path(
    config$diagnostic_dir, "crosswalk_specification_comparison_deltas_vs_pure_m2.csv"
  )
  config$crosswalk_comparison_delta_png <- file.path(
    config$figure_dir, "fig_crosswalk_specification_comparison_deltas_vs_pure_m2.png"
  )
  config$crosswalk_comparison_delta_pdf <- file.path(
    config$figure_dir, "fig_crosswalk_specification_comparison_deltas_vs_pure_m2.pdf"
  )
  config$crosswalk_support_county_map_png <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_counties.png"
  )
  config$crosswalk_support_county_map_pdf <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_counties.pdf"
  )
  config$crosswalk_support_cz_map_png <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_czs.png"
  )
  config$crosswalk_support_cz_map_pdf <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_czs.pdf"
  )
  config$crosswalk_support_county_map_simplified_png <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_counties_simplified.png"
  )
  config$crosswalk_support_county_map_small_png <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_counties_small.png"
  )
  config$crosswalk_support_county_map_simplified_pdf <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_counties_simplified.pdf"
  )
  config$crosswalk_support_cz_map_simplified_png <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_czs_simplified.png"
  )
  config$crosswalk_support_cz_map_small_png <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_czs_small.png"
  )
  config$crosswalk_support_cz_map_simplified_pdf <- file.path(
    config$figure_dir, "fig_crosswalk_unavailable_weights_1990_czs_simplified.pdf"
  )
  config$outcome_duplicate_report_csv <- file.path(
    config$diagnostic_dir, "aa2021_presidential_duplicate_county_years.csv"
  )
  config$outcome_validation_csv <- file.path(
    config$diagnostic_dir, "aa2021_presidential_county_year_validation.csv"
  )
  config$national_vote_totals_csv <- file.path(
    config$diagnostic_dir, "aa2021_presidential_national_vote_totals.csv"
  )
  config$state_vote_totals_csv <- file.path(
    config$diagnostic_dir, "aa2021_presidential_state_vote_totals.csv"
  )
  config$bridge_diagnostics_csv <- file.path(
    config$diagnostic_dir, paste0("county_bridge_diagnostics_by_year_", slug, ".csv")
  )
  config$unmatched_source_counties_csv <- file.path(
    config$diagnostic_dir, paste0("unmatched_source_counties_", slug, ".csv")
  )
  config$missing_cz_years_csv <- file.path(
    config$diagnostic_dir, paste0("missing_cz_year_outcomes_", slug, ".csv")
  )
  config$panel_coverage_csv <- file.path(
    config$diagnostic_dir, paste0("panel_coverage_by_year_", slug, ".csv")
  )
  config$spec_status_csv <- file.path(config$diagnostic_dir, "event_study_spec_status.csv")
  config$model_diagnostics_csv <- file.path(config$diagnostic_dir, "event_study_model_diagnostics.csv")
  config$controls_manifest_csv <- file.path(config$diagnostic_dir, "event_study_controls_manifest.csv")
  config$vcov_diagnostics_csv <- file.path(config$diagnostic_dir, "event_study_vcov_diagnostics.csv")
  config$vcov_eigenvalues_csv <- file.path(config$diagnostic_dir, "event_study_vcov_eigenvalues.csv")
  config$pretrend_tests_csv <- file.path(config$diagnostic_dir, "event_study_pretrend_tests.csv")
  config$pretrend_coefficients_csv <- file.path(config$diagnostic_dir, "event_study_pretrend_coefficients.csv")

  config$table_tex <- file.path(config$table_dir, paste0("tab_event_study_rep_margin_main_specs_", slug, ".tex"))
  config$table_csv <- file.path(config$table_dir, paste0("tab_event_study_rep_margin_main_specs_", slug, ".csv"))
  config$table_pdf <- file.path(config$table_dir, paste0("tab_event_study_rep_margin_main_specs_", slug, ".pdf"))
  config$table_standalone_tex <- file.path(
    config$table_dir, paste0("tab_event_study_rep_margin_main_specs_", slug, "_standalone.tex")
  )
  config$figure_pdf <- file.path(config$figure_dir, paste0("fig_event_study_rep_margin_main_specs_", slug, ".pdf"))
  config$figure_png <- file.path(config$figure_dir, paste0("fig_event_study_rep_margin_main_specs_", slug, ".png"))

  # Sixth-section extension outputs. These are intentionally keyed to the
  # primary crosswalk slug when relevant, but are otherwise single-project
  # diagnostics that should not be re-estimated separately for every
  # crosswalk-sensitivity specification.
  config$bartik_first_stage_csv <- file.path(config$diagnostic_dir, "bartik_first_stage_diagnostics.csv")
  config$bartik_balance_csv <- file.path(config$diagnostic_dir, "bartik_balance_correlations.csv")
  config$bartik_pretrend_csv <- file.path(config$diagnostic_dir, "bartik_pretrend_placebos.csv")
  config$bartik_data_availability_csv <- file.path(config$diagnostic_dir, "bartik_rotemberg_data_availability.csv")
  config$bartik_identification_memo_md <- file.path(config$diagnostic_dir, "bartik_identification_memo.md")
  config$bartik_first_stage_plot_png <- file.path(config$figure_dir, "fig_bartik_first_stage.png")
  config$bartik_first_stage_plot_pdf <- file.path(config$figure_dir, "fig_bartik_first_stage.pdf")
  config$bartik_industry_shift_summary_csv <- file.path(config$diagnostic_dir, "bartik_industry_shift_summary.csv")
  config$bartik_preperiod_workfile_csv <- file.path(config$diagnostic_dir, "bartik_preperiod_workfile_diagnostics.csv")

  config$alternative_outcomes_coefficients_csv <- file.path(config$diagnostic_dir, "alternative_political_outcome_event_studies.csv")
  config$alternative_outcomes_summary_csv <- file.path(config$diagnostic_dir, "alternative_political_outcome_summary.csv")
  config$alternative_outcomes_plot_png <- file.path(config$figure_dir, "fig_alternative_political_outcomes.png")
  config$alternative_outcomes_plot_pdf <- file.path(config$figure_dir, "fig_alternative_political_outcomes.pdf")
  config$alternative_outcomes_status_csv <- file.path(config$diagnostic_dir, "alternative_political_outcome_status.csv")
  config$alternative_outcomes_decomposition_plot_png <- file.path(config$figure_dir, "fig_alternative_political_outcome_decomposition.png")
  config$alternative_outcomes_decomposition_plot_pdf <- file.path(config$figure_dir, "fig_alternative_political_outcome_decomposition.pdf")

  config$nanda_cz_panel_csv <- file.path(config$intermediate_dir, "nanda_2004_2022_cz_panel.csv")
  config$nanda_outcomes_coefficients_csv <- file.path(config$diagnostic_dir, "nanda_outcome_event_studies.csv")
  config$nanda_outcomes_summary_csv <- file.path(config$diagnostic_dir, "nanda_outcome_summary.csv")
  config$nanda_outcomes_plot_png <- file.path(config$figure_dir, "fig_nanda_outcome_event_studies.png")
  config$nanda_outcomes_plot_pdf <- file.path(config$figure_dir, "fig_nanda_outcome_event_studies.pdf")
  config$nanda_outcomes_status_csv <- file.path(config$diagnostic_dir, "nanda_outcome_event_study_status.csv")
  config$nanda_county_validation_csv <- file.path(config$diagnostic_dir, "nanda_county_year_validation.csv")
  config$nanda_cz_validation_csv <- file.path(config$diagnostic_dir, "nanda_cz_year_validation.csv")
  config$nanda_outlier_rates_csv <- file.path(config$diagnostic_dir, "nanda_outlier_rates.csv")

  config$adhm2020_summary_csv <- file.path(config$diagnostic_dir, "adhm2020_public_data_summary.csv")
  config$adhm2020_regressions_csv <- file.path(config$diagnostic_dir, "adhm2020_mechanism_regressions.csv")
  config$adhm2020_outcome_dictionary_csv <- file.path(config$diagnostic_dir, "adhm2020_outcome_dictionary.csv")
  config$adhm2020_status_csv <- file.path(config$diagnostic_dir, "adhm2020_mechanism_regression_status.csv")
  config$adhm2020_heterogeneity_availability_csv <- file.path(config$diagnostic_dir, "adhm2020_heterogeneity_data_availability.csv")
  config$adhm2020_plot_png <- file.path(config$figure_dir, "fig_adhm2020_mechanism_regressions.png")
  config$adhm2020_plot_pdf <- file.path(config$figure_dir, "fig_adhm2020_mechanism_regressions.pdf")

  config$subperiod_exposure_csv <- file.path(config$intermediate_dir, "adh_subperiod_exposure_by_cz.csv")
  config$subperiod_exposure_summary_csv <- file.path(config$diagnostic_dir, "adh_subperiod_exposure_summary.csv")
  config$subperiod_event_study_coefficients_csv <- file.path(config$diagnostic_dir, "subperiod_exposure_event_study_coefficients.csv")
  config$subperiod_event_study_status_csv <- file.path(config$diagnostic_dir, "subperiod_exposure_event_study_status.csv")
  config$subperiod_event_study_plot_png <- file.path(config$figure_dir, "fig_subperiod_exposure_event_study.png")
  config$subperiod_event_study_plot_pdf <- file.path(config$figure_dir, "fig_subperiod_exposure_event_study.pdf")
  config$subperiod_first_stage_csv <- file.path(config$diagnostic_dir, "subperiod_exposure_first_stage.csv")
  config$subperiod_equality_tests_csv <- file.path(config$diagnostic_dir, "subperiod_exposure_equality_tests.csv")

  config
}

fips_to_gisjoin <- function(fips) {
  fips <- stringr::str_pad(as.character(fips), width = 5, side = "left", pad = "0")
  paste0("G", substr(fips, 1, 2), "0", substr(fips, 3, 5), "0")
}

gisjoin_to_fips <- function(gisjoin) {
  gisjoin <- as.character(gisjoin)
  out <- ifelse(is.na(gisjoin), NA_character_, paste0(substr(gisjoin, 2, 3), substr(gisjoin, 5, 7)))
  stringr::str_pad(out, width = 5, side = "left", pad = "0")
}

year_to_source_decade <- function(year) {
  dplyr::case_when(
    year < 1980 ~ 1970L,
    year < 1990 ~ 1980L,
    year < 2000 ~ 1990L,
    year < 2010 ~ 2000L,
    year < 2020 ~ 2010L,
    TRUE ~ 2020L
  )
}

read_aa_rdata_object <- function(path, object_name = NULL) {
  e <- new.env(parent = emptyenv())
  loaded <- load(path, envir = e)
  if (!is.null(object_name)) return(e[[object_name]])
  candidates <- setdiff(loaded, ".Random.seed")
  if (length(candidates) != 1) stop("Expected exactly one non-.Random.seed object in: ", path)
  e[[candidates]]
}

sum_preserve_all_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

read_ftz_crosswalk <- function(source_decade, zip_path, county_fips_1990_identity = NULL,
                               weight_col = "m5_weight",
                               missing_weight_policy = "fail",
                               renormalize_weights = FALSE,
                               tolerance = 1e-8) {
  if (source_decade == 1990L) {
    ids <- unique(as.character(county_fips_1990_identity))
    return(tibble::tibble(
      source_decade = 1990L,
      gisjoin_source = fips_to_gisjoin(ids),
      gisjoin_1990 = fips_to_gisjoin(ids),
      county_fips_source = stringr::str_pad(ids, 5, pad = "0"),
      county_fips_1990 = stringr::str_pad(ids, 5, pad = "0"),
      raw_weight = 1,
      fallback_m4_weight = 1,
      fallback_m2_weight = 1,
      raw_weight_missing_parts = 0L,
      raw_weight_parts = 1L,
      weight_sum_raw = 1,
      selected_weight_sum_raw = 1,
      crosswalk_weight = 1,
      crosswalk_weight_sum = 1,
      raw_weight_undefined = FALSE,
      raw_weight_sum_valid = TRUE,
      raw_weight_requires_policy = FALSE,
      fallback_used = FALSE,
      fallback_policy = "identity_1990",
      weight_source = "identity_1990"
    ))
  }

  file_name <- paste0("Crosswalk_", source_decade, "_1990.csv")
  source_col <- paste0("gisjoin_", source_decade)
  raw <- readr::read_csv(unz(zip_path, file_name), show_col_types = FALSE)
  required <- c(source_col, "gisjoin_1990", weight_col, "m4_weight", "m2_weight")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Missing required FTZ columns in ", file_name, ": ", paste(missing, collapse = ", "))
  }

  parts <- raw %>%
    dplyr::transmute(
      source_decade = as.integer(source_decade),
      gisjoin_source = as.character(.data[[source_col]]),
      gisjoin_1990 = as.character(.data[["gisjoin_1990"]]),
      raw_weight_part = as.numeric(.data[[weight_col]]),
      fallback_m4_part = as.numeric(.data[["m4_weight"]]),
      fallback_m2_part = as.numeric(.data[["m2_weight"]])
    ) %>%
    dplyr::mutate(
      gisjoin_source = dplyr::if_else(
        source_decade == 2020L,
        fips_to_gisjoin(stringr::str_pad(gisjoin_source, 5, pad = "0")),
        gisjoin_source
      )
    )

  out <- parts %>%
    dplyr::group_by(source_decade, gisjoin_source, gisjoin_1990) %>%
    dplyr::summarise(
      raw_weight = sum_preserve_all_na(raw_weight_part),
      fallback_m4_weight = sum_preserve_all_na(fallback_m4_part),
      fallback_m2_weight = sum_preserve_all_na(fallback_m2_part),
      raw_weight_missing_parts = sum(is.na(raw_weight_part)),
      raw_weight_parts = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      county_fips_source = gisjoin_to_fips(gisjoin_source),
      county_fips_1990 = gisjoin_to_fips(gisjoin_1990)
    ) %>%
    dplyr::group_by(source_decade, gisjoin_source) %>%
    dplyr::mutate(
      weight_sum_raw = if (all(is.na(raw_weight))) NA_real_ else sum(raw_weight, na.rm = TRUE),
      raw_weight_undefined = is.na(weight_sum_raw) | weight_sum_raw <= tolerance,
      raw_weight_sum_valid = !is.na(weight_sum_raw) & abs(weight_sum_raw - 1) <= tolerance,
      raw_weight_requires_policy = !raw_weight_sum_valid
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      selected_raw_weight = dplyr::case_when(
        !raw_weight_requires_policy ~ raw_weight,
        missing_weight_policy == "fallback_m4" ~ fallback_m4_weight,
        missing_weight_policy == "fallback_m2" ~ fallback_m2_weight,
        missing_weight_policy == "identity_if_same_fips" & county_fips_source == county_fips_1990 ~ 1,
        missing_weight_policy == "identity_if_same_fips" ~ 0,
        TRUE ~ NA_real_
      ),
      fallback_used = raw_weight_requires_policy & missing_weight_policy != "fail",
      fallback_policy = dplyr::case_when(
        !fallback_used ~ "none",
        missing_weight_policy == "fallback_m4" ~ "fallback_m4",
        missing_weight_policy == "fallback_m2" ~ "fallback_m2",
        missing_weight_policy == "identity_if_same_fips" ~ "identity_if_same_fips",
        TRUE ~ missing_weight_policy
      ),
      weight_source = dplyr::case_when(
        !fallback_used ~ weight_col,
        fallback_policy == "fallback_m4" ~ "m4_weight",
        fallback_policy == "fallback_m2" ~ "m2_weight",
        fallback_policy == "identity_if_same_fips" ~ "identity_if_same_fips",
        TRUE ~ weight_col
      )
    ) %>%
    dplyr::group_by(source_decade, gisjoin_source) %>%
    dplyr::mutate(
      selected_weight_sum_raw = if (all(is.na(selected_raw_weight))) {
        NA_real_
      } else {
        sum(selected_raw_weight, na.rm = TRUE)
      },
      crosswalk_weight = dplyr::if_else(
        !is.na(selected_weight_sum_raw) & selected_weight_sum_raw > tolerance,
        if (renormalize_weights) selected_raw_weight / selected_weight_sum_raw else selected_raw_weight,
        NA_real_
      ),
      crosswalk_weight_sum = if (all(is.na(crosswalk_weight))) NA_real_ else sum(crosswalk_weight, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      source_decade, gisjoin_source, gisjoin_1990, county_fips_source, county_fips_1990,
      raw_weight, fallback_m4_weight, fallback_m2_weight, raw_weight_missing_parts,
      raw_weight_parts, weight_sum_raw, selected_weight_sum_raw, crosswalk_weight,
      crosswalk_weight_sum, raw_weight_undefined, raw_weight_sum_valid,
      raw_weight_requires_policy, fallback_used, fallback_policy, weight_source
    )

  out
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}

star_code <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

make_interacted_control_vars <- function(data, controls, years, ref_year) {
  years_est <- setdiff(sort(unique(years)), ref_year)
  new_names <- character(0)
  for (ctrl in controls) {
    for (yr in years_est) {
      nm <- paste0("ctrl_", ctrl, "_", yr)
      data[[nm]] <- data[[ctrl]] * as.integer(data$year == yr)
      new_names <- c(new_names, nm)
    }
  }
  list(data = data, vars = new_names)
}

validation_check <- function(name, passed, fatal = TRUE, n_failed = NA_integer_, details = "") {
  tibble::tibble(
    check = name,
    status = if (isTRUE(passed)) "pass" else "fail",
    fatal = isTRUE(fatal),
    n_failed = n_failed,
    details = as.character(details)
  )
}

reported_check <- function(name, n_reported = 0L, details = "") {
  tibble::tibble(
    check = name,
    status = if (is.na(n_reported) || n_reported == 0L) "pass" else "reported",
    fatal = FALSE,
    n_failed = 0L,
    n_reported = as.integer(dplyr::coalesce(as.integer(n_reported), 0L)),
    details = as.character(details)
  )
}

has_fatal_failures <- function(checks) {
  if (nrow(checks) == 0 || !("status" %in% names(checks))) return(FALSE)
  any(checks$fatal %in% TRUE & checks$status == "fail", na.rm = TRUE)
}

has_any_failures <- function(checks) {
  if (nrow(checks) == 0 || !("status" %in% names(checks))) return(FALSE)
  any(checks$status == "fail", na.rm = TRUE)
}

has_reported_conditions <- function(checks) {
  if (nrow(checks) == 0 || !("status" %in% names(checks))) return(FALSE)
  any(checks$status == "reported", na.rm = TRUE)
}

write_dependency_manifest <- function(config) {
  core <- required_packages(config)
  optional <- optional_packages()
  pkgs <- unique(c(core, optional))
  dep <- tibble::tibble(
    package = pkgs,
    dependency_type = dplyr::case_when(
      package %in% core ~ "core",
      package %in% optional ~ "optional",
      TRUE ~ "other"
    ),
    installed = vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE),
    version = vapply(pkgs, function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) return(NA_character_)
      as.character(utils::packageVersion(pkg))
    }, character(1))
  )
  jsonlite::write_json(
    list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      r_version = R.version.string,
      packages = dep
    ),
    config$dependency_manifest_json,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  invisible(dep)
}

file_checksum <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

portable_path <- function(path, config = CONFIG) {
  if (is.null(path) || length(path) == 0 || is.na(path)) return(path)
  path_chr <- as.character(path)
  root <- normalizePath(config$replication_dir, winslash = "/", mustWork = FALSE)
  norm <- normalizePath(path_chr, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  dplyr::case_when(
    norm == root ~ ".",
    startsWith(norm, prefix) ~ substring(norm, nchar(prefix) + 1L),
    TRUE ~ path_chr
  )
}

sanitize_manifest_paths <- function(x, config = CONFIG) {
  if (is.list(x)) {
    nms <- names(x)
    out <- lapply(x, sanitize_manifest_paths, config = config)
    names(out) <- nms
    return(out)
  }
  if (is.character(x) && length(x) == 1 && (grepl("/", x) || grepl("\\\\", x))) {
    return(portable_path(x, config))
  }
  x
}

chunk_dir_for_rds <- function(path) {
  file.path(dirname(path), paste0(tools::file_path_sans_ext(basename(path)), "_rds_chunks"))
}

write_chunked_rds <- function(object, path, config = CONFIG) {
  raw <- serialize(object, connection = NULL, xdr = TRUE)
  compressed <- memCompress(raw, type = "xz")
  chunk_size <- as.integer((config$rds_chunk_size_mib %||% 45) * 1024^2)
  if (!is.finite(chunk_size) || chunk_size <= 0) chunk_size <- 45L * 1024L^2L

  chunk_dir <- chunk_dir_for_rds(path)
  if (dir.exists(chunk_dir)) unlink(chunk_dir, recursive = TRUE, force = TRUE)
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)

  n <- length(compressed)
  n_chunks <- max(1L, ceiling(n / chunk_size))
  chunk_files <- character(n_chunks)
  for (i in seq_len(n_chunks)) {
    start <- (i - 1L) * chunk_size + 1L
    end <- min(i * chunk_size, n)
    chunk_files[[i]] <- file.path(chunk_dir, sprintf("part-%03d.rdsbin", i))
    writeBin(compressed[start:end], chunk_files[[i]], useBytes = TRUE)
  }

  manifest <- list(
    format = "serialized-r-object-xdr-memCompress-xz-chunked",
    original_path = portable_path(path, config),
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    chunk_size_mib = config$rds_chunk_size_mib %||% 45,
    n_chunks = n_chunks,
    uncompressed_bytes = length(raw),
    compressed_bytes = length(compressed),
    chunks = lapply(chunk_files, function(cf) list(
      path = portable_path(cf, config),
      bytes = file.info(cf)$size,
      md5 = file_checksum(cf)
    ))
  )
  jsonlite::write_json(manifest, file.path(chunk_dir, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
  if (file.exists(path)) unlink(path)
  invisible(list(path = path, chunk_dir = chunk_dir, manifest = manifest))
}

read_chunked_rds <- function(path) {
  chunk_dir <- chunk_dir_for_rds(path)
  manifest_path <- file.path(chunk_dir, "manifest.json")
  if (!file.exists(manifest_path)) stop("Chunked RDS manifest not found: ", manifest_path, call. = FALSE)
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  chunk_paths <- file.path(chunk_dir, basename(manifest$chunks$path))
  if (!all(file.exists(chunk_paths))) {
    # Fall back to paths relative to the project root when the manifest was moved.
    root <- dirname(dirname(dirname(path)))
    chunk_paths <- file.path(root, manifest$chunks$path)
  }
  if (!all(file.exists(chunk_paths))) {
    missing <- chunk_paths[!file.exists(chunk_paths)]
    stop("Missing chunked RDS files: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  compressed <- unlist(Map(function(pth, nbytes) {
    readBin(pth, what = "raw", n = nbytes)
  }, chunk_paths, file.info(chunk_paths)$size), use.names = FALSE)
  unserialize(memDecompress(compressed, type = "xz"))
}

write_model_object <- function(object, path, config = CONFIG) {
  if (isTRUE(config$save_single_rds)) {
    saveRDS(object, path, compress = "xz")
    chunk_dir <- chunk_dir_for_rds(path)
    if (dir.exists(chunk_dir)) unlink(chunk_dir, recursive = TRUE, force = TRUE)
    invisible(list(path = path, mode = "single_rds"))
  } else {
    out <- write_chunked_rds(object, path, config)
    invisible(c(out, list(mode = "chunked_rds")))
  }
}

read_model_object <- function(path) {
  if (file.exists(path)) return(readRDS(path))
  read_chunked_rds(path)
}

model_object_reference <- function(path, config = CONFIG) {
  if (file.exists(path)) {
    return(list(path = path, md5 = file_checksum(path), storage = "single_rds"))
  }
  chunk_dir <- chunk_dir_for_rds(path)
  manifest_path <- file.path(chunk_dir, "manifest.json")
  if (file.exists(manifest_path)) {
    return(list(
      path = path,
      chunk_manifest = manifest_path,
      chunk_dir = chunk_dir,
      manifest_md5 = file_checksum(manifest_path),
      storage = "chunked_rds"
    ))
  }
  list(path = path, md5 = NA_character_, storage = "missing")
}


output_file_category <- function(path, config = CONFIG) {
  rel <- portable_path(path, config)
  dplyr::case_when(
    startsWith(rel, "output/diagnostics/") ~ "diagnostics",
    startsWith(rel, "output/intermediate/") ~ "intermediate",
    startsWith(rel, "output/tables/") ~ "tables",
    startsWith(rel, "output/figures/") ~ "figures",
    TRUE ~ "other"
  )
}

infer_crosswalk_slug_from_path <- function(path) {
  base <- basename(path)
  hit <- stringr::str_match(base, "_(m2|m4|m5|m6)(\\.|_|$)")[, 2]
  ifelse(is.na(hit), NA_character_, hit)
}

write_output_manifest <- function(config = CONFIG) {
  config <- finalize_config(config)
  if (!dir.exists(config$output_dir)) {
    empty <- tibble::tibble(
      path = character(), category = character(), extension = character(), bytes = numeric(),
      md5 = character(), crosswalk_slug = character(), modified_time = character()
    )
    readr::write_csv(empty, config$output_manifest_csv)
    jsonlite::write_json(list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), files = empty),
                         config$output_manifest_json, pretty = TRUE, auto_unbox = TRUE)
    return(invisible(empty))
  }
  files <- list.files(config$output_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  # Avoid self-recursive checksum churn by writing the manifest after listing but
  # excluding the previous copy of itself from checksum-based reproducibility checks.
  info <- file.info(files)
  out <- tibble::tibble(
    path = vapply(files, portable_path, character(1), config = config),
    category = vapply(files, output_file_category, character(1), config = config),
    extension = tolower(tools::file_ext(files)),
    bytes = as.numeric(info$size),
    md5 = vapply(files, file_checksum, character(1)),
    crosswalk_slug = vapply(files, infer_crosswalk_slug_from_path, character(1)),
    modified_time = format(info$mtime, "%Y-%m-%d %H:%M:%S %Z")
  ) %>%
    dplyr::arrange(category, path)
  readr::write_csv(out, config$output_manifest_csv)
  jsonlite::write_json(
    list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      n_files = nrow(out),
      total_bytes = sum(out$bytes, na.rm = TRUE),
      files = out
    ),
    config$output_manifest_json,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  invisible(out)
}

write_pipeline_manifest <- function(config, checks, stage, sources = list(), extra = list()) {
  sources <- sanitize_manifest_paths(sources, config)
  extra <- sanitize_manifest_paths(extra, config)
  manifest <- list(
    stage = stage,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    config = config[c(
      "first_election_year", "diagnostic_first_election_year", "last_election_year",
      "reference_year", "crosswalk_weight", "crosswalk_missing_weight_policy",
      "renormalize_crosswalk_weights", "weight_sum_tolerance", "min_retained_vote_share",
      "require_balanced_panel", "require_no_duplicate_county_years",
      "require_no_vcov_repair_for_main", "allow_vcov_repair", "allow_failed_diagnostics",
      "max_abs_vote_identity_error", "max_missing_cz_year_share",
      "conley_cutoffs_km", "main_se_type", "nw_lag", "dk_lag", "interacted_controls",
      "standardize_interacted_controls", "export_crosswalk_maps", "run_crosswalk_sensitivity",
      "save_single_rds", "rds_chunk_size_mib", "county1990_shapefile_path",
      "nhgis_extract_dir", "output_manifest_csv", "output_manifest_json"
    )],
    sources = sources,
    checks = checks,
    fatal_failure = has_fatal_failures(checks),
    extra = extra
  )
  manifest <- sanitize_manifest_paths(manifest, config)
  jsonlite::write_json(manifest, config$pipeline_manifest_json, pretty = TRUE, auto_unbox = TRUE, null = "null")
  # Keep the standalone output inventory current after each manifest write.
  tryCatch(write_output_manifest(config), error = function(e) warning(
    "Could not update output manifest after writing pipeline manifest: ",
    conditionMessage(e), call. = FALSE
  ))
  invisible(manifest)
}


`%||%` <- function(x, y) if (is.null(x)) y else x

standardized_control_name <- function(ctrl) paste0("z_", ctrl)

standardize_baseline_controls <- function(data, controls) {
  out <- data
  summary <- purrr::map_dfr(controls, function(ctrl) {
    vals <- out[[ctrl]]
    mu <- mean(vals, na.rm = TRUE)
    sig <- stats::sd(vals, na.rm = TRUE)
    zname <- standardized_control_name(ctrl)
    out[[zname]] <<- if (is.finite(sig) && sig > 0) (vals - mu) / sig else NA_real_
    tibble::tibble(
      control = ctrl,
      standardized_control = zname,
      n_nonmissing = sum(!is.na(vals)),
      mean = mu,
      sd = sig,
      min = suppressWarnings(min(vals, na.rm = TRUE)),
      max = suppressWarnings(max(vals, na.rm = TRUE))
    )
  })
  list(data = out, summary = summary)
}


read_county_bridge_exceptions <- function(config) {
  empty <- tibble::tibble(
    year = integer(), county_fips = character(), county_name_pattern = character(),
    override_source_decade = integer(), override_county_fips_source = character(),
    issue_class = character(), reason = character(), apply = logical()
  )
  path <- file.path(config$src_dir, "county_bridge_exceptions.csv")
  if (!file.exists(path)) return(empty)
  out <- readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      year = as.integer(year),
      county_fips = stringr::str_pad(as.character(county_fips), 5, pad = "0"),
      override_county_fips_source = stringr::str_pad(as.character(override_county_fips_source), 5, pad = "0"),
      override_source_decade = as.integer(override_source_decade),
      apply = as.logical(apply)
    ) %>%
    dplyr::filter(apply %in% TRUE)
  dplyr::bind_rows(empty, out)
}

classify_unmatched_county <- function(state_fips, county_fips, county_name, year) {
  state_fips <- stringr::str_pad(as.character(state_fips), 2, pad = "0")
  county_fips <- stringr::str_pad(as.character(county_fips), 5, pad = "0")
  nm <- tolower(as.character(county_name))
  dplyr::case_when(
    state_fips %in% c("02", "15", "72") ~ "excluded_nonmainland",
    county_fips %in% c("04012", "35006", "08014", "46113", "46102", "51683", "51685", "51735") ~ "county_boundary_change_needs_bridge",
    state_fips == "51" & stringr::str_detect(nm, "south boston|clifton forge|bedford|manassas|poquoson") ~ "county_boundary_change_needs_bridge",
    year < 1972L ~ "pre_1972_diagnostic_only",
    TRUE ~ "unexpected_unmatched"
  )
}

is_adh_mainland_source <- function(state_fips) {
  !stringr::str_pad(as.character(state_fips), 2, pad = "0") %in% c("02", "15", "72")
}

find_first_shapefile <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NA_character_)
  if (file.exists(path) && grepl("\\.shp$", path, ignore.case = TRUE)) return(path)
  if (dir.exists(path)) {
    shps <- list.files(path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(shps) > 0) return(shps[[1]])
  }
  NA_character_
}

infer_county_fips_from_sf <- function(sf_obj) {
  nm <- names(sf_obj)
  if ("county_fips_1990" %in% nm) return(stringr::str_pad(as.character(sf_obj$county_fips_1990), 5, pad = "0"))
  if ("FIPS" %in% nm) return(stringr::str_pad(as.character(sf_obj$FIPS), 5, pad = "0"))
  if ("GEOID" %in% nm) return(stringr::str_pad(as.character(sf_obj$GEOID), 5, pad = "0"))
  if ("GISJOIN" %in% nm) return(gisjoin_to_fips(sf_obj$GISJOIN))
  if (all(c("STATEFP", "COUNTYFP") %in% nm)) return(paste0(stringr::str_pad(sf_obj$STATEFP, 2, pad = "0"), stringr::str_pad(sf_obj$COUNTYFP, 3, pad = "0")))
  if (all(c("STATEA", "COUNTYA") %in% nm)) return(paste0(stringr::str_pad(sf_obj$STATEA, 2, pad = "0"), stringr::str_pad(sf_obj$COUNTYA, 3, pad = "0")))
  if (all(c("STATE_FIPS", "FIPS") %in% nm)) return(stringr::str_pad(as.character(sf_obj$FIPS), 5, pad = "0"))
  stop("Could not infer county FIPS from shapefile. Expected county_fips_1990, FIPS, GEOID, GISJOIN, STATEFP+COUNTYFP, STATEA+COUNTYA, or STATE_FIPS+FIPS.")
}

safe_read_county1990_shapefile <- function(config) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("Package 'sf' is not installed; skipping crosswalk support maps.", call. = FALSE)
    return(NULL)
  }

  candidate_paths <- unique(c(
    config$county1990_shapefile_path,
    config$nhgis_extract_dir,
    configured_shapefile_defaults(config)
  ))
  candidate_paths <- candidate_paths[!is.na(candidate_paths) & nzchar(candidate_paths)]
  candidate_paths <- candidate_paths[!grepl("^path/to", candidate_paths, ignore.case = TRUE)]

  shp <- NA_character_
  for (candidate in candidate_paths) {
    shp <- find_first_shapefile(candidate)
    if (!is.na(shp)) break
  }

  if (is.na(shp)) {
    warning(
      "No 1990 county shapefile found. Put co1990p020.shp (and companion files) under ",
      file.path(config$replication_dir, "spatial-data", "counties-1990"),
      ", or set COUNTY1990_SHAPEFILE to a 1990 county .shp file or folder. ",
      "Skipping map rendering but writing non-spatial diagnostics.",
      call. = FALSE
    )
    return(NULL)
  }

  sf_obj <- sf::st_read(shp, quiet = TRUE)
  sf_obj$county_fips_1990 <- infer_county_fips_from_sf(sf_obj)
  sf_obj <- sf_obj %>%
    dplyr::filter(!is.na(county_fips_1990))

  # Many historical county shapefiles store multipart counties/islands as multiple
  # rows with the same FIPS. Dissolve before any diagnostic joins so maps do not
  # trigger unexpected many-to-many warnings or over-represent multipart counties.
  sf_obj <- sf_obj %>%
    dplyr::group_by(county_fips_1990) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

  attr(sf_obj, "source_shapefile") <- shp
  sf_obj
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlon <- (lon2 - lon1) * rad
  dlat <- (lat2 - lat1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

summarise_conley_neighbors <- function(panel, cutoffs_km) {
  coords <- panel %>%
    dplyr::distinct(czone, lon, lat) %>%
    dplyr::filter(is.finite(lon), is.finite(lat))
  if (nrow(coords) == 0) return(tibble::tibble())
  dist_mat <- outer(seq_len(nrow(coords)), seq_len(nrow(coords)), Vectorize(function(i, j) {
    haversine_km(coords$lon[[i]], coords$lat[[i]], coords$lon[[j]], coords$lat[[j]])
  }))
  purrr::map_dfr(cutoffs_km, function(cutoff) {
    counts <- rowSums(dist_mat <= cutoff, na.rm = TRUE) - 1L
    tibble::tibble(
      cutoff_km = cutoff,
      n_cz = nrow(coords),
      min_neighbors = min(counts),
      p25_neighbors = as.numeric(stats::quantile(counts, 0.25)),
      median_neighbors = stats::median(counts),
      mean_neighbors = mean(counts),
      p75_neighbors = as.numeric(stats::quantile(counts, 0.75)),
      max_neighbors = max(counts),
      cz_with_zero_neighbors = sum(counts == 0)
    )
  })
}

render_table_pdf <- function(fragment_tex, standalone_tex, output_pdf) {
  input_name <- basename(fragment_tex)
  lines <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[letterpaper,landscape,margin=0.7in]{geometry}",
    "\\usepackage{array}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{threeparttablex}",
    "\\usepackage{caption}",
    "\\renewcommand{\\arraystretch}{1.08}",
    "\\newcolumntype{L}[1]{>{\\raggedright\\arraybackslash}p{#1}}",
    "\\newcolumntype{C}[1]{>{\\centering\\arraybackslash}p{#1}}",
    "\\begin{document}",
    paste0("\\input{", input_name, "}"),
    "\\end{document}"
  )
  writeLines(lines, standalone_tex)

  tex_engine <- Sys.which("pdflatex")
  if (!nzchar(tex_engine)) {
    warning("No LaTeX engine found (pdflatex not on PATH). Wrote TeX sources but did not render a PDF.")
    return(invisible(NULL))
  }

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(dirname(standalone_tex))

  out1 <- system2(tex_engine, c("-interaction=nonstopmode", basename(standalone_tex)), stdout = TRUE, stderr = TRUE)
  out2 <- system2(tex_engine, c("-interaction=nonstopmode", basename(standalone_tex)), stdout = TRUE, stderr = TRUE)
  pdf_path <- file.path(dirname(standalone_tex), paste0(tools::file_path_sans_ext(basename(standalone_tex)), ".pdf"))

  if (!file.exists(pdf_path)) {
    warning("pdflatex ran but no PDF was produced. Last output:\n", paste(c(out1, out2), collapse = "\n"))
    return(invisible(NULL))
  }

  file.copy(pdf_path, output_pdf, overwrite = TRUE)

  sidecars <- file.path(
    dirname(standalone_tex),
    paste0(tools::file_path_sans_ext(basename(standalone_tex)), c(".aux", ".log", ".out", ".toc"))
  )
  unlink(sidecars[file.exists(sidecars)])

  invisible(output_pdf)
}

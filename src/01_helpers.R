# replication/src/01_helpers.R

required_packages <- function() {
  c(
    "dplyr", "fixest", "ggplot2", "haven", "jsonlite", "kableExtra", "knitr",
    "purrr", "readr", "readxl", "rlang", "stringr", "tibble", "tidyr"
  )
}

load_required_packages <- function() {
  pkgs <- required_packages()
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
  allowed_weights <- c("m5_weight", "m6_weight")
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

  config$table_tex <- file.path(config$table_dir, paste0("tab_event_study_rep_margin_two_specs_", slug, ".tex"))
  config$table_csv <- file.path(config$table_dir, paste0("tab_event_study_rep_margin_two_specs_", slug, ".csv"))
  config$table_pdf <- file.path(config$table_dir, paste0("tab_event_study_rep_margin_two_specs_", slug, ".pdf"))
  config$table_standalone_tex <- file.path(
    config$table_dir, paste0("tab_event_study_rep_margin_two_specs_", slug, "_standalone.tex")
  )
  config$figure_pdf <- file.path(config$figure_dir, paste0("fig_event_study_rep_margin_two_specs_", slug, ".pdf"))
  config$figure_png <- file.path(config$figure_dir, paste0("fig_event_study_rep_margin_two_specs_", slug, ".png"))
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

has_fatal_failures <- function(checks) {
  any(checks$fatal & checks$status == "fail")
}

has_any_failures <- function(checks) {
  any(checks$status == "fail")
}

write_dependency_manifest <- function(config) {
  pkgs <- required_packages()
  dep <- tibble::tibble(
    package = pkgs,
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

write_pipeline_manifest <- function(config, checks, stage, sources = list(), extra = list()) {
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
      "conley_cutoffs_km", "main_se_type", "nw_lag", "dk_lag", "interacted_controls"
    )],
    sources = sources,
    checks = checks,
    fatal_failure = has_fatal_failures(checks),
    extra = extra
  )
  jsonlite::write_json(manifest, config$pipeline_manifest_json, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(manifest)
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

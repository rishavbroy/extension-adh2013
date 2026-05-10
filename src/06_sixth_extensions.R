# extension-adh2013/src/06_sixth_extensions.R

# These modules implement the highest-priority Sixth-section extensions. They are
# intentionally run once for the primary M5 + fallback-M2 design, even when the
# appendix crosswalk-sensitivity wrapper is active. That keeps mechanism and
# identification diagnostics focused on the preferred estimating sample rather
# than multiplying exploratory outputs by crosswalk specification.

extension_primary_config <- function(config = CONFIG) {
  out <- config
  out$crosswalk_weight <- config$extension_crosswalk_weight %||% "m5_weight"
  out$crosswalk_missing_weight_policy <- config$extension_crosswalk_missing_weight_policy %||% "fallback_m2"
  out <- finalize_config(out)

  # In ordinary project runs this points to the preferred M5 + fallback-M2
  # panel. If a user intentionally runs only another crosswalk specification
  # and has not generated the preferred panel, fall back to the current run so
  # the extension modules still produce diagnostics rather than silently doing
  # nothing. Crosswalk-sensitivity runs generate M5 first, so final appendix runs
  # still use the preferred panel.
  current <- finalize_config(config)
  if (!file.exists(out$analysis_panel_rds) && file.exists(current$analysis_panel_rds)) {
    out <- current
  }
  out
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

scalar_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x[[1]])
}

weighted_mean_safe <- function(x, w = NULL) {
  x <- safe_num(x)
  if (is.null(w)) w <- rep(1, length(x)) else w <- safe_num(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

first_existing_col <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

read_first_rdata_frame <- function(path) {
  e <- new.env(parent = emptyenv())
  loaded <- load(path, envir = e)
  candidates <- loaded[vapply(loaded, function(nm) is.data.frame(e[[nm]]), logical(1))]
  if (length(candidates) == 0) stop("No data.frame object found in ", path)
  e[[candidates[[1]]]]
}

load_adh_extension_data <- function(config = CONFIG) {
  adh_stack_path <- file.path(config$replication_dir, "adh2013", "dta", "workfile_china.dta")
  adh_long_path <- file.path(config$replication_dir, "adh2013", "dta", "workfile_china_long.dta")
  stack <- haven::read_dta(adh_stack_path)
  long <- haven::read_dta(adh_long_path)

  baseline <- stack %>%
    dplyr::filter(yr == 1990) %>%
    dplyr::transmute(
      czone = as.integer(czone),
      statefip = as.integer(statefip),
      dplyr::across(dplyr::all_of(config$baseline_controls), safe_num)
    ) %>%
    dplyr::distinct()

  exposure <- long %>%
    dplyr::transmute(
      czone = as.integer(czone),
      statefip = as.integer(statefip),
      exposure_1990_2007 = safe_num(d_tradeusch_pw),
      instrument_1990_2007 = safe_num(d_tradeotch_pw_lag),
      adh_weight = safe_num(timepwt48)
    ) %>%
    dplyr::distinct()

  baseline %>%
    dplyr::inner_join(exposure, by = c("czone", "statefip")) %>%
    standardize_baseline_controls(config$baseline_controls) %>%
    purrr::pluck("data")
}

coeftable_to_tibble <- function(model, vcov = NULL, term_prefix = NULL) {
  ct <- tryCatch({
    if (is.null(vcov)) fixest::coeftable(model) else fixest::coeftable(model, vcov = vcov)
  }, error = function(e) NULL)
  if (is.null(ct)) return(tibble::tibble())
  out <- as.data.frame(ct) %>%
    tibble::rownames_to_column("term") %>%
    tibble::as_tibble()
  names(out) <- c("term", "estimate", "std.error", "statistic", "p.value")
  if (!is.null(term_prefix)) out <- out %>% dplyr::filter(startsWith(term, term_prefix))
  out
}

fit_weighted_feols <- function(formula, data, weight_var = "adh_weight", cluster_var = "czone") {
  weights_fml <- if (!is.null(weight_var) && weight_var %in% names(data)) stats::as.formula(paste0("~", weight_var)) else NULL
  cluster_fml <- if (!is.null(cluster_var) && cluster_var %in% names(data)) stats::as.formula(paste0("~", cluster_var)) else NULL
  fixest::feols(formula, data = data, weights = weights_fml, cluster = cluster_fml, warn = FALSE, notes = FALSE)
}

make_single_exposure_event_terms <- function(data, exposure_var, years, ref_year, prefix = "es") {
  years <- sort(unique(years))
  for (yr in setdiff(years, ref_year)) {
    nm <- paste0(prefix, "_", yr)
    data[[nm]] <- data[[exposure_var]] * as.integer(data$year == yr)
  }
  data
}

fit_single_outcome_event_study <- function(panel, outcome_var, controls, spec_name, config, ref_year = config$reference_year,
                                           standardize_controls = TRUE, sample_label = "main_1972_start") {
  yrs <- sort(unique(panel$year[!is.na(panel[[outcome_var]])]))
  if (!ref_year %in% yrs) ref_year <- min(yrs, na.rm = TRUE)
  dat <- panel %>% dplyr::filter(year %in% yrs, !is.na(.data[[outcome_var]]), !is.na(exposure))
  if (nrow(dat) == 0) return(list(status = "empty", coefficients = tibble::tibble()))
  dat <- make_single_exposure_event_terms(dat, "exposure", yrs, ref_year, prefix = "es")
  event_terms <- paste0("es_", setdiff(yrs, ref_year))
  controls_use <- model_control_names(controls, standardize = standardize_controls)
  controls_use <- controls_use[controls_use %in% names(dat)]
  if (length(controls_use) > 0) dat <- make_interacted_control_vars(dat, controls_use, yrs, ref_year)
  ctrl_terms <- unlist(lapply(controls_use, function(ctrl) paste0("ctrl_", ctrl, "_", setdiff(yrs, ref_year))))
  rhs <- c(event_terms, ctrl_terms)
  if (length(rhs) == 0) return(list(status = "no_terms", coefficients = tibble::tibble()))
  fml <- stats::as.formula(paste(outcome_var, "~", paste(rhs, collapse = " + "), "| czone + year"))
  res <- tryCatch({
    model <- fixest::feols(fml, data = dat, weights = ~adh_weight, cluster = ~czone, panel.id = ~czone + election_index, warn = FALSE, notes = FALSE)
    coef <- coeftable_to_tibble(model) %>%
      dplyr::filter(term %in% event_terms) %>%
      dplyr::mutate(
        outcome = outcome_var,
        spec = spec_name,
        sample = sample_label,
        reference_year = ref_year,
        year = as.integer(stringr::str_extract(term, "[0-9]{4}")),
        n_obs = stats::nobs(model),
        n_cz = dplyr::n_distinct(dat$czone),
        se_type = "cluster_cz"
      ) %>%
      dplyr::select(outcome, spec, sample, se_type, term, year, reference_year, estimate, std.error, statistic, p.value, n_obs, n_cz)
    list(status = "ok", coefficients = coef)
  }, error = function(e) {
    list(status = paste0("error: ", conditionMessage(e)), coefficients = tibble::tibble())
  })
  res
}

write_plot_safely <- function(plot, png_path, pdf_path, width = 10, height = 6) {
  try(ggplot2::ggsave(png_path, plot, width = width, height = height, dpi = 300, bg = "white"), silent = TRUE)
  try(ggplot2::ggsave(pdf_path, plot, width = width, height = height, bg = "white"), silent = TRUE)
  invisible(NULL)
}

run_bartik_identification_diagnostics <- function(config = CONFIG) {
  config <- finalize_config(config)
  adh <- load_adh_extension_data(config)

  controls_core <- model_control_names(config$core_interacted_controls, standardize = TRUE)
  controls_full <- model_control_names(config$interacted_controls, standardize = TRUE)
  controls_core <- controls_core[controls_core %in% names(adh)]
  controls_full <- controls_full[controls_full %in% names(adh)]

  fs_specs <- list(
    minimal = character(0),
    core_controls = controls_core,
    full_controls = controls_full
  )
  fs_tbl <- purrr::imap_dfr(fs_specs, function(ctrls, spec) {
    rhs <- c("instrument_1990_2007", ctrls)
    fml <- stats::as.formula(paste("exposure_1990_2007 ~", paste(rhs, collapse = " + ")))
    model <- fit_weighted_feols(fml, adh, weight_var = "adh_weight", cluster_var = "statefip")
    ct <- coeftable_to_tibble(model) %>% dplyr::filter(term == "instrument_1990_2007")
    tibble::tibble(
      spec = spec,
      endogenous_variable = "ADH exposure, 1990-2007",
      instrument = "ADH other-high-income-country import growth instrument",
      estimate = scalar_or_na(ct$estimate),
      std.error = scalar_or_na(ct$std.error),
      statistic = scalar_or_na(ct$statistic),
      p.value = scalar_or_na(ct$p.value),
      first_stage_F_t2 = (scalar_or_na(ct$statistic))^2,
      n_obs = stats::nobs(model),
      r2 = tryCatch(fixest::r2(model, type = "r2"), error = function(e) NA_real_),
      controls = if (length(ctrls) == 0) "none" else paste(ctrls, collapse = "; ")
    )
  })
  readr::write_csv(fs_tbl, config$bartik_first_stage_csv)

  balance_vars <- unique(c(config$baseline_controls, controls_full, "statefip"))
  balance_vars <- balance_vars[balance_vars %in% names(adh)]
  balance_tbl <- purrr::map_dfr(balance_vars, function(v) {
    x <- adh[[v]]
    tibble::tibble(
      variable = v,
      correlation_with_exposure = suppressWarnings(stats::cor(x, adh$exposure_1990_2007, use = "pairwise.complete.obs")),
      correlation_with_instrument = suppressWarnings(stats::cor(x, adh$instrument_1990_2007, use = "pairwise.complete.obs")),
      weighted_mean = weighted_mean_safe(x, adh$adh_weight),
      sd = stats::sd(safe_num(x), na.rm = TRUE)
    )
  })
  readr::write_csv(balance_tbl, config$bartik_balance_csv)

  panel_path <- config$diagnostic_panel_rds
  pretrend_tbl <- tibble::tibble()
  if (file.exists(panel_path)) {
    panel <- readRDS(panel_path) %>%
      dplyr::filter(year < config$reference_year) %>%
      dplyr::arrange(czone, year)
    if (nrow(panel) > 0) {
      slope_tbl <- panel %>%
        dplyr::group_by(czone) %>%
        dplyr::summarise(
          pretrend_slope_rep_margin = tryCatch(stats::coef(stats::lm(rep_margin ~ year))[2], error = function(e) NA_real_),
          pretrend_change_first_to_last = rep_margin[which.max(year)] - rep_margin[which.min(year)],
          n_pre_years = sum(!is.na(rep_margin)),
          .groups = "drop"
        ) %>%
        dplyr::left_join(adh, by = "czone")
      for (outcome in c("pretrend_slope_rep_margin", "pretrend_change_first_to_last")) {
        for (rhs_var in c("exposure_1990_2007", "instrument_1990_2007")) {
          dat <- slope_tbl %>% dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[rhs_var]]), n_pre_years >= 2)
          if (nrow(dat) > 0) {
            fml <- stats::as.formula(paste(outcome, "~", rhs_var, "+", paste(controls_core, collapse = " + ")))
            model <- tryCatch(fit_weighted_feols(fml, dat, weight_var = "adh_weight", cluster_var = "statefip"), error = function(e) NULL)
            if (!is.null(model)) {
              ct <- coeftable_to_tibble(model) %>% dplyr::filter(term == rhs_var)
              pretrend_tbl <- dplyr::bind_rows(pretrend_tbl, tibble::tibble(
                outcome = outcome,
                regressor = rhs_var,
                estimate = scalar_or_na(ct$estimate),
                std.error = scalar_or_na(ct$std.error),
                statistic = scalar_or_na(ct$statistic),
                p.value = scalar_or_na(ct$p.value),
                n_obs = stats::nobs(model),
                note = "CZ-level pre-1988 presidential Republican-margin pretrend diagnostic; not a causal test."
              ))
            }
          }
        }
      }
    }
  }
  readr::write_csv(pretrend_tbl, config$bartik_pretrend_csv)

  rot_tbl <- tibble::tibble(
    diagnostic = c("rotemberg_weights_exact", "industry_by_cz_shares_available", "industry_shift_data_available"),
    status = c("not_computable_from_public_ADH_files_in_this_project", "not_found_in_ADH_workfiles", "available_in_sic87dd_trade_data"),
    details = c(
      "Exact GPSS Rotemberg weights require CZ-by-industry baseline shares matched to industry-level shocks. The attached ADH workfiles include CZ-level exposure and instrument, and sic87dd_trade_data includes trade by industry, but the CZ-by-industry share matrix is not present as a reusable public file in this project.",
      "The project can still report first-stage, balance, and pretrend diagnostics using CZ-level exposure and instrument. Exact Rotemberg weights should be added only if the CZ-by-industry employment-share matrix is reconstructed from ADH raw CBP inputs or obtained from Dorn/ADH auxiliary files.",
      "sic87dd_trade_data.dta can support industry-shift summaries, but not Rotemberg weights without local shares."
    )
  )
  readr::write_csv(rot_tbl, config$bartik_data_availability_csv)

  memo <- c(
    "# Bartik / shift-share identification memo",
    "",
    "This project estimates reduced-form event-study relationships between CZ-level ADH China import exposure and political outcomes. The ADH exposure variable is a shift-share object: local baseline industry composition is combined with national or foreign import-growth shocks.",
    "",
    "## Interpretation",
    "",
    "For the OLS event-study coefficients to be interpreted causally, high-exposure CZs must not have been on different political trajectories for reasons unrelated to the China shock after conditioning on fixed effects and baseline-control-by-year interactions. For the ADH-style instrument, one must additionally defend either exogeneity of the foreign import-growth shifts or exogeneity of the initial industry shares, depending on the preferred shift-share identification argument.",
    "",
    "## Diagnostics written by the pipeline",
    "",
    "- `bartik_first_stage_diagnostics.csv`: first-stage strength of the ADH other-high-income-country instrument for ADH exposure.",
    "- `bartik_balance_correlations.csv`: correlations of exposure and instrument with baseline controls.",
    "- `bartik_pretrend_placebos.csv`: whether exposure or the instrument predict pre-1988 Republican-margin trends.",
    "- `bartik_rotemberg_data_availability.csv`: explains why exact GPSS Rotemberg weights are not computed from the currently attached public ADH workfiles.",
    "",
    "## How to use in the paper",
    "",
    "Treat these diagnostics as evidence about the plausibility and limits of the identifying assumptions, not as proof. If a small number of industries dominate a future reconstructed Rotemberg-weight diagnostic, the political estimates should be interpreted as exposure to those high-weight industries rather than a generic China-shock effect."
  )
  writeLines(memo, config$bartik_identification_memo_md)

  invisible(list(first_stage = fs_tbl, balance = balance_tbl, pretrend = pretrend_tbl))
}

add_alternative_political_outcomes <- function(panel) {
  panel %>%
    dplyr::arrange(czone, year) %>%
    dplyr::group_by(czone) %>%
    dplyr::mutate(
      rep_2party_share = rep_votes / twoparty_votes,
      dem_2party_share = dem_votes / twoparty_votes,
      two_party_votes_per_1990_pop = twoparty_votes / pop1990,
      total_votes_per_1990_pop = total_votes / pop1990,
      rep_votes_per_1990_pop = rep_votes / pop1990,
      dem_votes_per_1990_pop = dem_votes / pop1990,
      rep_margin_swing = rep_margin - dplyr::lag(rep_margin),
      rep_2party_share_swing = rep_2party_share - dplyr::lag(rep_2party_share)
    ) %>%
    dplyr::ungroup()
}

run_alternative_political_outcome_event_studies <- function(config = CONFIG) {
  config <- finalize_config(config)
  if (!file.exists(config$analysis_panel_rds)) {
    warning("Skipping alternative political outcomes: missing ", config$analysis_panel_rds, call. = FALSE)
    return(invisible(NULL))
  }
  panel <- readRDS(config$analysis_panel_rds) %>% add_alternative_political_outcomes()
  outcomes <- c(
    "rep_margin", "rep_2party_share", "dem_2party_share", "two_party_votes_per_1990_pop",
    "total_votes_per_1990_pop", "rep_votes_per_1990_pop", "dem_votes_per_1990_pop",
    "rep_margin_swing", "rep_2party_share_swing"
  )
  outcome_labels <- tibble::tibble(
    outcome = outcomes,
    label = c(
      "Republican margin", "Republican two-party share", "Democratic two-party share",
      "Two-party votes / 1990 population", "Total votes / 1990 population",
      "Republican votes / 1990 population", "Democratic votes / 1990 population",
      "Republican-margin swing since previous election", "Republican-share swing since previous election"
    )
  )
  specs <- list(
    minimal = list(controls = character(0), standardize = FALSE),
    core_controls = list(controls = config$core_interacted_controls, standardize = TRUE),
    full_controls = list(controls = config$interacted_controls, standardize = TRUE)
  )
  coef_tbl <- purrr::map_dfr(outcomes, function(outcome) {
    purrr::imap_dfr(specs, function(spec, spec_name) {
      fit_single_outcome_event_study(panel, outcome, spec$controls, spec_name, config, standardize_controls = spec$standardize)$coefficients
    })
  }) %>% dplyr::left_join(outcome_labels, by = "outcome")
  readr::write_csv(coef_tbl, config$alternative_outcomes_coefficients_csv)
  summary_tbl <- panel %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(outcomes), list(mean = ~mean(.x, na.rm = TRUE), sd = ~stats::sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "stat", values_to = "value")
  readr::write_csv(summary_tbl, config$alternative_outcomes_summary_csv)
  if (nrow(coef_tbl) > 0) {
    plot_df <- coef_tbl %>% dplyr::filter(spec %in% c("minimal", "core_controls", "full_controls"), outcome %in% outcomes[1:7])
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) +
      ggplot2::geom_pointrange(linewidth = 0.25) +
      ggplot2::facet_grid(label ~ spec, scales = "free_y") +
      ggplot2::labs(title = "Alternative political outcome event studies", subtitle = "CZ-clustered 95% intervals; 1988 reference where available", x = NULL, y = "Coefficient on ADH exposure × election year") +
      ggplot2::theme_minimal(base_size = 10)
    write_plot_safely(p, config$alternative_outcomes_plot_png, config$alternative_outcomes_plot_pdf, width = 12, height = 10)
  }
  invisible(coef_tbl)
}

build_generic_county_to_cz_panel <- function(config, county_year_data, value_cols, weight_col_for_rates = NULL) {
  config <- finalize_config(config)
  cz_path <- file.path(config$replication_dir, "cz-data", "cz-198090.xls")
  ftz_zip <- file.path(config$replication_dir, "ftz2024", "crosswalks", "CountyToCounty", "1990", "1990_csv.zip")
  cz_lookup <- readxl::read_xls(cz_path) %>%
    dplyr::transmute(
      county_fips_1990 = stringr::str_pad(as.character(`County FIPS Code`), width = 5, side = "left", pad = "0"),
      czone = as.integer(CZ90),
      pop1990 = safe_num(`Population 1990`)
    )
  source_decades <- sort(unique(year_to_source_decade(county_year_data$year)))
  xwalk <- purrr::map_dfr(source_decades, ~ read_ftz_crosswalk(
    source_decade = .x,
    zip_path = ftz_zip,
    county_fips_1990_identity = cz_lookup$county_fips_1990,
    weight_col = config$crosswalk_weight,
    missing_weight_policy = config$crosswalk_missing_weight_policy,
    renormalize_weights = config$renormalize_crosswalk_weights,
    tolerance = config$weight_sum_tolerance
  )) %>% dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > 0)

  county_year_data %>%
    dplyr::mutate(source_decade = year_to_source_decade(year), county_fips_source = county_fips) %>%
    dplyr::left_join(xwalk, by = c("source_decade", "county_fips_source"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(county_fips_1990), !is.na(crosswalk_weight), crosswalk_weight > 0) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(value_cols), ~ safe_num(.x) * crosswalk_weight)) %>%
    dplyr::group_by(year, county_fips_1990) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::inner_join(cz_lookup, by = "county_fips_1990") %>%
    dplyr::group_by(czone, year) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols), ~ sum(.x, na.rm = TRUE)), pop1990 = sum(pop1990, na.rm = TRUE), .groups = "drop")
}

run_nanda_outcome_event_studies <- function(config = CONFIG) {
  config <- finalize_config(config)
  nanda_path <- file.path(config$replication_dir, "outcomes-data", "nanda-2004-2022", "DS0001", "38506-0001-Data.rda")
  if (!file.exists(nanda_path)) {
    readr::write_csv(tibble::tibble(status = "missing", path = nanda_path), config$nanda_outcomes_summary_csv)
    return(invisible(NULL))
  }
  raw <- read_first_rdata_frame(nanda_path)
  names(raw) <- toupper(names(raw))
  fips_col <- first_existing_col(raw, c("STCOFIPS10", "STCOFIPS20", "FIPS", "COUNTY_FIPS"))
  if (is.na(fips_col) || !"YEAR" %in% names(raw)) stop("NaNDA data must contain county FIPS and YEAR columns.")
  numeric_candidates <- c("REG_VOTERS", "BALLOTS_CAST", "CVAP", "PRES_DEM_VOTES", "PRES_REP_VOTES", "SEN_DEM_VOTES", "SEN_REP_VOTES")
  count_cols <- numeric_candidates[numeric_candidates %in% names(raw)]
  index_cols <- c("PARTISAN_INDEX_DEM", "PARTISAN_INDEX_REP")
  index_cols <- index_cols[index_cols %in% names(raw)]
  nanda <- raw %>%
    dplyr::transmute(
      county_fips = stringr::str_pad(as.character(.data[[fips_col]]), width = 5, side = "left", pad = "0"),
      year = as.integer(YEAR),
      dplyr::across(dplyr::all_of(c(count_cols, index_cols)), safe_num)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c(count_cols, index_cols)), ~ dplyr::na_if(.x, -1)))

  # Convert index shares to CVAP-weighted pseudo-counts before bridging.
  for (idx in index_cols) {
    nanda[[paste0(idx, "_NUM")]] <- nanda[[idx]] * if ("CVAP" %in% count_cols) nanda$CVAP else 1
  }
  bridge_cols <- c(count_cols, paste0(index_cols, "_NUM"), if ("CVAP" %in% count_cols) character(0) else NULL)
  bridge_cols <- unique(bridge_cols[bridge_cols %in% names(nanda)])
  cz_panel <- build_generic_county_to_cz_panel(config, nanda, bridge_cols)
  if ("CVAP" %in% names(cz_panel)) {
    if ("REG_VOTERS" %in% names(cz_panel)) cz_panel$reg_voters_pct <- cz_panel$REG_VOTERS / cz_panel$CVAP
    if ("BALLOTS_CAST" %in% names(cz_panel)) cz_panel$voter_turnout_pct <- cz_panel$BALLOTS_CAST / cz_panel$CVAP
    if (all(c("BALLOTS_CAST", "REG_VOTERS") %in% names(cz_panel))) cz_panel$reg_voter_turnout_pct <- cz_panel$BALLOTS_CAST / cz_panel$REG_VOTERS
    if (all(c("PRES_REP_VOTES", "PRES_DEM_VOTES") %in% names(cz_panel))) {
      cz_panel$pres_rep_ratio <- cz_panel$PRES_REP_VOTES / (cz_panel$PRES_REP_VOTES + cz_panel$PRES_DEM_VOTES)
      cz_panel$pres_dem_ratio <- cz_panel$PRES_DEM_VOTES / (cz_panel$PRES_REP_VOTES + cz_panel$PRES_DEM_VOTES)
    }
    for (idx in index_cols) {
      num <- paste0(idx, "_NUM")
      if (num %in% names(cz_panel)) cz_panel[[tolower(idx)]] <- cz_panel[[num]] / cz_panel$CVAP
    }
  }
  readr::write_csv(cz_panel, config$nanda_cz_panel_csv)

  adh <- load_adh_extension_data(config) %>% dplyr::select(czone, statefip, exposure = exposure_1990_2007, adh_weight, dplyr::all_of(config$baseline_controls), dplyr::starts_with("z_"))
  nanda_panel <- cz_panel %>%
    dplyr::inner_join(adh, by = "czone") %>%
    dplyr::arrange(czone, year) %>%
    dplyr::group_by(czone) %>%
    dplyr::mutate(election_index = dplyr::dense_rank(year)) %>%
    dplyr::ungroup()
  outcomes <- c("reg_voters_pct", "voter_turnout_pct", "reg_voter_turnout_pct", "pres_rep_ratio", "pres_dem_ratio", "partisan_index_dem", "partisan_index_rep")
  outcomes <- outcomes[outcomes %in% names(nanda_panel)]
  specs <- list(minimal = list(controls = character(0), standardize = FALSE), core_controls = list(controls = config$core_interacted_controls, standardize = TRUE), full_controls = list(controls = config$interacted_controls, standardize = TRUE))
  coef_tbl <- purrr::map_dfr(outcomes, function(outcome) {
    ref <- min(nanda_panel$year[!is.na(nanda_panel[[outcome]])], na.rm = TRUE)
    purrr::imap_dfr(specs, function(spec, spec_name) {
      fit_single_outcome_event_study(nanda_panel, outcome, spec$controls, spec_name, config, ref_year = ref, standardize_controls = spec$standardize, sample_label = "nanda_2004_2022")$coefficients
    })
  })
  readr::write_csv(coef_tbl, config$nanda_outcomes_coefficients_csv)
  summary_tbl <- nanda_panel %>%
    dplyr::summarise(n_cz = dplyr::n_distinct(czone), min_year = min(year, na.rm = TRUE), max_year = max(year, na.rm = TRUE), dplyr::across(dplyr::all_of(outcomes), ~sum(!is.na(.x)), .names = "n_nonmissing_{.col}"))
  readr::write_csv(summary_tbl, config$nanda_outcomes_summary_csv)
  if (nrow(coef_tbl) > 0) {
    p <- ggplot2::ggplot(coef_tbl, ggplot2::aes(x = year, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) +
      ggplot2::geom_pointrange(linewidth = 0.25) +
      ggplot2::facet_grid(outcome ~ spec, scales = "free_y") +
      ggplot2::labs(title = "NaNDA turnout and partisanship outcomes", subtitle = "County outcomes bridged to 1990 CZs; CZ-clustered intervals", x = NULL, y = "Coefficient on ADH exposure × year") +
      ggplot2::theme_minimal(base_size = 10)
    write_plot_safely(p, config$nanda_outcomes_plot_png, config$nanda_outcomes_plot_pdf, width = 12, height = 10)
  }
  invisible(coef_tbl)
}

run_adhm2020_mechanism_diagnostics <- function(config = CONFIG) {
  config <- finalize_config(config)
  adhm_dir <- file.path(config$replication_dir, "adhm2020", "dta")
  house_path <- file.path(adhm_dir, "house_2002_2016.dta")
  pres_path <- file.path(adhm_dir, "president_2000_2016.dta")
  if (!file.exists(house_path) && !file.exists(pres_path)) {
    readr::write_csv(tibble::tibble(status = "missing", path = adhm_dir), config$adhm2020_summary_csv)
    return(invisible(NULL))
  }
  adh <- load_adh_extension_data(config)
  controls <- model_control_names(config$core_interacted_controls, standardize = TRUE)
  controls <- controls[controls %in% names(adh)]
  regressions <- tibble::tibble()
  outcome_dict <- tibble::tibble(
    outcome = c("d2_rwin_2002_2016", "d2_cfavg_repcon_2002_2016", "d2_cfavg_repmod_2002_2016", "d2_cfavg_demmod_2002_2016", "d2_cfavg_demlib_2002_2016", "d2_teaparty_2002_2010", "d_shnr_pres_2000_2016", "d_shnr_pres_2000_2008"),
    interpretation = c(
      "Change in Republican House win indicator/share, ADHM public data",
      "Change in conservative Republican candidate/legislator CF-score share, ADHM public data",
      "Change in moderate Republican CF-score share, ADHM public data",
      "Change in moderate Democratic CF-score share, ADHM public data",
      "Change in liberal Democratic CF-score share, ADHM public data",
      "Change in Tea Party outcome, ADHM public data",
      "Change in Republican presidential vote share, 2000-2016, ADHM public data",
      "Change in Republican presidential vote share, 2000-2008, ADHM public data"
    )
  )
  readr::write_csv(outcome_dict, config$adhm2020_outcome_dictionary_csv)

  fit_cross_section <- function(df, outcomes, source, weight_var = NULL) {
    purrr::map_dfr(outcomes[outcomes %in% names(df)], function(outcome) {
      dat <- df %>% dplyr::inner_join(adh, by = "czone") %>% dplyr::filter(!is.na(.data[[outcome]]), !is.na(exposure_1990_2007))
      if (nrow(dat) == 0) return(tibble::tibble())
      rhs <- c("exposure_1990_2007", controls)
      fml <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
      model <- tryCatch(fit_weighted_feols(fml, dat, weight_var = if (!is.null(weight_var) && weight_var %in% names(dat)) weight_var else "adh_weight", cluster_var = "statefip"), error = function(e) NULL)
      if (is.null(model)) return(tibble::tibble(outcome = outcome, source = source, status = "error"))
      ct <- coeftable_to_tibble(model) %>% dplyr::filter(term == "exposure_1990_2007")
      tibble::tibble(source = source, outcome = outcome, estimate = scalar_or_na(ct$estimate), std.error = scalar_or_na(ct$std.error), statistic = scalar_or_na(ct$statistic), p.value = scalar_or_na(ct$p.value), n_obs = stats::nobs(model), controls = paste(controls, collapse = "; "))
    })
  }

  if (file.exists(house_path)) {
    house <- haven::read_dta(house_path) %>%
      dplyr::mutate(czone = as.integer(czone), sh_district_2002 = dplyr::coalesce(safe_num(sh_district_2002), 1))
    house_outcomes <- c("d2_rwin_2002_2016", "d2_cfavg_repcon_2002_2016", "d2_cfavg_repmod_2002_2016", "d2_cfavg_demmod_2002_2016", "d2_cfavg_demlib_2002_2016", "d2_teaparty_2002_2010")
    house_cz <- house %>%
      dplyr::group_by(czone) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(house_outcomes[house_outcomes %in% names(house)]), ~ weighted_mean_safe(.x, sh_district_2002)), .groups = "drop")
    regressions <- dplyr::bind_rows(regressions, fit_cross_section(house_cz, house_outcomes, "adhm2020_house_2002_2016"))
  }
  if (file.exists(pres_path)) {
    pres <- haven::read_dta(pres_path) %>%
      dplyr::mutate(czone = as.integer(czone), totvote_2000pres = dplyr::coalesce(safe_num(totvote_2000pres), 1))
    pres_cz <- pres %>%
      dplyr::group_by(czone) %>%
      dplyr::summarise(
        shnr_pres2000 = weighted_mean_safe(shnr_pres2000, totvote_2000pres),
        shnr_pres2008 = weighted_mean_safe(shnr_pres2008, totvote_2000pres),
        shnr_pres2016 = weighted_mean_safe(shnr_pres2016, totvote_2000pres),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        d_shnr_pres_2000_2016 = shnr_pres2016 - shnr_pres2000,
        d_shnr_pres_2000_2008 = shnr_pres2008 - shnr_pres2000
      )
    regressions <- dplyr::bind_rows(regressions, fit_cross_section(pres_cz, c("d_shnr_pres_2000_2016", "d_shnr_pres_2000_2008"), "adhm2020_president_2000_2016"))
  }
  readr::write_csv(regressions, config$adhm2020_regressions_csv)
  summary <- tibble::tibble(
    source = c("house_2002_2016", "president_2000_2016"),
    file_present = c(file.exists(house_path), file.exists(pres_path)),
    purpose = c("Political-supply and House ideology outcomes from ADHM public replication data", "Presidential-vote benchmark outcomes from ADHM public replication data")
  )
  readr::write_csv(summary, config$adhm2020_summary_csv)
  invisible(regressions)
}

run_subperiod_exposure_event_studies <- function(config = CONFIG) {
  config <- finalize_config(config)
  adh_stack_path <- file.path(config$replication_dir, "adh2013", "dta", "workfile_china.dta")
  if (!file.exists(adh_stack_path) || !file.exists(config$analysis_panel_rds)) return(invisible(NULL))
  stack <- haven::read_dta(adh_stack_path)
  exp_period <- stack %>%
    dplyr::filter(yr %in% c(1990, 2000)) %>%
    dplyr::transmute(
      czone = as.integer(czone),
      period = dplyr::case_when(yr == 1990 ~ "1990_2000", yr == 2000 ~ "2000_2007", TRUE ~ as.character(yr)),
      exposure = safe_num(d_tradeusch_pw),
      instrument = safe_num(d_tradeotch_pw_lag)
    ) %>%
    tidyr::pivot_wider(names_from = period, values_from = c(exposure, instrument), names_sep = "_")
  readr::write_csv(exp_period, config$subperiod_exposure_csv)
  readr::write_csv(exp_period %>% dplyr::summarise(dplyr::across(-czone, list(mean = ~mean(.x, na.rm = TRUE), sd = ~stats::sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")), config$subperiod_exposure_summary_csv)

  panel <- readRDS(config$analysis_panel_rds) %>% dplyr::left_join(exp_period, by = "czone")
  yrs <- config$event_years
  ref <- config$reference_year
  for (ev in c("exposure_1990_2000", "exposure_2000_2007")) {
    for (yr in setdiff(yrs, ref)) {
      panel[[paste0(ev, "_x_", yr)]] <- panel[[ev]] * as.integer(panel$year == yr)
    }
  }
  specs <- list(
    minimal = list(controls = character(0), standardize = FALSE),
    core_controls = list(controls = config$core_interacted_controls, standardize = TRUE),
    full_controls = list(controls = config$interacted_controls, standardize = TRUE)
  )
  coef_tbl <- tibble::tibble()
  status_tbl <- tibble::tibble()
  for (spec_name in names(specs)) {
    spec <- specs[[spec_name]]
    dat <- panel
    controls_use <- model_control_names(spec$controls, standardize = spec$standardize)
    controls_use <- controls_use[controls_use %in% names(dat)]
    if (length(controls_use) > 0) dat <- make_interacted_control_vars(dat, controls_use, yrs, ref)
    event_terms <- unlist(lapply(c("exposure_1990_2000", "exposure_2000_2007"), function(ev) paste0(ev, "_x_", setdiff(yrs, ref))))
    ctrl_terms <- unlist(lapply(controls_use, function(ctrl) paste0("ctrl_", ctrl, "_", setdiff(yrs, ref))))
    fml <- stats::as.formula(paste("rep_margin ~", paste(c(event_terms, ctrl_terms), collapse = " + "), "| czone + year"))
    fit <- tryCatch(fixest::feols(fml, data = dat, weights = ~adh_weight, cluster = ~czone, panel.id = ~czone + election_index, warn = FALSE, notes = FALSE), error = function(e) e)
    if (inherits(fit, "error")) {
      status_tbl <- dplyr::bind_rows(status_tbl, tibble::tibble(spec = spec_name, status = "error", error_message = conditionMessage(fit)))
    } else {
      ct <- coeftable_to_tibble(fit) %>%
        dplyr::filter(term %in% event_terms) %>%
        dplyr::mutate(
          spec = spec_name,
          exposure_period = dplyr::case_when(startsWith(term, "exposure_1990_2000") ~ "1990-2000", startsWith(term, "exposure_2000_2007") ~ "2000-2007", TRUE ~ NA_character_),
          year = as.integer(stringr::str_extract(term, "[0-9]{4}$")),
          reference_year = ref,
          n_obs = stats::nobs(fit),
          n_cz = dplyr::n_distinct(dat$czone)
        ) %>%
        dplyr::select(spec, exposure_period, term, year, reference_year, estimate, std.error, statistic, p.value, n_obs, n_cz)
      coef_tbl <- dplyr::bind_rows(coef_tbl, ct)
      status_tbl <- dplyr::bind_rows(status_tbl, tibble::tibble(spec = spec_name, status = "ok", error_message = NA_character_))
    }
  }
  readr::write_csv(coef_tbl, config$subperiod_event_study_coefficients_csv)
  readr::write_csv(status_tbl, config$subperiod_event_study_status_csv)
  if (nrow(coef_tbl) > 0) {
    p <- ggplot2::ggplot(coef_tbl, ggplot2::aes(x = year, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, color = exposure_period)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) +
      ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.6), linewidth = 0.25) +
      ggplot2::facet_wrap(~spec, ncol = 1, scales = "free_y") +
      ggplot2::labs(title = "Subperiod ADH exposure event study", subtitle = "1990-2000 and 2000-2007 exposure terms entered jointly", x = NULL, y = "Coefficient", color = "Exposure period") +
      ggplot2::theme_minimal(base_size = 11)
    write_plot_safely(p, config$subperiod_event_study_plot_png, config$subperiod_event_study_plot_pdf, width = 10, height = 8)
  }
  invisible(coef_tbl)
}

run_sixth_extensions <- function(config = CONFIG) {
  config <- extension_primary_config(config)
  ensure_output_dirs(config)
  load_required_packages(config)
  message("Running Sixth-section extension diagnostics for primary design: ", toupper(config$crosswalk_weight_slug), " + ", config$crosswalk_missing_weight_policy)

  results <- list()
  results$bartik <- tryCatch(run_bartik_identification_diagnostics(config), error = function(e) { warning("Bartik diagnostics failed: ", conditionMessage(e), call. = FALSE); NULL })
  results$alternative_outcomes <- tryCatch(run_alternative_political_outcome_event_studies(config), error = function(e) { warning("Alternative political outcomes failed: ", conditionMessage(e), call. = FALSE); NULL })
  results$nanda <- tryCatch(run_nanda_outcome_event_studies(config), error = function(e) { warning("NaNDA outcomes failed: ", conditionMessage(e), call. = FALSE); NULL })
  results$adhm2020 <- tryCatch(run_adhm2020_mechanism_diagnostics(config), error = function(e) { warning("ADHM 2020 diagnostics failed: ", conditionMessage(e), call. = FALSE); NULL })
  results$subperiod <- tryCatch(run_subperiod_exposure_event_studies(config), error = function(e) { warning("Subperiod exposure diagnostics failed: ", conditionMessage(e), call. = FALSE); NULL })

  checks <- if (file.exists(config$validation_checks_csv)) readr::read_csv(config$validation_checks_csv, show_col_types = FALSE) else tibble::tibble()
  write_pipeline_manifest(
    config = config,
    checks = checks,
    stage = "sixth_extensions",
    sources = list(
      adh2013 = list(path = file.path(config$replication_dir, "adh2013")),
      nanda = list(path = file.path(config$replication_dir, "outcomes-data", "nanda-2004-2022")),
      adhm2020 = list(path = file.path(config$replication_dir, "adhm2020"))
    ),
    extra = list(
      extension_modules = c("bartik_identification", "alternative_political_outcomes", "nanda_turnout_partisanship", "adhm2020_political_supply", "subperiod_exposure"),
      crosswalk_used_for_extensions = paste0(config$crosswalk_weight, " / ", config$crosswalk_missing_weight_policy)
    )
  )
  invisible(results)
}

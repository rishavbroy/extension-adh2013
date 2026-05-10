# extension-adh2013/src/06_sixth_extensions.R

# Highest-priority Sixth-section extensions. These modules intentionally run once
# for the preferred M5 + fallback-M2 design, even when the crosswalk-sensitivity
# wrapper is active. The mechanism/identification diagnostics are meant to be
# interpreted for the preferred analysis sample rather than repeated for every
# crosswalk robustness specification.

extension_primary_config <- function(config = CONFIG) {
  out <- config
  out$crosswalk_weight <- config$extension_crosswalk_weight %||% "m5_weight"
  out$crosswalk_missing_weight_policy <- config$extension_crosswalk_missing_weight_policy %||% "fallback_m2"
  out <- finalize_config(out)
  current <- finalize_config(config)
  if (!file.exists(out$analysis_panel_rds) && file.exists(current$analysis_panel_rds)) out <- current
  out
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

safe_divide <- function(num, den) {
  num <- safe_num(num); den <- safe_num(den)
  out <- ifelse(is.na(num) | is.na(den) | den <= 0, NA_real_, num / den)
  out[!is.finite(out)] <- NA_real_
  out
}

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

coeftable_to_tibble <- function(model, vcov = NULL, term_prefix = NULL) {
  ct <- tryCatch({
    if (is.null(vcov)) fixest::coeftable(model) else fixest::coeftable(model, vcov = vcov)
  }, error = function(e) NULL)
  if (is.null(ct)) return(tibble::tibble())
  out <- as.data.frame(ct) %>% tibble::rownames_to_column("term") %>% tibble::as_tibble()
  names(out) <- c("term", "estimate", "std.error", "statistic", "p.value")
  if (!is.null(term_prefix)) out <- out %>% dplyr::filter(startsWith(term, term_prefix))
  out
}

fit_weighted_feols <- function(formula, data, weight_var = "adh_weight", cluster_var = "czone") {
  weights_fml <- if (!is.null(weight_var) && weight_var %in% names(data)) stats::as.formula(paste0("~", weight_var)) else NULL
  cluster_fml <- if (!is.null(cluster_var) && cluster_var %in% names(data)) stats::as.formula(paste0("~", cluster_var)) else NULL
  fixest::feols(formula, data = data, weights = weights_fml, cluster = cluster_fml, warn = FALSE, notes = FALSE)
}

write_plot_safely <- function(plot, png_path, pdf_path, width = 10, height = 6) {
  try(ggplot2::ggsave(png_path, plot, width = width, height = height, dpi = 300, bg = "white"), silent = TRUE)
  try(ggplot2::ggsave(pdf_path, plot, width = width, height = height, bg = "white"), silent = TRUE)
  invisible(NULL)
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

make_single_exposure_event_terms <- function(data, exposure_var, years, ref_year, prefix = "es") {
  years <- sort(unique(years))
  for (yr in setdiff(years, ref_year)) {
    nm <- paste0(prefix, "_", yr)
    data[[nm]] <- data[[exposure_var]] * as.integer(data$year == yr)
  }
  data
}

apply_control_interactions <- function(data, controls, years, ref_year, standardize = TRUE) {
  controls_use <- model_control_names(controls, standardize = standardize)
  controls_use <- controls_use[controls_use %in% names(data)]
  if (length(controls_use) == 0) return(list(data = data, controls = controls_use, terms = character(0)))
  built <- make_interacted_control_vars(data, controls_use, years, ref_year)
  list(data = built$data, controls = controls_use, terms = built$vars)
}

fit_single_outcome_event_study <- function(panel, outcome_var, controls, spec_name, config,
                                           ref_year = config$reference_year,
                                           standardize_controls = TRUE,
                                           sample_label = "main_1972_start") {
  yrs <- sort(unique(panel$year[!is.na(panel[[outcome_var]])]))
  if (length(yrs) == 0) return(list(status = "empty", coefficients = tibble::tibble()))
  if (!ref_year %in% yrs) ref_year <- min(yrs, na.rm = TRUE)
  dat <- panel %>% dplyr::filter(year %in% yrs, !is.na(.data[[outcome_var]]), !is.na(exposure))
  if (nrow(dat) == 0) return(list(status = "empty", coefficients = tibble::tibble()))
  dat <- make_single_exposure_event_terms(dat, "exposure", yrs, ref_year, prefix = "es")
  event_terms <- paste0("es_", setdiff(yrs, ref_year))
  ctrl_build <- apply_control_interactions(dat, controls, yrs, ref_year, standardize = standardize_controls)
  dat <- ctrl_build$data
  rhs <- c(event_terms, ctrl_build$terms)
  if (length(rhs) == 0) return(list(status = "no_terms", coefficients = tibble::tibble()))
  fml <- stats::as.formula(paste(outcome_var, "~", paste(rhs, collapse = " + "), "| czone + year"))
  tryCatch({
    model <- fixest::feols(fml, data = dat, weights = ~adh_weight, cluster = ~czone, panel.id = ~czone + election_index, warn = FALSE, notes = FALSE)
    coef <- coeftable_to_tibble(model) %>%
      dplyr::filter(term %in% event_terms) %>%
      dplyr::mutate(
        outcome = outcome_var,
        spec = spec_name,
        sample = sample_label,
        se_type = "cluster_cz",
        reference_year = ref_year,
        year = as.integer(stringr::str_extract(term, "[0-9]{4}")),
        n_obs = stats::nobs(model),
        n_cz = dplyr::n_distinct(dat$czone),
        controls = if (length(ctrl_build$controls) == 0) "none" else paste(ctrl_build$controls, collapse = "; ")
      ) %>%
      dplyr::select(outcome, spec, sample, se_type, term, year, reference_year, estimate, std.error, statistic, p.value, n_obs, n_cz, controls)
    list(status = "ok", coefficients = coef)
  }, error = function(e) {
    list(status = paste0("error: ", conditionMessage(e)), coefficients = tibble::tibble())
  })
}

# -----------------------------------------------------------------------------
# 1. Bartik identification diagnostics
# -----------------------------------------------------------------------------

extract_trade_shift_summary <- function(config) {
  trade_path <- file.path(config$replication_dir, "adh2013", "dta", "sic87dd_trade_data.dta")
  if (!file.exists(trade_path)) return(tibble::tibble(status = "missing", path = trade_path))
  tr <- haven::read_dta(trade_path)
  names(tr) <- tolower(names(tr))
  req <- c("year", "importer", "exporter", "imports", "sic87dd")
  if (!all(req %in% names(tr))) {
    return(tibble::tibble(status = "missing_required_columns", columns_found = paste(names(tr), collapse = ";")))
  }
  tr <- tr %>%
    dplyr::mutate(
      year = as.integer(year),
      importer = toupper(trimws(as.character(importer))),
      exporter = toupper(trimws(as.character(exporter))),
      imports = safe_num(imports),
      sic87dd = as.character(sic87dd)
    )
  periods <- tibble::tibble(start_year = c(1991L, 2000L, 1991L), end_year = c(1999L, 2007L, 2007L), period = c("1991-1999", "2000-2007", "1991-2007"))
  purrr::pmap_dfr(periods, function(start_year, end_year, period) {
    base <- tr %>% dplyr::filter(year %in% c(start_year, end_year), exporter %in% c("CHN", "CHINA"), importer %in% c("USA", "OTH", "OTHER", "OTHCAN"))
    if (nrow(base) == 0) return(tibble::tibble(period = period, status = "no_matching_trade_rows"))
    wide <- base %>%
      dplyr::group_by(sic87dd, importer, year) %>%
      dplyr::summarise(imports = sum(imports, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = year, values_from = imports, names_prefix = "imports_")
    start_col <- paste0("imports_", start_year)
    end_col <- paste0("imports_", end_year)
    if (!all(c(start_col, end_col) %in% names(wide))) return(tibble::tibble(period = period, status = "missing_period_endpoints"))
    wide %>%
      dplyr::mutate(
        start_imports = .data[[start_col]],
        end_imports = .data[[end_col]],
        import_growth = end_imports - start_imports
      ) %>%
      dplyr::group_by(period = period, importer) %>%
      dplyr::slice_max(order_by = abs(import_growth), n = 15, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(status = "ok") %>%
      dplyr::select(status, period, importer, sic87dd, start_imports, end_imports, import_growth)
  })
}

run_bartik_identification_diagnostics <- function(config = CONFIG) {
  config <- finalize_config(config)
  adh <- load_adh_extension_data(config)
  controls_core <- model_control_names(config$core_interacted_controls, standardize = TRUE)
  controls_full <- model_control_names(config$interacted_controls, standardize = TRUE)
  controls_core <- controls_core[controls_core %in% names(adh)]
  controls_full <- controls_full[controls_full %in% names(adh)]
  fs_specs <- list(minimal = character(0), core_controls = controls_core, full_controls = controls_full)
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
  p_fs <- ggplot2::ggplot(adh, ggplot2::aes(x = instrument_1990_2007, y = exposure_1990_2007, size = adh_weight)) +
    ggplot2::geom_point(alpha = 0.35) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
    ggplot2::guides(size = "none") +
    ggplot2::labs(title = "ADH exposure first stage", subtitle = "CZ-level China exposure and ADH other-country instrument", x = "Instrument", y = "Exposure") +
    ggplot2::theme_minimal(base_size = 11)
  write_plot_safely(p_fs, config$bartik_first_stage_plot_png, config$bartik_first_stage_plot_pdf, width = 8, height = 5)

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

  pretrend_tbl <- tibble::tibble()
  if (file.exists(config$diagnostic_panel_rds)) {
    panel <- readRDS(config$diagnostic_panel_rds) %>% dplyr::filter(year < config$reference_year) %>% dplyr::arrange(czone, year)
    if (nrow(panel) > 0) {
      slope_tbl <- panel %>%
        dplyr::group_by(czone) %>%
        dplyr::summarise(
          pretrend_slope_rep_margin = tryCatch(stats::coef(stats::lm(rep_margin ~ year))[2], error = function(e) NA_real_),
          pretrend_change_first_to_last = rep_margin[which.max(year)] - rep_margin[which.min(year)],
          n_pre_years = sum(!is.na(rep_margin)),
          .groups = "drop"
        ) %>% dplyr::left_join(adh, by = "czone")
      for (outcome in c("pretrend_slope_rep_margin", "pretrend_change_first_to_last")) {
        for (rhs_var in c("exposure_1990_2007", "instrument_1990_2007")) {
          for (spec_name in names(fs_specs)) {
            ctrls <- fs_specs[[spec_name]]
            dat <- slope_tbl %>% dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[rhs_var]]), n_pre_years >= 2)
            if (nrow(dat) > 0) {
              rhs <- c(rhs_var, ctrls)
              fml <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
              model <- tryCatch(fit_weighted_feols(fml, dat, weight_var = "adh_weight", cluster_var = "statefip"), error = function(e) NULL)
              if (!is.null(model)) {
                ct <- coeftable_to_tibble(model) %>% dplyr::filter(term == rhs_var)
                pretrend_tbl <- dplyr::bind_rows(pretrend_tbl, tibble::tibble(
                  outcome = outcome, spec = spec_name, regressor = rhs_var,
                  estimate = scalar_or_na(ct$estimate), std.error = scalar_or_na(ct$std.error),
                  statistic = scalar_or_na(ct$statistic), p.value = scalar_or_na(ct$p.value),
                  n_obs = stats::nobs(model), note = "CZ-level pre-1988 presidential Republican-margin pretrend diagnostic; not a causal test."
                ))
              }
            }
          }
        }
      }
    }
  }
  readr::write_csv(pretrend_tbl, config$bartik_pretrend_csv)

  preperiod_path <- file.path(config$replication_dir, "adh2013", "dta", "workfile_china_preperiod.dta")
  preperiod_tbl <- tibble::tibble()
  if (file.exists(preperiod_path)) {
    pre <- haven::read_dta(preperiod_path) %>% dplyr::mutate(czone = as.integer(czone), dplyr::across(dplyr::everything(), ~ .x))
    names(pre) <- make.names(names(pre), unique = TRUE)
    names(pre) <- gsub("\\.", "_", names(pre))
    pre <- pre %>% dplyr::mutate(czone = as.integer(czone)) %>% dplyr::left_join(adh, by = "czone")
    candidate_outcomes <- names(pre)[grepl("^d_", names(pre))]
    candidate_outcomes <- setdiff(candidate_outcomes, c("d_tradeusch_pw", "d_tradeotch_pw_lag", "d_tradeusch_pw_future", "d_tradeotch_pw_lag_future"))
    for (outcome in candidate_outcomes) {
      for (rhs_var in c("exposure_1990_2007", "instrument_1990_2007")) {
        dat <- pre %>% dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[rhs_var]]))
        if (nrow(dat) >= 50) {
          fml <- stats::as.formula(paste(outcome, "~", rhs_var, "+", paste(controls_core, collapse = " + ")))
          mod <- tryCatch(fit_weighted_feols(fml, dat, weight_var = "adh_weight", cluster_var = "statefip"), error = function(e) NULL)
          if (!is.null(mod)) {
            ct <- coeftable_to_tibble(mod) %>% dplyr::filter(term == rhs_var)
            preperiod_tbl <- dplyr::bind_rows(preperiod_tbl, tibble::tibble(
              outcome = outcome, regressor = rhs_var, estimate = scalar_or_na(ct$estimate), std.error = scalar_or_na(ct$std.error),
              statistic = scalar_or_na(ct$statistic), p.value = scalar_or_na(ct$p.value), n_obs = stats::nobs(mod),
              note = "ADH preperiod-workfile diagnostic: future exposure/instrument predicting preperiod/local-labor-market changes."
            ))
          }
        }
      }
    }
  }
  readr::write_csv(preperiod_tbl, config$bartik_preperiod_workfile_csv)

  trade_summary <- tryCatch(extract_trade_shift_summary(config), error = function(e) tibble::tibble(status = "error", message = conditionMessage(e)))
  readr::write_csv(trade_summary, config$bartik_industry_shift_summary_csv)

  rot_tbl <- tibble::tibble(
    diagnostic = c("rotemberg_weights_exact", "industry_by_cz_shares_status", "industry_shift_summary_status", "next_step"),
    status = c("not_yet_computed", "requires_reconstructing_CZ_by_industry_shares", ifelse(nrow(trade_summary) > 0, "written", "not_written"), "feasible_with_ADH_raw_CBP_scripts_but_not_a_one-file_join"),
    details = c(
      "Exact GPSS Rotemberg weights require a CZ-by-industry baseline share matrix matched to industry-level shifts. This patch adds industry-shift summaries and documents the reconstruction path, but does not yet run the full ADH CBP-to-CZ industry-share reconstruction.",
      "Relevant public ADH files include other/cbp1980_to_cz.do, cbp1990_to_cz.do, cbp2000_to_cz.do, cbp*_imputations.do, cw_n97_s87.dta, and sic87dd_trade_data.dta.",
      "Industry-level China import-growth summaries are exported from sic87dd_trade_data.dta where columns are available.",
      "To compute exact GPSS weights: reconstruct 1990 CZ x SIC87DD employment shares, create shift vectors from Chinese import growth in the instrument country group, residualize by controls, and compute Rotemberg weights."
    )
  )
  readr::write_csv(rot_tbl, config$bartik_data_availability_csv)

  memo <- c(
    "# Bartik / shift-share identification memo", "",
    "This project estimates reduced-form event-study relationships between CZ-level ADH China import exposure and political outcomes. The ADH exposure variable is a shift-share object: local baseline industry composition is combined with national or foreign import-growth shocks.", "",
    "## Required assumptions", "",
    "A causal reading requires high-exposure CZs not to have been on different political trajectories for reasons unrelated to the China shock after conditioning on fixed effects and baseline-control-by-year interactions. For the ADH-style instrument, one can emphasize exogeneity of foreign import-growth shifts or exogeneity of baseline industry shares; both routes require substantive defense.", "",
    "## Project-specific threats", "",
    "- Initial industry shares may be correlated with prior political realignment, union decline, automation, racial composition, religiosity, education, or local media markets.",
    "- If only a few high-shock industries drive exposure, the estimates may capture politics of those industries rather than a generic China-shock effect.",
    "- Migration and turnout can make CZ-level political outcomes change even if individual beliefs do not change.",
    "- Candidate supply and elite rhetoric may translate material shocks into political outcomes; the ADHM 2020 mechanism outputs are diagnostic for this channel.", "",
    "## Diagnostics written by the pipeline", "",
    "- `bartik_first_stage_diagnostics.csv` and `fig_bartik_first_stage.png`: first-stage strength of the ADH other-country instrument.",
    "- `bartik_balance_correlations.csv`: exposure/instrument correlations with baseline controls.",
    "- `bartik_pretrend_placebos.csv`: whether exposure/instrument predict pre-1988 Republican-margin trends.",
    "- `bartik_preperiod_workfile_diagnostics.csv`: diagnostics using ADH's preperiod workfile.",
    "- `bartik_industry_shift_summary.csv`: industry-level import-shift summaries from ADH public trade data.",
    "- `bartik_rotemberg_data_availability.csv`: exact Rotemberg weights are a feasible future reconstruction, not a completed diagnostic here.", ""
  )
  writeLines(memo, config$bartik_identification_memo_md)
  invisible(list(first_stage = fs_tbl, balance = balance_tbl, pretrend = pretrend_tbl, preperiod = preperiod_tbl, trade = trade_summary))
}

# -----------------------------------------------------------------------------
# 2. Alternative political outcomes
# -----------------------------------------------------------------------------

add_alternative_political_outcomes <- function(panel) {
  panel %>%
    dplyr::arrange(czone, year) %>%
    dplyr::group_by(czone) %>%
    dplyr::mutate(
      rep_2party_share = safe_divide(rep_votes, twoparty_votes),
      dem_2party_share = safe_divide(dem_votes, twoparty_votes),
      two_party_votes_per_1990_pop = safe_divide(twoparty_votes, pop1990),
      total_votes_per_1990_pop = safe_divide(total_votes, pop1990),
      rep_votes_per_1990_pop = safe_divide(rep_votes, pop1990),
      dem_votes_per_1990_pop = safe_divide(dem_votes, pop1990),
      rep_margin_swing = rep_margin - dplyr::lag(rep_margin),
      rep_2party_share_swing = rep_2party_share - dplyr::lag(rep_2party_share)
    ) %>%
    dplyr::ungroup()
}

run_alternative_political_outcome_event_studies <- function(config = CONFIG) {
  config <- finalize_config(config)
  if (!file.exists(config$analysis_panel_rds)) {
    readr::write_csv(tibble::tibble(module = "alternative_political_outcomes", status = "missing_panel", detail = config$analysis_panel_rds), config$alternative_outcomes_status_csv)
    return(invisible(NULL))
  }
  panel <- readRDS(config$analysis_panel_rds) %>% add_alternative_political_outcomes()
  outcomes <- c("rep_margin", "rep_2party_share", "dem_2party_share", "two_party_votes_per_1990_pop", "total_votes_per_1990_pop", "rep_votes_per_1990_pop", "dem_votes_per_1990_pop", "rep_margin_swing", "rep_2party_share_swing")
  outcome_labels <- tibble::tibble(outcome = outcomes, label = c("Republican margin", "Republican two-party share", "Democratic two-party share", "Two-party votes / 1990 population", "Total votes / 1990 population", "Republican votes / 1990 population", "Democratic votes / 1990 population", "Republican-margin swing since previous election", "Republican-share swing since previous election"))
  specs <- list(minimal = list(controls = character(0), standardize = FALSE), core_controls = list(controls = config$core_interacted_controls, standardize = TRUE), full_controls = list(controls = config$interacted_controls, standardize = TRUE))
  coef_tbl <- tibble::tibble(); status_tbl <- tibble::tibble()
  for (outcome in outcomes) {
    for (spec_name in names(specs)) {
      spec <- specs[[spec_name]]
      fit <- fit_single_outcome_event_study(panel, outcome, spec$controls, spec_name, config, standardize_controls = spec$standardize)
      coef_tbl <- dplyr::bind_rows(coef_tbl, fit$coefficients)
      status_tbl <- dplyr::bind_rows(status_tbl, tibble::tibble(module = "alternative_political_outcomes", outcome = outcome, spec = spec_name, status = fit$status, n_coefficients = nrow(fit$coefficients)))
    }
  }
  coef_tbl <- coef_tbl %>% dplyr::left_join(outcome_labels, by = "outcome")
  readr::write_csv(coef_tbl, config$alternative_outcomes_coefficients_csv)
  readr::write_csv(status_tbl, config$alternative_outcomes_status_csv)
  summary_tbl <- panel %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(outcomes), list(n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm = TRUE), sd = ~stats::sd(.x, na.rm = TRUE), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "stat", values_to = "value")
  readr::write_csv(summary_tbl, config$alternative_outcomes_summary_csv)
  if (nrow(coef_tbl) > 0) {
    plot_df <- coef_tbl %>% dplyr::filter(spec %in% c("minimal", "core_controls", "full_controls"), outcome %in% outcomes[1:7])
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) + ggplot2::geom_pointrange(linewidth = 0.25) +
      ggplot2::facet_grid(label ~ spec, scales = "free_y") +
      ggplot2::labs(title = "Alternative political outcome event studies", subtitle = "CZ-clustered 95% intervals; 1988 reference where available", x = NULL, y = "Coefficient on ADH exposure × election year") +
      ggplot2::theme_minimal(base_size = 10)
    write_plot_safely(p, config$alternative_outcomes_plot_png, config$alternative_outcomes_plot_pdf, width = 12, height = 10)
    decomp <- coef_tbl %>% dplyr::filter(spec == "full_controls", outcome %in% c("rep_margin", "rep_votes_per_1990_pop", "dem_votes_per_1990_pop", "two_party_votes_per_1990_pop"))
    p2 <- ggplot2::ggplot(decomp, ggplot2::aes(x = year, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) + ggplot2::geom_pointrange(linewidth = 0.25) +
      ggplot2::facet_wrap(~label, scales = "free_y", ncol = 2) +
      ggplot2::labs(title = "Vote-margin decomposition diagnostics", subtitle = "Full-control specification; CZ-clustered 95% intervals", x = NULL, y = "Coefficient") +
      ggplot2::theme_minimal(base_size = 10)
    write_plot_safely(p2, config$alternative_outcomes_decomposition_plot_png, config$alternative_outcomes_decomposition_plot_pdf, width = 10, height = 7)
  }
  invisible(coef_tbl)
}

# -----------------------------------------------------------------------------
# Shared county-to-CZ bridge for county-year auxiliary outcomes
# -----------------------------------------------------------------------------

build_generic_county_to_cz_panel <- function(config, county_year_data, value_cols) {
  config <- finalize_config(config)
  cz_path <- file.path(config$replication_dir, "cz-data", "cz-198090.xls")
  ftz_zip <- file.path(config$replication_dir, "ftz2024", "crosswalks", "CountyToCounty", "1990", "1990_csv.zip")
  cz_lookup <- readxl::read_xls(cz_path) %>%
    dplyr::transmute(county_fips_1990 = stringr::str_pad(as.character(`County FIPS Code`), width = 5, side = "left", pad = "0"), czone = as.integer(CZ90), pop1990 = safe_num(`Population 1990`))
  source_decades <- sort(unique(year_to_source_decade(county_year_data$year)))
  xwalk <- purrr::map_dfr(source_decades, ~ read_ftz_crosswalk(.x, ftz_zip, cz_lookup$county_fips_1990, config$crosswalk_weight, config$crosswalk_missing_weight_policy, config$renormalize_crosswalk_weights, config$weight_sum_tolerance)) %>%
    dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > 0)
  bridge_exceptions <- read_county_bridge_exceptions(config)
  county_year_data %>%
    dplyr::mutate(source_decade_default = year_to_source_decade(year)) %>%
    dplyr::left_join(
      bridge_exceptions %>% dplyr::select(year, county_fips, override_source_decade, override_county_fips_source),
      by = c("year", "county_fips")
    ) %>%
    dplyr::mutate(
      source_decade = dplyr::coalesce(override_source_decade, source_decade_default),
      county_fips_source = dplyr::coalesce(override_county_fips_source, county_fips)
    ) %>%
    dplyr::left_join(xwalk, by = c("source_decade", "county_fips_source"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(county_fips_1990), !is.na(crosswalk_weight), crosswalk_weight > 0) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(value_cols), ~ safe_num(.x) * crosswalk_weight)) %>%
    dplyr::group_by(year, county_fips_1990) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::inner_join(cz_lookup, by = "county_fips_1990") %>%
    dplyr::group_by(czone, year) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols), ~ sum(.x, na.rm = TRUE)), pop1990 = sum(pop1990, na.rm = TRUE), .groups = "drop")
}

# -----------------------------------------------------------------------------
# 3. NaNDA turnout and partisanship
# -----------------------------------------------------------------------------

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
  fips_source <- if (all(c("STCOFIPS10", "STCOFIPS20") %in% names(raw))) {
    dplyr::if_else(as.integer(raw$YEAR) >= 2020L, as.character(raw$STCOFIPS20), as.character(raw$STCOFIPS10))
  } else {
    as.character(raw[[fips_col]])
  }
  numeric_candidates <- c("REG_VOTERS", "BALLOTS_CAST", "CVAP", "PRES_DEM_VOTES", "PRES_REP_VOTES", "SEN_DEM_VOTES", "SEN_REP_VOTES")
  count_cols <- numeric_candidates[numeric_candidates %in% names(raw)]
  index_cols <- c("PARTISAN_INDEX_DEM", "PARTISAN_INDEX_REP")
  index_cols <- index_cols[index_cols %in% names(raw)]
  raw$COUNTY_FIPS_SELECTED_FOR_BRIDGE <- fips_source
  nanda <- raw %>%
    dplyr::transmute(county_fips = stringr::str_pad(as.character(COUNTY_FIPS_SELECTED_FOR_BRIDGE), 5, pad = "0"), year = as.integer(YEAR), dplyr::across(dplyr::all_of(c(count_cols, index_cols)), safe_num)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c(count_cols, index_cols)), ~ dplyr::na_if(.x, -1)))
  # Restrict to documented NaNDA availability; avoid legacy/structural zeros.
  nanda <- nanda %>% dplyr::filter(year >= 2004, year <= 2022)
  county_validation <- nanda %>%
    dplyr::mutate(
      reg_voters_pct_raw = if (all(c("REG_VOTERS", "CVAP") %in% names(nanda))) safe_divide(REG_VOTERS, CVAP) else NA_real_,
      voter_turnout_pct_raw = if (all(c("BALLOTS_CAST", "CVAP") %in% names(nanda))) safe_divide(BALLOTS_CAST, CVAP) else NA_real_,
      reg_voter_turnout_pct_raw = if (all(c("BALLOTS_CAST", "REG_VOTERS") %in% names(nanda))) safe_divide(BALLOTS_CAST, REG_VOTERS) else NA_real_,
      county_rate_flag = dplyr::if_else(reg_voters_pct_raw > 1.5 | voter_turnout_pct_raw > 1.5 | reg_voter_turnout_pct_raw > 1.5, TRUE, FALSE, missing = FALSE)
    )
  readr::write_csv(county_validation %>% dplyr::select(county_fips, year, dplyr::ends_with("_raw"), county_rate_flag), config$nanda_county_validation_csv)
  # Convert index shares to CVAP-weighted pseudo-counts before bridging.
  for (idx in index_cols) nanda[[paste0(idx, "_NUM")]] <- nanda[[idx]] * if ("CVAP" %in% count_cols) nanda$CVAP else 1
  bridge_cols <- unique(c(count_cols, paste0(index_cols, "_NUM")))
  bridge_cols <- bridge_cols[bridge_cols %in% names(nanda)]
  cz_panel <- build_generic_county_to_cz_panel(config, nanda, bridge_cols)
  if ("CVAP" %in% names(cz_panel)) {
    if ("REG_VOTERS" %in% names(cz_panel)) cz_panel$reg_voters_pct <- safe_divide(cz_panel$REG_VOTERS, cz_panel$CVAP)
    if ("BALLOTS_CAST" %in% names(cz_panel)) cz_panel$voter_turnout_pct <- safe_divide(cz_panel$BALLOTS_CAST, cz_panel$CVAP)
    if (all(c("BALLOTS_CAST", "REG_VOTERS") %in% names(cz_panel))) cz_panel$reg_voter_turnout_pct <- safe_divide(cz_panel$BALLOTS_CAST, cz_panel$REG_VOTERS)
    if (all(c("PRES_REP_VOTES", "PRES_DEM_VOTES") %in% names(cz_panel))) {
      denom <- cz_panel$PRES_REP_VOTES + cz_panel$PRES_DEM_VOTES
      cz_panel$pres_rep_ratio <- safe_divide(cz_panel$PRES_REP_VOTES, denom)
      cz_panel$pres_dem_ratio <- safe_divide(cz_panel$PRES_DEM_VOTES, denom)
    }
    for (idx in index_cols) {
      num <- paste0(idx, "_NUM")
      if (num %in% names(cz_panel)) cz_panel[[tolower(idx)]] <- safe_divide(cz_panel[[num]], cz_panel$CVAP)
    }
  }
  rate_cols <- c("reg_voters_pct", "voter_turnout_pct", "reg_voter_turnout_pct", "pres_rep_ratio", "pres_dem_ratio", "partisan_index_dem", "partisan_index_rep")
  rate_cols <- rate_cols[rate_cols %in% names(cz_panel)]
  cz_val <- cz_panel %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(rate_cols), ~ dplyr::if_else(.x < 0 | .x > 1.5 | !is.finite(.x), NA_real_, .x)))
  outlier_rows <- cz_panel %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(rate_cols), ~ !is.na(.x) & (!is.finite(.x) | .x < 0 | .x > 1.5))) %>%
    dplyr::select(czone, year, dplyr::all_of(rate_cols))
  readr::write_csv(outlier_rows, config$nanda_outlier_rates_csv)
  cz_summary <- cz_val %>%
    dplyr::summarise(n_cz = dplyr::n_distinct(czone), min_year = min(year, na.rm = TRUE), max_year = max(year, na.rm = TRUE), dplyr::across(dplyr::all_of(rate_cols), list(n_nonmissing = ~sum(!is.na(.x)), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}"))
  readr::write_csv(cz_summary, config$nanda_cz_validation_csv)
  readr::write_csv(cz_val, config$nanda_cz_panel_csv)
  adh <- load_adh_extension_data(config) %>% dplyr::select(czone, statefip, exposure = exposure_1990_2007, adh_weight, dplyr::all_of(config$baseline_controls), dplyr::starts_with("z_"))
  nanda_panel <- cz_val %>% dplyr::inner_join(adh, by = "czone") %>% dplyr::arrange(czone, year) %>% dplyr::group_by(czone) %>% dplyr::mutate(election_index = dplyr::dense_rank(year)) %>% dplyr::ungroup()
  outcomes <- rate_cols
  specs <- list(minimal = list(controls = character(0), standardize = FALSE), core_controls = list(controls = config$core_interacted_controls, standardize = TRUE), full_controls = list(controls = config$interacted_controls, standardize = TRUE))
  coef_tbl <- tibble::tibble(); status_tbl <- tibble::tibble()
  for (outcome in outcomes) {
    valid_years <- sort(unique(nanda_panel$year[!is.na(nanda_panel[[outcome]])]))
    ref <- if (2004 %in% valid_years) 2004L else if (length(valid_years) > 0) min(valid_years) else NA_integer_
    for (spec_name in names(specs)) {
      spec <- specs[[spec_name]]
      fit <- fit_single_outcome_event_study(nanda_panel, outcome, spec$controls, spec_name, config, ref_year = ref, standardize_controls = spec$standardize, sample_label = "nanda_2004_2022")
      coef_tbl <- dplyr::bind_rows(coef_tbl, fit$coefficients)
      status_tbl <- dplyr::bind_rows(status_tbl, tibble::tibble(module = "nanda_turnout_partisanship", outcome = outcome, spec = spec_name, reference_year = ref, status = fit$status, n_coefficients = nrow(fit$coefficients)))
    }
  }
  readr::write_csv(coef_tbl, config$nanda_outcomes_coefficients_csv)
  readr::write_csv(status_tbl, config$nanda_outcomes_status_csv)
  readr::write_csv(cz_summary, config$nanda_outcomes_summary_csv)
  if (nrow(coef_tbl) > 0) {
    p <- ggplot2::ggplot(coef_tbl, ggplot2::aes(x = year, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) + ggplot2::geom_pointrange(linewidth = 0.25) +
      ggplot2::facet_grid(outcome ~ spec, scales = "free_y") +
      ggplot2::labs(title = "NaNDA turnout and partisanship outcomes", subtitle = "County outcomes bridged to 1990 CZs; rates outside [0, 1.5] set missing; CZ-clustered intervals", x = NULL, y = "Coefficient on ADH exposure × year") +
      ggplot2::theme_minimal(base_size = 10)
    write_plot_safely(p, config$nanda_outcomes_plot_png, config$nanda_outcomes_plot_pdf, width = 12, height = 10)
  }
  invisible(coef_tbl)
}

# -----------------------------------------------------------------------------
# 4. ADHM 2020 political-supply/mechanism diagnostics
# -----------------------------------------------------------------------------

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
  spec_controls <- list(minimal = character(0), core_controls = model_control_names(config$core_interacted_controls, standardize = TRUE), full_controls = model_control_names(config$interacted_controls, standardize = TRUE))
  spec_controls <- purrr::map(spec_controls, ~ .x[.x %in% names(adh)])
  regressions <- tibble::tibble(); status <- tibble::tibble()
  outcome_dict <- tibble::tibble(
    outcome = c("d2_rwin_2002_2016", "d2_cfavg_repcon_2002_2016", "d2_cfavg_repmod_2002_2016", "d2_cfavg_demmod_2002_2016", "d2_cfavg_demlib_2002_2016", "d2_teaparty_2002_2010", "d_shnr_pres_2000_2016", "d_shnr_pres_2000_2008"),
    interpretation = c("Change in Republican House win indicator/share", "Change in conservative Republican CF-score share", "Change in moderate Republican CF-score share", "Change in moderate Democratic CF-score share", "Change in liberal Democratic CF-score share", "Change in Tea Party outcome", "Change in Republican presidential vote share, 2000-2016", "Change in Republican presidential vote share, 2000-2008")
  )
  readr::write_csv(outcome_dict, config$adhm2020_outcome_dictionary_csv)
  fit_cross_section <- function(df, outcomes, source, weight_var = NULL) {
    purrr::map_dfr(outcomes[outcomes %in% names(df)], function(outcome) {
      purrr::imap_dfr(spec_controls, function(ctrls, spec_name) {
        dat <- df %>% dplyr::inner_join(adh, by = "czone") %>% dplyr::filter(!is.na(.data[[outcome]]), !is.na(exposure_1990_2007))
        if (nrow(dat) == 0) {
          status <<- dplyr::bind_rows(status, tibble::tibble(source = source, outcome = outcome, spec = spec_name, status = "empty", n_obs = 0L, note = NA_character_))
          return(tibble::tibble())
        }
        rhs <- c("exposure_1990_2007", ctrls)
        fml <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
        mod <- tryCatch(fit_weighted_feols(fml, dat, weight_var = if (!is.null(weight_var) && weight_var %in% names(dat)) weight_var else "adh_weight", cluster_var = "statefip"), error = function(e) e)
        if (inherits(mod, "error")) {
          status <<- dplyr::bind_rows(status, tibble::tibble(source = source, outcome = outcome, spec = spec_name, status = "error", n_obs = nrow(dat), note = conditionMessage(mod)))
          return(tibble::tibble())
        }
        ct <- coeftable_to_tibble(mod) %>% dplyr::filter(term == "exposure_1990_2007")
        status <<- dplyr::bind_rows(status, tibble::tibble(source = source, outcome = outcome, spec = spec_name, status = "ok", n_obs = stats::nobs(mod), note = NA_character_))
        tibble::tibble(source = source, outcome = outcome, spec = spec_name, estimate = scalar_or_na(ct$estimate), std.error = scalar_or_na(ct$std.error), statistic = scalar_or_na(ct$statistic), p.value = scalar_or_na(ct$p.value), n_obs = stats::nobs(mod), controls = if (length(ctrls) == 0) "none" else paste(ctrls, collapse = "; "))
      })
    })
  }
  if (file.exists(house_path)) {
    house <- haven::read_dta(house_path) %>% dplyr::mutate(czone = as.integer(czone), sh_district_2002 = dplyr::coalesce(safe_num(sh_district_2002), 1))
    house_outcomes <- c("d2_rwin_2002_2016", "d2_cfavg_repcon_2002_2016", "d2_cfavg_repmod_2002_2016", "d2_cfavg_demmod_2002_2016", "d2_cfavg_demlib_2002_2016", "d2_teaparty_2002_2010")
    house_cz <- house %>% dplyr::group_by(czone) %>% dplyr::summarise(dplyr::across(dplyr::all_of(house_outcomes[house_outcomes %in% names(house)]), ~ weighted_mean_safe(.x, sh_district_2002)), .groups = "drop")
    regressions <- dplyr::bind_rows(regressions, fit_cross_section(house_cz, house_outcomes, "adhm2020_house_2002_2016"))
  }
  if (file.exists(pres_path)) {
    pres <- haven::read_dta(pres_path) %>% dplyr::mutate(czone = as.integer(czone), totvote_2000pres = dplyr::coalesce(safe_num(totvote_2000pres), 1))
    pres_cz <- pres %>%
      dplyr::group_by(czone) %>%
      dplyr::summarise(shnr_pres2000 = weighted_mean_safe(shnr_pres2000, totvote_2000pres), shnr_pres2008 = weighted_mean_safe(shnr_pres2008, totvote_2000pres), shnr_pres2016 = weighted_mean_safe(shnr_pres2016, totvote_2000pres), .groups = "drop") %>%
      dplyr::mutate(d_shnr_pres_2000_2016 = shnr_pres2016 - shnr_pres2000, d_shnr_pres_2000_2008 = shnr_pres2008 - shnr_pres2000)
    regressions <- dplyr::bind_rows(regressions, fit_cross_section(pres_cz, c("d_shnr_pres_2000_2016", "d_shnr_pres_2000_2008"), "adhm2020_president_2000_2016"))
  }
  readr::write_csv(regressions, config$adhm2020_regressions_csv)
  readr::write_csv(status, config$adhm2020_status_csv)
  hetero_vars <- c("l_sh_pop_white", "l_sh_popwht", "sh_pop_white", "l_sh_pop_black", "l_sh_pop_hispanic")
  avail <- tibble::tibble(candidate_variable = hetero_vars, available_in_current_ADH_extension_data = hetero_vars %in% names(adh), note = "If a majority-white or race-composition variable is available, add exposure × baseline-race heterogeneity to compare with ADHM 2020 heterogeneity results.")
  readr::write_csv(avail, config$adhm2020_heterogeneity_availability_csv)
  summary <- tibble::tibble(source = c("house_2002_2016", "president_2000_2016"), file_present = c(file.exists(house_path), file.exists(pres_path)), purpose = c("Political-supply and House ideology outcomes from ADHM public replication data", "Presidential-vote benchmark outcomes from ADHM public replication data"))
  readr::write_csv(summary, config$adhm2020_summary_csv)
  if (nrow(regressions) > 0) {
    p <- ggplot2::ggplot(regressions, ggplot2::aes(x = estimate, y = outcome, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error)) +
      ggplot2::geom_vline(xintercept = 0, linewidth = 0.25) + ggplot2::geom_pointrange(linewidth = 0.25) +
      ggplot2::facet_grid(source ~ spec, scales = "free_y", space = "free_y") +
      ggplot2::labs(title = "ADHM 2020 political-supply diagnostics", subtitle = "Cross-sectional CZ regressions on ADH exposure; CZ-level public ADHM outcomes", x = "Coefficient on ADH exposure", y = NULL) +
      ggplot2::theme_minimal(base_size = 10)
    write_plot_safely(p, config$adhm2020_plot_png, config$adhm2020_plot_pdf, width = 12, height = 7)
  }
  invisible(regressions)
}

# -----------------------------------------------------------------------------
# 6. Subperiod exposure
# -----------------------------------------------------------------------------

run_subperiod_exposure_event_studies <- function(config = CONFIG) {
  config <- finalize_config(config)
  adh_stack_path <- file.path(config$replication_dir, "adh2013", "dta", "workfile_china.dta")
  if (!file.exists(adh_stack_path) || !file.exists(config$analysis_panel_rds)) return(invisible(NULL))
  stack <- haven::read_dta(adh_stack_path)
  exp_period <- stack %>%
    dplyr::filter(yr %in% c(1990, 2000)) %>%
    dplyr::transmute(czone = as.integer(czone), period = dplyr::case_when(yr == 1990 ~ "1990_2000", yr == 2000 ~ "2000_2007", TRUE ~ as.character(yr)), exposure = safe_num(d_tradeusch_pw), instrument = safe_num(d_tradeotch_pw_lag)) %>%
    tidyr::pivot_wider(names_from = period, values_from = c(exposure, instrument), names_sep = "_")
  readr::write_csv(exp_period, config$subperiod_exposure_csv)
  readr::write_csv(exp_period %>% dplyr::summarise(dplyr::across(-czone, list(mean = ~mean(.x, na.rm = TRUE), sd = ~stats::sd(.x, na.rm = TRUE), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")), config$subperiod_exposure_summary_csv)
  adh <- load_adh_extension_data(config)
  fs <- exp_period %>% dplyr::inner_join(adh %>% dplyr::select(czone, adh_weight, statefip, dplyr::starts_with("z_")), by = "czone")
  fs_tbl <- tibble::tibble()
  for (pair in list(c("exposure_1990_2000", "instrument_1990_2000"), c("exposure_2000_2007", "instrument_2000_2007"))) {
    if (all(pair %in% names(fs))) {
      for (spec_name in c("minimal", "core_controls", "full_controls")) {
        ctrls <- if (spec_name == "minimal") character(0) else if (spec_name == "core_controls") model_control_names(config$core_interacted_controls, TRUE) else model_control_names(config$interacted_controls, TRUE)
        ctrls <- ctrls[ctrls %in% names(fs)]
        fml <- stats::as.formula(paste(pair[1], "~", paste(c(pair[2], ctrls), collapse = " + ")))
        mod <- tryCatch(fit_weighted_feols(fml, fs, weight_var = "adh_weight", cluster_var = "statefip"), error = function(e) NULL)
        if (!is.null(mod)) {
          ct <- coeftable_to_tibble(mod) %>% dplyr::filter(term == pair[2])
          fs_tbl <- dplyr::bind_rows(fs_tbl, tibble::tibble(exposure_period = gsub("exposure_", "", pair[1]), spec = spec_name, instrument = pair[2], estimate = scalar_or_na(ct$estimate), std.error = scalar_or_na(ct$std.error), statistic = scalar_or_na(ct$statistic), p.value = scalar_or_na(ct$p.value), first_stage_F_t2 = scalar_or_na(ct$statistic)^2, n_obs = stats::nobs(mod)))
        }
      }
    }
  }
  readr::write_csv(fs_tbl, config$subperiod_first_stage_csv)

  panel <- readRDS(config$analysis_panel_rds) %>% dplyr::left_join(exp_period, by = "czone")
  yrs <- config$event_years; ref <- config$reference_year
  for (ev in c("exposure_1990_2000", "exposure_2000_2007")) for (yr in setdiff(yrs, ref)) panel[[paste0(ev, "_x_", yr)]] <- panel[[ev]] * as.integer(panel$year == yr)
  specs <- list(minimal = list(controls = character(0), standardize = FALSE), core_controls = list(controls = config$core_interacted_controls, standardize = TRUE), full_controls = list(controls = config$interacted_controls, standardize = TRUE))
  coef_tbl <- tibble::tibble(); status_tbl <- tibble::tibble()
  for (spec_name in names(specs)) {
    spec <- specs[[spec_name]]
    dat <- panel
    ctrl_build <- apply_control_interactions(dat, spec$controls, yrs, ref, standardize = spec$standardize)
    dat <- ctrl_build$data
    event_terms <- unlist(lapply(c("exposure_1990_2000", "exposure_2000_2007"), function(ev) paste0(ev, "_x_", setdiff(yrs, ref))))
    fml <- stats::as.formula(paste("rep_margin ~", paste(c(event_terms, ctrl_build$terms), collapse = " + "), "| czone + year"))
    fit <- tryCatch(fixest::feols(fml, data = dat, weights = ~adh_weight, cluster = ~czone, panel.id = ~czone + election_index, warn = FALSE, notes = FALSE), error = function(e) e)
    if (inherits(fit, "error")) {
      status_tbl <- dplyr::bind_rows(status_tbl, tibble::tibble(spec = spec_name, status = "error", error_message = conditionMessage(fit), n_coefficients = 0L))
    } else {
      ct <- coeftable_to_tibble(fit) %>%
        dplyr::filter(term %in% event_terms) %>%
        dplyr::mutate(
          spec = spec_name,
          exposure_period = dplyr::case_when(startsWith(term, "exposure_1990_2000") ~ "1990-2000", startsWith(term, "exposure_2000_2007") ~ "2000-2007", TRUE ~ NA_character_),
          year = as.integer(stringr::str_extract(term, "[0-9]{4}$")),
          period_type = dplyr::case_when(year < 1990 ~ "placebo_pre_shock", year <= 2008 ~ "during_or_immediate_post_shock", year <= 2016 ~ "medium_run", TRUE ~ "long_run"),
          reference_year = ref,
          n_obs = stats::nobs(fit), n_cz = dplyr::n_distinct(dat$czone)
        ) %>%
        dplyr::select(spec, exposure_period, period_type, term, year, reference_year, estimate, std.error, statistic, p.value, n_obs, n_cz)
      coef_tbl <- dplyr::bind_rows(coef_tbl, ct)
      status_tbl <- dplyr::bind_rows(status_tbl, tibble::tibble(spec = spec_name, status = "ok", error_message = NA_character_, n_coefficients = nrow(ct)))
    }
  }
  readr::write_csv(coef_tbl, config$subperiod_event_study_coefficients_csv)
  readr::write_csv(status_tbl, config$subperiod_event_study_status_csv)
  equality <- tibble::tibble()
  if (nrow(coef_tbl) > 0) {
    wide <- coef_tbl %>% dplyr::select(spec, year, exposure_period, estimate, std.error) %>% tidyr::pivot_wider(names_from = exposure_period, values_from = c(estimate, std.error), names_sep = "_")
    if (all(c("estimate_1990-2000", "estimate_2000-2007") %in% names(wide))) {
      equality <- wide %>% dplyr::mutate(difference_early_minus_late = `estimate_1990-2000` - `estimate_2000-2007`, approx_se_difference = sqrt((`std.error_1990-2000`)^2 + (`std.error_2000-2007`)^2), approx_t_difference = difference_early_minus_late / approx_se_difference, approx_p_difference = 2 * stats::pnorm(-abs(approx_t_difference)), note = "Approximate test ignores covariance between early and late exposure coefficients; use as descriptive diagnostic.")
    }
  }
  readr::write_csv(equality, config$subperiod_equality_tests_csv)
  if (nrow(coef_tbl) > 0) {
    p <- ggplot2::ggplot(coef_tbl, ggplot2::aes(x = year, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, color = exposure_period)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) + ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.6), linewidth = 0.25) +
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
  write_pipeline_manifest(config = config, checks = checks, stage = "sixth_extensions", sources = list(adh2013 = list(path = file.path(config$replication_dir, "adh2013")), nanda = list(path = file.path(config$replication_dir, "outcomes-data", "nanda-2004-2022")), adhm2020 = list(path = file.path(config$replication_dir, "adhm2020"))), extra = list(extension_modules = c("bartik_identification", "alternative_political_outcomes", "nanda_turnout_partisanship", "adhm2020_political_supply", "subperiod_exposure"), crosswalk_used_for_extensions = paste0(config$crosswalk_weight, " / ", config$crosswalk_missing_weight_policy)))
  invisible(results)
}

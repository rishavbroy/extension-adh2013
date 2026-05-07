# replication/src/03_estimate_event_study.R

add_event_study_terms <- function(data, controls, config) {
  years <- sort(unique(data$year))
  years_est <- setdiff(years, config$reference_year)
  event_vars <- paste0("es_", years_est)
  for (i in seq_along(years_est)) {
    data[[event_vars[[i]]]] <- data[[config$exposure_var]] * as.integer(data$year == years_est[[i]])
  }
  ctrl_build <- make_interacted_control_vars(data, controls, years, config$reference_year)
  list(data = ctrl_build$data, event_vars = event_vars, control_vars = ctrl_build$vars)
}

residualized_design_diagnostics <- function(data, rhs_terms, config, sample_name, spec_name) {
  x <- as.matrix(data[, rhs_terms, drop = FALSE])
  fe <- stats::model.matrix(~ factor(czone) + factor(year) - 1, data = data)
  w <- data[[config$weight_var]]
  w <- ifelse(is.finite(w) & w > 0, sqrt(w), 1)
  xw <- x * w
  few <- fe * w
  fit <- stats::lm.fit(few, xw)
  resid_x <- fit$residuals
  qr_obj <- qr(resid_x, tol = 1e-7)
  singular_values <- tryCatch(svd(resid_x, nu = 0, nv = 0)$d, error = function(e) numeric(0))
  positive_sv <- singular_values[singular_values > 1e-10]
  condition_number <- if (length(positive_sv) == 0) NA_real_ else max(positive_sv) / min(positive_sv)
  tibble::tibble(
    sample = sample_name,
    spec = spec_name,
    n_obs = nrow(data),
    n_cz = dplyr::n_distinct(data$czone),
    n_years = dplyr::n_distinct(data$year),
    n_rhs_terms = length(rhs_terms),
    residualized_rank = qr_obj$rank,
    residualized_rank_deficiency = length(rhs_terms) - qr_obj$rank,
    condition_number = condition_number,
    min_singular_value = if (length(singular_values) == 0) NA_real_ else min(singular_values),
    max_singular_value = if (length(singular_values) == 0) NA_real_ else max(singular_values)
  )
}

vcov_eigen_summary <- function(vcov_mat) {
  if (is.null(vcov_mat) || any(!is.finite(vcov_mat))) {
    return(list(
      eigenvalues = NA_real_, min_eigenvalue = NA_real_,
      max_eigenvalue = NA_real_, repair_required = TRUE,
      n_negative_eigenvalues = NA_integer_,
      eigenvalue_ratio_abs_min_to_max = NA_real_,
      nonfinite = TRUE
    ))
  }
  ev <- eigen((vcov_mat + t(vcov_mat)) / 2, symmetric = TRUE, only.values = TRUE)$values
  min_ev <- min(ev)
  max_ev <- max(ev)
  list(
    eigenvalues = ev,
    min_eigenvalue = min_ev,
    max_eigenvalue = max_ev,
    repair_required = min_ev < -1e-8,
    n_negative_eigenvalues = sum(ev < 0),
    eigenvalue_ratio_abs_min_to_max = if (is.finite(max_ev) && max_ev > 0) abs(min_ev) / max_ev else NA_real_,
    nonfinite = FALSE
  )
}

extract_event_coefficients <- function(model, vcov_mat, spec_name, sample_name, se_type,
                                       repair_required, vcov_repaired) {
  ct <- fixest::coeftable(model, vcov = vcov_mat)
  ct_df <- data.frame(term = rownames(ct), ct, row.names = NULL, check.names = FALSE)
  names(ct_df)[names(ct_df) == "Estimate"] <- "estimate"
  names(ct_df)[names(ct_df) == "Std. Error"] <- "std.error"
  if ("Pr(>|t|)" %in% names(ct_df)) names(ct_df)[names(ct_df) == "Pr(>|t|)"] <- "p.value"
  if ("Pr(>|z|)" %in% names(ct_df)) names(ct_df)[names(ct_df) == "Pr(>|z|)"] <- "p.value"
  if ("t value" %in% names(ct_df)) names(ct_df)[names(ct_df) == "t value"] <- "statistic"
  if ("z value" %in% names(ct_df)) names(ct_df)[names(ct_df) == "z value"] <- "statistic"
  if (!("p.value" %in% names(ct_df))) ct_df$p.value <- NA_real_
  if (!("statistic" %in% names(ct_df))) ct_df$statistic <- NA_real_

  tibble::as_tibble(ct_df) %>%
    dplyr::filter(grepl("^es_", term)) %>%
    dplyr::mutate(
      sample = sample_name,
      spec = spec_name,
      se_type = se_type,
      year = as.integer(sub("^es_", "", term)),
      conf.low = estimate - stats::qnorm(0.975) * std.error,
      conf.high = estimate + stats::qnorm(0.975) * std.error,
      vcov_repair_required = repair_required,
      vcov_repaired = vcov_repaired
    ) %>%
    dplyr::select(
      sample, spec, se_type, term, year, estimate, std.error, conf.low, conf.high,
      statistic, p.value, vcov_repair_required, vcov_repaired
    ) %>%
    dplyr::arrange(sample, spec, se_type, year)
}

vcov_specifications <- function(config) {
  conley_specs <- purrr::map(config$conley_cutoffs_km, function(cutoff) {
    list(
      se_type = paste0("conley_", cutoff, "km"),
      compute = function(model, repair) {
        fixest::vcov_conley(
          model,
          lat = "lat",
          lon = "lon",
          cutoff = cutoff,
          distance = config$conley_distance,
          vcov_fix = repair
        )
      }
    )
  })
  c(
    list(
      list(
        se_type = "heteroskedastic",
        compute = function(model, repair) fixest::vcov_hetero(model, vcov_fix = repair)
      ),
      list(
        se_type = "cluster_cz",
        compute = function(model, repair) {
          fixest::vcov_cluster(model, cluster = stats::as.formula("~czone"), vcov_fix = repair)
        }
      ),
      list(
        se_type = "cluster_state",
        compute = function(model, repair) {
          fixest::vcov_cluster(model, cluster = stats::as.formula("~statefip"), vcov_fix = repair)
        }
      ),
      list(
        se_type = "cluster_cz_year",
        compute = function(model, repair) {
          fixest::vcov_cluster(model, cluster = stats::as.formula("~czone + year"), vcov_fix = repair)
        }
      ),
      list(
        se_type = paste0("newey_west_lag", config$nw_lag),
        compute = function(model, repair) {
          fixest::vcov_NW(
            model, unit = "czone", time = "election_index",
            lag = config$nw_lag, vcov_fix = repair
          )
        }
      ),
      list(
        se_type = paste0("driscoll_kraay_lag", config$dk_lag),
        compute = function(model, repair) {
          fixest::vcov_DK(model, time = "election_index", lag = config$dk_lag, vcov_fix = repair)
        }
      )
    ),
    conley_specs
  )
}

compute_vcov_outputs <- function(model, spec_name, sample_name, config) {
  specs <- vcov_specifications(config)
  coefficients <- list()
  diagnostics <- list()
  eigenvalues <- list()
  vcov_mats <- list()

  for (s in specs) {
    se_type <- s$se_type
    raw <- tryCatch(s$compute(model, FALSE), error = function(e) e)
    if (inherits(raw, "error")) {
      diagnostics[[se_type]] <- tibble::tibble(
        sample = sample_name, spec = spec_name, se_type = se_type,
        status = "failed", error_message = conditionMessage(raw),
        min_eigenvalue = NA_real_, max_eigenvalue = NA_real_,
        n_negative_eigenvalues = NA_integer_,
        eigenvalue_ratio_abs_min_to_max = NA_real_,
        repair_required = TRUE, vcov_repaired = FALSE
      )
      next
    }

    eig <- vcov_eigen_summary(raw)
    eigenvalues[[se_type]] <- tibble::tibble(
      sample = sample_name,
      spec = spec_name,
      se_type = se_type,
      eigen_index = seq_along(eig$eigenvalues),
      eigenvalue = eig$eigenvalues
    )

    use_vcov <- raw
    repaired <- FALSE
    status <- "ok"
    err <- NA_character_

    if (isTRUE(eig$repair_required)) {
      status <- "repair_required"
      err <- "Unrepaired VCOV is not positive semidefinite."
      if (isTRUE(config$allow_vcov_repair)) {
        repaired_try <- tryCatch(s$compute(model, TRUE), error = function(e) e)
        if (inherits(repaired_try, "error")) {
          status <- "failed"
          err <- conditionMessage(repaired_try)
        } else {
          use_vcov <- repaired_try
          repaired <- TRUE
          status <- "repaired"
        }
      }
    }

    diagnostics[[se_type]] <- tibble::tibble(
      sample = sample_name, spec = spec_name, se_type = se_type,
      status = status, error_message = err,
      min_eigenvalue = eig$min_eigenvalue, max_eigenvalue = eig$max_eigenvalue,
      n_negative_eigenvalues = eig$n_negative_eigenvalues,
      eigenvalue_ratio_abs_min_to_max = eig$eigenvalue_ratio_abs_min_to_max,
      repair_required = eig$repair_required, vcov_repaired = repaired
    )

    if (status %in% c("ok", "repaired")) {
      coef_tbl <- tryCatch(
        extract_event_coefficients(
          model = model, vcov_mat = use_vcov, spec_name = spec_name,
          sample_name = sample_name, se_type = se_type,
          repair_required = eig$repair_required, vcov_repaired = repaired
        ),
        error = function(e) e
      )
      if (inherits(coef_tbl, "error")) {
        diagnostics[[se_type]]$status <- "failed"
        diagnostics[[se_type]]$error_message <- conditionMessage(coef_tbl)
      } else {
        coefficients[[se_type]] <- coef_tbl
        vcov_mats[[se_type]] <- use_vcov
      }
    }
  }

  list(
    coefficients = dplyr::bind_rows(coefficients),
    diagnostics = dplyr::bind_rows(diagnostics),
    eigenvalues = dplyr::bind_rows(eigenvalues),
    vcov = vcov_mats
  )
}

controls_manifest_for_spec <- function(sample_name, spec_name, controls, config, model) {
  dropped <- model$collin.var %||% character(0)
  tibble::tibble(control = config$baseline_controls) %>%
    dplyr::mutate(
      sample = sample_name,
      spec = spec_name,
      requested = control %in% controls,
      status = dplyr::case_when(
        !requested ~ "omitted_from_spec",
        vapply(control, function(ctrl) any(startsWith(dropped, paste0("ctrl_", ctrl, "_"))), logical(1)) ~
          "dropped_by_fixest",
        TRUE ~ "included"
      )
    ) %>%
    dplyr::select(sample, spec, control, requested, status)
}

estimate_one_spec <- function(data, sample_name, spec_name, controls, controls_label, config) {
  prepared <- add_event_study_terms(data, controls, config)
  rhs_terms <- c(prepared$event_vars, prepared$control_vars)
  formula_chr <- paste0(config$outcome_var, " ~ ", paste(rhs_terms, collapse = " + "), " | czone + year")
  design_diag <- residualized_design_diagnostics(prepared$data, rhs_terms, config, sample_name, spec_name)

  tryCatch({
    message("Estimating ", sample_name, " / ", spec_name)
    model <- fixest::feols(
      fml = stats::as.formula(formula_chr),
      data = prepared$data,
      weights = stats::as.formula(paste0("~", config$weight_var)),
      warn = TRUE,
      notes = FALSE
    )
    vcovs <- compute_vcov_outputs(model, spec_name, sample_name, config)
    main_vcov_diag <- vcovs$diagnostics %>% dplyr::filter(se_type == config$main_se_type)
    main_vcov_failed <- nrow(main_vcov_diag) == 0 ||
      main_vcov_diag$status %in% c("failed", "repair_required") ||
      (isTRUE(config$require_no_vcov_repair_for_main) && isTRUE(main_vcov_diag$repair_required[[1]]))
    status <- if (main_vcov_failed) "failed" else "ok"
    error_message <- if (main_vcov_failed) {
      paste0("Main SE ", config$main_se_type, " failed or required VCOV repair.")
    } else {
      NA_character_
    }

    model_stats <- list(
      "Observations" = format(nrow(prepared$data), big.mark = ","),
      "Commuting zones" = format(dplyr::n_distinct(prepared$data$czone), big.mark = ","),
      "Election years" = paste0(min(prepared$data$year), "--", max(prepared$data$year)),
      "Reference election year" = as.character(config$reference_year),
      "CZ fixed effects" = "Yes",
      "Election-year fixed effects" = "Yes",
      "Controls" = controls_label,
      "Sample" = sample_name,
      "ADH weights" = "timepwt48",
      "Main SE type" = config$main_se_type
    )

    list(
      sample = sample_name, spec = spec_name, status = status, error_message = error_message,
      formula_chr = formula_chr, model = model, vcov = vcovs$vcov,
      coefficients = vcovs$coefficients, vcov_diagnostics = vcovs$diagnostics,
      vcov_eigenvalues = vcovs$eigenvalues, model_diagnostics = design_diag,
      controls_manifest = controls_manifest_for_spec(sample_name, spec_name, controls, config, model),
      model_stats = model_stats, n_obs = nrow(prepared$data),
      n_cz = dplyr::n_distinct(prepared$data$czone), min_year = min(prepared$data$year),
      max_year = max(prepared$data$year)
    )
  }, error = function(e) {
    list(
      sample = sample_name, spec = spec_name, status = "failed",
      error_message = conditionMessage(e), formula_chr = formula_chr,
      model = NULL, vcov = list(), coefficients = tibble::tibble(),
      vcov_diagnostics = tibble::tibble(), vcov_eigenvalues = tibble::tibble(),
      model_diagnostics = design_diag, controls_manifest = tibble::tibble(),
      model_stats = list(
        "Observations" = format(nrow(prepared$data), big.mark = ","),
        "Commuting zones" = format(dplyr::n_distinct(prepared$data$czone), big.mark = ","),
        "Election years" = paste0(min(prepared$data$year), "--", max(prepared$data$year)),
        "Reference election year" = as.character(config$reference_year),
        "CZ fixed effects" = "Yes",
        "Election-year fixed effects" = "Yes",
        "Controls" = controls_label,
        "Sample" = sample_name,
        "ADH weights" = "timepwt48",
        "Main SE type" = "Failed"
      ),
      n_obs = nrow(prepared$data), n_cz = dplyr::n_distinct(prepared$data$czone),
      min_year = min(prepared$data$year), max_year = max(prepared$data$year)
    )
  })
}

run_pretrend_tests <- function(results, config) {
  purrr::map_dfr(results, function(res) {
    if (is.null(res$model) || res$sample != "main_1972_start") return(tibble::tibble())
    main_vcov <- res$vcov[[config$main_se_type]]
    if (is.null(main_vcov)) return(tibble::tibble())
    pre_years <- sort(unique(res$coefficients$year[res$coefficients$year < config$reference_year]))
    if (length(pre_years) == 0) return(tibble::tibble())
    keep_pattern <- paste0("^es_(", paste(pre_years, collapse = "|"), ")$")
    wt <- tryCatch(
      fixest::wald(res$model, keep = keep_pattern, vcov = main_vcov, print = FALSE),
      error = function(e) e
    )
    if (inherits(wt, "error")) {
      tibble::tibble(
        sample = res$sample, spec = res$spec, se_type = config$main_se_type,
        pre_years = paste(pre_years, collapse = ","),
        statistic = NA_real_, p.value = NA_real_, df1 = NA_real_, df2 = NA_real_,
        status = "failed", error_message = conditionMessage(wt)
      )
    } else {
      tibble::tibble(
        sample = res$sample, spec = res$spec, se_type = config$main_se_type,
        pre_years = paste(pre_years, collapse = ","),
        statistic = wt$stat, p.value = wt$p, df1 = wt$df1, df2 = wt$df2,
        status = "ok", error_message = NA_character_
      )
    }
  })
}

estimate_event_study <- function(config = CONFIG, stop_on_fatal = TRUE) {
  load_required_packages()
  config <- finalize_config(config)

  existing_checks <- if (file.exists(config$validation_checks_csv)) {
    readr::read_csv(config$validation_checks_csv, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }
  if (nrow(existing_checks) > 0 && has_fatal_failures(existing_checks) && isTRUE(stop_on_fatal)) {
    stop("Fatal build-stage validation checks are present. Rebuild with a nonfatal policy before estimating.", call. = FALSE)
  }

  main_panel <- readRDS(config$analysis_panel_rds)
  diagnostic_panel <- readRDS(config$diagnostic_panel_rds)

  spec_definitions <- list(
    minimal_full_panel = list(
      controls = character(0),
      label = "None"
    ),
    interacted_core_controls_diagnostic = list(
      controls = config$core_interacted_controls,
      label = "1990 manufacturing share, college share, and foreign-born share interacted with election-year indicators"
    ),
    interacted_controls_full_panel = list(
      controls = config$interacted_controls,
      label = "Configured 1990 ADH baseline controls interacted with election-year indicators"
    )
  )

  samples <- list(
    main_1972_start = main_panel,
    diagnostic_1952_start = diagnostic_panel
  )

  all_results <- list()
  for (sample_name in names(samples)) {
    for (spec_name in names(spec_definitions)) {
      def <- spec_definitions[[spec_name]]
      key <- paste(sample_name, spec_name, sep = "__")
      all_results[[key]] <- estimate_one_spec(
        data = samples[[sample_name]],
        sample_name = sample_name,
        spec_name = spec_name,
        controls = def$controls,
        controls_label = def$label,
        config = config
      )
    }
  }

  status_tbl <- purrr::map_dfr(all_results, ~ tibble::tibble(
    sample = .x$sample, spec = .x$spec, status = .x$status, error_message = .x$error_message,
    n_obs = .x$n_obs, n_cz = .x$n_cz, min_year = .x$min_year, max_year = .x$max_year,
    formula_chr = .x$formula_chr
  ))
  coef_tbl_all <- purrr::map_dfr(all_results, ~ .x$coefficients)
  model_diagnostics <- purrr::map_dfr(all_results, ~ {
    dropped <- if (is.null(.x$model)) character(0) else (.x$model$collin.var %||% character(0))
    .x$model_diagnostics %>%
      dplyr::mutate(
        fixest_dropped_variables = paste(dropped, collapse = "; "),
        fixest_dropped_count = length(dropped)
      )
  })
  controls_manifest <- purrr::map_dfr(all_results, ~ .x$controls_manifest)
  vcov_diagnostics <- purrr::map_dfr(all_results, ~ .x$vcov_diagnostics)
  vcov_eigenvalues <- purrr::map_dfr(all_results, ~ .x$vcov_eigenvalues)
  pretrend_tests <- run_pretrend_tests(all_results, config)
  pretrend_coefficients <- coef_tbl_all %>%
    dplyr::filter(sample == "main_1972_start", se_type == config$main_se_type, year < config$reference_year)

  rank_failures <- model_diagnostics %>%
    dplyr::filter(residualized_rank_deficiency > 0)
  main_vcov_problem_count <- vcov_diagnostics %>%
    dplyr::filter(
      sample == "main_1972_start",
      spec %in% c("minimal_full_panel", "interacted_controls_full_panel"),
      se_type == config$main_se_type,
      !status %in% c("ok", "repaired")
    ) %>%
    nrow()
  main_vcov_repair_count <- vcov_diagnostics %>%
    dplyr::filter(
      sample == "main_1972_start",
      spec %in% c("minimal_full_panel", "interacted_controls_full_panel"),
      se_type == config$main_se_type,
      repair_required %in% TRUE
    ) %>%
    nrow()
  conley_repair_count <- vcov_diagnostics %>%
    dplyr::filter(grepl("^conley_", se_type), repair_required %in% TRUE) %>%
    nrow()

  build_checks <- if (file.exists(config$validation_checks_csv)) {
    readr::read_csv(config$validation_checks_csv, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }
  estimation_checks <- dplyr::bind_rows(
    validation_check(
      "event_study_residualized_design_full_rank",
      nrow(rank_failures) == 0,
      fatal = TRUE,
      n_failed = nrow(rank_failures),
      details = "Requires no residualized RHS rank deficiency in 1952-start or 1972-start specifications."
    ),
    validation_check(
      "event_study_main_vcov_available_without_repair",
      main_vcov_problem_count == 0 &&
        (!isTRUE(config$require_no_vcov_repair_for_main) || main_vcov_repair_count == 0),
      fatal = TRUE,
      n_failed = main_vcov_problem_count + main_vcov_repair_count,
      details = paste0("Main SE type: ", config$main_se_type)
    ),
    validation_check(
      "event_study_conley_repair_diagnostics_recorded",
      TRUE,
      fatal = FALSE,
      n_failed = conley_repair_count,
      details = paste0("Conley VCOVs requiring repair across all samples/specs/cutoffs: ", conley_repair_count)
    )
  )
  combined_checks <- dplyr::bind_rows(build_checks, estimation_checks)
  readr::write_csv(combined_checks, config$validation_checks_csv)

  saveRDS(all_results, config$all_specs_rds)
  readr::write_csv(status_tbl, config$spec_status_csv)
  readr::write_csv(coef_tbl_all, config$all_specs_csv)
  readr::write_csv(model_diagnostics, config$model_diagnostics_csv)
  readr::write_csv(controls_manifest, config$controls_manifest_csv)
  readr::write_csv(vcov_diagnostics, config$vcov_diagnostics_csv)
  readr::write_csv(vcov_eigenvalues, config$vcov_eigenvalues_csv)
  readr::write_csv(pretrend_tests, config$pretrend_tests_csv)
  readr::write_csv(pretrend_coefficients, config$pretrend_coefficients_csv)

  main_key <- "main_1972_start__minimal_full_panel"
  if (all_results[[main_key]]$status == "ok") {
    main_out <- list(
      model = all_results[[main_key]]$model,
      vcov = all_results[[main_key]]$vcov[[config$main_se_type]],
      coefficients = all_results[[main_key]]$coefficients %>%
        dplyr::filter(se_type == config$main_se_type),
      model_stats = all_results[[main_key]]$model_stats,
      formula_chr = all_results[[main_key]]$formula_chr
    )
    saveRDS(main_out, config$event_study_rds)
    readr::write_csv(main_out$coefficients, config$event_study_csv)
  }

  failed_main_vcov <- vcov_diagnostics %>%
    dplyr::filter(
      sample == "main_1972_start",
      spec %in% c("minimal_full_panel", "interacted_controls_full_panel"),
      se_type == config$main_se_type,
      !status %in% c("ok", "repaired")
    )

  write_pipeline_manifest(
    config = config,
    checks = combined_checks,
    stage = "estimate_event_study",
    sources = list(
      analysis_panel = list(path = config$analysis_panel_rds, md5 = file_checksum(config$analysis_panel_rds)),
      diagnostic_panel = list(path = config$diagnostic_panel_rds, md5 = file_checksum(config$diagnostic_panel_rds))
    ),
    extra = list(
      specs = status_tbl,
      conley_repair_count = conley_repair_count,
      main_vcov_problem_count = main_vcov_problem_count,
      main_vcov_repair_count = main_vcov_repair_count
    )
  )

  if (nrow(failed_main_vcov) > 0 && isTRUE(stop_on_fatal) && !isTRUE(config$allow_failed_diagnostics)) {
    stop(
      "Main event-study VCOV failed or required repair. See ",
      config$vcov_diagnostics_csv,
      call. = FALSE
    )
  }

  if (has_fatal_failures(combined_checks) && isTRUE(stop_on_fatal) && !isTRUE(config$allow_failed_diagnostics)) {
    stop("Fatal event-study validation checks failed. See ", config$validation_checks_csv, call. = FALSE)
  }

  invisible(list(config = config, results = all_results, status = status_tbl, coefficients = coef_tbl_all))
}

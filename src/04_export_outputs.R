# extension-adh2013/src/04_export_outputs.R

export_event_study_outputs <- function(config = CONFIG) {
  load_required_packages(config)
  config <- finalize_config(config)

  checks <- if (file.exists(config$validation_checks_csv)) {
    readr::read_csv(config$validation_checks_csv, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }
  if (nrow(checks) > 0 && has_fatal_failures(checks) && !isTRUE(config$allow_failed_diagnostics)) {
    stop("Refusing to export figures/tables because fatal validation checks are present.", call. = FALSE)
  }
  has_validation_warnings <- nrow(checks) > 0 && has_any_failures(checks)

  res_all <- read_model_object(config$all_specs_rds)
  spec_order <- c("minimal_full_panel", "interacted_core_controls_diagnostic", "interacted_controls_full_panel")
  pretty_spec_names <- c(
    minimal_full_panel = "Minimal",
    interacted_core_controls_diagnostic = "Core controls",
    interacted_controls_full_panel = "Full controls"
  )

  ok_keys <- names(res_all)[vapply(res_all, function(x) {
    x$sample == "main_1972_start" && x$spec %in% spec_order && x$status == "ok"
  }, logical(1))]
  if (length(ok_keys) == 0) stop("No successful main specifications found in ", config$all_specs_rds)

  coef_tbl_all <- purrr::map_dfr(ok_keys, function(k) {
    res_all[[k]]$coefficients %>%
      dplyr::filter(se_type == config$main_se_type) %>%
      dplyr::mutate(spec_label = pretty_spec_names[spec])
  })

  table_csv <- coef_tbl_all %>%
    dplyr::transmute(
      sample, spec, spec_label, se_type, year, term, estimate, std.error,
      conf.low, conf.high, statistic, p.value, vcov_repair_required, vcov_repaired
    ) %>%
    dplyr::arrange(factor(spec, levels = spec_order), year)
  readr::write_csv(table_csv, config$table_csv)
  uses_repaired_main_vcov <- any(table_csv$vcov_repaired %in% TRUE, na.rm = TRUE)

  event_years_display <- sort(unique(c(coef_tbl_all$year, config$reference_year)))
  coef_cell <- function(df, year_value) {
    row <- df %>% dplyr::filter(year == year_value)
    if (year_value == config$reference_year) return("--")
    if (nrow(row) == 0) return("")
    paste0(fmt_num(row$estimate[[1]]), star_code(row$p.value[[1]]), " (", fmt_num(row$std.error[[1]]), ")")
  }

  table_body <- tibble::tibble(`Election year` = as.character(event_years_display))
  for (s in spec_order) {
    df_s <- coef_tbl_all %>% dplyr::filter(spec == s)
    if (nrow(df_s) > 0) {
      table_body[[pretty_spec_names[[s]]]] <- vapply(event_years_display, function(y) {
        coef_cell(df_s, y)
      }, character(1))
    }
  }

  stat_names <- c(
    "Observations", "Commuting zones", "Election years", "Reference election year",
    "CZ fixed effects", "Election-year fixed effects", "Controls", "Sample",
    "ADH weights", "Controls standardized", "Main SE type"
  )
  get_stat_value <- function(ms, nm) {
    if (is.null(ms) || is.null(ms[[nm]]) || length(ms[[nm]]) == 0) return("")
    as.character(ms[[nm]][1])
  }
  stat_rows <- tibble::tibble(`Election year` = stat_names)
  for (s in spec_order) {
    key <- ok_keys[vapply(ok_keys, function(k) res_all[[k]]$spec == s, logical(1))]
    if (length(key) > 0) {
      vals <- vapply(stat_names, function(nm) get_stat_value(res_all[[key[[1]]]]$model_stats, nm), character(1))
      stat_rows[[pretty_spec_names[[s]]]] <- vals
    }
  }

  table_for_tex <- dplyr::bind_rows(table_body, stat_rows)
  fallback_label <- dplyr::case_when(
    config$crosswalk_missing_weight_policy == "fallback_m2" ~ "M2 fallback",
    config$crosswalk_missing_weight_policy == "fallback_m4" ~ "M4 fallback",
    config$crosswalk_missing_weight_policy == "identity_if_same_fips" ~ "same-FIPS identity fallback",
    TRUE ~ config$crosswalk_missing_weight_policy
  )
  crosswalk_note <- if (identical(config$crosswalk_missing_weight_policy, "fail")) {
    paste0(toupper(config$crosswalk_weight_slug), " weights with no fallback")
  } else {
    paste0(
      toupper(config$crosswalk_weight_slug),
      " weights where valid, with ", fallback_label,
      " for source counties whose selected weights are undefined or invalid"
    )
  }

  notes_text <- paste0(
    if (uses_repaired_main_vcov || has_validation_warnings) {
      "PROVISIONAL: at least one validation check failed or a numerical repair/warning was recorded; treat these as diagnostic output until reviewed. "
    } else {
      ""
    },
    "Each coefficient is the interaction between ADH's 1990-2007 import-exposure measure and the indicated presidential-election year, ",
    "with ", config$reference_year, " omitted as the reference year. Exposure is measured in ",
    config$exposure_units, ". The dependent variable is the commuting-zone Republican presidential vote margin, ",
    "defined as Republican minus Democratic votes divided by two-party votes. County presidential returns are from Amlani and Algara (2021), ",
    "bridged to 1990 counties with the Ferrara, Testa, and Zhou (2024) ",
    crosswalk_note, ", then aggregated to 1990 commuting zones. ",
    "Standard errors use ", config$main_se_type, ". Significance stars: *** p<0.01, ** p<0.05, * p<0.10."
  )

  latex_tbl <- knitr::kable(
    table_for_tex,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    escape = TRUE,
    caption = paste0(
      if (uses_repaired_main_vcov || has_validation_warnings) "Provisional " else "",
      "event-study estimates: ADH China exposure and Republican presidential vote margin"
    ),
    align = c("l", rep("c", ncol(table_for_tex) - 1))
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("repeat_header", "hold_position"),
      position = "center",
      font_size = 9
    ) %>%
    kableExtra::footnote(general = notes_text, threeparttable = TRUE, escape = TRUE)

  writeLines(as.character(latex_tbl), config$table_tex)
  render_table_pdf(config$table_tex, config$table_standalone_tex, config$table_pdf)

  plot_tbl <- coef_tbl_all %>%
    dplyr::mutate(
      spec_label = factor(spec_label, levels = c("Minimal", "Core controls", "Full controls")),
      line_group = ifelse(year < config$reference_year, "pre", "post")
    )
  ref_rows <- plot_tbl %>%
    dplyr::distinct(sample, spec, se_type, spec_label) %>%
    dplyr::mutate(
      term = paste0("es_", config$reference_year),
      year = config$reference_year,
      estimate = 0,
      std.error = NA_real_,
      conf.low = 0,
      conf.high = 0,
      statistic = NA_real_,
      p.value = NA_real_,
      vcov_repair_required = FALSE,
      vcov_repaired = FALSE,
      line_group = "reference"
    )
  plot_with_ref <- dplyr::bind_rows(plot_tbl, ref_rows)
  line_tbl <- dplyr::bind_rows(
    plot_with_ref %>%
      dplyr::filter(year <= config$reference_year) %>%
      dplyr::mutate(line_group = "pre"),
    plot_with_ref %>%
      dplyr::filter(year >= config$reference_year) %>%
      dplyr::mutate(line_group = "post")
  )

  period_bands <- tibble::tibble(
    xmin = c(1990, 2008, 2016, 2020),
    xmax = c(2007, 2012, 2019.8, 2020.8),
    label = c("ADH exposure", "Great Recession / Obama", "Trump era", "COVID era"),
    fill = c("#9ecae1", "#fdd0a2", "#c7e9c0", "#dadaeb")
  )

  event_plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = period_bands,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = label),
      alpha = 0.16,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = config$reference_year, linetype = "dotted", linewidth = 0.35) +
    ggplot2::geom_errorbar(
      data = plot_tbl,
      ggplot2::aes(x = year, ymin = conf.low, ymax = conf.high),
      width = 0.6,
      linewidth = 0.35
    ) +
    ggplot2::geom_line(
      data = line_tbl,
      ggplot2::aes(x = year, y = estimate, group = interaction(spec_label, line_group)),
      linewidth = 0.4
    ) +
    ggplot2::geom_point(
      data = plot_with_ref,
      ggplot2::aes(x = year, y = estimate),
      size = 1.8
    ) +
    ggplot2::facet_wrap(~ spec_label, ncol = 1) +
    ggplot2::scale_fill_manual(values = stats::setNames(period_bands$fill, period_bands$label), name = NULL) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(plot_with_ref$year), max(plot_with_ref$year), by = 8),
      minor_breaks = seq(min(plot_with_ref$year), max(plot_with_ref$year), by = 4)
    ) +
    ggplot2::labs(
      title = "China exposure and Republican presidential vote margin, 1972-2020",
      subtitle = paste0("Event-study coefficients with 95% ", config$main_se_type, " CIs; ", config$reference_year, " normalized to zero."),
      x = "Presidential election year",
      y = paste0("Coefficient on ADH China exposure (per ", config$exposure_units, ")")
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  ggplot2::ggsave(filename = config$figure_pdf, plot = event_plot, width = 8.5, height = 7.5, bg = "white")
  ggplot2::ggsave(filename = config$figure_png, plot = event_plot, width = 8.5, height = 7.5, dpi = 320, bg = "white")

  write_pipeline_manifest(
    config = config,
    checks = checks,
    stage = "export_event_study_outputs",
    sources = list(
      all_specs = model_object_reference(config$all_specs_rds, config)
    ),
    extra = list(
      outputs = list(
        table_csv = list(path = config$table_csv, md5 = file_checksum(config$table_csv)),
        table_tex = list(path = config$table_tex, md5 = file_checksum(config$table_tex)),
        table_pdf = list(path = config$table_pdf, md5 = file_checksum(config$table_pdf)),
        figure_pdf = list(path = config$figure_pdf, md5 = file_checksum(config$figure_pdf)),
        figure_png = list(path = config$figure_png, md5 = file_checksum(config$figure_png))
      ),
      validation_warnings = has_validation_warnings
    )
  )

  invisible(list(table_csv = table_csv, table_tex = config$table_tex, figure = event_plot))
}

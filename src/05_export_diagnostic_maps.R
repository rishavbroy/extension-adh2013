# extension-adh2013/src/05_export_diagnostic_maps.R

crosswalk_sensitivity_plan <- function() {
  tibble::tribble(
    ~crosswalk_weight, ~crosswalk_missing_weight_policy, ~label, ~render_support_maps,
    "m5_weight", "fallback_m2", "M5 + fallback M2", TRUE,
    "m6_weight", "fallback_m2", "M6 + fallback M2", FALSE,
    "m2_weight", "fallback_m2", "Pure M2", FALSE,
    "m4_weight", "fallback_m2", "M4 + fallback M2", FALSE
  )
}

label_from_crosswalk_slug <- function(slug) {
  dplyr::case_when(
    slug == "m5" ~ "M5 + fallback M2",
    slug == "m6" ~ "M6 + fallback M2",
    slug == "m2" ~ "Pure M2",
    slug == "m4" ~ "M4 + fallback M2",
    TRUE ~ toupper(slug)
  )
}

export_crosswalk_specification_comparison <- function(config = CONFIG) {
  load_required_packages(config)
  config <- finalize_config(config)

  coef_files <- list.files(
    config$intermediate_dir,
    pattern = "^event_study_coefficients_all_specs_aa2021_rep_margin_.*\\.csv$",
    full.names = TRUE
  )
  if (length(coef_files) == 0) {
    warning("No all-spec coefficient CSVs found; skipping crosswalk specification comparison plot.", call. = FALSE)
    return(invisible(NULL))
  }

  crosswalk_levels <- c("M5 + fallback M2", "M6 + fallback M2", "Pure M2", "M4 + fallback M2")
  display_specs <- c("minimal_full_panel", "interacted_core_controls_diagnostic", "interacted_controls_full_panel")

  coef_long <- purrr::map_dfr(coef_files, function(path) {
    slug <- sub("^event_study_coefficients_all_specs_aa2021_rep_margin_", "", basename(path))
    slug <- sub("\\.csv$", "", slug)
    readr::read_csv(path, show_col_types = FALSE) %>%
      dplyr::mutate(
        crosswalk_specification = label_from_crosswalk_slug(slug),
        crosswalk_slug = slug
      )
  }) %>%
    dplyr::filter(
      sample == "main_1972_start",
      se_type == config$main_se_type,
      spec %in% display_specs
    ) %>%
    dplyr::mutate(
      spec_label = dplyr::recode(
        spec,
        minimal_full_panel = "Minimal",
        interacted_core_controls_diagnostic = "Core controls",
        interacted_controls_full_panel = "Full controls"
      ),
      spec_label = factor(spec_label, levels = c("Minimal", "Core controls", "Full controls")),
      crosswalk_specification = factor(crosswalk_specification, levels = crosswalk_levels)
    ) %>%
    dplyr::filter(!is.na(crosswalk_specification))

  if (nrow(coef_long) == 0) {
    warning("Crosswalk comparison inputs exist but no matching main-spec coefficients were found.", call. = FALSE)
    return(invisible(NULL))
  }

  # Draw thicker baseline/early layers first and thinner layers last. When the
  # four crosswalk estimates are nearly identical, this makes the overlap show
  # up as concentric colored strokes rather than one hidden line.
  draw_order_tbl <- tibble::tibble(
    crosswalk_specification = factor(crosswalk_levels, levels = crosswalk_levels),
    draw_order = c(4L, 3L, 1L, 2L),
    line_width = c(0.75, 0.95, 1.45, 1.20),
    point_size = c(1.25, 1.45, 2.05, 1.75),
    alpha_value = c(0.95, 0.90, 0.70, 0.82)
  )

  coef_long <- coef_long %>%
    dplyr::left_join(draw_order_tbl, by = "crosswalk_specification")

  readr::write_csv(coef_long, config$crosswalk_comparison_csv)

  n_specs <- dplyr::n_distinct(coef_long$crosswalk_specification)
  plot_title <- if (n_specs > 1) {
    "Crosswalk-specification sensitivity"
  } else {
    paste0("Event-study coefficients: ", as.character(unique(coef_long$crosswalk_specification)))
  }
  plot_subtitle <- if (n_specs > 1) {
    paste0("Main 1972-2020 event-study coefficients; colored/width-varied lines reveal nearly overlapping estimates. SE type = ", config$main_se_type)
  } else {
    paste0("Only one crosswalk specification was found; rerun sensitivity mode to add M6, M2, and M4. SE type = ", config$main_se_type)
  }

  # Colors are intentionally specified here because the plot is meant for a
  # final appendix figure where four nearly identical series must be visually
  # distinguishable.
  comparison_colors <- c(
    "M5 + fallback M2" = "#1b9e77",
    "M6 + fallback M2" = "#d95f02",
    "Pure M2" = "#7570b3",
    "M4 + fallback M2" = "#e7298a"
  )
  comparison_linetypes <- c(
    "M5 + fallback M2" = "solid",
    "M6 + fallback M2" = "22",
    "Pure M2" = "longdash",
    "M4 + fallback M2" = "dotdash"
  )
  comparison_linewidths <- c(
    "M5 + fallback M2" = 0.75,
    "M6 + fallback M2" = 0.95,
    "Pure M2" = 1.45,
    "M4 + fallback M2" = 1.20
  )
  comparison_alphas <- c(
    "M5 + fallback M2" = 0.95,
    "M6 + fallback M2" = 0.90,
    "Pure M2" = 0.70,
    "M4 + fallback M2" = 0.82
  )
  comparison_shapes <- c(
    "M5 + fallback M2" = 16,
    "M6 + fallback M2" = 17,
    "Pure M2" = 15,
    "M4 + fallback M2" = 18
  )

  coef_plot_data <- coef_long %>%
    dplyr::arrange(spec_label, draw_order, year)

  comp_plot <- ggplot2::ggplot(
    coef_plot_data,
    ggplot2::aes(
      x = year, y = estimate,
      color = crosswalk_specification,
      linetype = crosswalk_specification,
      linewidth = crosswalk_specification,
      alpha = crosswalk_specification,
      group = crosswalk_specification
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey45") +
    ggplot2::geom_line(lineend = "round") +
    ggplot2::geom_point(ggplot2::aes(shape = crosswalk_specification, size = crosswalk_specification), stroke = 0.2) +
    ggplot2::scale_color_manual(values = comparison_colors, drop = FALSE) +
    ggplot2::scale_linetype_manual(values = comparison_linetypes, drop = FALSE) +
    ggplot2::scale_linewidth_manual(values = comparison_linewidths, drop = FALSE, guide = "none") +
    ggplot2::scale_alpha_manual(values = comparison_alphas, drop = FALSE, guide = "none") +
    ggplot2::scale_shape_manual(values = comparison_shapes, drop = FALSE) +
    ggplot2::scale_size_manual(values = c(
      "M5 + fallback M2" = 1.25,
      "M6 + fallback M2" = 1.45,
      "Pure M2" = 2.05,
      "M4 + fallback M2" = 1.75
    ), drop = FALSE, guide = "none") +
    ggplot2::facet_wrap(~ spec_label, ncol = 1) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Presidential election year",
      y = paste0("Coefficient on ADH China exposure (per ", config$exposure_units, ")"),
      color = "Crosswalk",
      linetype = "Crosswalk",
      shape = "Crosswalk"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())

  ggplot2::ggsave(config$crosswalk_comparison_pdf, comp_plot, width = 8.5, height = 7.5, bg = "white")
  ggplot2::ggsave(config$crosswalk_comparison_png, comp_plot, width = 8.5, height = 7.5, dpi = 320, bg = "white")

  # Appendix companion: differences relative to Pure M2. This makes the actual
  # magnitude of crosswalk sensitivity visible when the level plot overlaps.
  pure_m2 <- coef_long %>%
    dplyr::filter(crosswalk_specification == "Pure M2") %>%
    dplyr::select(spec_label, year, pure_m2_estimate = estimate)

  delta_long <- coef_long %>%
    dplyr::left_join(pure_m2, by = c("spec_label", "year")) %>%
    dplyr::mutate(delta_vs_pure_m2 = estimate - pure_m2_estimate) %>%
    dplyr::arrange(spec_label, draw_order, year)

  readr::write_csv(delta_long, config$crosswalk_comparison_delta_csv)

  if (n_specs > 1 && any(is.finite(delta_long$delta_vs_pure_m2))) {
    delta_plot <- ggplot2::ggplot(
      delta_long,
      ggplot2::aes(
        x = year, y = delta_vs_pure_m2,
        color = crosswalk_specification,
        linetype = crosswalk_specification,
        linewidth = crosswalk_specification,
        alpha = crosswalk_specification,
        group = crosswalk_specification
      )
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey45") +
      ggplot2::geom_line(lineend = "round") +
      ggplot2::geom_point(ggplot2::aes(shape = crosswalk_specification, size = crosswalk_specification), stroke = 0.2) +
      ggplot2::scale_color_manual(values = comparison_colors, drop = FALSE) +
      ggplot2::scale_linetype_manual(values = comparison_linetypes, drop = FALSE) +
      ggplot2::scale_linewidth_manual(values = comparison_linewidths, drop = FALSE, guide = "none") +
      ggplot2::scale_alpha_manual(values = comparison_alphas, drop = FALSE, guide = "none") +
      ggplot2::scale_shape_manual(values = comparison_shapes, drop = FALSE) +
      ggplot2::scale_size_manual(values = c(
        "M5 + fallback M2" = 1.25,
        "M6 + fallback M2" = 1.45,
        "Pure M2" = 2.05,
        "M4 + fallback M2" = 1.75
      ), drop = FALSE, guide = "none") +
      ggplot2::facet_wrap(~ spec_label, ncol = 1, scales = "free_y") +
      ggplot2::labs(
        title = "Crosswalk-specification sensitivity relative to Pure M2",
        subtitle = paste0("Main 1972-2020 event-study coefficients; SE type = ", config$main_se_type),
        x = "Presidential election year",
        y = paste0("Coefficient difference vs. Pure M2 (per ", config$exposure_units, ")"),
        color = "Crosswalk",
        linetype = "Crosswalk",
        shape = "Crosswalk"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())

    ggplot2::ggsave(config$crosswalk_comparison_delta_pdf, delta_plot, width = 8.5, height = 7.5, bg = "white")
    ggplot2::ggsave(config$crosswalk_comparison_delta_png, delta_plot, width = 8.5, height = 7.5, dpi = 320, bg = "white")
  }

  invisible(comp_plot)
}

simplify_sf_for_plot <- function(sf_obj, tolerance = 0.03) {
  if (is.null(sf_obj)) return(sf_obj)
  out <- tryCatch(
    sf::st_simplify(sf_obj, dTolerance = tolerance, preserveTopology = TRUE),
    error = function(e) sf_obj
  )
  out
}

save_crosswalk_support_plot_pair <- function(plot, simplified_plot, pdf_path, png_path, simplified_pdf_path, simplified_png_path, small_png_path = NULL) {
  ggplot2::ggsave(pdf_path, plot, width = 10, height = 7.5, bg = "white")
  ggplot2::ggsave(png_path, plot, width = 10, height = 7.5, dpi = 320, bg = "white")
  ggplot2::ggsave(simplified_pdf_path, simplified_plot, width = 10, height = 7.5, bg = "white")
  ggplot2::ggsave(simplified_png_path, simplified_plot, width = 10, height = 7.5, dpi = 260, bg = "white")
  if (!is.null(small_png_path)) {
    ggplot2::ggsave(small_png_path, simplified_plot, width = 10, height = 7.5, dpi = 160, bg = "white")
  }
}

make_support_map_plot <- function(map_data, title, subtitle, caption, boundary_color = NA) {
  ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = map_fill), color = boundary_color, linewidth = 0.05) +
    ggplot2::facet_wrap(~ weight_type, ncol = 2) +
    ggplot2::scale_fill_manual(
      values = c(
        "No positive affected vote mass" = "grey90",
        "Receives positive affected vote mass" = "grey25"
      ),
      name = NULL
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold"))
}

export_crosswalk_support_maps <- function(config = CONFIG) {
  load_required_packages(config)
  config <- finalize_config(config)
  if (!isTRUE(config$export_crosswalk_maps)) return(invisible(NULL))

  if (!file.exists(config$target_weight_support_county_csv) || !file.exists(config$target_weight_support_cz_csv)) {
    warning("Crosswalk support diagnostics are missing; skipping support maps.", call. = FALSE)
    return(invisible(NULL))
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("Package 'sf' is not installed; skipping support maps.", call. = FALSE)
    return(invisible(NULL))
  }

  county_sf <- safe_read_county1990_shapefile(config)
  if (is.null(county_sf)) return(invisible(NULL))

  county_diag <- readr::read_csv(config$target_weight_support_county_csv, show_col_types = FALSE) %>%
    dplyr::mutate(
      weight_type = factor(
        weight_type,
        levels = c("m2_weight", "m4_weight", "m5_weight", "m6_weight"),
        labels = c("M2 unavailable", "M4 unavailable", "M5 unavailable", "M6 unavailable")
      )
    )
  county_map <- county_sf %>%
    dplyr::left_join(county_diag, by = "county_fips_1990") %>%
    dplyr::filter(!is.na(weight_type)) %>%
    dplyr::mutate(
      receives_unavailable_source = dplyr::coalesce(receives_unavailable_source, FALSE),
      map_fill = dplyr::if_else(receives_unavailable_source, "Receives positive affected vote mass", "No positive affected vote mass")
    )

  county_plot <- make_support_map_plot(
    county_map,
    title = "ADH-mainland 1990 counties receiving affected bridged vote mass",
    subtitle = "Demonstration map: flags counties receiving positive vote mass from a source county where the indicated FTZ weight type has zero/undefined support.",
    caption = "Uses the selected/fallback crosswalk as the vote-mass allocator and the 1990 county shapefile under spatial-data/counties-1990 unless overridden.",
    boundary_color = NA
  )
  county_map_simple <- simplify_sf_for_plot(county_map, tolerance = 0.03)
  county_plot_simple <- make_support_map_plot(
    county_map_simple,
    title = "ADH-mainland 1990 counties receiving affected bridged vote mass",
    subtitle = "Simplified-geometry version for smaller appendix files.",
    caption = "Uses the selected/fallback crosswalk as the vote-mass allocator and simplified 1990 county geometry.",
    boundary_color = NA
  )
  save_crosswalk_support_plot_pair(
    county_plot, county_plot_simple,
    config$crosswalk_support_county_map_pdf, config$crosswalk_support_county_map_png,
    config$crosswalk_support_county_map_simplified_pdf, config$crosswalk_support_county_map_simplified_png,
    config$crosswalk_support_county_map_small_png
  )

  cz_lookup <- readxl::read_xls(file.path(config$replication_dir, "cz-data", "cz-198090.xls")) %>%
    dplyr::transmute(
      county_fips_1990 = stringr::str_pad(as.character(`County FIPS Code`), width = 5, side = "left", pad = "0"),
      czone = as.integer(CZ90)
    )

  cz_diag <- readr::read_csv(config$target_weight_support_cz_csv, show_col_types = FALSE) %>%
    dplyr::mutate(
      weight_type = factor(
        weight_type,
        levels = c("m2_weight", "m4_weight", "m5_weight", "m6_weight"),
        labels = c("M2 unavailable", "M4 unavailable", "M5 unavailable", "M6 unavailable")
      )
    )

  cz_sf <- county_sf %>%
    dplyr::inner_join(cz_lookup, by = "county_fips_1990") %>%
    dplyr::filter(czone %in% unique(cz_diag$czone)) %>%
    dplyr::group_by(czone) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

  cz_map <- cz_sf %>%
    dplyr::left_join(cz_diag, by = "czone") %>%
    dplyr::filter(!is.na(weight_type)) %>%
    dplyr::mutate(
      receives_unavailable_source = dplyr::coalesce(receives_unavailable_source, FALSE),
      map_fill = dplyr::if_else(receives_unavailable_source, "Receives positive affected vote mass", "No positive affected vote mass")
    )

  cz_plot <- make_support_map_plot(
    cz_map,
    title = "ADH-mainland 1990 CZs receiving affected bridged vote mass",
    subtitle = "Demonstration map: flags CZs receiving positive vote mass from a source county where the indicated FTZ weight type has zero/undefined support.",
    caption = "CZ shapes are dissolved from 1990 counties using cz-198090.xls and limited to the ADH diagnostic CZ universe.",
    boundary_color = "white"
  )
  cz_map_simple <- simplify_sf_for_plot(cz_map, tolerance = 0.05)
  cz_plot_simple <- make_support_map_plot(
    cz_map_simple,
    title = "ADH-mainland 1990 CZs receiving affected bridged vote mass",
    subtitle = "Simplified-geometry version for smaller appendix files.",
    caption = "CZ shapes are dissolved from simplified 1990 counties and limited to the ADH diagnostic CZ universe.",
    boundary_color = "white"
  )
  save_crosswalk_support_plot_pair(
    cz_plot, cz_plot_simple,
    config$crosswalk_support_cz_map_pdf, config$crosswalk_support_cz_map_png,
    config$crosswalk_support_cz_map_simplified_pdf, config$crosswalk_support_cz_map_simplified_png,
    config$crosswalk_support_cz_map_small_png
  )

  invisible(list(county_plot = county_plot, cz_plot = cz_plot))
}

copy_if_exists <- function(from, to_dir) {
  if (is.null(from) || length(from) == 0 || is.na(from) || !file.exists(from)) return(invisible(FALSE))
  dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(from, file.path(to_dir, basename(from)), overwrite = TRUE)
  invisible(TRUE)
}

archive_crosswalk_run_diagnostics <- function(config, label, status = "completed", error_message = NA_character_) {
  config <- finalize_config(config)
  slug <- config$crosswalk_weight_slug
  out_dir <- file.path(config$diagnostic_dir, "by_crosswalk", slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  files <- unique(stats::na.omit(c(
    config$validation_checks_csv, config$pipeline_manifest_json, config$crosswalk_diagnostics_csv,
    config$crosswalk_zero_support_csv, config$crosswalk_row_diagnostics_csv,
    config$crosswalk_issue_vote_report_csv, config$bridge_diagnostics_csv,
    config$mainland_retention_csv, config$unmatched_source_counties_csv,
    config$county_bridge_classification_csv, config$missing_cz_years_csv,
    config$panel_coverage_csv, config$spec_status_csv, config$model_diagnostics_csv,
    config$vcov_diagnostics_csv, config$se_warning_diagnostics_csv, config$event_study_csv,
    config$all_specs_csv, config$table_csv
  )))
  invisible(lapply(files, copy_if_exists, to_dir = out_dir))

  tibble::tibble(
    crosswalk_slug = slug,
    crosswalk_specification = label,
    crosswalk_weight = config$crosswalk_weight,
    crosswalk_missing_weight_policy = config$crosswalk_missing_weight_policy,
    status = status,
    error_message = error_message,
    diagnostics_dir = portable_path(out_dir, config),
    archived_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  ) %>%
    readr::write_csv(file.path(out_dir, "run_status.csv"))

  invisible(out_dir)
}

write_crosswalk_sensitivity_status <- function(status_rows, config) {
  config <- finalize_config(config)
  status_tbl <- dplyr::bind_rows(status_rows)
  path <- file.path(config$diagnostic_dir, "crosswalk_sensitivity_run_status.csv")
  readr::write_csv(status_tbl, path)
  invisible(path)
}

export_diagnostic_maps_and_comparisons <- function(config = CONFIG) {
  config <- finalize_config(config)
  export_crosswalk_specification_comparison(config)
  export_crosswalk_support_maps(config)

  checks <- if (file.exists(config$validation_checks_csv)) {
    readr::read_csv(config$validation_checks_csv, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }
  output_inventory <- write_output_manifest(config)

  write_pipeline_manifest(
    config = config,
    checks = checks,
    stage = "export_diagnostic_maps_and_comparisons",
    sources = list(
      county_support = list(path = config$target_weight_support_county_csv, md5 = file_checksum(config$target_weight_support_county_csv)),
      cz_support = list(path = config$target_weight_support_cz_csv, md5 = file_checksum(config$target_weight_support_cz_csv))
    ),
    extra = list(
      outputs = list(
        crosswalk_comparison_png = list(path = config$crosswalk_comparison_png, md5 = file_checksum(config$crosswalk_comparison_png)),
        crosswalk_comparison_delta_png = list(path = config$crosswalk_comparison_delta_png, md5 = file_checksum(config$crosswalk_comparison_delta_png)),
        crosswalk_comparison_delta_csv = list(path = config$crosswalk_comparison_delta_csv, md5 = file_checksum(config$crosswalk_comparison_delta_csv)),
        crosswalk_support_county_map_png = list(path = config$crosswalk_support_county_map_png, md5 = file_checksum(config$crosswalk_support_county_map_png)),
        crosswalk_support_county_map_simplified_png = list(path = config$crosswalk_support_county_map_simplified_png, md5 = file_checksum(config$crosswalk_support_county_map_simplified_png)),
        crosswalk_support_county_map_small_png = list(path = config$crosswalk_support_county_map_small_png, md5 = file_checksum(config$crosswalk_support_county_map_small_png)),
        crosswalk_support_cz_map_png = list(path = config$crosswalk_support_cz_map_png, md5 = file_checksum(config$crosswalk_support_cz_map_png)),
        crosswalk_support_cz_map_simplified_png = list(path = config$crosswalk_support_cz_map_simplified_png, md5 = file_checksum(config$crosswalk_support_cz_map_simplified_png)),
        crosswalk_support_cz_map_small_png = list(path = config$crosswalk_support_cz_map_small_png, md5 = file_checksum(config$crosswalk_support_cz_map_small_png)),
        output_manifest_csv = list(path = config$output_manifest_csv, md5 = file_checksum(config$output_manifest_csv)),
        output_manifest_json = list(path = config$output_manifest_json, md5 = file_checksum(config$output_manifest_json))
      ),
      output_manifest_n_files = nrow(output_inventory)
    )
  )
  invisible(NULL)
}

run_crosswalk_sensitivity_pipeline <- function(config = CONFIG) {
  base_config <- finalize_config(config)
  plan <- crosswalk_sensitivity_plan()
  ensure_output_dirs(base_config)
  write_dependency_manifest(base_config)

  status_rows <- list()
  for (i in seq_len(nrow(plan))) {
    local_config <- base_config
    local_config$crosswalk_weight <- plan$crosswalk_weight[[i]]
    local_config$crosswalk_missing_weight_policy <- plan$crosswalk_missing_weight_policy[[i]]
    local_config$export_crosswalk_maps <- isTRUE(base_config$export_crosswalk_maps) && isTRUE(plan$render_support_maps[[i]])
    local_config <- finalize_config(local_config)

    label <- plan$label[[i]]
    message("Running crosswalk sensitivity specification: ", label, " (", local_config$crosswalk_weight, ", ", local_config$crosswalk_missing_weight_policy, ")")

    step_status <- "completed"
    step_error <- NA_character_
    started_at <- Sys.time()
    tryCatch({
      build_analysis_data(local_config, stop_on_fatal = TRUE)
      estimate_event_study(local_config, stop_on_fatal = TRUE)
      export_event_study_outputs(local_config)
      if (isTRUE(local_config$export_crosswalk_maps)) export_crosswalk_support_maps(local_config)
    }, error = function(e) {
      step_status <<- "failed"
      step_error <<- conditionMessage(e)
      message("Crosswalk sensitivity specification failed: ", label, ": ", step_error)
    })

    archive_crosswalk_run_diagnostics(local_config, label, status = step_status, error_message = step_error)
    status_rows[[length(status_rows) + 1L]] <- tibble::tibble(
      crosswalk_slug = local_config$crosswalk_weight_slug,
      crosswalk_specification = label,
      crosswalk_weight = local_config$crosswalk_weight,
      crosswalk_missing_weight_policy = local_config$crosswalk_missing_weight_policy,
      status = step_status,
      error_message = step_error,
      started_at = format(started_at, "%Y-%m-%d %H:%M:%S %Z"),
      finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
    write_crosswalk_sensitivity_status(status_rows, base_config)
  }

  # After all slug-specific coefficient files exist, create the appendix-ready comparison figure.
  comparison_config <- finalize_config(base_config)
  export_crosswalk_specification_comparison(comparison_config)

  checks <- if (file.exists(comparison_config$validation_checks_csv)) {
    readr::read_csv(comparison_config$validation_checks_csv, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }
  status_tbl <- dplyr::bind_rows(status_rows)
  write_crosswalk_sensitivity_status(status_rows, comparison_config)
  output_inventory <- write_output_manifest(comparison_config)
  write_pipeline_manifest(
    config = comparison_config,
    checks = checks,
    stage = "crosswalk_sensitivity_complete",
    sources = list(
      sensitivity_plan = list(specifications = plan),
      sensitivity_status = list(path = file.path(comparison_config$diagnostic_dir, "crosswalk_sensitivity_run_status.csv"), md5 = file_checksum(file.path(comparison_config$diagnostic_dir, "crosswalk_sensitivity_run_status.csv")))
    ),
    extra = list(
      any_failed_specifications = any(status_tbl$status != "completed"),
      completed_specifications = status_tbl %>% dplyr::filter(status == "completed") %>% dplyr::pull(crosswalk_specification),
      failed_specifications = status_tbl %>% dplyr::filter(status != "completed") %>% dplyr::pull(crosswalk_specification),
      comparison_csv = list(path = comparison_config$crosswalk_comparison_csv, md5 = file_checksum(comparison_config$crosswalk_comparison_csv)),
      comparison_png = list(path = comparison_config$crosswalk_comparison_png, md5 = file_checksum(comparison_config$crosswalk_comparison_png)),
      comparison_delta_csv = list(path = comparison_config$crosswalk_comparison_delta_csv, md5 = file_checksum(comparison_config$crosswalk_comparison_delta_csv)),
      comparison_delta_png = list(path = comparison_config$crosswalk_comparison_delta_png, md5 = file_checksum(comparison_config$crosswalk_comparison_delta_png)),
      output_manifest_csv = list(path = comparison_config$output_manifest_csv, md5 = file_checksum(comparison_config$output_manifest_csv)),
      output_manifest_json = list(path = comparison_config$output_manifest_json, md5 = file_checksum(comparison_config$output_manifest_json)),
      output_manifest_n_files = nrow(output_inventory)
    )
  )
  invisible(status_tbl)
}

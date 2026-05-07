# replication/src/02_build_analysis_data.R

preflight_ftz_builtup_support <- function(zip_path, source_decades, tolerance = 1e-6) {
  purrr::map_dfr(source_decades, function(source_decade) {
    if (source_decade == 1990L) {
      return(tibble::tibble(
        source_decade = 1990L,
        county_fips_source = NA_character_,
        weight_type = c("m5_weight", "m6_weight"),
        weight_sum = 1,
        zero_or_undefined_support = FALSE
      ))
    }

    file_name <- paste0("Crosswalk_", source_decade, "_1990.csv")
    source_col <- paste0("gisjoin_", source_decade)
    raw <- readr::read_csv(
      unz(zip_path, file_name),
      col_select = dplyr::all_of(c(source_col, "m5_weight", "m6_weight")),
      show_col_types = FALSE
    ) %>%
      dplyr::mutate(
        gisjoin_source = as.character(.data[[source_col]])
      ) %>%
      dplyr::select(gisjoin_source, m5_weight, m6_weight)
    if (source_decade == 2020L) {
      raw <- raw %>%
        dplyr::mutate(gisjoin_source = fips_to_gisjoin(stringr::str_pad(gisjoin_source, 5, pad = "0")))
    }

    raw %>%
      tidyr::pivot_longer(
        cols = c("m5_weight", "m6_weight"),
        names_to = "weight_type",
        values_to = "weight"
      ) %>%
      dplyr::group_by(gisjoin_source, weight_type) %>%
      dplyr::summarise(
        weight_sum = if (all(is.na(weight))) NA_real_ else sum(weight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        source_decade = as.integer(source_decade),
        county_fips_source = gisjoin_to_fips(gisjoin_source),
        weight_type,
        weight_sum,
        zero_or_undefined_support = is.na(weight_sum) | weight_sum <= tolerance
      )
  })
}

build_analysis_data <- function(config = CONFIG, stop_on_fatal = TRUE) {
  load_required_packages()
  config <- finalize_config(config)

  adh_stack_path <- file.path(config$replication_dir, "adh2013", "dta", "workfile_china.dta")
  adh_long_path <- file.path(config$replication_dir, "adh2013", "dta", "workfile_china_long.dta")
  cz_path <- file.path(config$replication_dir, "cz-data", "cz-198090.xls")
  centroid_path <- file.path(
    config$replication_dir, "ftz2024", "crosswalks", "County-CD-centroid-lat-lon",
    "lat_lon_coordinates_county_csv", "counties_1990_xy.csv"
  )
  ftz_zip <- file.path(
    config$replication_dir, "ftz2024", "crosswalks", "CountyToCounty", "1990", "1990_csv.zip"
  )
  aa_path <- file.path(
    config$replication_dir, "outcomes-data", "aa2021-nospatial",
    "County_Level_US_Elections_Data",
    "dataverse_shareable_presidential_county_returns_1868_2020.Rdata"
  )

  message("Reading ADH baseline and exposure data...")
  adh_stack <- haven::read_dta(adh_stack_path)
  adh_long <- haven::read_dta(adh_long_path)

  adh_baseline <- adh_stack %>%
    dplyr::filter(yr == 1990) %>%
    dplyr::transmute(
      czone = as.integer(czone),
      statefip = as.integer(statefip),
      dplyr::across(dplyr::all_of(config$baseline_controls), as.numeric)
    ) %>%
    dplyr::distinct()

  adh_exposure <- adh_long %>%
    dplyr::transmute(
      czone = as.integer(czone),
      statefip = as.integer(statefip),
      exposure = as.numeric(d_tradeusch_pw),
      adh_weight = as.numeric(timepwt48)
    ) %>%
    dplyr::distinct()

  adh_sample <- adh_baseline %>%
    dplyr::inner_join(adh_exposure, by = c("czone", "statefip"))

  adh_duplicate_czones <- adh_sample %>%
    dplyr::count(czone, name = "rows_per_czone") %>%
    dplyr::filter(rows_per_czone > 1)

  message("Reading 1990 county-to-CZ lookup and county centroids...")
  cz_lookup <- readxl::read_xls(cz_path) %>%
    dplyr::transmute(
      county_fips_1990 = stringr::str_pad(as.character(`County FIPS Code`), width = 5, side = "left", pad = "0"),
      czone = as.integer(CZ90),
      county_name_1990 = as.character(`County name`),
      pop1990 = as.numeric(`Population 1990`)
    )

  cz_lookup_duplicate_count <- cz_lookup %>%
    dplyr::count(county_fips_1990, name = "rows_per_1990_county") %>%
    dplyr::filter(rows_per_1990_county > 1) %>%
    nrow()
  cz_lookup_multi_czone_count <- cz_lookup %>%
    dplyr::distinct(county_fips_1990, czone) %>%
    dplyr::count(county_fips_1990, name = "czones_per_1990_county") %>%
    dplyr::filter(czones_per_1990_county > 1) %>%
    nrow()

  county_centroids <- readr::read_csv(centroid_path, show_col_types = FALSE) %>%
    dplyr::transmute(
      county_fips_1990 = gisjoin_to_fips(gisjoin_1990),
      lon = as.numeric(centroid_x),
      lat = as.numeric(centroid_y)
    )

  cz_centroids <- cz_lookup %>%
    dplyr::inner_join(county_centroids, by = "county_fips_1990") %>%
    dplyr::group_by(czone) %>%
    dplyr::mutate(pop_w = pop1990 / sum(pop1990, na.rm = TRUE)) %>%
    dplyr::summarise(
      lon = stats::weighted.mean(lon, pop_w, na.rm = TRUE),
      lat = stats::weighted.mean(lat, pop_w, na.rm = TRUE),
      pop1990 = sum(pop1990, na.rm = TRUE),
      counties_in_cz = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(adh_sample %>% dplyr::distinct(czone), by = "czone")
  readr::write_csv(cz_centroids, config$cz_centroids_csv)

  preflight_support <- preflight_ftz_builtup_support(
    zip_path = ftz_zip,
    source_decades = config$source_decades,
    tolerance = config$weight_sum_tolerance
  )
  preflight_summary <- preflight_support %>%
    dplyr::group_by(source_decade, weight_type) %>%
    dplyr::summarise(
      n_source_counties = dplyr::n(),
      n_zero_or_undefined_support = sum(zero_or_undefined_support),
      .groups = "drop"
    )
  readr::write_csv(preflight_summary, config$crosswalk_preflight_summary_csv)
  readr::write_csv(
    preflight_support %>% dplyr::filter(zero_or_undefined_support),
    config$crosswalk_preflight_counties_csv
  )

  if (any(preflight_support$zero_or_undefined_support)) {
    summary_msg <- preflight_summary %>%
      dplyr::filter(n_zero_or_undefined_support > 0) %>%
      dplyr::mutate(piece = paste0(weight_type, " ", source_decade, ": ", n_zero_or_undefined_support)) %>%
      dplyr::pull(piece) %>%
      paste(collapse = "; ")
    warning(
      "FTZ built-up-support preflight: strict M5/M6 weighting leaves some source counties ",
      "with zero or undefined built-up support, so a strict M5/M6 run cannot produce the balanced ",
      "ADH mainland CZ panel. Current policy is '", config$crosswalk_missing_weight_policy, "'. ",
      "Zero/undefined support counts: ", summary_msg, ". See ",
      config$crosswalk_preflight_summary_csv,
      call. = FALSE
    )
  }

  message("Reading FTZ county crosswalks with ", config$crosswalk_weight, "...")
  crosswalks <- purrr::map_dfr(
    config$source_decades,
    ~ read_ftz_crosswalk(
      source_decade = .x,
      zip_path = ftz_zip,
      county_fips_1990_identity = cz_lookup$county_fips_1990,
      weight_col = config$crosswalk_weight,
      missing_weight_policy = config$crosswalk_missing_weight_policy,
      renormalize_weights = config$renormalize_crosswalk_weights,
      tolerance = config$weight_sum_tolerance
    )
  )

  crosswalk_source_diagnostics <- crosswalks %>%
    dplyr::group_by(source_decade, gisjoin_source, county_fips_source) %>%
    dplyr::summarise(
      weight_sum_raw = dplyr::first(weight_sum_raw),
      selected_weight_sum_raw = dplyr::first(selected_weight_sum_raw),
      crosswalk_weight_sum = dplyr::first(crosswalk_weight_sum),
      raw_weight_undefined = dplyr::first(raw_weight_undefined),
      raw_weight_sum_valid = dplyr::first(raw_weight_sum_valid),
      raw_weight_requires_policy = dplyr::first(raw_weight_requires_policy),
      fallback_used = any(fallback_used),
      fallback_policy = if (any(fallback_policy != "none")) {
        dplyr::first(fallback_policy[fallback_policy != "none"])
      } else {
        "none"
      },
      n_targets = dplyr::n(),
      positive_targets = sum(!is.na(crosswalk_weight) & crosswalk_weight > 0),
      missing_raw_weight_parts = sum(raw_weight_missing_parts),
      final_weight_sum_ok = !is.na(dplyr::first(crosswalk_weight_sum)) &
        abs(dplyr::first(crosswalk_weight_sum) - 1) <= config$weight_sum_tolerance,
      .groups = "drop"
    )

  readr::write_csv(crosswalk_source_diagnostics, config$crosswalk_diagnostics_csv)
  readr::write_csv(
    crosswalk_source_diagnostics %>%
      dplyr::filter(raw_weight_undefined | !final_weight_sum_ok),
    config$crosswalk_zero_support_csv
  )
  readr::write_csv(
    crosswalks %>%
      dplyr::select(
        source_decade, county_fips_source, county_fips_1990, raw_weight,
        weight_sum_raw, crosswalk_weight, crosswalk_weight_sum,
        raw_weight_undefined, raw_weight_sum_valid, raw_weight_requires_policy,
        fallback_used, fallback_policy, weight_source
      ),
    config$crosswalk_row_diagnostics_csv
  )

  message("Reading and validating Amlani and Algara (2021) presidential county returns...")
  aa_pres <- read_aa_rdata_object(aa_path, object_name = "pres_elections_release")

  county_pres_raw <- aa_pres %>%
    dplyr::transmute(
      year = as.integer(election_year),
      county_fips = stringr::str_pad(as.character(fips), width = 5, side = "left", pad = "0"),
      state_fips = stringr::str_pad(as.character(sfips), width = 2, side = "left", pad = "0"),
      state = as.character(state),
      county_name = as.character(county_name),
      rep_votes = as.numeric(republican_raw_votes),
      dem_votes = as.numeric(democratic_raw_votes),
      twoparty_votes = as.numeric(pres_raw_county_vote_totals_two_party),
      total_votes = as.numeric(raw_county_vote_totals),
      complete_case = as.integer(complete_county_cases)
    ) %>%
    dplyr::filter(year %in% config$diagnostic_event_years)

  exact_duplicate_rows <- nrow(county_pres_raw) - nrow(dplyr::distinct(county_pres_raw))
  county_pres_distinct <- county_pres_raw %>% dplyr::distinct()

  duplicate_county_years <- county_pres_distinct %>%
    dplyr::count(year, county_fips, name = "rows_per_county_year") %>%
    dplyr::filter(rows_per_county_year > 1) %>%
    dplyr::left_join(county_pres_distinct, by = c("year", "county_fips"))
  readr::write_csv(duplicate_county_years, config$outcome_duplicate_report_csv)

  county_pres_validation <- county_pres_distinct %>%
    dplyr::mutate(
      valid_fips = !is.na(county_fips) & stringr::str_detect(county_fips, "^[0-9]{5}$"),
      nonmissing_votes = !is.na(rep_votes) & !is.na(dem_votes) & !is.na(twoparty_votes),
      nonnegative_votes = rep_votes >= 0 & dem_votes >= 0 & twoparty_votes >= 0 &
        (is.na(total_votes) | total_votes >= 0),
      twoparty_matches = abs((rep_votes + dem_votes) - twoparty_votes) <= 1e-6,
      usable_for_bridge = valid_fips & nonmissing_votes & nonnegative_votes &
        twoparty_matches & twoparty_votes > 0 & complete_case == 1L
    )
  readr::write_csv(county_pres_validation, config$outcome_validation_csv)

  invalid_fips_count <- county_pres_validation %>%
    dplyr::filter(!valid_fips) %>%
    nrow()
  max_vote_identity_error <- county_pres_validation %>%
    dplyr::filter(nonmissing_votes) %>%
    dplyr::mutate(abs_vote_identity_error = abs((rep_votes + dem_votes) - twoparty_votes)) %>%
    dplyr::pull(abs_vote_identity_error) %>%
    max(na.rm = TRUE)

  county_pres <- county_pres_validation %>%
    dplyr::filter(usable_for_bridge) %>%
    dplyr::mutate(
      source_decade = year_to_source_decade(year),
      gisjoin_source = fips_to_gisjoin(county_fips)
    ) %>%
    dplyr::select(
      year, county_fips, state_fips, state, county_name, rep_votes, dem_votes,
      twoparty_votes, total_votes, source_decade, gisjoin_source
    )

  crosswalk_issue_vote_report <- county_pres %>%
    dplyr::left_join(
      crosswalks %>%
        dplyr::select(
          source_decade, gisjoin_source, county_fips_1990, raw_weight,
          weight_sum_raw, crosswalk_weight, crosswalk_weight_sum,
          raw_weight_undefined, raw_weight_sum_valid, raw_weight_requires_policy,
          fallback_used, fallback_policy, weight_source
        ) %>%
        dplyr::left_join(cz_lookup %>% dplyr::select(county_fips_1990, czone), by = "county_fips_1990"),
      by = c("source_decade", "gisjoin_source"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      raw_weight_undefined %in% TRUE |
        raw_weight_requires_policy %in% TRUE |
        is.na(crosswalk_weight) |
        crosswalk_weight <= 0
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(vote_share_of_year = twoparty_votes / sum(twoparty_votes, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      year, source_decade, state_fips, state, county_fips, county_name,
      twoparty_votes, vote_share_of_year, county_fips_1990, czone,
      raw_weight, weight_sum_raw, crosswalk_weight, crosswalk_weight_sum,
      raw_weight_undefined, raw_weight_sum_valid, raw_weight_requires_policy,
      fallback_used, fallback_policy, weight_source
    )
  readr::write_csv(crosswalk_issue_vote_report, config$crosswalk_issue_vote_report_csv)

  national_pre <- county_pres %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      scope = "pre_bridge",
      rep_votes = sum(rep_votes, na.rm = TRUE),
      dem_votes = sum(dem_votes, na.rm = TRUE),
      twoparty_votes = sum(twoparty_votes, na.rm = TRUE),
      total_votes = sum(total_votes, na.rm = TRUE),
      source_counties = dplyr::n_distinct(county_fips),
      .groups = "drop"
    )

  state_pre <- county_pres %>%
    dplyr::group_by(year, state_fips, state) %>%
    dplyr::summarise(
      scope = "pre_bridge",
      rep_votes = sum(rep_votes, na.rm = TRUE),
      dem_votes = sum(dem_votes, na.rm = TRUE),
      twoparty_votes = sum(twoparty_votes, na.rm = TRUE),
      total_votes = sum(total_votes, na.rm = TRUE),
      source_counties = dplyr::n_distinct(county_fips),
      .groups = "drop"
    )

  message("Bridging county returns to 1990 counties...")
  county_bridged <- county_pres %>%
    dplyr::left_join(
      crosswalks %>%
        dplyr::select(
          source_decade, gisjoin_source, county_fips_1990, raw_weight,
          crosswalk_weight, raw_weight_undefined, raw_weight_sum_valid,
          raw_weight_requires_policy, fallback_used, fallback_policy, weight_source
        ),
      by = c("source_decade", "gisjoin_source"),
      relationship = "many-to-many"
    )

  unmatched_source_counties <- county_bridged %>%
    dplyr::group_by(year, source_decade, county_fips, state_fips, state, county_name) %>%
    dplyr::summarise(
      twoparty_votes = dplyr::first(twoparty_votes),
      has_positive_crosswalk_weight = any(!is.na(crosswalk_weight) & crosswalk_weight > 0),
      raw_weight_undefined = any(raw_weight_undefined %in% TRUE, na.rm = TRUE),
      fallback_used = any(fallback_used %in% TRUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(vote_share_of_year = twoparty_votes / sum(twoparty_votes, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!has_positive_crosswalk_weight)
  readr::write_csv(unmatched_source_counties, config$unmatched_source_counties_csv)

  original_by_year <- county_pres %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      source_counties = dplyr::n_distinct(county_fips),
      source_two_party_votes_prejoin = sum(twoparty_votes, na.rm = TRUE),
      source_rep_votes_prejoin = sum(rep_votes, na.rm = TRUE),
      source_dem_votes_prejoin = sum(dem_votes, na.rm = TRUE),
      .groups = "drop"
    )

  naive_postjoin_denominator <- county_bridged %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      source_two_party_votes_after_join_naive_for_diagnostic_only = sum(twoparty_votes, na.rm = TRUE),
      .groups = "drop"
    )

  bridged_by_year <- county_bridged %>%
    dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      matched_source_counties = dplyr::n_distinct(county_fips),
      bridged_two_party_votes = sum(twoparty_votes * crosswalk_weight, na.rm = TRUE),
      bridged_rep_votes = sum(rep_votes * crosswalk_weight, na.rm = TRUE),
      bridged_dem_votes = sum(dem_votes * crosswalk_weight, na.rm = TRUE),
      fallback_source_counties = dplyr::n_distinct(county_fips[fallback_used %in% TRUE]),
      .groups = "drop"
    )

  bridge_diagnostics <- original_by_year %>%
    dplyr::left_join(naive_postjoin_denominator, by = "year") %>%
    dplyr::left_join(bridged_by_year, by = "year") %>%
    dplyr::mutate(
      matched_source_counties = dplyr::coalesce(matched_source_counties, 0L),
      unmatched_source_counties = source_counties - matched_source_counties,
      retained_vote_share_correct = bridged_two_party_votes / source_two_party_votes_prejoin,
      retained_vote_share = retained_vote_share_correct
    )
  readr::write_csv(bridge_diagnostics, config$bridge_diagnostics_csv)

  national_post <- county_bridged %>%
    dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      scope = "post_bridge",
      rep_votes = sum(rep_votes * crosswalk_weight, na.rm = TRUE),
      dem_votes = sum(dem_votes * crosswalk_weight, na.rm = TRUE),
      twoparty_votes = sum(twoparty_votes * crosswalk_weight, na.rm = TRUE),
      total_votes = sum(total_votes * crosswalk_weight, na.rm = TRUE),
      source_counties = dplyr::n_distinct(county_fips),
      .groups = "drop"
    )
  readr::write_csv(dplyr::bind_rows(national_pre, national_post), config$national_vote_totals_csv)

  state_post <- county_bridged %>%
    dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > 0) %>%
    dplyr::group_by(year, state_fips, state) %>%
    dplyr::summarise(
      scope = "post_bridge",
      rep_votes = sum(rep_votes * crosswalk_weight, na.rm = TRUE),
      dem_votes = sum(dem_votes * crosswalk_weight, na.rm = TRUE),
      twoparty_votes = sum(twoparty_votes * crosswalk_weight, na.rm = TRUE),
      total_votes = sum(total_votes * crosswalk_weight, na.rm = TRUE),
      source_counties = dplyr::n_distinct(county_fips),
      .groups = "drop"
    )
  readr::write_csv(dplyr::bind_rows(state_pre, state_post), config$state_vote_totals_csv)

  county90_panel <- county_bridged %>%
    dplyr::filter(!is.na(county_fips_1990), !is.na(crosswalk_weight), crosswalk_weight > 0) %>%
    dplyr::mutate(
      rep_votes = rep_votes * crosswalk_weight,
      dem_votes = dem_votes * crosswalk_weight,
      twoparty_votes = twoparty_votes * crosswalk_weight,
      total_votes = total_votes * crosswalk_weight
    ) %>%
    dplyr::group_by(year, county_fips_1990) %>%
    dplyr::summarise(
      rep_votes = sum(rep_votes, na.rm = TRUE),
      dem_votes = sum(dem_votes, na.rm = TRUE),
      twoparty_votes = sum(twoparty_votes, na.rm = TRUE),
      total_votes = sum(total_votes, na.rm = TRUE),
      .groups = "drop"
    )

  message("Aggregating bridged county votes to 1990 commuting zones...")
  cz_panel <- county90_panel %>%
    dplyr::inner_join(cz_lookup, by = "county_fips_1990") %>%
    dplyr::group_by(czone, year) %>%
    dplyr::summarise(
      rep_votes = sum(rep_votes, na.rm = TRUE),
      dem_votes = sum(dem_votes, na.rm = TRUE),
      twoparty_votes = sum(twoparty_votes, na.rm = TRUE),
      total_votes = sum(total_votes, na.rm = TRUE),
      counties_contributing = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(rep_margin = (rep_votes - dem_votes) / twoparty_votes)

  make_panel <- function(years) {
    tidyr::expand_grid(czone = sort(unique(adh_sample$czone)), year = years) %>%
      dplyr::mutate(election_index = match(year, sort(unique(years)))) %>%
      dplyr::left_join(cz_panel, by = c("czone", "year")) %>%
      dplyr::inner_join(adh_sample, by = "czone") %>%
      dplyr::inner_join(cz_centroids, by = "czone") %>%
      dplyr::arrange(czone, year)
  }

  analysis_panel_full <- make_panel(config$event_years)
  diagnostic_panel_full <- make_panel(config$diagnostic_event_years)

  analysis_panel <- analysis_panel_full %>%
    dplyr::filter(!is.na(.data[[config$outcome_var]]))
  diagnostic_panel <- diagnostic_panel_full %>%
    dplyr::filter(!is.na(.data[[config$outcome_var]]))

  missing_cz_years <- dplyr::bind_rows(
    analysis_panel_full %>%
      dplyr::filter(is.na(.data[[config$outcome_var]])) %>%
      dplyr::transmute(sample = "main_1972_start", czone, year),
    diagnostic_panel_full %>%
      dplyr::filter(is.na(.data[[config$outcome_var]])) %>%
      dplyr::transmute(sample = "diagnostic_1952_start", czone, year)
  )
  readr::write_csv(missing_cz_years, config$missing_cz_years_csv)

  panel_coverage <- dplyr::bind_rows(
    analysis_panel_full %>% dplyr::mutate(sample = "main_1972_start"),
    diagnostic_panel_full %>% dplyr::mutate(sample = "diagnostic_1952_start")
  ) %>%
    dplyr::mutate(has_outcome = !is.na(.data[[config$outcome_var]])) %>%
    dplyr::group_by(sample, year) %>%
    dplyr::summarise(
      n_cz_total = dplyr::n(),
      n_cz_with_outcome = sum(has_outcome),
      missing_cz_years = n_cz_total - n_cz_with_outcome,
      share_cz_with_outcome = mean(has_outcome),
      .groups = "drop"
    )
  readr::write_csv(panel_coverage, config$panel_coverage_csv)

  saveRDS(analysis_panel, config$analysis_panel_rds)
  saveRDS(analysis_panel_full, config$analysis_panel_full_rds)
  saveRDS(diagnostic_panel, config$diagnostic_panel_rds)
  saveRDS(diagnostic_panel_full, config$diagnostic_panel_full_rds)

  main_expected_n <- length(unique(adh_sample$czone)) * length(config$event_years)
  main_missing_n <- nrow(analysis_panel_full) - nrow(analysis_panel)
  main_missing_share <- if (main_expected_n > 0) main_missing_n / main_expected_n else NA_real_
  adh_duplicate_czone_count <- nrow(adh_duplicate_czones)
  min_main_retained <- bridge_diagnostics %>%
    dplyr::filter(year %in% config$event_years) %>%
    dplyr::pull(retained_vote_share) %>%
    min(na.rm = TRUE)
  duplicate_count <- duplicate_county_years %>%
    dplyr::distinct(year, county_fips) %>%
    nrow()
  twoparty_mismatch_count <- county_pres_validation %>%
    dplyr::filter(complete_case == 1L, valid_fips, nonmissing_votes, !twoparty_matches) %>%
    nrow()
  nonnegative_failure_count <- county_pres_validation %>%
    dplyr::filter(complete_case == 1L, valid_fips, nonmissing_votes, !nonnegative_votes) %>%
    nrow()
  invalid_final_weight_count <- crosswalk_source_diagnostics %>%
    dplyr::filter(!final_weight_sum_ok) %>%
    nrow()
  invalid_positive_final_weight_count <- crosswalk_source_diagnostics %>%
    dplyr::filter(positive_targets > 0, !final_weight_sum_ok) %>%
    nrow()
  undefined_selected_weight_count <- crosswalk_source_diagnostics %>%
    dplyr::filter(raw_weight_undefined) %>%
    nrow()

  checks <- dplyr::bind_rows(
    validation_check(
      "adh_one_row_per_czone",
      adh_duplicate_czone_count == 0,
      fatal = TRUE,
      n_failed = adh_duplicate_czone_count,
      details = "Requires one ADH baseline/exposure row per commuting zone."
    ),
    validation_check(
      "cz_lookup_unique_1990_counties",
      cz_lookup_duplicate_count == 0 && cz_lookup_multi_czone_count == 0,
      fatal = TRUE,
      n_failed = cz_lookup_duplicate_count + cz_lookup_multi_czone_count,
      details = "Requires each 1990 county FIPS to appear once and map to one CZ."
    ),
    validation_check(
      "aa_all_county_fips_valid",
      invalid_fips_count == 0,
      fatal = TRUE,
      n_failed = invalid_fips_count,
      details = "Requires all county FIPS to be five numeric digits before filtering."
    ),
    validation_check(
      "aa_vote_identity_error_within_tolerance",
      is.finite(max_vote_identity_error) && max_vote_identity_error <= config$max_abs_vote_identity_error,
      fatal = TRUE,
      n_failed = sum(!county_pres_validation$twoparty_matches, na.rm = TRUE),
      details = paste0("Max absolute vote identity error: ", signif(max_vote_identity_error, 6))
    ),
    validation_check(
      "crosswalk_positive_final_weight_sums_equal_one",
      invalid_positive_final_weight_count == 0,
      fatal = TRUE,
      n_failed = invalid_positive_final_weight_count,
      details = paste0("Tolerance: ", config$weight_sum_tolerance)
    ),
    validation_check(
      "crosswalk_all_source_weight_sums_equal_one_or_reported",
      invalid_final_weight_count == 0,
      fatal = FALSE,
      n_failed = invalid_final_weight_count,
      details = "Nonfatal global report; estimation is gated by positive final weights, retained vote share, and panel balance."
    ),
    validation_check(
      "crosswalk_undefined_selected_weights_handled",
      config$crosswalk_missing_weight_policy != "fail" || undefined_selected_weight_count == 0,
      fatal = TRUE,
      n_failed = undefined_selected_weight_count,
      details = paste0("Policy: ", config$crosswalk_missing_weight_policy)
    ),
    validation_check(
      "aa_no_duplicate_county_years_after_exact_distinct",
      duplicate_count == 0 || !isTRUE(config$require_no_duplicate_county_years),
      fatal = isTRUE(config$require_no_duplicate_county_years),
      n_failed = duplicate_count,
      details = paste0("Exact duplicate rows removed: ", exact_duplicate_rows)
    ),
    validation_check(
      "aa_two_party_votes_match_party_votes",
      twoparty_mismatch_count == 0,
      fatal = TRUE,
      n_failed = twoparty_mismatch_count,
      details = "Requires republican + democratic votes to equal two-party votes."
    ),
    validation_check(
      "aa_votes_nonnegative",
      nonnegative_failure_count == 0,
      fatal = TRUE,
      n_failed = nonnegative_failure_count,
      details = "Requires nonnegative Republican, Democratic, two-party, and total votes."
    ),
    validation_check(
      "bridged_vote_retention_above_threshold",
      is.finite(min_main_retained) && min_main_retained >= config$min_retained_vote_share,
      fatal = TRUE,
      n_failed = sum(bridge_diagnostics$retained_vote_share < config$min_retained_vote_share, na.rm = TRUE),
      details = paste0("Minimum retained share in main years: ", signif(min_main_retained, 6))
    ),
    validation_check(
      "main_panel_balanced",
      main_missing_n == 0 || (
        !isTRUE(config$require_balanced_panel) &&
          is.finite(main_missing_share) &&
          main_missing_share <= config$max_missing_cz_year_share
      ),
      fatal = isTRUE(config$require_balanced_panel) || config$max_missing_cz_year_share <= 0,
      n_failed = main_missing_n,
      details = paste0(
        "Observed complete rows: ", nrow(analysis_panel), "; expected rows: ", main_expected_n,
        "; missing share: ", signif(main_missing_share, 6)
      )
    )
  )
  readr::write_csv(checks, config$validation_checks_csv)

  sources <- list(
    adh_stack = list(path = adh_stack_path, md5 = file_checksum(adh_stack_path)),
    adh_long = list(path = adh_long_path, md5 = file_checksum(adh_long_path)),
    cz_lookup = list(path = cz_path, md5 = file_checksum(cz_path)),
    ftz_zip = list(path = ftz_zip, md5 = file_checksum(ftz_zip)),
    aa_presidential = list(path = aa_path, md5 = file_checksum(aa_path))
  )
  manifest <- write_pipeline_manifest(
    config = config,
    checks = checks,
    stage = "build_analysis_data",
    sources = sources,
    extra = list(
      main_expected_rows = main_expected_n,
      main_observed_rows = nrow(analysis_panel),
      main_missing_rows = main_missing_n,
      main_missing_share = main_missing_share,
      min_main_retained_vote_share = min_main_retained
    )
  )

  failed <- checks %>%
    dplyr::filter(fatal, status == "fail")

  if (nrow(failed) > 0 && isTRUE(stop_on_fatal) && !isTRUE(config$allow_failed_diagnostics)) {
    failed_msg <- paste(
      paste0(
        failed$check,
        " (n_failed = ", failed$n_failed, "): ",
        failed$details
      ),
      collapse = "\n"
    )

    detail_parts <- character(0)
    if ("bridged_vote_retention_above_threshold" %in% failed$check) {
      failing_years <- bridge_diagnostics %>%
        dplyr::filter(year %in% config$event_years, retained_vote_share < config$min_retained_vote_share) %>%
        dplyr::transmute(piece = paste0(year, " retained_vote_share=", signif(retained_vote_share, 6))) %>%
        dplyr::pull(piece)
      if (length(failing_years) > 0) {
        detail_parts <- c(detail_parts, paste0("Failing retention years: ", paste(failing_years, collapse = "; ")))
      }
    }
    if ("main_panel_balanced" %in% failed$check) {
      missing_main <- missing_cz_years %>%
        dplyr::filter(sample == "main_1972_start") %>%
        dplyr::mutate(piece = paste0("CZ ", czone, " in ", year)) %>%
        dplyr::pull(piece)
      if (length(missing_main) > 0) {
        shown <- head(missing_main, 30)
        suffix <- if (length(missing_main) > length(shown)) {
          paste0("; ... +", length(missing_main) - length(shown), " more")
        } else {
          ""
        }
        detail_parts <- c(detail_parts, paste0("Missing main-panel CZ-years: ", paste(shown, collapse = "; "), suffix))
      }
    }
    if ("crosswalk_positive_final_weight_sums_equal_one" %in% failed$check) {
      bad_crosswalks <- crosswalk_source_diagnostics %>%
        dplyr::filter(positive_targets > 0, !final_weight_sum_ok) %>%
        dplyr::mutate(piece = paste0(
          source_decade, " county ", county_fips_source,
          " final_sum=", signif(crosswalk_weight_sum, 6)
        )) %>%
        dplyr::pull(piece)
      if (length(bad_crosswalks) > 0) {
        shown <- head(bad_crosswalks, 30)
        suffix <- if (length(bad_crosswalks) > length(shown)) {
          paste0("; ... +", length(bad_crosswalks) - length(shown), " more")
        } else {
          ""
        }
        detail_parts <- c(detail_parts, paste0("Crosswalk source counties with invalid positive final sums: ", paste(shown, collapse = "; "), suffix))
      }
    }

    detail_msg <- if (length(detail_parts) > 0) {
      paste0("\n\nAdditional failing rows:\n", paste(detail_parts, collapse = "\n"))
    } else {
      ""
    }

    stop(
      "Fatal validation checks failed before estimation:\n",
      failed_msg,
      detail_msg,
      "\nSee ", config$pipeline_manifest_json,
      call. = FALSE
    )
  }

  invisible(list(
    config = config,
    checks = checks,
    manifest = manifest,
    analysis_panel = analysis_panel,
    analysis_panel_full = analysis_panel_full,
    diagnostic_panel = diagnostic_panel,
    diagnostic_panel_full = diagnostic_panel_full,
    crosswalks = crosswalks
  ))
}

# extension-adh2013/src/02_build_analysis_data.R

preflight_ftz_builtup_support <- function(zip_path, source_decades, tolerance = 1e-6,
                                           weight_types = c("m2_weight", "m4_weight", "m5_weight", "m6_weight")) {
  # 1990 is identity-mapped to itself in this project, so M2/M4/M5/M6 support
  # is not meaningful for that decade. Omitting it avoids the misleading
  # n_source_counties = 1 synthetic row seen in earlier diagnostics.
  source_decades <- setdiff(as.integer(source_decades), 1990L)

  purrr::map_dfr(source_decades, function(source_decade) {
    file_name <- paste0("Crosswalk_", source_decade, "_1990.csv")
    source_col <- paste0("gisjoin_", source_decade)
    raw <- readr::read_csv(
      unz(zip_path, file_name),
      col_select = dplyr::all_of(c(source_col, weight_types)),
      show_col_types = FALSE
    ) %>%
      dplyr::mutate(
        gisjoin_source = as.character(.data[[source_col]])
      ) %>%
      dplyr::select(gisjoin_source, dplyr::all_of(weight_types))
    if (source_decade == 2020L) {
      raw <- raw %>%
        dplyr::mutate(gisjoin_source = fips_to_gisjoin(stringr::str_pad(gisjoin_source, 5, pad = "0")))
    }

    raw %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(weight_types),
        names_to = "weight_type",
        values_to = "weight"
      ) %>%
      dplyr::group_by(gisjoin_source, weight_type) %>%
      dplyr::summarise(
        weight_sum = sum_preserve_all_na(weight),
        n_parts = dplyr::n(),
        n_missing_parts = sum(is.na(weight)),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        source_decade = as.integer(source_decade),
        gisjoin_source,
        county_fips_source = gisjoin_to_fips(gisjoin_source),
        weight_type,
        weight_sum,
        n_parts,
        n_missing_parts,
        available = !is.na(weight_sum) & weight_sum > tolerance,
        zero_or_undefined_support = is.na(weight_sum) | weight_sum <= tolerance
      )
  })
}

read_ftz_raw_target_rows <- function(source_decade, zip_path, weight_types = c("m2_weight", "m4_weight", "m5_weight", "m6_weight")) {
  if (source_decade == 1990L) return(tibble::tibble())
  file_name <- paste0("Crosswalk_", source_decade, "_1990.csv")
  source_col <- paste0("gisjoin_", source_decade)
  raw <- readr::read_csv(
    unz(zip_path, file_name),
    col_select = dplyr::all_of(c(source_col, "gisjoin_1990", weight_types)),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      source_decade = as.integer(source_decade),
      gisjoin_source = as.character(.data[[source_col]]),
      gisjoin_source = dplyr::if_else(
        source_decade == 2020L,
        fips_to_gisjoin(stringr::str_pad(gisjoin_source, 5, pad = "0")),
        gisjoin_source
      ),
      county_fips_source = gisjoin_to_fips(gisjoin_source),
      county_fips_1990 = gisjoin_to_fips(gisjoin_1990)
    )
  raw %>%
    dplyr::select(source_decade, gisjoin_source, county_fips_source, gisjoin_1990, county_fips_1990, dplyr::all_of(weight_types))
}

build_target_weight_support_diagnostics <- function(zip_path, source_decades, county_pres, cz_lookup,
                                                    selected_crosswalks, tolerance = 1e-6,
                                                    adh_czones = NULL) {
  weight_types <- c("m2_weight", "m4_weight", "m5_weight", "m6_weight")
  source_support <- preflight_ftz_builtup_support(zip_path, source_decades, tolerance, weight_types)

  raw_targets <- purrr::map_dfr(setdiff(as.integer(source_decades), 1990L), read_ftz_raw_target_rows,
                                zip_path = zip_path, weight_types = weight_types)
  support_long <- source_support %>%
    dplyr::select(source_decade, gisjoin_source, county_fips_source, weight_type, weight_sum, available)

  # Attach election-year vote mass so the map can indicate which 1990 counties/CZs
  # actually receive votes from source counties whose M2/M4/M5/M6 support is unavailable.
  source_votes <- county_pres %>%
    dplyr::select(year, source_decade, gisjoin_source, county_fips, state_fips, state, county_name, twoparty_votes)

  # Use the selected/fallback crosswalk weights as the vote-mass allocator for the
  # demonstration map. A target is flagged only if the selected/fallback crosswalk
  # sends positive vote mass to that 1990 target from a source county where the
  # diagnostic weight type is unavailable. This avoids over-flagging ordinary
  # zero-weight target rows in the raw many-to-many FTZ crosswalk.
  selected_targets <- selected_crosswalks %>%
    dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > tolerance) %>%
    dplyr::select(source_decade, gisjoin_source, county_fips_1990,
                  selected_crosswalk_weight = crosswalk_weight) %>%
    dplyr::distinct()

  target_long <- raw_targets %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(weight_types),
      names_to = "weight_type",
      values_to = "raw_weight_for_type"
    ) %>%
    dplyr::left_join(support_long, by = c("source_decade", "gisjoin_source", "county_fips_source", "weight_type")) %>%
    dplyr::left_join(source_votes, by = c("source_decade", "gisjoin_source"), relationship = "many-to-many") %>%
    dplyr::left_join(selected_targets, by = c("source_decade", "gisjoin_source", "county_fips_1990"), relationship = "many-to-many") %>%
    dplyr::mutate(
      unavailable = !dplyr::coalesce(available, FALSE),
      selected_crosswalk_weight = dplyr::coalesce(selected_crosswalk_weight, 0),
      received_vote_mass_proxy = twoparty_votes * selected_crosswalk_weight,
      receives_positive_vote_mass = !is.na(received_vote_mass_proxy) & received_vote_mass_proxy > 0,
      affected_received_vote_mass_proxy = dplyr::if_else(
        unavailable & receives_positive_vote_mass,
        received_vote_mass_proxy,
        0
      )
    )

  county_diag <- target_long %>%
    dplyr::filter(!is.na(county_fips_1990), !is.na(year)) %>%
    dplyr::group_by(county_fips_1990, weight_type) %>%
    dplyr::summarise(
      receives_unavailable_source = any(affected_received_vote_mass_proxy > 0, na.rm = TRUE),
      n_unavailable_source_counties = dplyr::n_distinct(county_fips_source[affected_received_vote_mass_proxy > 0]),
      unavailable_source_vote_mass_proxy = sum(affected_received_vote_mass_proxy, na.rm = TRUE),
      total_received_vote_mass_proxy = sum(received_vote_mass_proxy, na.rm = TRUE),
      share_received_from_unavailable_sources = dplyr::if_else(
        total_received_vote_mass_proxy > 0,
        unavailable_source_vote_mass_proxy / total_received_vote_mass_proxy,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::left_join(cz_lookup %>% dplyr::select(county_fips_1990, czone), by = "county_fips_1990")

  if (!is.null(adh_czones)) {
    adh_czones <- unique(as.integer(adh_czones))
    county_diag <- county_diag %>% dplyr::filter(czone %in% adh_czones)
  }

  cz_diag <- county_diag %>%
    dplyr::filter(!is.na(czone)) %>%
    dplyr::group_by(czone, weight_type) %>%
    dplyr::summarise(
      receives_unavailable_source = any(receives_unavailable_source %in% TRUE, na.rm = TRUE),
      n_1990_counties_receiving_unavailable_source = sum(receives_unavailable_source %in% TRUE, na.rm = TRUE),
      n_unavailable_source_counties = sum(n_unavailable_source_counties, na.rm = TRUE),
      unavailable_source_vote_mass_proxy = sum(unavailable_source_vote_mass_proxy, na.rm = TRUE),
      total_received_vote_mass_proxy = sum(total_received_vote_mass_proxy, na.rm = TRUE),
      share_received_from_unavailable_sources = dplyr::if_else(
        total_received_vote_mass_proxy > 0,
        unavailable_source_vote_mass_proxy / total_received_vote_mass_proxy,
        NA_real_
      ),
      .groups = "drop"
    )

  list(source_support = source_support, county_diag = county_diag, cz_diag = cz_diag)
}

build_analysis_data <- function(config = CONFIG, stop_on_fatal = TRUE) {
  load_required_packages(config)
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

  std_controls <- standardize_baseline_controls(adh_sample, config$baseline_controls)
  adh_sample <- std_controls$data
  readr::write_csv(std_controls$summary, config$baseline_control_summary_csv)

  control_corr <- stats::cor(
    adh_sample[, config$baseline_controls, drop = FALSE],
    use = "pairwise.complete.obs"
  )
  control_corr_long <- as.data.frame(as.table(control_corr), stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(control_1 = Var1, control_2 = Var2, correlation = Freq)
  readr::write_csv(control_corr_long, config$baseline_control_correlation_csv)

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

  builtup_preflight <- preflight_support %>%
    dplyr::filter(weight_type %in% c("m5_weight", "m6_weight"))
  if (any(builtup_preflight$zero_or_undefined_support)) {
    summary_msg <- preflight_summary %>%
      dplyr::filter(weight_type %in% c("m5_weight", "m6_weight"), n_zero_or_undefined_support > 0) %>%
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

  bridge_exceptions <- read_county_bridge_exceptions(config)
  if (nrow(bridge_exceptions) > 0) {
    readr::write_csv(bridge_exceptions, file.path(config$diagnostic_dir, "county_bridge_exceptions_applied.csv"))
  }

  county_pres <- county_pres_validation %>%
    dplyr::filter(usable_for_bridge) %>%
    dplyr::mutate(source_decade_default = year_to_source_decade(year)) %>%
    dplyr::left_join(
      bridge_exceptions %>%
        dplyr::select(year, county_fips, override_source_decade, override_county_fips_source, exception_issue_class = issue_class, exception_reason = reason),
      by = c("year", "county_fips")
    ) %>%
    dplyr::mutate(
      source_decade = dplyr::coalesce(override_source_decade, source_decade_default),
      county_fips_for_crosswalk = dplyr::coalesce(override_county_fips_source, county_fips),
      gisjoin_source = fips_to_gisjoin(county_fips_for_crosswalk),
      source_scope = dplyr::if_else(is_adh_mainland_source(state_fips), "adh_mainland_eligible", "excluded_nonmainland"),
      bridge_exception_applied = !is.na(override_county_fips_source) | !is.na(override_source_decade)
    ) %>%
    dplyr::select(
      year, county_fips, county_fips_for_crosswalk, state_fips, state, county_name, rep_votes, dem_votes,
      twoparty_votes, total_votes, source_decade, source_decade_default, gisjoin_source, source_scope,
      bridge_exception_applied, exception_issue_class, exception_reason
    )

  target_support <- build_target_weight_support_diagnostics(
    zip_path = ftz_zip,
    source_decades = config$source_decades,
    county_pres = county_pres,
    cz_lookup = cz_lookup,
    selected_crosswalks = crosswalks,
    tolerance = config$weight_sum_tolerance,
    adh_czones = adh_sample$czone
  )
  readr::write_csv(target_support$source_support, config$all_weight_support_csv)
  readr::write_csv(target_support$county_diag, config$target_weight_support_county_csv)
  readr::write_csv(target_support$cz_diag, config$target_weight_support_cz_csv)

  source_vote_denominators <- county_pres %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(source_two_party_votes_prejoin = sum(twoparty_votes, na.rm = TRUE), .groups = "drop")

  positive_target_summary <- crosswalks %>%
    dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > config$weight_sum_tolerance) %>%
    dplyr::left_join(cz_lookup %>% dplyr::select(county_fips_1990, czone), by = "county_fips_1990") %>%
    dplyr::group_by(source_decade, gisjoin_source) %>%
    dplyr::summarise(
      positive_target_1990_counties = paste(sort(unique(county_fips_1990)), collapse = ";"),
      positive_target_czones = paste(sort(unique(stats::na.omit(czone))), collapse = ";"),
      positive_target_count = dplyr::n_distinct(county_fips_1990),
      positive_target_cz_count = dplyr::n_distinct(czone, na.rm = TRUE),
      .groups = "drop"
    )

  # Source-level issue report. Do not filter on row-level crosswalk_weight <= 0:
  # valid source counties naturally have many zero-weight target rows in the raw
  # many-to-many FTZ crosswalk. A source is an issue only if its selected raw
  # weight is undefined/invalid, its final weight sum is invalid, or it has no
  # positive final target weight after the chosen fallback policy.
  crosswalk_source_issue_rows <- crosswalk_source_diagnostics %>%
    dplyr::mutate(
      no_positive_final_target = positive_targets == 0,
      final_weight_invalid = is.na(crosswalk_weight_sum) |
        abs(crosswalk_weight_sum - 1) > config$weight_sum_tolerance
    ) %>%
    dplyr::filter(raw_weight_undefined | raw_weight_requires_policy | final_weight_invalid | no_positive_final_target) %>%
    dplyr::left_join(positive_target_summary, by = c("source_decade", "gisjoin_source"))

  crosswalk_issue_vote_report <- county_pres %>%
    dplyr::left_join(
      crosswalk_source_issue_rows,
      by = c("source_decade", "gisjoin_source")
    ) %>%
    dplyr::filter(
      raw_weight_undefined %in% TRUE |
        raw_weight_requires_policy %in% TRUE |
        final_weight_invalid %in% TRUE |
        no_positive_final_target %in% TRUE
    ) %>%
    dplyr::left_join(source_vote_denominators, by = "year") %>%
    dplyr::mutate(
      vote_share_of_year = twoparty_votes / source_two_party_votes_prejoin,
      source_issue_class = dplyr::case_when(
        no_positive_final_target %in% TRUE & fallback_used %in% TRUE ~ paste0(
          "selected_", weight_slug(config$crosswalk_weight), "_unavailable_", fallback_policy, "_used_but_no_positive_target"
        ),
        no_positive_final_target %in% TRUE & raw_weight_requires_policy %in% TRUE ~ paste0(
          "selected_", weight_slug(config$crosswalk_weight), "_unavailable_no_fallback"
        ),
        no_positive_final_target %in% TRUE ~ classify_unmatched_county(state_fips, county_fips, county_name, year),
        raw_weight_requires_policy %in% TRUE & fallback_used %in% TRUE ~ paste0(
          "selected_", weight_slug(config$crosswalk_weight), "_unavailable_", fallback_policy, "_used"
        ),
        raw_weight_requires_policy %in% TRUE & !(fallback_used %in% TRUE) ~ paste0(
          "selected_", weight_slug(config$crosswalk_weight), "_unavailable_no_fallback"
        ),
        final_weight_invalid %in% TRUE ~ "final_weight_invalid",
        TRUE ~ "reported_crosswalk_issue"
      )
    ) %>%
    dplyr::select(
      year, source_decade, source_decade_default, source_scope, source_issue_class,
      state_fips, state, county_fips, county_fips_for_crosswalk, county_name,
      bridge_exception_applied, exception_issue_class, exception_reason,
      twoparty_votes, source_two_party_votes_prejoin, vote_share_of_year,
      weight_sum_raw, selected_weight_sum_raw, crosswalk_weight_sum,
      raw_weight_undefined, raw_weight_sum_valid, raw_weight_requires_policy,
      fallback_used, fallback_policy, no_positive_final_target, final_weight_invalid,
      n_targets, positive_targets, positive_target_1990_counties, positive_target_czones,
      positive_target_count, positive_target_cz_count
    ) %>%
    dplyr::arrange(year, source_issue_class, state_fips, county_fips)
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
    dplyr::group_by(year, source_decade, source_decade_default, county_fips, county_fips_for_crosswalk, state_fips, state, county_name, source_scope, bridge_exception_applied, exception_issue_class, exception_reason) %>%
    dplyr::summarise(
      twoparty_votes = dplyr::first(twoparty_votes),
      has_positive_crosswalk_weight = any(!is.na(crosswalk_weight) & crosswalk_weight > 0),
      raw_weight_undefined = any(raw_weight_undefined %in% TRUE, na.rm = TRUE),
      fallback_used = any(fallback_used %in% TRUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(source_vote_denominators, by = "year") %>%
    dplyr::mutate(
      vote_share_of_year = twoparty_votes / source_two_party_votes_prejoin,
      source_issue_class = classify_unmatched_county(state_fips, county_fips, county_name, year)
    ) %>%
    dplyr::filter(!has_positive_crosswalk_weight)
  readr::write_csv(unmatched_source_counties, config$unmatched_source_counties_csv)
  readr::write_csv(
    unmatched_source_counties %>%
      dplyr::count(year, source_scope, source_issue_class, name = "unmatched_source_counties") %>%
      dplyr::left_join(
        unmatched_source_counties %>%
          dplyr::group_by(year, source_scope, source_issue_class) %>%
          dplyr::summarise(unmatched_two_party_votes = sum(twoparty_votes, na.rm = TRUE), .groups = "drop"),
        by = c("year", "source_scope", "source_issue_class")
      ),
    config$county_bridge_classification_csv
  )

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

  original_by_scope <- county_pres %>%
    dplyr::group_by(year, source_scope) %>%
    dplyr::summarise(
      source_counties = dplyr::n_distinct(county_fips),
      source_two_party_votes_prejoin = sum(twoparty_votes, na.rm = TRUE),
      .groups = "drop"
    )
  bridged_by_scope <- county_bridged %>%
    dplyr::filter(!is.na(crosswalk_weight), crosswalk_weight > 0) %>%
    dplyr::group_by(year, source_scope) %>%
    dplyr::summarise(
      matched_source_counties = dplyr::n_distinct(county_fips),
      bridged_two_party_votes = sum(twoparty_votes * crosswalk_weight, na.rm = TRUE),
      fallback_source_counties = dplyr::n_distinct(county_fips[fallback_used %in% TRUE]),
      .groups = "drop"
    )
  retention_by_scope <- original_by_scope %>%
    dplyr::left_join(bridged_by_scope, by = c("year", "source_scope")) %>%
    dplyr::mutate(
      matched_source_counties = dplyr::coalesce(matched_source_counties, 0L),
      bridged_two_party_votes = dplyr::coalesce(bridged_two_party_votes, 0),
      fallback_source_counties = dplyr::coalesce(fallback_source_counties, 0L),
      unmatched_source_counties = source_counties - matched_source_counties,
      retained_vote_share = bridged_two_party_votes / source_two_party_votes_prejoin
    )
  readr::write_csv(retention_by_scope, config$mainland_retention_csv)

  mainland_retention_wide <- retention_by_scope %>%
    dplyr::select(year, source_scope, source_two_party_votes_prejoin, bridged_two_party_votes, retained_vote_share) %>%
    tidyr::pivot_wider(
      names_from = source_scope,
      values_from = c(source_two_party_votes_prejoin, bridged_two_party_votes, retained_vote_share),
      names_sep = "_"
    )

  bridge_diagnostics <- original_by_year %>%
    dplyr::left_join(naive_postjoin_denominator, by = "year") %>%
    dplyr::left_join(bridged_by_year, by = "year") %>%
    dplyr::left_join(mainland_retention_wide, by = "year") %>%
    dplyr::mutate(
      matched_source_counties = dplyr::coalesce(matched_source_counties, 0L),
      unmatched_source_counties = source_counties - matched_source_counties,
      retained_vote_share_correct = bridged_two_party_votes / source_two_party_votes_prejoin,
      retained_vote_share = retained_vote_share_correct,
      retained_vote_share_adh_mainland_eligible = retained_vote_share_adh_mainland_eligible
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
    reported_check(
      "crosswalk_all_source_weight_sums_equal_one_or_reported",
      n_reported = invalid_final_weight_count,
      details = paste0(
        "Global non-ADH or unsupported source-county rows are reported rather than treated as validation failures. ",
        "Estimation is gated by positive final weights, retained vote share, and panel balance. Tolerance: ",
        config$weight_sum_tolerance
      )
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

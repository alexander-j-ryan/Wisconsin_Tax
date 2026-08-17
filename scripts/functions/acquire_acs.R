# acquire_acs.R — ACS acquisition helpers (county tables + PUMS)
#
# Purpose : Pull the Census denominators and microdata the project needs for
#           Layer 2 (geographic redistribution) and Layer 4 (household
#           incidence). County tables supply per-capita / per-household /
#           per-pupil denominators and home-value / property-tax context.
#           County income x race counts supply the denominator for the
#           CEX/sales-tax allocation. PUMS supplies the household microdata
#           for statewide incidence.
# Inputs  : Census API key (set once via tidycensus::census_api_key(), or
#           CENSUS_API_KEY in the environment).
# Outputs : tibbles (callers persist them — see 02_code/1a-pull-acs.R).
# Depends : tidycensus, dplyr, tidyr, purrr, stringr, jsonlite
# Author  : Lade lab (drafted for Alex)
# Modified: 2026-07-20
#
# Notes
#   * Default vintage is the 2020–2024 ACS 5-year release (year = 2024),
#     matching the data ledger row ACS_PUMS_WI_2024_5yr.
#   * County tables are the *denominator* source; PUMS is the *incidence*
#     source. Do not use county medians for household microsimulation.

# --------------------------------------------------------------------------
# COUNTY DENOMINATORS — ACS 5-year detailed tables (Layer 2 base)
# --------------------------------------------------------------------------

#' Variable dictionary for the county-level ACS pull
#'
#' Readable names map to ACS detailed-table variable IDs. Edit here, not in
#' the call site, so the ledger and the appendix stay in sync with the codes.
#'
#' @return Named character vector: names = readable, values = ACS variable IDs.
acs_county_vars <- function() {
  c(
    population         = "B01003_001",  # total population
    households         = "B11001_001",  # total households
    med_hh_income      = "B19013_001",  # median household income ($)
    occ_units          = "B25003_001",  # occupied housing units
    owner_occ          = "B25003_002",  # owner-occupied units
    renter_occ         = "B25003_003",  # renter-occupied units
    med_home_value     = "B25077_001",  # median value, owner-occupied ($)
    med_gross_rent     = "B25064_001",  # median gross rent ($)
    med_real_est_tax   = "B25103_001",  # median real estate taxes paid ($)
    # Public K-12 school enrollment (S06 benefit-incidence per-pupil
    # denominator, 2026-07-16 batch item #65): B14003 (SEX BY SCHOOL
    # ENROLLMENT BY TYPE OF SCHOOL BY AGE), "enrolled in public school",
    # ages 5-9/10-14/15-17, male + female -- the closest ACS age-band
    # approximation to grades K-12. acquire_acs_county() sums these six
    # cells into one public_school_enrollment column. ODE official ADM
    # (average daily membership) is the noted upgrade path if a district-
    # exact count is ever needed.
    pub_school_m_5_9   = "B14003_005",
    pub_school_m_10_14 = "B14003_006",
    pub_school_m_15_17 = "B14003_007",
    pub_school_f_5_9   = "B14003_033",
    pub_school_f_10_14 = "B14003_034",
    pub_school_f_15_17 = "B14003_035"
  )
}

#' Pull ACS 5-year county tables for Wisconsin
#'
#' Wide-format county table with the denominators and housing-cost context the
#' fiscal layers need, plus owner share. Returns the raw ACS medians
#' (med_real_est_tax, med_home_value) as-is; it does NOT derive an approximate
#' effective property tax rate from them. That effective-rate cross-check (a
#' ratio of two county medians, not a median of household-level ratios) is
#' computed downstream as an audit exhibit by 02_code/1a-pull-acs.R
#' (05_output/audit/acs_effective_rate_crosscheck.csv) — the authoritative
#' effective rate comes from owner_effective_rate() (02_code/functions/
#' incidence.R), not ACS medians.
#'
#' @param year   Integer ACS 5-year end-year. Default 2024.
#' @param survey ACS survey string. Default "acs5".
#' @param state  Two-letter state. Default "WI".
#' @return A tibble, one row per county, wide.
acquire_acs_county <- function(year = 2024, survey = "acs5", state = "WI") {
  raw <- tidycensus::get_acs(
    geography = "county",
    state     = state,
    variables = acs_county_vars(),
    year      = year,
    survey    = survey,
    output    = "wide",
    geometry  = FALSE
  )

  # Keep estimates (cols ending in "E"), drop margins ("M"). Strip the "E"
  # suffix from estimate columns ONLY — not from GEOID/NAME (NAME ends in "E"
  # too, so a blanket sub() would turn it into "NAM").
  est <- raw |>
    dplyr::select(GEOID, NAME, dplyr::ends_with("E")) |>
    dplyr::rename_with(~ stringr::str_remove(.x, "E$"), .cols = -c(GEOID, NAME))

  dplyr::mutate(
    est,
    county = stringr::str_remove(NAME, " County, Wisconsin$"),
    owner_share = owner_occ / occ_units,
    # Public K-12 school enrollment (S06 per-pupil denominator): sum the six
    # age-band x sex cells (see acs_county_vars()'s comment for the exact
    # B14003 codes), then drop the six raw cells so they don't leak into
    # downstream tables as unlabeled columns.
    public_school_enrollment = pub_school_m_5_9 + pub_school_m_10_14 + pub_school_m_15_17 +
      pub_school_f_5_9 + pub_school_f_10_14 + pub_school_f_15_17,
    acs_vintage = paste0(survey, "_", year)
  ) |>
    dplyr::select(-pub_school_m_5_9, -pub_school_m_10_14, -pub_school_m_15_17,
                  -pub_school_f_5_9, -pub_school_f_10_14, -pub_school_f_15_17)
}

# --------------------------------------------------------------------------
# COUNTY INCOME x RACE HOUSEHOLD COUNTS — CEX/sales-tax allocation denominator
# --------------------------------------------------------------------------

#' Race-of-householder ACS household income tables
#'
#' Maps each race-iteration of ACS table B19001 (household income in the past
#' 12 months) to a readable race-group label. B19001F and B19001G both
#' collapse to "Other or multiple" for this project's race groupings.
#'
#' @return Data frame: table (ACS table ID), race_group (readable label).
acs_income_race_tables <- function() {
  data.frame(
    table = c(
      "B19001A",
      "B19001B",
      "B19001C",
      "B19001D",
      "B19001E",
      "B19001F",
      "B19001G"
    ),
    race_group = c(
      "White",
      "Black",
      "American Indian or Alaska Native",
      "Asian",
      "Native Hawaiian or Pacific Islander",
      "Other or multiple",
      "Other or multiple"
    ),
    stringsAsFactors = FALSE
  )
}

#' ACS income bins collapsed to CEX income groups
#'
#' Table B19001's 16 income bins (variable suffixes 002-017) collapsed to the
#' six income groups used in the CEX sales/use-tax allocation, so ACS
#' household counts and CEX expenditure shares can be joined on a common
#' income axis.
#'
#' @return Data frame: suffix (ACS variable suffix, "002".."017"),
#'   income_group (readable CEX income-group label).
acs_income_bin_map <- function() {
  data.frame(
    suffix = sprintf("%03d", 2:17),
    income_group = c(
      "<$25k",
      "<$25k",
      "<$25k",
      "<$25k",

      "$25k-$49,999",
      "$25k-$49,999",
      "$25k-$49,999",
      "$25k-$49,999",
      "$25k-$49,999",

      "$50k-$74,999",
      "$50k-$74,999",

      "$75k-$99,999",

      "$100k-$149,999",
      "$100k-$149,999",

      "$150k+",
      "$150k+"
    ),
    stringsAsFactors = FALSE
  )
}

#' Pull ACS county household counts by income group x race of householder
#'
#' Denominator for the CEX sales/use-tax allocation. Iterates the seven
#' race-iterated B19001 tables via the raw Census API (jsonlite::fromJSON),
#' then collapses ACS's 16 income bins to the project's six CEX income groups
#' and sums counts within county x income_group x race_group. MOEs are
#' aggregated with the Census formula for a sum of estimates (sqrt of sum of
#' squared MOEs) and are therefore approximate once bins are combined.
#'
#' @param year        Integer ACS 5-year end-year for the pull. Default 2024.
#' @param survey      ACS survey string. Default "acs5".
#' @param state       Two-letter state, or a 2-digit state FIPS. Default "WI".
#' @param output_year Year to stamp on the output. Lets the county
#'   denominator be labeled for the calendar year of the tax file it joins to
#'   (e.g. the 2025 sales/use-tax allocation) without re-pulling an ACS
#'   vintage that doesn't exist yet. Defaults to `year`.
#' @return Tibble: county_fips, county, year, acs_vintage, income_group,
#'   race_group, group_cu_count, group_cu_count_moe_approx. acs_vintage
#'   records the ACTUAL PULL vintage (survey_year), not output_year -- the
#'   two diverge whenever output_year relabels the join year.
acquire_acs_county_income_race_counts <- function(year = 2024,
                                                    survey = "acs5",
                                                    state = "WI",
                                                    output_year = NULL) {
  if (is.null(output_year)) {
    output_year <- year
  }

  if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    stop("CENSUS_API_KEY is not set in this R session.", call. = FALSE)
  }

  race_tables <- acs_income_race_tables()
  income_bins <- acs_income_bin_map()

  # Convert "WI" to "55"; also allow state = "55" directly.
  state_fips <- if (stringr::str_detect(state, "^\\d{2}$")) {
    state
  } else {
    tidycensus::fips_codes |>
      dplyr::filter(.data$state == !!state) |>
      dplyr::distinct(.data$state_code) |>
      dplyr::pull(.data$state_code) |>
      dplyr::first()
  }

  if (is.na(state_fips) || !nzchar(state_fips)) {
    stop("Could not resolve state FIPS for state = ", state, call. = FALSE)
  }

  pull_one_income_race_table <- function(table_id, race_group_label) {
    message("  Pulling ", table_id, " for ", race_group_label, " via Census API ...")

    # Estimate and MOE variables for B19001A_002 through B19001A_017.
    var_map_one <- income_bins |>
      dplyr::mutate(
        table = table_id,
        race_group = race_group_label,
        estimate_var = paste0(table_id, "_", suffix, "E"),
        moe_var = paste0(table_id, "_", suffix, "M")
      )

    api_vars <- c("NAME", var_map_one$estimate_var, var_map_one$moe_var)

    base_url <- paste0("https://api.census.gov/data/", year, "/acs/", survey)

    query_url <- paste0(
      base_url,
      "?get=", paste(api_vars, collapse = ","),
      "&for=county:*",
      "&in=state:", state_fips,
      "&key=", Sys.getenv("CENSUS_API_KEY")
    )

    api_json <- tryCatch(
      jsonlite::fromJSON(query_url),
      error = function(e) {
        stop(
          "Census API request failed for ", table_id, ".\n",
          "URL without key:\n",
          paste0(base_url, "?get=", paste(api_vars, collapse = ","),
                 "&for=county:*", "&in=state:", state_fips),
          "\nOriginal error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )

    # Census API returns a matrix-like object where row 1 is column names.
    api_df <- as.data.frame(api_json[-1, ], stringsAsFactors = FALSE)
    names(api_df) <- api_json[1, ]

    long_est <- api_df |>
      dplyr::select(GEOID = county, NAME, dplyr::all_of(var_map_one$estimate_var)) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(var_map_one$estimate_var),
        names_to = "estimate_var",
        values_to = "estimate"
      )

    long_moe <- api_df |>
      dplyr::select(GEOID = county, dplyr::all_of(var_map_one$moe_var)) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(var_map_one$moe_var),
        names_to = "moe_var",
        values_to = "moe"
      ) |>
      dplyr::mutate(estimate_var = stringr::str_replace(moe_var, "M$", "E")) |>
      dplyr::select(GEOID, estimate_var, moe)

    long_est |>
      dplyr::left_join(long_moe, by = c("GEOID", "estimate_var")) |>
      dplyr::left_join(var_map_one, by = "estimate_var") |>
      dplyr::mutate(
        state = state_fips,
        county = GEOID,
        GEOID = paste0(state, county),
        estimate = suppressWarnings(as.numeric(estimate)),
        moe = suppressWarnings(as.numeric(moe))
      ) |>
      dplyr::select(GEOID, NAME, variable = estimate_var, estimate, moe, suffix, income_group, race_group)
  }

  raw <- purrr::map2_dfr(race_tables$table, race_tables$race_group, pull_one_income_race_table)

  # acs_vintage must record the ACTUAL PULL year, computed from the `year`
  # argument BEFORE it is overwritten by output_year below -- otherwise the
  # vintage label silently claims whatever calendar year the file is being
  # relabeled for (e.g. "acs5_2025" when no such ACS release exists).
  vintage_lbl <- paste0(survey, "_", year)

  raw |>
    dplyr::mutate(
      county_fips = GEOID,
      county = stringr::str_remove(NAME, " County, Wisconsin$"),
      year = as.integer(output_year),
      acs_vintage = vintage_lbl
    ) |>
    dplyr::group_by(county_fips, county, year, acs_vintage, income_group, race_group) |>
    dplyr::summarise(
      group_cu_count = sum(estimate, na.rm = TRUE),
      # Census MOE-aggregation formula for a sum of estimates.
      group_cu_count_moe_approx = sqrt(sum(moe^2, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::arrange(county_fips, income_group, race_group)
}

# --------------------------------------------------------------------------
# PUMS MICRODATA — Wisconsin household/person records (Layer 4 incidence base)
# --------------------------------------------------------------------------

#' Pull ACS 5-year PUMS for Wisconsin (household microdata)
#'
#' Person-level records with housing variables attached; filter to SPORDER == 1
#' downstream to get one row per household, and use WGTP (housing weight) for
#' household-weighted estimates. TAXAMT (property tax bin) and VALP (home value)
#' drive the property-tax allocation in Layer 4.
#'
#' @param year   Integer ACS 5-year end-year. Default 2024.
#' @param survey ACS survey string. Default "acs5".
#' @param state  Two-letter state. Default "WI".
#' @param recode Logical; attach factor labels. Default FALSE, matching the
#'   default `year = 2024`: tidycensus 1.7.3's `recode = TRUE` path fails on
#'   year = 2024 with a "Join columns in `x` must be present in the data.
#'   Problem with `STATE_label`" error (verified 2026-07-20). Safe under
#'   FALSE for this project -- see 1a-pull-acs.R's own_rent case_when, which
#'   already dual-handles TEN as code or label, and this function's own
#'   post-pull numeric coercion below. Callers pulling a PRE-2024 vintage
#'   (where `recode = TRUE` works) may pass it explicitly; that path is not
#'   verified against this project's downstream code post-2026-07-20.
#' @return A tibble of PUMS records with replicate housing weights.
acquire_acs_pums <- function(year = 2024,
                              survey = "acs5",
                              state = "WI",
                              recode = FALSE) {
  raw <- tidycensus::get_pums(
    variables = c(
      # --- geography & household/person structure ---
      "PUMA",     # geography (PUMA, not county)
      "SPORDER",  # person number within household (==1 -> householder row)
      "NP",       # number of persons in household
      "RELSHIPP", # relationship to householder (build tax units / dependents)
      "AGEP",     # age (filing status, dependent/elderly tests)
      "MAR",      # marital status (joint vs single filing)
      "SEX",
      # --- housing (property-tax incidence) ---
      "TEN",      # tenure (own / rent)
      "VALP",     # property value ($)
      "TAXAMT",   # annual property tax, dollars (-1 = not applicable)
      "GRNTP",    # gross rent ($)
      "BDSP",     # bedrooms
      # --- income: household total + the person-level components TAXSIM needs ---
      "HINCP",    # household income ($)
      "PINCP",    # person total income ($)
      "WAGP",     # wages/salary ($)        [ADJINC]
      "SEMP",     # self-employment ($)     [ADJINC]
      "INTP",     # interest/dividend/net-rental ($)  [ADJINC]
      "RETP",     # retirement income ($)   [ADJINC]
      "SSP",      # Social Security ($)     [ADJINC]
      "SSIP",     # Supplemental Security Income ($)  [ADJINC]
      "PAP",      # public assistance ($)   [ADJINC]
      "OIP",      # other income ($)        [ADJINC]
      # --- inflation adjustment factors ---
      "ADJHSG",   # housing-dollar adjustment (5-yr files)
      "ADJINC"    # income adjustment (5-yr files)
    ),
    state        = state,
    survey       = survey,
    year         = year,
    recode       = recode,
    rep_weights  = "housing"   # WGTP1..WGTP80 for household-level SEs. For person-
                               # weighted person-level stats, do a rep_weights =
                               # "person" pull (adds PWGTP1..80); base PWGTP is
                               # returned regardless.
  )

  # get_pums(recode = FALSE) returns EVERY non-replicate-weight column as
  # character (verified 2026-07-20: HINCP, TAXAMT, VALP, GRNTP, NP, BDSP,
  # etc. all arrive as character; only WGTP/PWGTP/WGTP1..80 are numeric).
  # recode = TRUE appears to numeric-coerce these internally, which is why
  # this never surfaced before the 2024-vintage recode=FALSE workaround
  # (see this function's @param recode note). Coerce the CONTINUOUS dollar/
  # count fields explicitly and unconditionally here, at the source, so
  # every caller gets arithmetic-ready columns regardless of the recode
  # setting -- do NOT rely on get_pums()'s internal behavior. CODE fields
  # (TEN, RELSHIPP, MAR, SEX, PUMA, STATE, SPORDER) are intentionally left
  # untouched: callers already compare them as strings (e.g. 1a-pull-acs.R's
  # own_rent case_when) or cast them explicitly where a numeric form is
  # needed (e.g. 3b-income-incidence.R's as.integer(SPORDER)).
  dplyr::mutate(
    raw,
    dplyr::across(
      c(HINCP, TAXAMT, VALP, GRNTP, NP, BDSP),
      ~ suppressWarnings(as.numeric(.x))
    )
  )
}

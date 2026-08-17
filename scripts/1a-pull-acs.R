############################################################################
# Wisconsin Property Tax Abolition Analysis
# THIS FILE:
#   1) Pulls ACS 5-year county tables (population, households, income,
#      tenure, home value, gross rent, real estate tax) for Wisconsin's 72
#      counties — the Layer 2 fiscal-gap denominators.
#   2) Pulls ACS county household counts by income group x race of
#      householder — the denominator for the CEX/sales-tax allocation.
#   3) Pulls Wisconsin ACS PUMS microdata and builds the household-level
#      (Layer 4 incidence base) and person-level (tax-unit construction
#      base) processed files.
#   4) Pulls ACS county household-income quintile thresholds (table
#      B19080) and quintile-based household counts (2024 vintage).
# INPUTS:  Census API key in the environment (see the one-time setup note
#          below)
# OUTPUTS: data/clean/acs_county_wi_2024_5yr.parquet
#          data/clean/pums_households_wi.parquet
#          data/clean/pums_persons_wi.parquet
#          data/clean/income_quintile_persons_wi.parquet
#          data/clean/wisconsin_county_income_quintile_values.parquet
#          data/cleanintermediate/pums_wi_2024_5yr.parquet
#          data/clean/intermediate/cex/county_income_race_counts.csv
#          data/clean/intermediate/cex/diagnostic_county_income_race_totals.csv
# NOTES:   Large PUMS download — a full run takes several minutes. ACS vintage
#          is 2020-2024 5-year (year = 2024); PUMS pull uses recode = FALSE
#          because tidycensus 1.7.3's recode=TRUE path fails on this vintage
#          with a STATE_label join error (verified 2026-07-20; safe here --
#          TEN is the only label-dependent field and is dual-handled below).
# Last updated: 2026-07-20
############################################################################

# One-time setup (do NOT commit your key):
#   install.packages("tidycensus")
#   tidycensus::census_api_key("YOUR_KEY", install = TRUE)   # request at
#     https://api.census.gov/data/key_signup.html
#   readRenviron("~/.Renviron")

# --- SETUP ---
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)
library(arrow)
library(here)
library(jsonlite)

source(here::here("scripts/functions/acquire_acs.R"))
source(here::here("scripts/functions/utils.R"))  # write_parquet_safe()
source(here::here("scripts/functions/incidence.R"))  # owner_effective_rate()

set.seed(20260618)

ACS_YEAR   <- 2024L
ACS_SURVEY <- "acs5"

############################################################################
# I. PREFLIGHT — CONFIRM THE CENSUS API KEY IS LOADED
############################################################################

# A missing/unrecognized key makes the Census API return an HTML error page,
# which surfaces downstream as a confusing "lexical error: invalid char in
# json text ... <html>". Fail early with a clear message instead.
if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  stop(
    "CENSUS_API_KEY is not set in this R session.\n",
    "  1. tidycensus::census_api_key(\"YOUR_KEY\", install = TRUE, overwrite = TRUE)\n",
    "  2. readRenviron(\"~/.Renviron\")   # or restart R\n",
    "  Request a key at https://api.census.gov/data/key_signup.html",
    call. = FALSE
  )
}

############################################################################
# II. COUNTY DENOMINATORS + HOUSING CONTEXT
############################################################################

message("Pulling ACS ", ACS_SURVEY, " ", ACS_YEAR, " county tables for Wisconsin ...")
acs_county <- acquire_acs_county(year = ACS_YEAR, survey = ACS_SURVEY)

stopifnot(nrow(acs_county) == 72L)  # all Wisconsin counties present

# Per-household / per-capita denominators join onto ODT county property-tax
# revenue (clean step 02) to produce the Layer-2 fiscal-gap base. We keep ONLY
# denominators + context here; revenue lives in the ODT property-tax processed file.
acs_county_out <- acs_county |>
  transmute(
    geoid = GEOID,
    county,
    population,
    households,
    occ_units,
    owner_occ,
    renter_occ,
    owner_share,
    med_hh_income,
    med_home_value,
    med_gross_rent,
    med_real_est_tax,
    public_school_enrollment,   # S06 per-pupil denominator (2026-07-16 batch item #65)
    acs_vintage
  )

write_parquet_safe(
  acs_county_out,
  here::here("data/clean/acs_county_wi_2024_5yr.parquet")
)
message("  wrote acs_county_wi_2024_5yr.parquet  (", nrow(acs_county_out), " counties)")

############################################################################
# III. COUNTY INCOME X RACE HOUSEHOLD COUNTS
#      Denominator for the CEX sales/use-tax allocation.
############################################################################

# NOTE (2026-07-20 ACS-vintage migration): this county_income_race_counts.csv
# now carries ACS_YEAR's vintage (2020-2024). The CEX branch that consumes it
# (1f -> cex-ucc-manual-merge -> 1g -> 1m) sits behind a manual UCC-review
# gate and is NOT rebuilt by `make all` -- that gate exists to protect Alex's
# hand-reviewed UCC->NAICS crosswalk from being silently clobbered by an
# automated rebuild. The gate was re-run by hand on 2026-07-20: 1f carried
# the existing crosswalk forward cleanly (zero UCCs needing a new manual
# selection, zero dropped), so sales_tax_burden_by_income_group.csv (1m's
# output) is now rebuilt on this same 2020-2024 denominator and the CEX
# chain is on a single ACS vintage; see 3c's resolved audit row
# (05_output/audit/sales_incidence_calibration_audit.csv,
# 'cex_income_race_denominator_vintage') for the closed-out record.

# Sales/use tax file is calendar year 2025.
# The county demographic denominator uses the ACS 5-year vintage set at the
# top of the script (ACS_YEAR), but we write year = 2025 so it joins to the
# ODT sales/use tax file.
SALES_TAX_ALLOCATION_YEAR <- 2025L

message("Pulling ACS ", ACS_SURVEY, " ", ACS_YEAR,
        " county income x race household counts for Wisconsin ...")

county_income_race_counts <- acquire_acs_county_income_race_counts(
  year        = ACS_YEAR,
  survey      = ACS_SURVEY,
  state       = "WI",
  output_year = SALES_TAX_ALLOCATION_YEAR
)

# This is the file expected by the CEX/Wisconsin sales-tax allocation script.
dir.create(
  here::here("data/clean/intermediate/cex"),
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  county_income_race_counts,
  here::here("data/clean/intermediate/cex/county_income_race_counts.csv")
)

message("  wrote county_income_race_counts.csv  (",
        nrow(county_income_race_counts),
        " county-income-race cells)")

# Diagnostics: should be Wisconsin counties.
stopifnot(dplyr::n_distinct(county_income_race_counts$county_fips) == 72L)

county_income_race_diag <- county_income_race_counts |>
  dplyr::group_by(county_fips, county, year, acs_vintage) |>
  dplyr::summarise(
    households_income_race_total = sum(group_cu_count, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  county_income_race_diag,
  here::here("data/clean/intermediate/cex/diagnostic_county_income_race_totals.csv")
)

############################################################################
# IV. PUMS MICRODATA — HOUSEHOLD FILE (Layer 4 incidence base)
############################################################################

message("Pulling ACS ", ACS_SURVEY, " ", ACS_YEAR, " PUMS for Wisconsin ",
        "(large download — a few minutes) ...")
# recode = FALSE: tidycensus 1.7.3's recode = TRUE path fails on the 2024
# 5-year PUMS with "Join columns in `x` must be present in the data. Problem
# with `STATE_label`" (verified 2026-07-20). Safe: TEN is the only field
# compared against a recoded label anywhere downstream, and that comparison
# already handles both the code and the label form (see own_rent below).
pums_raw <- acquire_acs_pums(year = ACS_YEAR, survey = ACS_SURVEY, recode = FALSE)

# get_pums(recode = TRUE) returns labelled/factor columns. Arrow dictionary-
# encodes factors and errors ("Cannot insert dictionary values containing
# nulls") when a factor contains NA. Coerce factors to plain character so
# parquet stores them as nullable strings. Codes (TEN, etc.) are unaffected.
pums_raw <- pums_raw |>
  dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))

write_parquet_safe(
  pums_raw,
  here::here("data/clean/pums_wi_2024_5yr.parquet")
)

# ACS adjustment factors carry 6 implied decimals (e.g., 1029928 -> 1.029928).
# Guard against either the raw-integer or an already-divided representation.
adj6 <- function(x) { x <- as.numeric(x); dplyr::if_else(x > 100, x / 1e6, x) }

# One row per occupied HOUSING UNIT (the householder record carries the housing
# variables). We must drop GROUP-QUARTERS records: people in dorms/prisons/
# nursing homes are not households. They are identifiable three ways (all agree):
# SERIALNO contains "GQ", housing weight WGTP == 0, and tenure is blank. They
# carry not-applicable placeholders (hh_income = -60000, prop_tax = -1, etc.),
# which is exactly the "unrealistic rows" symptom. Filter them out.
pums_hh <- pums_raw |>
  filter(SPORDER == 1, WGTP > 0, !stringr::str_detect(SERIALNO, "GQ")) |>
  transmute(
    serialno = SERIALNO,
    puma     = PUMA,
    wgtp     = WGTP,
    tenure   = TEN,            # code: 1 owned w/ mortgage, 2 owned free, 3 rented, 4 occ. no pay
    # Handle TEN as code OR recoded label so this is robust either way.
    own_rent = dplyr::case_when(
      TEN %in% c("1", "2") | stringr::str_starts(TEN, "Owned") ~ "owner",
      TEN == "3" | TEN == "Rented"                             ~ "renter",
      TRUE                                                     ~ "other"  # occ. without payment
    ),
    # Dollars -> constant 2024$ via the ACS adjustment factors (required when
    # pooling a 5-year file across survey years 2020-2024): ADJINC for income,
    # ADJHSG for housing dollars (TAXAMT, VALP, GRNTP).
    hh_income  = HINCP * adj6(ADJINC),
    # TAXAMT is reported as BINNED midpoints (not a continuous dollar amount)
    # in every 5-year vintage checked so far -- 2019-2023: 79 distinct bins,
    # top-coded at $24,500; 2020-2024: 80 distinct bins, top-coded at $27,500.
    # -1 = not applicable (renters / no tax) -> NA. Home value / gross rent
    # apply only to owners / renters respectively; blank-as-0 for the other
    # group -> NA.
    prop_tax   = dplyr::na_if(TAXAMT, -1) * adj6(ADJHSG),
    home_value = dplyr::if_else(own_rent == "owner",  VALP  * adj6(ADJHSG), NA_real_),
    gross_rent = dplyr::if_else(own_rent == "renter", GRNTP * adj6(ADJHSG), NA_real_),
    n_persons  = NP,
    bedrooms   = BDSP,
    across(matches("^WGTP[0-9]+$"), identity)  # 80 replicate weights for SEs (drops dup WGTP)
  )

write_parquet_safe(
  pums_hh,
  here::here("data/clean/pums_households_wi.parquet")
)
message("  wrote pums_households_wi.parquet  (", nrow(pums_hh), " households)")

############################################################################
# V. PUMS MICRODATA — PERSON FILE (tax-unit construction base)
############################################################################

# Base for tax-unit construction (state income-tax / TAXSIM incidence). Keeps
# ALL persons (not just householders); drops group quarters. Income components
# -> constant 2024$ via ADJINC. The tax-FILING-UNIT step (grouping persons
# into returns; assigning filing status & dependents) is the next job
# DOWNSTREAM and is not done here.
numA <- function(x) suppressWarnings(as.numeric(x))
pums_persons <- pums_raw |>
  filter(!stringr::str_detect(SERIALNO, "GQ")) |>
  transmute(
    serialno     = SERIALNO,
    sporder      = SPORDER,
    puma         = PUMA,
    wgtp         = WGTP,           # housing weight (tax unit ~ household for a 1st cut)
    age          = suppressWarnings(as.integer(AGEP)),
    marital      = MAR,           # married / widowed / divorced / separated / never-married
    sex          = SEX,
    relationship = RELSHIPP,      # relationship to householder (spouse / dependents)
    # income components in constant 2024$ (ADJINC) — feed TAXSIM's input fields
    wages         = numA(WAGP) * adj6(ADJINC),
    self_emp      = numA(SEMP) * adj6(ADJINC),
    interest      = numA(INTP) * adj6(ADJINC),
    retirement    = numA(RETP) * adj6(ADJINC),
    soc_sec       = numA(SSP)  * adj6(ADJINC),
    ssi           = numA(SSIP) * adj6(ADJINC),
    pub_assist    = numA(PAP)  * adj6(ADJINC),
    other_inc     = numA(OIP)  * adj6(ADJINC),
    person_income = numA(PINCP) * adj6(ADJINC),
    hh_income     = numA(HINCP) * adj6(ADJINC)
  )

write_parquet_safe(
  pums_persons,
  here::here("data/clean/pums_persons_wi.parquet")
)

############################################################################
# VI. COUNTY INCOME QUINTILES (2024 vintage)
############################################################################

# Table B19080 ("Household Income Quintile Upper Limits") returns the dollar
# cutpoints between quintiles (q20/q40/q60/q80) and the lower limit of the
# top 5 percent, one row per county. income_quintile_lookup pairs each of the
# five labeled quintiles with an assumed equal 20% population share, used
# below to split each county's total households into quintile-sized cells.
income_quintile_lookup <- tibble(
  income_quintile = factor(
    c(
      "Q1: Lowest 20%",
      "Q2",
      "Q3",
      "Q4",
      "Q5: Highest 20%"
    ),
    levels = c(
      "Q1: Lowest 20%",
      "Q2",
      "Q3",
      "Q4",
      "Q5: Highest 20%"
    )
  ),
  quintile_share = 0.20
)

quintile_vars <- c(
  q20 = "B19080_001",
  q40 = "B19080_002",
  q60 = "B19080_003",
  q80 = "B19080_004",
  top5_lower = "B19080_005"
)

# Reuse the script's single vintage source of truth (ACS_YEAR/ACS_SURVEY,
# set at the top of the file) rather than a second hardcoded literal --
# a duplicated vintage constant is exactly the silent-drift risk a future
# vintage migration could miss (this file already had to fix that failure
# mode once, in acquire_acs_county_income_race_counts()'s acs_vintage
# label; see 2026-07-20 migration notes).
acs_year <- ACS_YEAR
acs_survey <- ACS_SURVEY

wisconsin_county_quintile_values <- tidycensus::get_acs(
  geography = "county",
  state = "WI",
  variables = quintile_vars,
  year = acs_year,
  survey = acs_survey,
  output = "wide"
) |>
  transmute(
    county_fips = GEOID,
    county_name = NAME,
    acs_year = acs_year,

    q20 = q20E,
    q40 = q40E,
    q60 = q60E,
    q80 = q80E,
    top5_lower = top5_lowerE,

    q20_moe = q20M,
    q40_moe = q40M,
    q60_moe = q60M,
    q80_moe = q80M,
    top5_lower_moe = top5_lowerM
  )

wisconsin_county_households <- tidycensus::get_acs(
  geography = "county",
  state = "WI",
  variables = c(total_households = "B19001_001"),
  year = acs_year,
  survey = acs_survey,
  output = "wide"
) |>
  transmute(
    county_fips = GEOID,
    county_name = NAME,
    acs_year = acs_year,
    total_households = total_householdsE,
    total_households_moe = total_householdsM
  )

wisconsin_county_quintile_counts <- wisconsin_county_households |>
  tidyr::crossing(income_quintile_lookup) |>
  mutate(
    estimated_households = total_households * quintile_share,
    estimated_households_moe = total_households_moe * quintile_share
  )

# Consistency check (printed to the console, not persisted): each county's
# five quintile-cell household counts should sum back to its total_households.
wisconsin_county_quintile_counts |>
  group_by(county_fips, county_name) |>
  summarise(
    total_from_quintiles = sum(estimated_households, na.rm = TRUE),
    total_households = first(total_households),
    diff = total_from_quintiles - total_households,
    .groups = "drop"
  ) |>
  arrange(desc(abs(diff)))

write_parquet_safe(
  wisconsin_county_quintile_counts,
  here::here("data/clean/income_quintile_persons_wi.parquet")
)
# NOTE: raw arrow::write_parquet(), not write_parquet_safe(), unlike every
# other write in this script. Flagged for a substantive cleanup pass, not
# touched in this restyle.
write_parquet(
  wisconsin_county_quintile_values,
  here::here("data/clean/wisconsin_county_income_quintile_values.parquet")
)

message("  wrote pums_persons_wi.parquet  (", nrow(pums_persons), " persons) ",
        "- base for tax-unit construction")

############################################################################
# VII. AUDIT — ACS EFFECTIVE-RATE CROSS-CHECK
############################################################################

# eff_rate_acs_approx (med_real_est_tax / med_home_value) used to ship inside
# acs_county_wi_2023_5yr.parquet as a "context only" column -- dropped
# 2026-07-20 (PI) because it is a RATIO OF TWO COUNTY MEDIANS, not the same
# quantity as a median of household-level ratios, and therefore does not
# belong in a published analysis artifact. Re-expressed here as a proper
# cross-check against the project's effective-rate measure of record,
# owner_effective_rate() (scripts/functions/incidence.R), which correctly
# computes prop_tax / home_value per PUMS household, then takes the
# weighted median.
#
# Schema note: this project's other audit CSVs (e.g. 05_output/audit/
# tax_bases_audit.csv) use a `value_usd` column for dollar quantities. Every
# quantity below is a RATE (property tax dollars per dollar of home value),
# so this file uses a plain `value` column instead.

acs_eff_rate_detail <- acs_county_out |>
  transmute(
    recon_id = "RECON_ACS_EFF_RATE",
    quantity = "county_ratio_med_tax_over_med_value",
    county,
    geoid,
    med_real_est_tax,
    med_home_value,
    value = med_real_est_tax / med_home_value,
    source = paste(
      "acs_county_wi_2024_5yr.parquet (ACS B25103 median real estate tax /",
      "B25077 median home value)"
    ),
    status = "cross-check only -- not for publication",
    note = paste(
      "County-level ratio of two ACS medians (median real estate tax /",
      "median home value) -- not the same quantity as a median of",
      "household-level tax/value ratios; see the summary rows in this file",
      "for the reconciliation to owner_effective_rate(), the project's",
      "effective-rate measure of record."
    )
  )

unweighted_median_ratio <- median(acs_eff_rate_detail$value, na.rm = TRUE)
hh_weighted_mean_ratio <- weighted.mean(
  acs_eff_rate_detail$value, acs_county_out$households, na.rm = TRUE
)
owner_eff_rate_pums <- owner_effective_rate(pums_hh)

message(
  "  ACS eff-rate cross-check -- unweighted median: ", round(unweighted_median_ratio, 5),
  " | hh-weighted mean: ", round(hh_weighted_mean_ratio, 5),
  " | owner_effective_rate(pums_hh): ", round(owner_eff_rate_pums, 5)
)

acs_eff_rate_summary <- tibble::tibble(
  recon_id = "RECON_ACS_EFF_RATE",
  quantity = c(
    "unweighted_median_county_ratio",
    "hh_weighted_mean_county_ratio",
    "owner_effective_rate_pums",
    "gap_unweighted_median_vs_hh_weighted_mean_pct",
    "gap_hh_weighted_mean_vs_owner_effective_rate_pct"
  ),
  county = NA_character_,
  geoid = NA_character_,
  med_real_est_tax = NA_real_,
  med_home_value = NA_real_,
  value = c(
    unweighted_median_ratio,
    hh_weighted_mean_ratio,
    owner_eff_rate_pums,
    (hh_weighted_mean_ratio - unweighted_median_ratio) / hh_weighted_mean_ratio,
    (hh_weighted_mean_ratio - owner_eff_rate_pums) / hh_weighted_mean_ratio
  ),
  source = c(
    "median() of the 72 county_ratio_med_tax_over_med_value rows above",
    "weighted.mean() of the same 72 rows, weight = acs_county_wi_2024_5yr households",
    "owner_effective_rate(pums_hh) -- scripts/functions/incidence.R",
    paste(
      "(hh_weighted_mean_county_ratio - unweighted_median_county_ratio) /",
      "hh_weighted_mean_county_ratio"
    ),
    paste(
      "(hh_weighted_mean_county_ratio - owner_effective_rate_pums) /",
      "hh_weighted_mean_county_ratio"
    )
  ),
  status = c(
    "cross-check only -- not for publication",
    "cross-check only -- not for publication",
    "measure of record",
    "info",
    "info"
  ),
  note = c(
    paste(
      "Unweighted median across the 72 counties. Both ACS inputs are",
      "self-reported by the householder -- home value is the owner's own",
      "market estimate, not the county auditor's appraised value -- and",
      "whether the self-reported tax figure is gross or net of Wisconsin's",
      "rollbacks and homestead exemption is ambiguous, unlike the ODT PD",
      "tables, which distinguish charged from net. This definitional gap is",
      "why an ACS county ratio cannot be the project's effective-rate",
      "measure of record."
    ),
    paste(
      "Household-weighted (weight = households) mean across the 72 county",
      "ratios. Same self-reported / gross-vs-net ambiguity as the",
      "unweighted row above. Included because a plain unweighted",
      "mean/median across the 72 counties understates this weighted figure",
      "by roughly 25% -- the ratio is strongly composition-dependent, and",
      "populous counties sit well above the typical rural county."
    ),
    paste(
      "owner_effective_rate(pums_hh) -- the project's effective-rate",
      "measure of record: per-household prop_tax / home_value computed on",
      "PUMS microdata, then weighted median. The correct household-level",
      "quantity; the ACS county-ratio rows above are a cross-check only,",
      "never a substitute."
    ),
    paste(
      "Footgun quantification: naive unweighted averaging of the 72 county",
      "ratios materially understates a statewide effective rate relative to",
      "the household-weighted county mean."
    ),
    paste(
      "The household-weighted ACS county-ratio proxy sits closer to the",
      "PUMS measure of record than the unweighted median does, but the",
      "residual gap reflects the ratio-of-medians, self-reported-input, and",
      "gross/net ambiguities documented in the rows above -- this proxy is",
      "never a substitute for owner_effective_rate()."
    )
  )
)

acs_eff_rate_audit <- dplyr::bind_rows(acs_eff_rate_detail, acs_eff_rate_summary) |>
  dplyr::select(recon_id, quantity, county, geoid, med_real_est_tax, med_home_value,
                value, source, status, note)



############################################################################
# VIII. LEDGER STUB — paste these into 03_data-raw/metadata/data_ledger.xlsx
############################################################################

message("\n--- data_ledger rows to record (date_accessed = ", Sys.Date(), ") ---")
message("ACS_COUNTY_WI_2024_5yr | ACS 5-yr county tables (pop, HH, income, ",
        "tenure, value, rent, RE tax) | Census ACS | vintage ", ACS_YEAR,
        " | processed: acs_county_wi_2024_5yr.parquet")
message("ACS_PUMS_WI_2024_5yr   | ACS 5-yr PUMS (household incidence) | Census ACS",
        " | vintage ", ACS_YEAR, " | processed: pums_households_wi.parquet")
message("Done.")

############################################################################
# Wisconsin Property Tax Analysis
# THIS FILE:
#   1) Builds TAXSIM-style tax units (one row per unit, with primary /
#      spouse / dependent roles) from the ACS PUMS person file.
#   2) Computes each unit's Wisconsin current-law income tax under TY2025
#      law -- HAND-CODED (progressive brackets + income-phased standard
#      deduction + $700-per-person personal exemption, by filing status).
#      TAXSIM cannot compute any law year after 2023 (usincometaxes 0.7.1),
#      and Wisconsin's schedule is simple enough to hand-code transparently.
#   3) Writes the per-unit Wisconsin income tax file, then prints a quick
#      incidence check (weighted total, and effective rate by income
#      quintile).
# INPUTS:  data/clean/pums_persons_wi.parquet
#          scripts/scenarios/scenarios.yml (state_income_brackets_2025_single,
#            state_income_brackets_2025_mfj, state_income_standard_deduction_
#            2025, state_income_exemption_per_person)
# OUTPUTS: data/clean/income_tax_taxsim.csv
#            (per-unit Wisconsin income tax + wi_taxable_income (post-standard-
#            deduction, post-exemption WI taxable income); filename kept for
#            continuity with the downstream reader -- output/tax_collections_
#            figures.qmd's fig-taxsim-vs-handcoded reads it by this path --
#            even though this file does not call TAXSIM.)
# NOTES:   Tax units: householder + spouse file jointly, every other adult
#          files single, minors are dependents of the householder unit
#          (built by build_taxsim_units() in scripts/functions/incidence.R).
#          PUMS -> WI AGI mapping is approximate: WAGP -> wages, SEMP ->
#          self-emp, INTP -> interest (PUMS bundles interest + dividends +
#          net rental), RETP -> pensions, OIP -> other non-property income;
#          Social Security (SSP) is excluded, since Wisconsin exempts it from
#          taxable income. ACS PUMS has no dividends, capital gains, or
#          itemization detail, so those are 0 here -- a real measurement gap,
#          not a coding shortcut; document it when reporting. With no
#          itemization detail, the federal SALT deduction is never modeled.
# Author:  Adapted for Wisconsin from the Lade lab Ohio project (2a-income-
#          tax-baseline.R).
############################################################################

# --- SETUP ---
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(yaml)
  library(here)
})

source(here("scripts/functions/incidence.R"))   # build_taxsim_units(), wisconsin_income_tax_taxsim_units(), wtd_quantile_bins()

# Assemble the TY2025 Wisconsin schedule from scenarios.yml into the params
# shape wisconsin_income_tax_taxsim_units() expects (matches wi_tax_params).
yml <- yaml::read_yaml(here("scripts/scenarios/scenarios.yml"))
cl  <- yml$current_law
params <- list(
  brackets_single      = dplyr::bind_rows(cl$state_income_brackets_2025_single),
  brackets_mfj         = dplyr::bind_rows(cl$state_income_brackets_2025_mfj),
  sd_single            = cl$state_income_standard_deduction_2025$single,
  sd_mfj               = cl$state_income_standard_deduction_2025$mfj,
  exemption_per_person = cl$state_income_exemption_per_person
)

WI_LAW_YEAR <- as.integer(cl$state_income_year)   # TY2025 (label on `units` only)
SAMPLE_N    <- NULL   # full population; set an integer to cap the run for a quick local test

############################################################################
# I. TAX UNITS — PRIMARY / SPOUSE / DEPENDENT ROLES
############################################################################

persons <- read_parquet(here("data/clean/pums_persons_wi.parquet"))
units <- build_taxsim_units(persons, year = WI_LAW_YEAR)

if (!is.null(SAMPLE_N)) {
  set.seed(20260618)
  units <- slice_sample(units, n = min(SAMPLE_N, nrow(units)))
}

message(
  "Tax units built: ", nrow(units),
  if (!is.null(SAMPLE_N)) "  (SAMPLE — set SAMPLE_N <- NULL for all)" else ""
)

############################################################################
# II. HAND-CODED TY2025 WISCONSIN SCHEDULE
############################################################################

wi_tax <- wisconsin_income_tax_taxsim_units(units, params)

############################################################################
# III. INCOME MEASURES AND EXPORT
############################################################################

# Two distinct income measures, deliberately kept separate:
#   * tu_income      — sum of the WI-AGI input fields (wages/self-emp floored at 0,
#                      interest, pensions, other). Reconstructs what feeds the WI
#                      hand-code; it EXCLUDES Social Security and floors losses, so it
#                      is NOT a clean economic-income measure.
#   * tu_income_dist — full PUMS person income (PINCP) summed over the tax unit,
#                      INCLUDING Social Security. This is the complete income concept,
#                      consistent with hh_income (HINCP) used elsewhere. USE THIS for
#                      quintile assignment and effective-rate denominators; ranking on
#                      tu_income parks SS-only retirees in a spurious near-zero bottom bin.
# build_taxsim_units() returns only the aggregated per-unit rows, so re-derive
# the tu key here for this one sum (deliberately small, separate duplication of
# build_taxsim_units()'s tu-assignment rule; the full unit-construction pipeline
# stays in one place, scripts/functions/incidence.R).
persons_tu <- persons |>
  mutate(
    age = suppressWarnings(as.numeric(age)),
    rel = as.character(relationship),
    tu = if_else(rel %in% c("20", "21", "23") | age < 18,
                paste0(serialno, "_H"), paste0(serialno, "_", sporder))
  )

tu_full_income <- persons_tu |>
  group_by(tu) |>
  summarise(
    tu_income_dist = sum(suppressWarnings(as.numeric(person_income)), na.rm = TRUE),
    .groups = "drop"
  )

out <- units |>
  select(taxsimid, tu, serialno, wgtp, pwages, swages, psemp, ssemp, intrec, pensions, nonprop) |>
  left_join(select(wi_tax, taxsimid, wi_income_tax = wi_tax_current, wi_taxable_income), by = "taxsimid") |>
  left_join(tu_full_income, by = "tu") |>
  mutate(
    # wi_income_tax already floored at 0 by apply_bracket_schedule() -- no redundant pmax().
    tu_income = pwages + swages + psemp + ssemp + intrec + pensions + nonprop
  )

# Fail loudly on an NA liability -- an NA here would be silently dropped by the
# weighted-total's na.rm = TRUE below, understating the reported total with no
# warning.
stopifnot("wi_income_tax must be NA-free -- an NA here would be silently dropped by na.rm = TRUE downstream" =
            !anyNA(out$wi_income_tax))

write_csv(out, here("data/clean/income_tax_taxsim.csv"))
message("wrote data/clean/income_tax_taxsim.csv")

############################################################################
# IV. QUICK CHECK — SCALE TO POPULATION & INCIDENCE BY INCOME QUINTILE
############################################################################

scale_factor <- if (!is.null(SAMPLE_N)) "  (SAMPLE — scale by population for a real total)" else ""
message(
  "Wisconsin income tax (TY2025 hand-code, weighted): $",
  round(sum(out$wi_income_tax * out$wgtp, na.rm = TRUE) / 1e9, 3), "B", scale_factor
)

out |>
  filter(tu_income_dist > 0) |>                      # rank on the complete income measure
  mutate(grp = wtd_quantile_bins(tu_income_dist, wgtp, 5L)) |>
  group_by(grp) |>
  summarise(
    mean_income = stats::weighted.mean(tu_income_dist, wgtp),
    eff_rate    = stats::weighted.mean(wi_income_tax, wgtp) / stats::weighted.mean(tu_income_dist, wgtp),
    .groups = "drop"
  ) |>
  print()

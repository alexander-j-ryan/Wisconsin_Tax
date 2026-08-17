############################################################################
# Ohio Property Tax Abolition Analysis
# THIS FILE:
#   1) Defines household property-tax incidence helpers built on the ACS
#      PUMS household file: weighted-quantile group assignment and the
#      current owner-occupant burden by income group (Layer 4 foundation).
#   2) Defines a first-cut renter pass-through imputation and a combined
#      owner + renter incidence function.
#   3) Defines a hand-coded Ohio flat-tax proxy on PUMS income (household-
#      as-unit and real-tax-filing-unit versions) as a fast, transparent
#      alternative to TAXSIM.
#   4) Defines build_taxsim_units() — builds Ohio-specific TAXSIM input tax
#      units from ACS PUMS person records, shared by
#      02_code/2a-income-tax-baseline.R and 02_code/3b-income-incidence.R so both
#      callers submit identical units to TAXSIM.
#   5) Defines wisconsin_income_tax_taxsim_units() — the hand-coded TY2025
#      Wisconsin current-law schedule (progressive brackets + phased standard
#      deduction + $700 personal exemption, by filing status) at the SAME
#      taxsim-unit grain build_taxsim_units() returns, used by
#      scripts/2a-income-tax-baseline.R for WI liability/marginal rate.
# INPUTS:  None (defines functions only). Callers pass a PUMS households or
#          persons tibble; see each function's Roxygen block for required
#          columns.
# OUTPUTS: None (sourced by 02_code/2a-income-tax-baseline.R,
#          02_code/3b-income-incidence.R, 02_code/3c-sales-incidence.R,
#          02_code/3d-scenario-matrix.R, and 06_writing/presentations/
#          tax_collections_figures.qmd -- each caller's own source() line
#          comments which functions it uses. Full export list:
#          wtd_quantile_bins(), property_tax_incidence(), compute_p2r(),
#          owner_effective_rate(), owner_relief_gross_up(), add_ptax_burden(),
#          incidence_by_who(), wisconsin_income_tax_units(), build_tax_units(),
#          wisconsin_income_tax_taxunits(), build_taxsim_units(),
#          apply_bracket_schedule(),
#          wisconsin_income_tax_taxsim_units()).
# NOTES:   Weighted by the housing weight WGTP (`wgtp`) throughout. Effective
#          rate is reported as the AGGREGATE measure — weighted total tax /
#          weighted total income within a group. This is the Minnesota-DOR/
#          ITEP convention, and it avoids the mean-of-ratios blow-up at the
#          bottom of the distribution, where a few near-zero-income (asset-
#          rich) owners produce tax/income > 1.
#          Owner-occupants are the primary case (Section I); renters bear
#          property tax through rent pass-through, an explicit, separately-
#          flagged assumption (Section II) — not modeled from first
#          principles. Annual income basis throughout, per the project
#          guardrails (not lifetime incidence).
# Author: Gabe Lade (drafted with Claude)
# Last updated: 2026-07-20
############################################################################

############################################################################
# I. WEIGHTED INCOME GROUPS AND OWNER-OCCUPANT INCIDENCE (Layer 4 foundation)
############################################################################

#' Assign weighted quantile groups (1..n) by a continuous variable.
#'
#' Each observation's group is set by its cumulative weight fraction in sorted
#' order, so groups hold equal shares of total weight (not equal counts).
#'
#' @param x Numeric vector to rank (e.g., household income).
#' @param w Numeric weights (e.g., wgtp).
#' @param n Number of groups (5 = quintiles, 10 = deciles).
#' @return Integer vector of group ids 1..n, aligned to x.
wtd_quantile_bins <- function(x, w, n = 5L) {
  o   <- order(x)
  cwf <- cumsum(w[o]) / sum(w[o])           # cumulative weight fraction, sorted
  # ceiling()-based assignment puts an observation sitting exactly on a quantile
  # boundary into the lower group, so equal-weight inputs split into equal-size
  # groups (the old floor()+1 pushed boundary points up, unbalancing the bins).
  grp_sorted <- pmin(pmax(ceiling(cwf * n), 1L), n)
  grp        <- integer(length(x))
  grp[o]     <- grp_sorted
  grp
}

#' Owner-occupant property-tax incidence by income group.
#'
#' @param pums Tibble from pums_households_oh.parquet (constant-$ dollar cols).
#' @param n_groups Number of income groups (default 5).
#' @return Tibble: grp, mean_income, mean_tax, eff_rate_agg, median_rate,
#'   households (weighted).
property_tax_incidence <- function(pums, n_groups = 5L) {
  own <- pums |>
    dplyr::filter(own_rent == "owner", !is.na(prop_tax), hh_income > 0) |>
    dplyr::mutate(grp = wtd_quantile_bins(hh_income, wgtp, n_groups))

  own |>
    dplyr::group_by(grp) |>
    dplyr::summarise(
      mean_income  = stats::weighted.mean(hh_income, wgtp),
      mean_tax     = stats::weighted.mean(prop_tax, wgtp),
      # aggregate effective rate = weighted total tax / weighted total income
      eff_rate_agg = stats::weighted.mean(prop_tax, wgtp) / stats::weighted.mean(hh_income, wgtp),
      # robust alternative: weight-aware median of household-level tax/income
      median_rate  = wtd_median(prop_tax / hh_income, wgtp),
      households   = sum(wgtp),
      .groups = "drop"
    )
}

#' Weighted-median helper (avoids a hard dependency on matrixStats/Hmisc for
#' this one calculation).
#' @param x Numeric vector.
#' @param w Numeric weights, same length as x.
#' @return The weight-aware median of x.
wtd_median <- function(x, w) {
  o  <- order(x)
  xs <- x[o]
  ws <- w[o]
  cw <- cumsum(ws) / sum(ws)
  xs[which(cw >= 0.5)[1]]
}

############################################################################
# II. RENTER PASS-THROUGH (first cut)
############################################################################
# Renters report no property-tax bill in PUMS, so we impute the tax on the
# rental unit and assign a pass-through share to the tenant. ASSUMPTIONS
# (flag for Alex to refine):
#   * eff_rate    : owner effective property-tax rate (data-derived median).
#   * p2r         : price-to-rent ratio to turn annual rent into an implied
#                   value -- LOCKED 2026-07-16, computed in-pipeline via
#                   compute_p2r() (16.85 as of acs5_2024, updated 2026-07-20
#                   ACS-vintage migration -- was 16.78 on acs5_2023); see the
#                   Section II note below and scenarios.yml's incidence
#                   block for the citation and the Zillow low-arm caveat.
#   * pass_through: tenant's share of the unit's property tax. Default 0.50.
#     The recent quasi-experimental literature (Philadelphia Fed WP 2025; MIT
#     CRE) estimates $0.50-$0.89 per $1; 0.50 is the conservative low end.
#
#   Codex M4 (2026-07-14) flagged three input biases in this formula.
#   Dispositions as of the 2026-07-16 PI review (lock detail in
#   scenarios.yml's incidence block):
#   * GROSS-VS-CONTRACT-RENT BIAS -- RESOLVED BY THE p2r LOCK. ACS PUMS
#     does carry contract rent (RNTP), but the 1a extract pulled only
#     GRNTP (gross rent, incl. utilities/fuels). Because only the PRODUCT
#     rent x p2r enters the formula, the locked p2r is defined as a
#     value-to-GROSS-rent ratio computed in-sample (weighted-median owner
#     home value / weighted-median annual renter gross rent, Ohio ACS
#     2020-24 -- compute_p2r() below), so no contract-rent variable or
#     gross-to-contract conversion is needed at all. IMPLEMENTED 2026-07-16
#     (batch item #57): computed in-pipeline (02_code/3b/3c/3d call
#     compute_p2r() directly; the yml no longer carries a hand-set value).
#     One LOW sensitivity arm accompanies it: Zillow ZHVI/ZORI Ohio
#     (owner-stock values overstate rental-stock values) -- 02_code/3d Section
#     II-D.
#   * CLASS I VS CLASS II APARTMENTS -- STATED ASSUMPTION (PI, 2026-07-16):
#     every rental is imputed at the Class I owner-based rate. In fact
#     Ohio classes apartment buildings of 4+ rental units as commercial/
#     Class II (county-auditor land-use code 401, "apartments 4-19 rental
#     units"; the project taxonomy's 5+ line is a CoreLogic bucket
#     boundary, not the statutory line), and Class II faces higher
#     effective rates with no owner rollbacks -- so this UNDERSTATES the
#     removed property tax embedded in apartment renters' rents. ACS PUMS
#     does carry a building-size field (BLD); the 1a extract didn't pull
#     it. Phase-5 path if wanted: re-pull with BLD (operational cutoff 5+,
#     since PUMS buckets 3-4-unit buildings together).
#   * OWNER-RATE RELIEF BIAS -- LOCKED 2026-07-16: the BASELINE rental
#     rate becomes the FULLY-GROSS rate. The 2025 reforms phase out the
#     10% non-business rollback for rental property entirely (owner-
#     occupied keeps its relief), so a rental parcel now receives NONE of
#     the Class I relief owner bills embed: gross-up = RPU-Abs lines
#     24+25+26 (10% rollback 8.29% + owner-occupancy 1.53% + homestead
#     1.91% = 11.72% at 2023 vintage, computed live). The old
#     relief_adjusted arm (lines 25-26 only, 3.44%) was right for 2023
#     law and becomes a pre-reform comparator; owner_median becomes a
#     sensitivity. IMPLEMENTED 2026-07-16 (batch item #58):
#     owner_relief_gross_up() below reads the RPU-Abs lines live; 02_code/3b
#     and 02_code/3c's renter imputation uses its fully_gross_share as the
#     BASELINE eff_rate; 02_code/3d Section II-D's sensitivity table adds
#     fully_gross alongside relief_adjusted/owner_median. Statutory bill
#     citation still TBD via LSC -- do not invent it.

#' In-sample price-to-rent ratio (p2r): weighted-median owner home value
#' divided by weighted-median annual renter GROSS rent -- LOCKED 2026-07-16
#' (PI), IMPLEMENTED (batch item #57). Computed live from this project's own
#' ACS PUMS extract (same survey, vintage, dollars, and geography as the
#' microdata it feeds -- see scenarios.yml's incidence$p2r lock note for the
#' citation). CAVEAT this travels with: owner-stock values applied to
#' rental-stock rents overstate rental values (investors price on cap rates;
#' multifamily gross-rent multipliers run lower) -- carry the Zillow
#' ZHVI/ZORI low sensitivity arm alongside (02_code/3d Section II-D).
#' @param pums_hh Tibble with own_rent, home_value, gross_rent, wgtp.
#' @return List: p2r (the ratio), owner_median_value, renter_median_annual_gross_rent.
compute_p2r <- function(pums_hh) {
  owner <- dplyr::filter(pums_hh, own_rent == "owner", !is.na(home_value), home_value > 0)
  renter <- dplyr::filter(pums_hh, own_rent == "renter", !is.na(gross_rent), gross_rent > 0)
  owner_median_value <- wtd_median(owner$home_value, owner$wgtp)
  renter_median_annual_gross_rent <- 12 * wtd_median(renter$gross_rent, renter$wgtp)
  list(
    p2r = owner_median_value / renter_median_annual_gross_rent,
    owner_median_value = owner_median_value,
    renter_median_annual_gross_rent = renter_median_annual_gross_rent
  )
}

#' Owner effective property-tax rate (weighted median of tax / home value).
#' @param pums Tibble with own_rent, prop_tax, home_value, wgtp.
#' @return Scalar weighted median of prop_tax / home_value among owners.
owner_effective_rate <- function(pums) {
  o <- dplyr::filter(pums, own_rent == "owner", !is.na(prop_tax),
                     !is.na(home_value), home_value > 0)
  wtd_median(o$prop_tax / o$home_value, o$wgtp)
}

#' Ohio Class I owner-relief gross-up shares, read live from ODT RPU-Abs.
#'
#' Rental property receives NONE of the Class I relief lines an owner-
#' occupied bill embeds -- LOCKED 2026-07-16 (PI): the FULLY-GROSS share
#' (lines 24+25+26) is the baseline gross-up a renter's imputed tax needs
#' applied to the owner median effective rate, since the 2025 reforms phase
#' out the non-business 10% rollback (line 24) for rental property entirely
#' while owner-occupied property keeps (an increased) rollback. The
#' lines-25-26-only share (owner-occupancy + homestead, no line 24) is kept
#' as `relief_adjusted_share` -- correct under PRE-reform law, now a labeled
#' comparator, not the baseline. Single reader for this factor -- both
#' 02_code/3b/3c's renter imputation and 02_code/3d Section II-D's sensitivity
#' table call this function rather than each computing the RPU-Abs read
#' independently.
#'
#' @param rpuabs_path Path to an ODT RPU-Abs workbook (e.g. rpuabs23.xlsx).
#' @return List: line24_usd, line25_usd, line26_usd, line27_usd (raw dollar
#'   totals, Total Class 1 column, summed across all taxing districts
#'   statewide), fully_gross_share ((24+25+26)/27), relief_adjusted_share
#'   ((25+26)/27).
owner_relief_gross_up <- function(rpuabs_path) {
  raw <- readxl::read_excel(rpuabs_path, sheet = "RPUAbs23", skip = 3) |>
    dplyr::filter(`Line Number` %in% c(24, 25, 26, 27)) |>
    dplyr::group_by(`Line Number`) |>
    dplyr::summarise(total = sum(`Total Class 1`, na.rm = TRUE), .groups = "drop")
  line24 <- raw$total[raw$`Line Number` == 24]
  line25 <- raw$total[raw$`Line Number` == 25]
  line26 <- raw$total[raw$`Line Number` == 26]
  line27 <- raw$total[raw$`Line Number` == 27]
  stopifnot(
    "RPU-Abs lines 24, 25, 26, and 27 must each resolve to a single value" =
      length(line24) == 1 && length(line25) == 1 && length(line26) == 1 && length(line27) == 1
  )
  list(
    line24_usd = line24, line25_usd = line25, line26_usd = line26, line27_usd = line27,
    fully_gross_share = (line24 + line25 + line26) / line27,
    relief_adjusted_share = (line25 + line26) / line27
  )
}

#' Add a unified property-tax burden column: owners pay `prop_tax`; renters
#' bear `pass_through * (annual rent * p2r * eff_rate)`.
#' @param pums Tibble with own_rent, prop_tax, gross_rent, wgtp.
#' @param p2r Price-to-rent ratio. Real callers pass compute_p2r(pums_hh)$p2r
#'   explicitly (LOCKED 2026-07-16); the 16 default here is a fallback only,
#'   never the current-law value -- see Section II note above.
#' @param pass_through Tenant's share of the unit's property tax (default 0.50).
#' @param eff_rate Owner effective rate; computed via owner_effective_rate()
#'   if not supplied.
#' @return `pums` with an added `ptax_burden` column.
add_ptax_burden <- function(pums, p2r = 16, pass_through = 0.50, eff_rate = NULL) {
  if (is.null(eff_rate)) eff_rate <- owner_effective_rate(pums)
  pums |>
    dplyr::mutate(ptax_burden = dplyr::case_when(
      own_rent == "owner"                        ~ prop_tax,
      own_rent == "renter" & !is.na(gross_rent)   ~ pass_through * (gross_rent * 12 * p2r * eff_rate),
      TRUE                                        ~ NA_real_
    ))
}

#' Incidence (aggregate effective rate) by income group for owners, renters,
#' or combined. Requires a `ptax_burden` column (see add_ptax_burden()).
#' @param pums Tibble with hh_income, wgtp, own_rent, ptax_burden.
#' @param who One of "owner", "renter", "combined".
#' @param n_groups Number of income groups (default 5).
#' @return Tibble: grp, mean_income, mean_paid, total_paid, eff_rate_agg,
#'   households (weighted), who.
incidence_by_who <- function(pums, who = c("owner", "renter", "combined"), n_groups = 5L) {
  who <- match.arg(who)
  d <- dplyr::filter(pums, hh_income > 0, !is.na(ptax_burden))
  if (who == "owner")  d <- dplyr::filter(d, own_rent == "owner")
  if (who == "renter") d <- dplyr::filter(d, own_rent == "renter")
  d |>
    dplyr::mutate(grp = wtd_quantile_bins(hh_income, wgtp, n_groups)) |>
    dplyr::group_by(grp) |>
    dplyr::summarise(
      mean_income  = stats::weighted.mean(hh_income, wgtp),
      mean_paid    = stats::weighted.mean(ptax_burden, wgtp),
      total_paid   = sum(ptax_burden * wgtp),   # aggregate annual $ borne by the group
      eff_rate_agg = stats::weighted.mean(ptax_burden, wgtp) / stats::weighted.mean(hh_income, wgtp),
      households   = sum(wgtp),
      .groups = "drop"
    ) |>
    dplyr::mutate(who = who)
}

############################################################################
# III. HAND-CODED WISCONSIN INCOME TAX (simple household construction)
############################################################################
# CRUDE first move, kept as Part I's simple descriptive cut (see
# tax_collections_figures.qmd's "first move toward income-tax incidence"):
#   * Tax unit = household, treated as a SINGLE filer (ignores multiple
#     filing units within a household; single/HoH brackets and standard
#     deduction throughout).
#   * Wisconsin taxable income proxied as household income MINUS Social
#     Security / SSI / public assistance (Wisconsin exempts Social Security).
#     A single standard deduction and one $700 personal exemption are netted
#     off -- deliberately coarser than the per-filer treatment in
#     wisconsin_income_tax_taxunits() below, which splits the household into
#     real filing units.
#   * Applies the TY2025 progressive bracket schedule (see wi_tax_params).
#   * No federal interaction. scripts/2a-income-tax-baseline.R hand-codes the
#     full statutory construction at the real tax-unit grain (filing-status
#     brackets + phased standard deduction + per-person exemptions) as the
#     anchored baseline.
# Because it collapses every household to one single filer, this construction
# UNDERSTATES the exemptions/standard deductions a multi-filer household would
# actually claim -> it is a coarse upper cut, refined by the two constructions
# that follow it in the document.

#' Build household tax units and a hand-coded Wisconsin income tax, treating
#' each household as a single filer.
#' @param persons Tibble from pums_persons_wi.parquet (constant-$ income cols).
#' @param params TY2025 Wisconsin schedule (defaults to wi_tax_params).
#' @return Tibble: serialno, wgtp, hh_income, wi_taxable, income_tax.
wisconsin_income_tax_units <- function(persons, params = wi_tax_params) {
  persons |>
    dplyr::group_by(serialno) |>
    dplyr::summarise(
      wgtp      = dplyr::first(wgtp),
      hh_income = dplyr::first(hh_income),                       # HINCP (household)
      nontax    = sum(soc_sec, ssi, pub_assist, na.rm = TRUE),    # WI-exempt income (Social Security)
      .groups   = "drop"
    ) |>
    dplyr::filter(wgtp > 0) |>
    dplyr::mutate(
      wi_agi     = pmax(hh_income - nontax, 0),
      wi_taxable = pmax(wi_agi - .wi_std_deduction(wi_agi, params$sd_single) -
                          params$exemption_per_person, 0),        # single filer: one exemption
      income_tax = apply_bracket_schedule(wi_taxable, params$brackets_single)$tax
    )
}

#' Build REAL tax-filing units from PUMS person records (more sophisticated
#' than household = tax unit). Simplified rules:
#'   * householder (RELSHIPP 20) + married spouse (21/23) file jointly -> one
#'     unit;
#'   * every other adult (age >= 18) files as their own single unit;
#'   * minors (age < 18) attach to the householder unit as dependents.
#' Each unit inherits the household weight WGTP. This splits multi-adult
#' households so each filing unit gets its own exemption. (Next step: TAXSIM
#' on these units, see 02_code/2a-income-tax-baseline.R.)
#' @param persons Tibble from pums_persons_wi.parquet.
#' @return Tibble: tu (unit id), serialno, wgtp, tu_income, exempt, mfj,
#'   n_exempt. `mfj` flags units with a married spouse present (they file
#'   jointly); `n_exempt` is the person count in the unit (self + spouse +
#'   dependents), i.e. the number of personal exemptions the unit may claim.
build_tax_units <- function(persons) {
  persons |>
    dplyr::mutate(
      age        = suppressWarnings(as.numeric(age)),
      rel        = as.character(relationship),
      in_hh_unit = rel %in% c("20", "21", "23") | age < 18,
      tu = dplyr::if_else(in_hh_unit, paste0(serialno, "_H"),
                          paste0(serialno, "_", sporder)),
      is_spouse = rel %in% c("21", "23"),                  # married spouse of householder
      exempt = suppressWarnings(as.numeric(soc_sec)) +
               suppressWarnings(as.numeric(ssi)) +
               suppressWarnings(as.numeric(pub_assist)),
      person_income = suppressWarnings(as.numeric(person_income))
    ) |>
    dplyr::group_by(tu) |>
    dplyr::summarise(
      serialno  = dplyr::first(serialno),
      wgtp      = dplyr::first(wgtp),
      tu_income = sum(person_income, na.rm = TRUE),
      exempt    = sum(exempt, na.rm = TRUE),
      mfj       = any(is_spouse),               # a spouse present -> married filing jointly
      n_exempt  = dplyr::n(),                    # one personal exemption per person in the unit
      .groups   = "drop"
    ) |>
    dplyr::filter(wgtp > 0)
}

#' TY2025 Wisconsin individual income tax schedule -- marginal brackets by
#' filing status, the income-phased standard deduction, and the flat $700
#' per-exemption personal exemption. Source: Wisconsin DOR TY2025 tax-rate
#' schedule (single/HoH and married-filing-jointly columns) and Form 1
#' instructions; every figure is inflation-indexed annually and these are the
#' most recently published (TY2025) values. Wisconsin publishes MARGINAL
#' rates, so the cumulative `base` at each bracket floor telescopes exactly
#' from the rates below it -- derived here (see .wi_brackets) rather than
#' hand-typed, unlike Ohio's separately-indexed published base amounts.
.wi_brackets <- function(cuts, rates) {
  base <- c(0, cumsum(rates[-length(rates)] * diff(cuts)))
  data.frame(over = cuts, base = base, rate = rates)
}

wi_tax_params <- list(
  brackets_single = .wi_brackets(c(0, 14680, 50480, 323290),
                                 c(0.0350, 0.0440, 0.0530, 0.0765)),
  brackets_mfj    = .wi_brackets(c(0, 19580, 67300, 431060),
                                 c(0.0350, 0.0440, 0.0530, 0.0765)),
  # Standard deduction: max amount, income where phase-out begins, income at $0.
  sd_single = list(max = 13560, start = 19549, zero = 132549),  # ~ -12.0%/$ over start
  sd_mfj    = list(max = 25110, start = 28209, zero = 155169),  # ~ -19.78%/$ over start
  exemption_per_person = 700
)

#' Wisconsin income-phased standard deduction, vectorized. Holds at `p$max` up
#' to `p$start` of income, then falls linearly to $0 at `p$zero`.
.wi_std_deduction <- function(income, p) {
  slope <- p$max / (p$zero - p$start)
  pmax(pmin(p$max, p$max - slope * (pmax(income, 0) - p$start)), 0)
}

#' Hand-coded Wisconsin income tax on REAL tax units (see build_tax_units()).
#' Applies the TY2025 progressive bracket schedule by filing status, after the
#' income-phased standard deduction and the $700-per-person personal exemption.
#' Social Security / SSI / public assistance (build_tax_units()'s `exempt`) are
#' removed first, matching Wisconsin's exclusion of Social Security from taxable
#' income. Units with a married spouse present get the wider MFJ brackets and
#' standard deduction; every other unit is taxed as single/HoH. The bracket
#' math reuses apply_bracket_schedule() -- the same engine 2a's statutory
#' construction uses -- so this file keeps one bracket implementation, not two.
#' @param persons Tibble from the PUMS persons parquet.
#' @param params TY2025 Wisconsin schedule (defaults to wi_tax_params).
#' @return build_tax_units() output plus wi_agi, std_deduction, wi_taxable,
#'   income_tax.
wisconsin_income_tax_taxunits <- function(persons, params = wi_tax_params) {
  units <- build_tax_units(persons) |>
    dplyr::mutate(
      wi_agi        = pmax(tu_income - exempt, 0),                 # SS/SSI/assistance excluded
      std_deduction = dplyr::if_else(mfj,
                        .wi_std_deduction(wi_agi, params$sd_mfj),
                        .wi_std_deduction(wi_agi, params$sd_single)),
      wi_taxable    = pmax(wi_agi - std_deduction -
                             params$exemption_per_person * n_exempt, 0)
    )

  # apply_bracket_schedule() takes a single schedule; evaluate both and select
  # each unit's liability by filing status.
  tax_single <- apply_bracket_schedule(units$wi_taxable, params$brackets_single)$tax
  tax_mfj    <- apply_bracket_schedule(units$wi_taxable, params$brackets_mfj)$tax

  dplyr::mutate(units, income_tax = dplyr::if_else(mfj, tax_mfj, tax_single))
}

############################################################################
# IV. TAXSIM TAX-UNIT CONSTRUCTION (shared by 02_code/2a and 02_code/3b)
############################################################################

#' Build TAXSIM input tax units from ACS PUMS person records.
#'
#' Ohio-specific unit rules: householder + married spouse file jointly
#' ("married, jointly"), every other adult files single, minors are
#' dependents of the HOUSEHOLDER's unit specifically, regardless of which
#' adult in the household is the minor's actual parent (r-reviewer, 2026-
#' 07-15: PUMS has no explicit parent-child linkage beyond relationship-to-
#' householder, so a multi-generational household -- e.g. grandparent-
#' householder + adult child + that child's own minor children -- will
#' misattribute the grandchildren's dependent exemptions to the
#' grandparent's unit instead of the adult child's; a defensible
#' simplification given the data, not a claim of exact accuracy). PUMS ->
#' TAXSIM income mapping is
#' approximate (see 02_code/2a-income-tax-baseline.R header for the full
#' measurement-gap caveat): WAGP -> wages, SEMP -> self-emp, INTP ->
#' interest (PUMS bundles interest + dividends + net rental together),
#' RETP -> pensions, SSP -> gross OASDI, OIP -> other non-property income.
#' No PUMS dividends/capital-gains/itemized-deduction detail, so those
#' TAXSIM input fields are always 0 downstream.
#'
#' Extracted from 02_code/2a-income-tax-baseline.R so both it and 02_code/3b-
#' income-incidence.R (which needs a second TAXSIM call, requesting
#' different output columns, on the SAME units) build identical tax units
#' from a single implementation -- never duplicate this logic.
#'
#' @param persons Tibble from pums_persons_oh.parquet.
#' @param year TAXSIM law year (integer scalar, e.g. 2023L).
#' @return Tibble, one row per tax unit, in TAXSIM-submission row order:
#'   tu, serialno, wgtp, taxsimid, year, state, mstat, page, sage, depx,
#'   pwages, swages, psemp, ssemp, intrec, pensions, gssi, nonprop.
#'   Callers `transmute()` this into TAXSIM's exact input schema (see
#'   02_code/2a-income-tax-baseline.R Section II) and keep tu/serialno/wgtp
#'   to join results back on taxsimid.
build_taxsim_units <- function(persons, year) {
  persons <- persons |>
    dplyr::mutate(
      dplyr::across(c(age, wages, self_emp, interest, retirement, soc_sec, ssi,
                      pub_assist, other_inc, wgtp), ~ suppressWarnings(as.numeric(.x))),
      rel = as.character(relationship)
    ) |>
    dplyr::mutate(
      in_hh_unit = rel %in% c("20", "21", "23") | age < 18,
      tu = dplyr::if_else(in_hh_unit, paste0(serialno, "_H"), paste0(serialno, "_", sporder)),
      role = dplyr::case_when(
        rel == "20"             ~ "primary",     # householder
        rel %in% c("21", "23")  ~ "spouse",      # married spouse of householder
        !in_hh_unit             ~ "primary",     # any other adult heads their own unit
        TRUE                    ~ "dependent"    # minors attached to the householder unit
      )
    )

  prim <- persons |>
    dplyr::filter(role == "primary") |>
    dplyr::group_by(tu) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      tu, serialno, wgtp,
      page = pmin(pmax(round(age), 1), 120),
      pwages = pmax(wages, 0),
      psemp = self_emp,
      p_int = pmax(interest, 0),
      p_pens = pmax(retirement, 0),
      p_gssi = pmax(soc_sec, 0),
      p_other = pmax(other_inc, 0)
    )

  spouse <- persons |>
    dplyr::filter(role == "spouse") |>
    dplyr::group_by(tu) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      tu,
      sage = pmin(pmax(round(age), 1), 120),
      swages = pmax(wages, 0),
      ssemp = self_emp,
      s_int = pmax(interest, 0),
      s_pens = pmax(retirement, 0),
      s_gssi = pmax(soc_sec, 0),
      s_other = pmax(other_inc, 0)
    )

  deps <- persons |>
    dplyr::filter(role == "dependent") |>
    dplyr::count(tu, name = "depx")

  prim |>
    dplyr::left_join(spouse, by = "tu") |>
    dplyr::left_join(deps,   by = "tu") |>
    dplyr::mutate(
      taxsimid = dplyr::row_number(),
      year     = year,
      state    = "WI",
      mstat    = dplyr::if_else(!is.na(sage), "married, jointly", "single"),
      sage     = dplyr::coalesce(sage, 0),
      depx     = dplyr::coalesce(depx, 0L),
      # primary + spouse combined into TAXSIM fields
      swages   = dplyr::coalesce(swages, 0),
      ssemp    = dplyr::coalesce(ssemp, 0),
      psemp    = dplyr::coalesce(psemp, 0),
      intrec   = p_int  + dplyr::coalesce(s_int, 0),
      pensions = p_pens + dplyr::coalesce(s_pens, 0),
      gssi     = p_gssi + dplyr::coalesce(s_gssi, 0),
      nonprop  = p_other + dplyr::coalesce(s_other, 0)
    )
}

############################################################################
# V. HAND-CODED TY2025 WISCONSIN SCHEDULE AT THE TAXSIM-UNIT GRAIN
############################################################################
# Wisconsin's income tax is hand-coded here (the installed usincometaxes
# 0.7.1 cannot compute any law year after 2023 at all; 0.7.1 is the latest
# CRAN version). scripts/2a-income-tax-baseline.R computes current-law WI
# liability here; TAXSIM, if reintroduced, would be kept only for federal
# marginal rates. TY2025 Wisconsin law is a genuinely progressive FOUR-bracket
# schedule (3.50 / 4.40 / 5.30 / 7.65%) whose bracket thresholds AND the
# income-phased standard deduction differ by filing status (single/HoH vs.
# married-filing-jointly), with a flat $700-per-exemption personal exemption.
# Because Wisconsin publishes MARGINAL rates, the cumulative `base` at each
# bracket floor telescopes exactly from the rates below it (derived in
# .wi_brackets(), Section III) -- unlike Ohio's separately-indexed published
# base amounts. Sourced constants live in scripts/scenarios/scenarios.yml
# (current_law$state_income_*), mirrored by wi_tax_params (Section III) for
# the simple hand-code functions.

#' Apply a "base + rate x (excess over threshold)" bracket schedule.
#'
#' Wisconsin's rate schedule is worded "$0 -- $14,680 ... $14,680 --
#' $50,480 ...", i.e. bracket boundaries are OPEN on the left: income of
#' exactly $14,680 stays in the lower bracket, and only income strictly above
#' the threshold moves up. `findInterval(..., left.open = TRUE)` matches that
#' convention; the default (closed-on-left) would wrongly tax a unit sitting
#' exactly on a bracket boundary at the higher rate.
#'
#' The liability at income x, under the bracket with the largest `over`
#' strictly below x, is `base + rate * (x - over)`. Because Wisconsin
#' publishes marginal rates, `base` telescopes exactly from the rates below
#' it (see .wi_brackets(), Section III). The function is schedule-agnostic:
#' pass whichever filing-status bracket table applies.
#'
#' @param taxable_income Numeric vector, already net of standard deduction
#'   and exemptions.
#' @param brackets Data frame with columns `over`, `base`, `rate`, one row
#'   per bracket (any order; sorted internally).
#' @return List with `tax` and `mtr` numeric vectors, aligned to
#'   `taxable_income`.
apply_bracket_schedule <- function(taxable_income, brackets) {
  brackets <- brackets[order(brackets$over), ]
  bracket_idx <- pmax(findInterval(taxable_income, brackets$over, left.open = TRUE), 1L)
  tax <- brackets$base[bracket_idx] + brackets$rate[bracket_idx] * (taxable_income - brackets$over[bracket_idx])
  list(tax = pmax(tax, 0), mtr = brackets$rate[bracket_idx])
}

#' Hand-coded TY2025 Wisconsin income tax, at the TAXSIM-unit grain.
#'
#' Computes current-law Wisconsin liability and marginal rate on the SAME
#' units build_taxsim_units() returns, so scripts/2a can join this directly
#' by `taxsimid`. This is the FULL statutory construction that anchors the
#' income-tax baseline; wisconsin_income_tax_taxunits() (Section III) is the
#' simpler cut it refines. Wisconsin AGI is approximated as wages +
#' self-employment + interest + pensions + other, excluding Social Security
#' (`gssi`), which Wisconsin exempts -- a documented PUMS measurement
#' approximation (no dividends/capital-gains/itemization detail), not a
#' coding shortcut. Filing status comes from `mstat`: married-jointly units
#' get the wider MFJ brackets and standard deduction, everyone else single/HoH.
#' Taxable income nets the income-phased standard deduction and the flat
#' $700-per-exemption personal exemption off WI AGI BEFORE the bracket
#' schedule. The standard-deduction phase-out uses WI AGI as its income base
#' (the same pre-deduction measure), a documented approximation of the
#' statutory household-income basis.
#'
#' @param units Tibble from build_taxsim_units(): `taxsimid`, `mstat`,
#'   `depx`, `pwages`, `swages`, `psemp`, `ssemp`, `intrec`, `pensions`,
#'   `nonprop` (`gssi` intentionally excluded -- WI-exempt).
#' @param params TY2025 Wisconsin schedule with `brackets_single`,
#'   `brackets_mfj` (data frames of `over`/`base`/`rate`), `sd_single`,
#'   `sd_mfj` (lists of `max`/`start`/`zero`), and `exemption_per_person`.
#'   Defaults to wi_tax_params; scripts/2a builds an equivalent list from
#'   scripts/scenarios/scenarios.yml.
#' @return Tibble: `taxsimid`, `mfj`, `wi_agi`, `n_exemptions`,
#'   `std_deduction`, `wi_exemption_amount`, `wi_taxable_income`,
#'   `wi_tax_current`, `wi_mtr_current`.
wisconsin_income_tax_taxsim_units <- function(units, params = wi_tax_params) {
  base <- units |>
    dplyr::transmute(
      taxsimid,
      mfj    = mstat == "married, jointly",
      wi_agi = pwages + swages + psemp + ssemp + intrec + pensions + nonprop,   # gssi (SS) excluded
      n_exemptions = dplyr::if_else(mstat == "married, jointly", 2L, 1L) + depx
    )

  base <- dplyr::mutate(
    base,
    std_deduction = dplyr::if_else(mfj,
                      .wi_std_deduction(wi_agi, params$sd_mfj),
                      .wi_std_deduction(wi_agi, params$sd_single)),
    wi_exemption_amount = n_exemptions * params$exemption_per_person,
    wi_taxable_income   = pmax(wi_agi - std_deduction - wi_exemption_amount, 0)
  )

  # apply_bracket_schedule() takes a single schedule; evaluate both and pick
  # each unit's liability / marginal rate by filing status.
  sched_single <- apply_bracket_schedule(base$wi_taxable_income, params$brackets_single)
  sched_mfj    <- apply_bracket_schedule(base$wi_taxable_income, params$brackets_mfj)

  dplyr::mutate(
    base,
    wi_tax_current = dplyr::if_else(mfj, sched_mfj$tax, sched_single$tax),
    wi_mtr_current = dplyr::if_else(mfj, sched_mfj$mtr, sched_single$mtr)
  )
}

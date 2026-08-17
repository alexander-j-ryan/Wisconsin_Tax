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
#   5) Defines ohio_income_tax_taxsim_units() — the hand-coded TY2026 Ohio
#      current-law schedule (ORC 5747.02/5747.025) at the SAME taxsim-unit
#      grain build_taxsim_units() returns, replacing the TAXSIM call
#      02_code/2a and 02_code/3b previously used for Ohio liability/marginal
#      rate (2026-07-15 income-handcode plan; TAXSIM stays in the loop only
#      for federal marginal rates, in 02_code/3b).
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
#          incidence_by_who(), ohio_income_tax_units(), build_tax_units(),
#          ohio_income_tax_taxunits(), build_taxsim_units(),
#          apply_bracket_schedule(), ohio_exemption_per_person(),
#          ohio_income_tax_taxsim_units()).
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
# III. HAND-CODED OHIO INCOME TAX (simple flat-rate construction)
############################################################################
# CRUDE first move, kept as Part I's simple descriptive cut (see
# tax_collections_figures.qmd's "first move toward income-tax incidence"):
#   * Tax unit = household (ignores multiple filing units within a household).
#   * Ohio taxable income proxied as household income MINUS Social Security /
#     SSI / public assistance (Ohio exempts these). Ignores all other Ohio
#     adjustments, deductions, exemptions, and credits -> overstates the base.
#   * Applies the TY2026 flat schedule: `rate` on income above `exempt`.
#   * No federal interaction. 02_code/2a-income-tax-baseline.R hand-codes the
#     full TY2026 statute (personal exemptions + the $332 base amount) as
#     the anchored baseline; TAXSIM cannot compute any law year after 2023
#     and is retained only in 02_code/3b-income-incidence.R, for FEDERAL
#     marginal rates as a documented 2023-law stable-structure proxy -- it
#     never computes Ohio-side liability anywhere in this project.
# Validation: on the 2020-2024 PUMS this totals ~$9.0B (was ~$8.7B on
# 2019-2023, updated 2026-07-20 ACS-vintage migration), close to Ohio's
# actual ~$9B individual income tax -> the crude proxy is in the right
# ballpark.

#' Build household tax units and a hand-coded Ohio income tax.
#' @param persons Tibble from pums_persons_oh.parquet (constant-$ income cols).
#' @param rate Flat marginal rate above the exemption (TY2026 = 0.0275).
#' @param exempt No-tax threshold (TY2026 ~ 26050).
#' @return Tibble: serialno, wgtp, hh_income, oh_taxable, income_tax.
ohio_income_tax_units <- function(persons, rate = 0.0275, exempt = 26050) {
  persons |>
    dplyr::group_by(serialno) |>
    dplyr::summarise(
      wgtp      = dplyr::first(wgtp),
      hh_income = dplyr::first(hh_income),                       # HINCP (household)
      nontax    = sum(soc_sec, ssi, pub_assist, na.rm = TRUE),    # Ohio-exempt income
      .groups   = "drop"
    ) |>
    dplyr::filter(wgtp > 0) |>
    dplyr::mutate(
      oh_taxable = pmax(hh_income - nontax, 0),
      income_tax = rate * pmax(oh_taxable - exempt, 0)
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
#' @param persons Tibble from pums_persons_oh.parquet.
#' @return Tibble: tu (unit id), serialno, wgtp, tu_income, exempt.
build_tax_units <- function(persons) {
  persons |>
    dplyr::mutate(
      age        = suppressWarnings(as.numeric(age)),
      rel        = as.character(relationship),
      in_hh_unit = rel %in% c("20", "21", "23") | age < 18,
      tu = dplyr::if_else(in_hh_unit, paste0(serialno, "_H"),
                          paste0(serialno, "_", sporder)),
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
      .groups   = "drop"
    ) |>
    dplyr::filter(wgtp > 0)
}

#' Hand-coded Ohio income tax on REAL tax units (see build_tax_units()).
#' @param persons Tibble from pums_persons_oh.parquet.
#' @param rate Flat marginal rate above the exemption (TY2026 = 0.0275).
#' @param exempt_thr No-tax threshold (TY2026 ~ 26050).
#' @return build_tax_units() output plus oh_taxable, income_tax.
ohio_income_tax_taxunits <- function(persons, rate = 0.0275, exempt_thr = 26050) {
  build_tax_units(persons) |>
    dplyr::mutate(
      oh_taxable = pmax(tu_income - exempt, 0),
      income_tax = rate * pmax(oh_taxable - exempt_thr, 0)
    )
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
      state    = "OH",
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
# V. HAND-CODED TY2026 OHIO SCHEDULE AT THE TAXSIM-UNIT GRAIN
############################################################################
# Ohio's income tax stopped being TAXSIM's value-add once the state moved
# to this near-flat schedule -- and the installed usincometaxes 0.7.1
# cannot compute any law year after 2023 at all (verified 2026-07-15: 2024-
# 2027 all error "year must be between 1960 and 2023"; 0.7.1 is the latest
# CRAN version). 02_code/2a and 02_code/3b hand-code Ohio current-law liability
# here instead, and keep TAXSIM only for federal marginal rates (2026-07-
# 15 income-handcode plan). TY2026 IS a single flat 2.75% bracket -- $0 at
# or below $26,050, then $332.00 plus 2.75% of the excess above $26,050,
# no second bracket -- so it is not a bare rate x income multiply (the
# $332 base and the $26,050 zero-bracket both matter) but it is genuinely
# flat above the threshold (ORC 5747.02, HB 96). CORRECTED 2026-07-15: an
# earlier version of this schedule used a stale two-bracket schedule (a
# $342 base plus a second 3.125% bracket above $100,000) under the TY2026
# label; the 3.125% tier is TY2025's, and the $342 base was a stale ODT
# worksheet's figure -- TY2025's actual statutory base is $360.69. See
# scenarios.yml current_law$state_income_brackets_2026 for the sourced,
# corrected bracket table and the full root-cause note.

#' Apply a "base + rate x (excess over threshold)" bracket schedule.
#'
#' ODT's own table is worded "More than $X -- Up to $Y", i.e. bracket
#' boundaries are OPEN on the left: income of exactly $26,050 is still in
#' the 0% bracket (it is not "more than $26,050"), and only income strictly
#' above the threshold moves up. `findInterval(..., left.open = TRUE)`
#' matches that convention; the default (closed-on-left) would wrongly tax
#' a unit sitting exactly on a bracket boundary at the higher rate.
#'
#' The liability at income x, under the bracket with the largest `over`
#' strictly below x, is `base + rate * (x - over)`. This is the exact shape
#' ODT publishes its own rate tables in (see scenarios.yml), so brackets
#' are used as published rather than re-derived from the marginal rates
#' alone -- the published base amounts do not perfectly telescope across
#' brackets (ODT indexes each bracket's own base and threshold separately
#' each August), and matching the published table exactly is more
#' defensible than a "cleaner" reconstruction.
#'
#' @param taxable_income Numeric vector, already net of exemptions.
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

#' Ohio's MAGI-tiered personal/dependent exemption, per exemption claimed.
#'
#' ORC 5747.025: the dollar exemption amount (multiplied by the taxpayer's
#' own exemption count -- self + spouse + dependents) depends on the
#' taxpayer's OWN modified adjusted gross income tier, not on which
#' dependent is being counted. AT OR ABOVE `magi_cutoff`, all exemptions are
#' eliminated (HB 96, scenarios.yml's sourced "at or above" wording) -- not
#' just reduced to the lowest tier. Tiers are worded "MAGI <= $40,000" /
#' "> $40,000 but <= $80,000" / "> $80,000" -- open on the left at each
#' boundary, same convention (and same left.open = TRUE fix) as
#' apply_bracket_schedule() above. `magi_cutoff` is a DIFFERENT (closed)
#' boundary direction from those tier lookups -- don't "simplify" the `>=`
#' below to `>` to match them; it would silently reintroduce a bug.
#'
#' @param magi Numeric vector, the MAGI used to pick a tier.
#' @param exemption_tiers Data frame with columns `magi_over`, `amount`,
#'   one row per tier (any order; sorted internally).
#' @param magi_cutoff MAGI at or above which the per-exemption amount is 0.
#' @return Numeric vector, dollars per exemption, aligned to `magi`.
ohio_exemption_per_person <- function(magi, exemption_tiers, magi_cutoff) {
  exemption_tiers <- exemption_tiers[order(exemption_tiers$magi_over), ]
  tier_idx <- pmax(findInterval(magi, exemption_tiers$magi_over, left.open = TRUE), 1L)
  dplyr::if_else(magi >= magi_cutoff, 0, exemption_tiers$amount[tier_idx])
}

#' Hand-coded TY2026 Ohio nonbusiness income tax, at the TAXSIM-unit grain.
#'
#' Computes current-law Ohio liability and marginal rate on the SAME units
#' build_taxsim_units() returns, so 02_code/2a and 02_code/3b can join this
#' directly by `taxsimid` instead of re-running TAXSIM for the Ohio side.
#' Ohio AGI is approximated the same way 02_code/2a's `tu_income` already is --
#' wages + self-employment + interest + pensions + other, excluding Social
#' Security (`gssi`), which Ohio exempts from taxable income. MAGI for the
#' exemption tier is approximated by that same pre-exemption AGI measure
#' (PUMS carries no Ohio addback detail to compute true MAGI) -- a
#' documented approximation, not a coding shortcut, matching this file's
#' existing TAXSIM measurement-gap notes (02_code/2a-income-tax-baseline.R
#' header). Taxable income nets the MAGI-tiered exemption off Ohio AGI
#' BEFORE applying the bracket schedule, matching the post-exemption base
#' semantics TAXSIM's `v36_state_taxable_income` used previously (2026-07-
#' 15 income-handcode plan, Section 4) -- preserved here for continuity of
#' meaning, not because TAXSIM is still in the loop for this side.
#'
#' @param units Tibble from build_taxsim_units(): `taxsimid`, `mstat`,
#'   `depx`, `pwages`, `swages`, `psemp`, `ssemp`, `intrec`, `pensions`,
#'   `nonprop` (`gssi` intentionally excluded -- Ohio-exempt).
#' @param brackets Data frame with columns `over`, `base`, `rate`
#'   (scenarios.yml `current_law$state_income_brackets_2026`).
#' @param exemption_tiers Data frame with columns `magi_over`, `amount`
#'   (scenarios.yml `current_law$state_income_exemption_magi_tiers`).
#' @param exemption_magi_cutoff MAGI at/above which exemptions are 0
#'   (scenarios.yml `current_law$state_income_exemption_magi_cutoff`).
#' @return Tibble: `taxsimid`, `oh_agi`, `n_exemptions`,
#'   `oh_exemption_amount`, `oh_taxable_income`, `oh_tax_current`,
#'   `oh_mtr_current`.
ohio_income_tax_taxsim_units <- function(units, brackets, exemption_tiers, exemption_magi_cutoff) {
  base <- units |>
    dplyr::transmute(
      taxsimid,
      oh_agi = pwages + swages + psemp + ssemp + intrec + pensions + nonprop,
      n_exemptions = dplyr::if_else(mstat == "married, jointly", 2L, 1L) + depx
    )

  base <- dplyr::mutate(
    base,
    oh_exemption_amount = n_exemptions * ohio_exemption_per_person(oh_agi, exemption_tiers, exemption_magi_cutoff),
    oh_taxable_income = pmax(oh_agi - oh_exemption_amount, 0)
  )

  schedule <- apply_bracket_schedule(base$oh_taxable_income, brackets)

  dplyr::mutate(
    base,
    oh_tax_current = schedule$tax,
    oh_mtr_current = schedule$mtr
  )
}

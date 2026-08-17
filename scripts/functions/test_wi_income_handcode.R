############################################################################
# Wisconsin Property Tax Analysis — Hand-Coded TY2025 Wisconsin Income Tax
# THIS FILE:
#   1) Runs executable checks against apply_bracket_schedule(),
#      .wi_std_deduction(), and wisconsin_income_tax_taxsim_units()
#      (scripts/functions/incidence.R Sections III & V), the TY2025
#      Wisconsin hand-code that anchors the income-tax baseline.
#   2) Covers the open-on-the-left bracket-boundary convention: Wisconsin's
#      schedule is worded "$0 -- $14,680 ...", so income landing exactly on
#      a boundary stays in the LOWER bracket. The default findInterval()
#      gets this wrong; left.open = TRUE is required.
#   3) Covers the income-phased standard deduction at its max / phase-out
#      start / zero-out boundaries, for both filing statuses.
#   4) Covers single vs. married-jointly-with-dependents exemption counting
#      and the wider MFJ brackets / standard deduction.
# INPUTS:  scripts/functions/incidence.R
# OUTPUTS: none -- prints [PASS]/[FAIL] per check to the console and exits
#          non-zero if any check fails.
# NOTES:   Synthetic units only (no PUMS read) -- a unit test of the schedule
#          arithmetic, not an integration test of scripts/2a. Run via
#          `Rscript scripts/functions/test_wi_income_handcode.R`.
#          Bracket/standard-deduction/exemption constants are duplicated here
#          as literal TY2025 values (not read from scenarios.yml) so this test
#          catches an accidental edit to the yml -- a yml read would silently
#          test "whatever the yml currently says".
# Author:  Adapted for Wisconsin from the Lade lab Ohio project
#          (test_ohio_income_handcode.R).
############################################################################

# --- SETUP ---
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source(here::here("scripts/functions/incidence.R"))

# TY2025 Wisconsin schedule, as literal constants (same shape as wi_tax_params
# and scripts/scenarios/scenarios.yml current_law$state_income_*). Marginal
# rates; `base` telescopes exactly from the rates below.
PARAMS <- list(
  brackets_single = tibble::tibble(
    over = c(0, 14680, 50480, 323290),
    base = c(0, 513.80, 2089.00, 16547.93),
    rate = c(0.0350, 0.0440, 0.0530, 0.0765)
  ),
  brackets_mfj = tibble::tibble(
    over = c(0, 19580, 67300, 431060),
    base = c(0, 685.30, 2784.98, 22064.26),
    rate = c(0.0350, 0.0440, 0.0530, 0.0765)
  ),
  sd_single = list(max = 13560, start = 19549, zero = 132549),
  sd_mfj    = list(max = 25110, start = 28209, zero = 155169),
  exemption_per_person = 700
)

############################################################################
# I. TEST SETUP
############################################################################

fails <- 0

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    message("  error: ", conditionMessage(e))
    FALSE
  })
  message(if (ok) "[PASS] " else "[FAIL] ", label)
  if (!ok) fails <<- fails + 1
  invisible(ok)
}

close_to <- function(x, target, tol = 1e-2) abs(x - target) < tol

############################################################################
# II. BRACKET SCHEDULE — apply_bracket_schedule() (single/HoH)
############################################################################

bs <- PARAMS$brackets_single

sched_at_boundary <- apply_bracket_schedule(14680, bs)
check("taxable income of exactly $14,680 stays in the 3.50% bracket (open-left boundary)",
      close_to(sched_at_boundary$tax, 513.80) && close_to(sched_at_boundary$mtr, 0.0350))

sched_just_above <- apply_bracket_schedule(14680.01, bs)
check("one cent above $14,680 crosses into the 4.40% bracket, on the $513.80 base",
      close_to(sched_just_above$tax, 513.80 + 0.0440 * 0.01) && close_to(sched_just_above$mtr, 0.0440))

sched_50k <- apply_bracket_schedule(50000, bs)
check("$50,000 taxable income pays $2,067.88 at 4.40% marginal",
      close_to(sched_50k$tax, 2067.88) && close_to(sched_50k$mtr, 0.0440))

sched_100k <- apply_bracket_schedule(100000, bs)
check("$100,000 taxable income pays $4,713.56 at 5.30% marginal",
      close_to(sched_100k$tax, 4713.56) && close_to(sched_100k$mtr, 0.0530))

sched_400k <- apply_bracket_schedule(400000, bs)
check("$400,000 taxable income reaches the 7.65% top bracket ($22,416.25)",
      close_to(sched_400k$tax, 22416.25) && close_to(sched_400k$mtr, 0.0765))

sched_zero <- apply_bracket_schedule(0, bs)
check("zero taxable income pays $0",
      close_to(sched_zero$tax, 0))

sched_vec <- apply_bracket_schedule(c(0, 14680, 14680.01, 100000, 400000), bs)
check("apply_bracket_schedule() is vectorized (5 inputs -> 5 aligned outputs)",
      length(sched_vec$tax) == 5L && length(sched_vec$mtr) == 5L)

############################################################################
# III. STANDARD DEDUCTION — .wi_std_deduction()
############################################################################

check("single SD holds at the $13,560 max at/below the $19,549 phase-out start",
      close_to(.wi_std_deduction(10000, PARAMS$sd_single), 13560) &&
        close_to(.wi_std_deduction(19549, PARAMS$sd_single), 13560))
check("single SD @ $50,000 income is $9,905.88 (12.0%/$ phase-out)",
      close_to(.wi_std_deduction(50000, PARAMS$sd_single), 9905.88))
check("single SD is fully phased out to $0 at/above $132,549",
      close_to(.wi_std_deduction(132549, PARAMS$sd_single), 0) &&
        close_to(.wi_std_deduction(200000, PARAMS$sd_single), 0))
check("MFJ SD holds at the $25,110 max at/below the $28,209 start, $0 at/above $155,169",
      close_to(.wi_std_deduction(20000, PARAMS$sd_mfj), 25110) &&
        close_to(.wi_std_deduction(155169, PARAMS$sd_mfj), 0))

############################################################################
# IV. FULL UNIT-LEVEL SCHEDULE — wisconsin_income_tax_taxsim_units()
############################################################################
# Synthetic taxsim units spanning single vs. married jointly, with and
# without dependents, across the bracket range.

units <- tibble::tribble(
  ~taxsimid, ~mstat,             ~depx, ~pwages, ~swages, ~psemp, ~ssemp, ~intrec, ~pensions, ~nonprop,
  1L,        "single",           0L,    20000,   0,       0,      0,      0,       0,         0,
  2L,        "single",           0L,    150000,  0,       0,      0,      0,       0,         0,
  3L,        "married, jointly", 2L,    60000,   30000,   0,      0,      0,       0,         0,
  4L,        "single",           0L,    600000,  0,       0,      0,      0,       0,         0,
  5L,        "single",           0L,    0,       0,       0,      0,      0,       0,         0
)

result <- wisconsin_income_tax_taxsim_units(units, PARAMS)

check("result has one row per input unit, in taxsimid order",
      nrow(result) == nrow(units) && all(result$taxsimid == units$taxsimid))

u1 <- filter(result, taxsimid == 1L)
check("unit 1 (single, $20,000 AGI): 1 exemption, SD $13,505.88, taxable $5,794.12, tax $202.79 at 3.50%",
      u1$n_exemptions == 1L && !u1$mfj && close_to(u1$std_deduction, 13505.88) &&
        close_to(u1$wi_taxable_income, 5794.12) && close_to(u1$wi_tax_current, 202.79) &&
        close_to(u1$wi_mtr_current, 0.0350))

u2 <- filter(result, taxsimid == 2L)
check("unit 2 (single, $150,000 AGI): SD fully phased out, taxable $149,300, tax $7,326.46 at 5.30%",
      close_to(u2$std_deduction, 0) && close_to(u2$wi_taxable_income, 149300) &&
        close_to(u2$wi_tax_current, 7326.46) && close_to(u2$wi_mtr_current, 0.0530))

u3 <- filter(result, taxsimid == 3L)
check("unit 3 (joint, 2 dependents, $90,000 AGI): 4 exemptions, MFJ brackets/SD, tax $3,156.56 at 5.30%",
      u3$mfj && u3$n_exemptions == 4L && close_to(u3$wi_exemption_amount, 4 * 700) &&
        close_to(u3$std_deduction, 12889.05) && close_to(u3$wi_taxable_income, 74310.95) &&
        close_to(u3$wi_tax_current, 3156.56) && close_to(u3$wi_mtr_current, 0.0530))

u4 <- filter(result, taxsimid == 4L)
check("unit 4 (single, $600,000 AGI): top 7.65% bracket, tax $37,662.70",
      close_to(u4$wi_taxable_income, 599300) && close_to(u4$wi_tax_current, 37662.70) &&
        close_to(u4$wi_mtr_current, 0.0765))

u5 <- filter(result, taxsimid == 5L)
check("unit 5 (zero income): taxable income floored at $0, tax $0",
      close_to(u5$wi_taxable_income, 0) && close_to(u5$wi_tax_current, 0))

############################################################################
# V. RESULTS
############################################################################
message("\n", if (fails == 0) "ALL TESTS PASS" else paste(fails, "TEST(S) FAILED"))
quit(status = as.integer(fails > 0))

############################################################################
# Wisconsin Property Tax Abolition Analysis
# THIS FILE:
#   1) Extracts Wisconsin 2022 local-government expenditure from the Government
#      Finance Database (GFD) Midwest extract, by jurisdiction type and by
#      major Census expenditure function.
#   2) Produces the expenditure side of the "what does the property tax
#      fund" exhibit — total expenditure for county, municipal, township,
#      school district, and special district governments, broken into
#      function categories (education, public welfare, fire, police, etc.)
#      plus an "Other" residual.
# INPUTS:  data/external/govt_finance_database/midwest_2005_present.parquet
#          (Pierson, Hand & Thompson 2015, PLoS ONE; see DATA_GUIDE.md there)
# OUTPUTS: data/clean//wisconsin_expenditure_by_jurisdiction_2022.csv
# NOTES:   GFD dollar values are reported in THOUSANDS of nominal USD, so
#          every category total below is multiplied by 1,000. 2022 is a
#          Census-of-Governments year, so govt_type_label coverage is
#          near-complete (non-Census years have more missingness).
# Last updated: 2026-07-13
############################################################################

# --- SETUP ---
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})

gfd_path <- here("data/raw/govt_finance_database/midwest_2005_present.parquet")

############################################################################
# I. EXPENDITURE FUNCTION CATEGORIES AND SCHEMA PREFLIGHT
############################################################################

# --------------------------------------------------------------------------
# Census expenditure function -> GFD variable(s)
# --------------------------------------------------------------------------
# Only the major Census functions are broken out; everything else falls into
# "Other" as a residual. Fire protection and sewerage ARE separate GFD
# variables (Fire_Prot_Total_Expend, Sewerage_Total_Expend) and are broken
# out here rather than folded into "Other" — fire in particular is a large,
# property-tax-funded service that belongs in the "services at risk" framing
# (06_writing/presentations/tax_collections_figures.qmd).
expenditure_categories <- list(
  "Education"                          = "Total_Educ_Total_Exp",
  "Public welfare"                     = "Public_Welf_Total_Exp",
  "Health & hospitals"                 = "Total_Hospital_Total_Exp",
  "Highways & roads"                   = "Regular_Hwy_Total_Exp",
  "Police"                             = "Police_Prot_Total_Exp",
  "Fire protection"                    = "Fire_Prot_Total_Expend",
  "Corrections"                        = "Correct_Total_Exp",
  "Sewerage"                           = "Sewerage_Total_Expend",
  "Utilities (water/electric/transit)" = "Total_Util_Total_Exp",
  "Parks & recreation"                 = "Parks___Rec_Total_Exp",
  "Housing & community dev"            = "Hous___Com_Total_Exp",
  "Government administration"          = c(
    "Fin_Admin_Total_Exp",
    "Gen_Pub_Bldg_Total_Exp",
    "General_NEC_Total_Exp",
    "Judicial_Total_Expend",
    "Cen_Staff_Total_Expend"
  ),
  "Interest on debt"                   = "Total_Interest_on_Debt"
)
expenditure_vars <- unique(unlist(expenditure_categories))

# --------------------------------------------------------------------------
# Preflight: fail loudly if the parquet schema has drifted
# --------------------------------------------------------------------------
# read_parquet() on a missing column throws an opaque tidyselect error deep
# in the call stack. Check column names against the schema first and name
# exactly what's missing.
gfd_cols_need <- c("state_name", "year", "govt_type_label", "Total_Expenditure", expenditure_vars)
gfd_cols_have <- names(arrow::open_dataset(gfd_path)$schema)
gfd_cols_missing <- setdiff(gfd_cols_need, gfd_cols_have)
if (length(gfd_cols_missing) > 0) {
  stop("GFD parquet is missing columns: ", paste(gfd_cols_missing, collapse = ", "))
}

############################################################################
# II. WISCONSIN 2022 EXPENDITURE BY JURISDICTION TYPE
############################################################################

gfd_wi_2022 <- read_parquet(gfd_path, col_select = all_of(gfd_cols_need)) |>
  filter(state_name == "Wisconsin", year == 2022)

local_govt_types <- c("County", "Municipal", "Township", "School District", "Special District")

# --------------------------------------------------------------------------
# wisconsin_expenditure_by_jurisdiction_2022.csv — feeds fig-expenditure and
# fig-services-at-risk in tax_collections_figures.qmd
# --------------------------------------------------------------------------
expenditure_by_type <- lapply(local_govt_types, function(govt_type) {
  govt_sub <- filter(gfd_wi_2022, govt_type_label == govt_type)
  govt_total <- sum(govt_sub$Total_Expenditure, na.rm = TRUE)
  category_totals <- vapply(
    expenditure_categories,
    function(var_names) sum(as.matrix(govt_sub[var_names]), na.rm = TRUE),
    numeric(1)
  )
  tibble(
    govt_type = govt_type,
    category = c(names(expenditure_categories), "Other"),
    # GFD values are in thousands of nominal USD -> multiply by 1,000.
    expenditure_usd = round(c(category_totals, govt_total - sum(category_totals)) * 1000)
  )
}) |>
  bind_rows()

write_csv(expenditure_by_type, here("data/clean/wisconsin_expenditure_by_jurisdiction_2022.csv"))

print(
  expenditure_by_type |>
    group_by(govt_type) |>
    summarise(total_b = sum(expenditure_usd) / 1e9),
  n = Inf
)

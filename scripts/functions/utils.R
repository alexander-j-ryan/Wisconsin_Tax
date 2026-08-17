############################################################################
# Ohio Property Tax Abolition Analysis
# THIS FILE:
#   1) Defines small, general-purpose utilities shared across the pipeline:
#      project-root path resolution, safe parquet read/write, a zero-safe
#      percent-difference helper, and the project's standard Ohio map CRS.
#   2) Exports here_proj(), read_parquet_safe(), write_parquet_safe(),
#      pct_diff(), and ohio_crs() for prep, baseline, and diagnostic scripts
#      to source (e.g. 02_code/1a-pull-acs.R, 02_code/diagnostics/eda_04_field_
#      coverage_by_county.R).
# INPUTS:  None (defines functions only; no data is read at source time)
# OUTPUTS: None (no side effects at source time — write_parquet_safe() writes
#          to whatever path its caller supplies)
# Last updated: 2026-07-13
############################################################################

# --- SETUP ---
# No library() calls — every function below reaches its dependency by
# explicit namespace (arrow::, here::), so sourcing this file never forces a
# hard dependency on a package a caller might not have installed.

#' Resolve a path relative to the project root, regardless of working directory
#'
#' Wraps here::here() so scripts behave identically whether run interactively,
#' via Rscript, or from a Makefile recipe (Make's working directory is
#' wherever `make` was invoked, not the script's folder). Falls back to
#' file.path() if the `here` package isn't installed, so this file carries no
#' hard dependency on it.
#'
#' @param ... Path components, passed through to here::here() (or file.path()).
#' @return A character path string.
here_proj <- function(...) {
  if (requireNamespace("here", quietly = TRUE)) {
    here::here(...)
  } else {
    file.path(...)
  }
}

#' Read a parquet file, failing with a clear message if it is missing
#'
#' arrow::read_parquet() on a missing path throws a low-level I/O error that
#' doesn't name the file. This wraps it with a one-line message naming the
#' path, so a broken pipeline dependency is obvious from the error alone.
#'
#' @param path Path to the .parquet file.
#' @return A tibble (see arrow::read_parquet()).
read_parquet_safe <- function(path) {
  if (!file.exists(path)) {
    stop("Parquet file not found: ", path)
  }
  arrow::read_parquet(path)
}

#' Write a parquet file, creating the parent directory if it doesn't exist
#'
#' Scripts don't call dir.create() for output directories themselves (the
#' Makefile owns that — see 02_code/AGENTS.md); this helper is the one
#' exception, since a parquet write fails outright if the parent folder is
#' missing.
#'
#' @param df A data frame or tibble to write.
#' @param path Destination .parquet path.
#' @return The path, invisibly.
write_parquet_safe <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, path)
  invisible(path)
}

#' Percent difference of x from a reference value, robust to a zero or missing reference
#'
#' Returns NA rather than Inf/NaN when the reference is 0 or NA, so
#' reconciliation tables (05_output/audit/) don't silently print Inf against a
#' $0 benchmark.
#'
#' @param x Numeric vector, the value being compared.
#' @param ref Numeric vector, the reference/benchmark value.
#' @return Numeric vector, (x - ref) / ref, with NA where ref is 0 or NA.
pct_diff <- function(x, ref) {
  ifelse(is.na(ref) | ref == 0, NA_real_, (x - ref) / ref)
}

#' Standard Ohio CRS for area-correct maps
#'
#' Ohio South State Plane, NAD83 (ftUS). Use whenever computing areas or
#' distances on Ohio geography (county choropleths, border-county buffers) —
#' EPSG:4326 (the tidycensus/tigris default) distorts area at this scale.
#'
#' @return Integer EPSG code, 3735L.
ohio_crs <- function() {
  3735L
}

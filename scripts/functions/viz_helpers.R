# ---------------------------------------------------------------------------
# viz_helpers.R — Map theme + spatial/format helpers for the tax-collections
#                 figures. Complements (does not replace) theme_swank() in
#                 theme_swank.R.
#
# Purpose : Shared ggplot2/sf building blocks for every county- and district-
#           level choropleth in the tax-collections figures deck: a minimal
#           map theme, the Ohio-county base layer, a Lake-Erie clipping
#           helper, sequential/diverging fill scales, and a dollar-millions
#           label formatter.
# Inputs  : 03_data-raw/external/shapefiles/cb_2018_us_county_500k/
#           cb_2018_us_county_500k.shp (Census cartographic-boundary county
#           file; shoreline-clipped, so Lake Erie is never a polygon)
# Outputs : None (defines functions only; sourced by
#           06_writing/presentations/tax_collections_figures.qmd)
# Depends : ggplot2, sf, dplyr, stringr, scales, grid, here
# Author  : Lade lab
# Notes   : Schwabish (2014) principles baked in: minimal chrome (theme_void
#           for maps), integrated/active titles, direct labeling over
#           legends where possible, sequential single-hue fills (not the
#           rainbow/diverging default), units on every scale.
# Last modified: 2026-06-18
# ---------------------------------------------------------------------------

#' Minimal map theme consistent with the Swank visual identity.
#'
#' theme_void base (no panel grid, axes, or ticks — a choropleth needs none),
#' bottom legend, active bold title. Pair with a sequential fill scale.
#'
#' @param base_size Base font size, points.
#' @param base_family Base font family (default: system default).
#' @return A ggplot2 theme object, add with `+`.
theme_swank_map <- function(base_size = 12, base_family = "") {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle    = ggplot2::element_text(color = "#404040", size = base_size, hjust = 0),
      plot.caption     = ggplot2::element_text(color = "#595959", size = base_size - 3, hjust = 0),
      legend.position  = "bottom",
      legend.key.width = grid::unit(1.4, "lines"),
      legend.title     = ggplot2::element_text(size = base_size - 1),
      plot.margin      = ggplot2::margin(8, 8, 8, 8)
    )
}

#' Load Ohio counties as an sf polygon layer, area-correct and de-watered.
#'
#' Uses the Census cartographic-boundary county file (`cb_*_500k`), which is
#' clipped to the shoreline — so Lake Erie open water does NOT appear as a
#' polygon (satisfies the project's "drop/mark Lake Erie" rule). Reprojects to
#' EPSG:3735 (Ohio South State Plane, ftUS) for an area-correct map.
#'
#' @param shp Path to cb county shapefile.
#' @return sf data frame of 88 Ohio counties with `county` (title-case) + GEOID.
load_ohio_counties <- function(shp = here::here("03_data-raw/external/shapefiles",
                                                 "cb_2018_us_county_500k/cb_2018_us_county_500k.shp")) {
  sf::st_read(shp, quiet = TRUE) |>
    dplyr::filter(STATEFP == "39") |>  # Ohio
    dplyr::transmute(geoid = GEOID, county = stringr::str_to_title(NAME)) |>
    sf::st_transform(3735L)
}

#' Dollar label for map/plot scales, scaled to millions.
#' @return A scales labelling function.
label_usd_millions <- function() {
  scales::label_dollar(scale = 1e-6, suffix = "M", accuracy = 1)
}

#' Sequential viridis fill for choropleths — smooth single-hue ramp (low =
#' light, high = dark), colourblind-safe, with a light-grey for missing
#' counties/districts so unmatched areas read as "no data" rather than as a
#' low value. Uses a HORIZONTAL colourbar so dollar labels spread out and
#' don't overlap.
#'
#' @param option Viridis palette name (default "mako").
#' @param direction Viridis direction, 1 or -1 (default -1: light = low).
#' @param na.value Fill for missing values (default "grey88").
#' @param n.breaks Number of colourbar breaks (default 5).
#' @param ... Passed to ggplot2::scale_fill_viridis_c (e.g., labels, limits).
scale_fill_swank_seq <- function(option = "mako", direction = -1, na.value = "grey88",
                                 n.breaks = 5, ...) {
  ggplot2::scale_fill_viridis_c(
    option = option,
    direction = direction,
    na.value = na.value,
    n.breaks = n.breaks,
    guide = ggplot2::guide_colourbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = grid::unit(12, "lines"),
      barheight = grid::unit(0.5, "lines")
    ),
    ...
  )
}

#' Diverging fill centred on zero — for gain/loss or "which tax dominates"
#' maps. Blue (negative) to red (positive) is avoided; we use a
#' colourblind-safe purple-green diverging ramp.
#'
#' @param ... Passed to ggplot2::scale_fill_gradient2.
scale_fill_swank_div <- function(...) {
  ggplot2::scale_fill_gradient2(
    low = "#762A83",
    mid = "grey92",
    high = "#1B7837",
    midpoint = 0,
    na.value = "grey88",
    guide = ggplot2::guide_colourbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = grid::unit(12, "lines"),
      barheight = grid::unit(0.5, "lines")
    ),
    ...
  )
}

#' Clip an sf layer to the land polygon (drops Lake Erie / open water).
#'
#' Pass `oh_land <- sf::st_union(sf::st_make_valid(oh_counties))` (counties
#' are shoreline-clipped, so their union is Ohio land only).
#'
#' @param layer sf layer to clip (e.g., cities, townships, school districts).
#' @param land sf polygon of Ohio land, from `sf::st_union()` on the counties.
#' @return `layer` intersected with `land`.
clip_to_land <- function(layer, land) {
  suppressWarnings(sf::st_intersection(sf::st_make_valid(layer), land))
}

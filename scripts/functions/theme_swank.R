# ---------------------------------------------------------------------------
# theme_swank.R — project-wide ggplot2 theme + categorical palette
#
# Purpose : Shared visual identity for the brief, technical appendix,
#           presentations, and webpage — every figure-producing script and
#           the Quarto docs source this file rather than styling plots
#           individually.
# Inputs  : none (defines objects only; no side effects at source time).
# Outputs : theme_swank(), swank_palette (exported to the sourcing script).
# Depends : ggplot2
# Author  : Lade lab
# Modified: 2026-07-13
# ---------------------------------------------------------------------------

#' Project ggplot2 theme — minimal base with Swank Program typography
#'
#' theme_minimal() base (no panel border, light grid), bold active titles,
#' muted subtitle/caption grays, bottom legend. Use for all non-map figures;
#' pair with theme_swank_map() (viz_helpers.R) for choropleths.
#'
#' @param base_size Base font size, points.
#' @param base_family Base font family; "" uses the device default.
#' @return A ggplot2 theme object, addable to a plot with `+`.
theme_swank <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 3),
      plot.subtitle    = ggplot2::element_text(color = "#404040", size = base_size),
      plot.caption     = ggplot2::element_text(color = "#595959", size = base_size - 2, hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E5E5E5", linewidth = 0.3),
      strip.text       = ggplot2::element_text(face = "bold"),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(face = "bold", size = base_size - 1),
      axis.title       = ggplot2::element_text(size = base_size - 1)
    )
}

# Categorical palette — adjust to match Swank Program / OSU style guide once approved.
# Order: navy (primary series) -> scarlet (OSU accent) -> neutral gray -> accent
# blue/green/orange (additional series, in that priority order).
swank_palette <- c(
  "#1F3864",  # navy
  "#BB0000",  # OSU scarlet
  "#7F7F7F",  # neutral gray
  "#5B9BD5",  # accent blue
  "#70AD47",  # accent green
  "#ED7D31"   # accent orange
)

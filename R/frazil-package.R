#' frazil: Antarctic sea ice figures from the NSIDC Sea Ice Index
#'
#' The package downloads the NSIDC Sea Ice Index, Version 4 (data set G02135)
#' for the Southern Hemisphere, computes extent statistics with a record rule
#' that allows for the 2025 change to the AMSR2 sensor, builds the terrain of
#' the BRI priority map from REMA elevation and the SCAR Antarctic Digital
#' Database, and renders slide figures and an animation for any date and any
#' polar map window.
#'
#' Start by pointing the package at its folders with [frazil_paths()], then
#' mirror the NSIDC files with [nsidc_download()]. The figure functions are
#' named `fig_*()`; [render_figure_set()] runs all of them.
#'
#' @keywords internal
"_PACKAGE"

#' @import ggplot2
#' @importFrom dplyr filter mutate select arrange group_by summarise ungroup
#'   left_join inner_join bind_rows distinct slice_min slice_max n if_else
#'   case_when count group_modify rename
#' @importFrom rlang .data check_installed
#' @importFrom stats lm coef confint predict quantile setNames ave
#' @importFrom utils head tail
#' @importFrom grDevices col2rgb colorRampPalette dev.off
#' @importFrom grid unit
#' @importFrom tidyterra geom_spatraster
NULL

utils::globalVariables(c(
  "extent", "year", "month", "day", "date", "missing", "source", "extent5",
  "piece", "line_id", "doy", "area", "sector", "base", "dep", "label", "clim",
  "p10", "p25", "p50", "p75", "p90", "max5", "max_doy", "min5", "min_doy",
  "anom", "idx", "run12", "fit", "pct", "pct_lo", "pct_hi", "role", "slope",
  "lo", "hi", "site_id", "species_id", "on_coast", "x", "y", "conc", "lw",
  "geometry", "name", "type", "region", "longitude", "latitude", "level",
  "contour", "md", "doy365", "site_name", "code", "nodata", "change", "v",
  "n_months", "wkt"
))

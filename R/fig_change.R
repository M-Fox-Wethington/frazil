# --- Change maps --------------------------------------------------------------

#' Figure: where monthly ice cover has changed
#'
#' Two circumpolar panels, one per month in `months`, of the change in mean
#' monthly ice cover between the two periods from [cover_change()], in
#' percentage points. By default February, the summer low, and September, the
#' winter high. Less ice is warm and more ice cool, as in the month-by-month
#' stripes, and the boundaries of one NSIDC sector are dashed.
#'
#' @param early,recent The years of the two periods.
#' @param months Two calendar months.
#' @param sector The sector marked on the map and summarized.
#' @param limits Range of the color scale, in percentage points.
#' @param paths The list from [frazil_paths()].
#' @inheritParams fig_extent_daily
#' @return The cowplot object, invisibly, with attribute `summary`, a list of
#'   [change_summary()] results, one per month.
#' @export
fig_map_cover_change <- function(early = 1979:1988, recent = 2016:2025, months = c(2, 9),
                                 sector = "Weddell", limits = c(-60, 60),
                                 out_dir = frazil_paths()$output, name = "map_cover_change",
                                 paths = frazil_paths()) {
  stopifnot(length(months) == 2)
  files <- monthly_conc_files(paths$cache)
  changes <- lapply(months, function(m) {
    cc <- cover_change(m, early, recent, files = files)
    s  <- change_summary(cc$change, cc$raw, sector)
    message(sprintf("%s: %s sector mean change %+.1f points over %d iced cells; all cells %+.1f points; %d cells lost more than 30 points",
                    month.name[m], sector, s$sector_mean, s$sector_cells, s$all_mean,
                    s$all_lost))
    list(change = cc$change, raw = cc$raw, summary = s)
  })

  raw_ref <- changes[[1]]$raw
  base <- circumpolar_base(raw_ref, frame = NULL, paths = paths)
  terrain_layer <- rgb_annotation(base$terrain$rgb)
  panels <- lapply(changes, function(cc)
    circumpolar_change_map(base, raw_ref, cc$change, limits, terrain_layer, sector = sector))

  season <- function(m) if (m %in% c(12, 1, 2, 3)) "summer low" else
    if (m %in% c(8, 9, 10)) "winter high" else ""
  panel_title <- function(m) trimws(paste0(month.name[m], if (nzchar(season(m))) ", " else "",
                                           season(m)))
  bar_labels <- c(paste0("\u2212", abs(limits[1]), " less ice"),
                  paste0("\u2212", abs(limits[1]) / 2), "0",
                  paste0("+", limits[2] / 2), paste0("+", limits[2], " more ice"))
  bar_breaks <- c(limits[1], limits[1] / 2, 0, limits[2] / 2, limits[2])

  title <- paste0("Change in sea ice cover, ", min(recent), "\u2013", max(recent),
                  " against ", min(early), "\u2013", max(early))
  slide <- slide_canvas(title, paste0(SOURCE_NSIDC, ", monthly grids. Terrain: REMA v2.0. ",
                                      "Coastline and rock: SCAR ADD.")) +
    cowplot::draw_label(panel_title(months[1]), x = 0.0375, y = 0.855, hjust = 0, vjust = 1,
                        size = TXT$legend_title, fontface = "bold", color = COLORS$title) +
    cowplot::draw_label(panel_title(months[2]), x = 0.405, y = 0.855, hjust = 0, vjust = 1,
                        size = TXT$legend_title, fontface = "bold", color = COLORS$title) +
    cowplot::draw_plot(panels[[1]], x = 0.0375, y = 0.10, width = 0.355, height = 0.70) +
    cowplot::draw_plot(panels[[2]], x = 0.405, y = 0.10, width = 0.355, height = 0.70) +
    cowplot::draw_plot(change_colorbar(limits, bar_breaks, bar_labels,
                                       "Change in ice cover\n(percentage points)",
                                       length_in = 3.2),
                       x = 0.79, y = 0.30, width = 0.19, height = 0.50) +
    cowplot::draw_plot(key_panel(data.frame(
      type = c("sector", "nodata"),
      label = c(paste(sector_row(sector)$name, "sector"), "No ice data near\nthe coast")),
      row_gap = 0.45),
      x = 0.79, y = 0.08, width = 0.20, height = 0.22)

  save_slide(slide, name, out_dir, bg = MAP_PAPER)
  invisible(structure(slide, summary = setNames(lapply(changes, `[[`, "summary"),
                                                month.name[months])))
}

#' Figure: change in the number of days a year with sea ice
#'
#' A circumpolar map of the change in ice days per year between the two
#' periods from [season_change()], with the mean change over one NSIDC sector
#' and over all iced cells beside it.
#'
#' @param limits Range of the color scale, in days.
#' @inheritParams fig_map_cover_change
#' @return The cowplot object, invisibly, with attribute `summary`, the
#'   [change_summary()] result.
#' @export
fig_map_season_change <- function(early = 1979:1988, recent = 2016:2025, sector = "Weddell",
                                  limits = c(-90, 90), out_dir = frazil_paths()$output,
                                  name = "map_season_change", paths = frazil_paths()) {
  sc <- season_change(early, recent, files = daily_conc_files(paths$cache),
                      terrain_cache = paths$terrain_cache)
  s  <- change_summary(sc$change, sc$raw, sector)
  message(sprintf("Mean change: %s sector %+.1f days, all iced cells %+.1f days",
                  sector, s$sector_mean, s$all_mean))
  message(sprintf("Cells losing more than 30 days: %s %d, all %d",
                  sector, s$sector_lost, s$all_lost))

  base  <- circumpolar_base(sc$raw, frame = NULL, paths = paths)
  p_map <- circumpolar_change_map(base, sc$raw, sc$change, limits, sector = sector)

  fmt_days <- function(x) minus(sprintf("%+.0f days", x))
  bar_breaks <- c(limits[1], limits[1] / 2, 0, limits[2] / 2, limits[2])
  bar_labels <- c(paste0("\u2212", abs(limits[1]), " days, less ice"),
                  paste0("\u2212", abs(limits[1]) / 2), "0",
                  paste0("+", limits[2] / 2), paste0("+", limits[2], " days, more ice"))
  sector_name <- sector_row(sector)$name
  title <- paste0("Days a year with sea ice, ", min(recent), "\u2013", max(recent),
                  " against ", min(early), "\u2013", max(early))
  slide <- slide_canvas(title, paste0(SOURCE_NSIDC, ", daily grids. Terrain: REMA v2.0. ",
                                      "Coastline and rock: SCAR ADD.")) +
    cowplot::draw_plot(p_map, x = 0.0375, y = 0.085, width = 0.47, height = 0.79) +
    cowplot::draw_label("Average change", x = 0.56, y = 0.835, hjust = 0, vjust = 1,
                        size = TXT$legend_title, fontface = "bold", color = COLORS$title) +
    cowplot::draw_label(paste0(sector_name, " sector: ", fmt_days(s$sector_mean)), x = 0.56,
                        y = 0.78, hjust = 0, vjust = 1, size = 22, fontface = "bold",
                        color = COLORS$title) +
    cowplot::draw_label(paste("All Antarctic sea ice:", fmt_days(s$all_mean)), x = 0.56,
                        y = 0.72, hjust = 0, vjust = 1, size = 18, color = COLORS$subtitle) +
    cowplot::draw_plot(change_colorbar(limits, bar_breaks, bar_labels,
                                       "Change in days a year\nwith sea ice", length_in = 3.0),
                       x = 0.56, y = 0.18, width = 0.22, height = 0.46) +
    cowplot::draw_plot(key_panel(data.frame(
      type = c("sector", "nodata"),
      label = c(paste(sector_name, "sector"), "No ice data near\nthe coast")),
      row_gap = 0.45),
      x = 0.78, y = 0.25, width = 0.20, height = 0.22)

  save_slide(slide, name, out_dir, bg = MAP_PAPER)
  invisible(structure(slide, summary = s))
}

# --- Extent figures -----------------------------------------------------------

#' Figure: sea ice extent through the year, every year
#'
#' One thin gray line per year since `first_year`, the 1981-2010 median with
#' its middle 50% and middle 80% of years shaded behind, the record high and
#' record low years labeled, and the latest year drawn bold in the focal
#' orange. Once the calendar turns over (the latest day falls in January to
#' April) the previous year is drawn too, dashed, so the season under way stays
#' on the slide.
#'
#' Lines are five-day trailing means ([add_trailing_mean()]). The record years
#' are found from the file with [record_year()], so a year from `amsr2_start`
#' on takes a record only if it beats the earlier record by more than `offset`.
#' The latest year's maximum is not final until the season peaks, so it cannot
#' take the record high.
#'
#' @param date The last day to draw, or `NULL` for the last day in the cache.
#' @param out_dir Folder to write `name.png` and `name.pdf` into, or `NULL` to
#'   write nothing.
#' @param name File name stem.
#' @param first_year First year drawn.
#' @inheritParams record_year
#' @inheritParams read_daily_extent
#' @return The ggplot object, invisibly, with attributes `latest` (the last
#'   date drawn) and `records` (the record high and low years).
#' @export
fig_extent_daily <- function(date = NULL, out_dir = frazil_paths()$output, name = "extent_daily",
                             first_year = 1979, amsr2_start = 2025, offset = 0.15,
                             cache = frazil_paths()$cache) {
  end_date <- if (is.null(date)) NULL else as.Date(date)
  d <- read_daily_extent(cache) |> add_trailing_mean() |> filter(year >= first_year)
  if (!is.null(end_date)) d <- d[d$date <= end_date, ]
  d <- d |> mutate(doy = as.integer(format(date, "%j"))) |> add_line_groups()
  clim <- read_climatology(cache) |> filter(doy <= 365)

  latest_date <- max(d$date)
  latest_year <- as.integer(format(latest_date, "%Y"))
  show_previous <- as.integer(format(latest_date, "%j")) <= 120

  ann <- d |>
    group_by(year) |>
    summarise(max5 = max(extent5), max_doy = doy[which.max(extent5)],
              min5 = min(extent5), min_doy = doy[which.min(extent5)], .groups = "drop")
  full <- ann$year < latest_year
  rec_high <- record_year(ann$max5[full], ann$year[full], lowest = FALSE,
                          amsr2_start = amsr2_start, offset = offset)
  rec_low  <- record_year(ann$min5, ann$year, lowest = TRUE,
                          amsr2_start = amsr2_start, offset = offset)
  message("Record high year: ", rec_high, "; record low year: ", rec_low)

  focus_years <- c(latest_year, if (show_previous) latest_year - 1)
  past   <- d |> filter(!year %in% c(focus_years, rec_high, rec_low))
  record <- d |> filter(year %in% c(rec_high, rec_low))
  latest <- d |> filter(year == latest_year)
  prev   <- d |> filter(show_previous, year == latest_year - 1)

  # The record low year is labeled at its own winter peak, from the empty space
  # under the curves, with a leader line up to it. The curves rise too steeply
  # earlier in the year for a label to sit beside them without crossing a line.
  hi <- ann |> filter(year == rec_high)
  lo <- ann |> filter(year == rec_low)
  lo_text_y <- lo$max5 - 4.4
  end_pt <- latest |> slice_max(date, n = 1)

  month_start <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  month_mid   <- (head(month_start, -1) + tail(month_start, -1)) / 2

  # Key, drawn in the empty upper left where the curves sit low in summer
  key_x <- 6; key_w <- 14; key_txt <- key_x + key_w + 4
  key_y <- c(19.6, 18.2, 16.8, 15.4)

  p <- ggplot() +
    geom_ribbon(data = clim, aes(doy, ymin = p10, ymax = p90),
                fill = COLORS$slate, alpha = 0.10) +
    geom_ribbon(data = clim, aes(doy, ymin = p25, ymax = p75),
                fill = COLORS$slate, alpha = 0.20) +
    geom_line(data = past, aes(doy, extent5, group = line_id),
              color = COLORS$aluminum, linewidth = 0.35, alpha = 0.75) +
    geom_line(data = clim, aes(doy, p50), color = COLORS$slate,
              linewidth = 1.0, linetype = "22") +
    geom_line(data = record, aes(doy, extent5, group = line_id),
              color = COLORS$subtitle, linewidth = 0.8) +
    geom_line(data = prev, aes(doy, extent5, group = line_id),
              color = COLORS$orange, linewidth = 1.4, linetype = "42") +
    geom_line(data = latest, aes(doy, extent5, group = line_id),
              color = COLORS$orange, linewidth = 2.2, lineend = "round") +
    geom_point(data = end_pt, aes(doy, extent5), color = COLORS$orange, size = 4.2) +
    annotate("text", x = end_pt$doy + 5, y = end_pt$extent5, label = latest_year,
             hjust = 0, vjust = 0.5, size = pt(22), fontface = "bold", color = COLORS$orange) +
    annotate("text", x = hi$max_doy, y = hi$max5 + 0.55,
             label = paste0(rec_high, ": record high"),
             size = pt(TXT$label), color = COLORS$subtitle, fontface = "bold") +
    annotate("segment", x = lo$max_doy, xend = lo$max_doy, y = lo_text_y + 0.55,
             yend = lo$max5 - 0.3, color = COLORS$subtitle, linewidth = 0.6) +
    annotate("text", x = lo$max_doy, y = lo_text_y,
             label = paste0(rec_low, ": record low"),
             size = pt(TXT$label), color = COLORS$subtitle, fontface = "bold") +
    # Key. The middle-half swatch stacks both fills, as the bands do in the plot.
    annotate("rect", xmin = key_x, xmax = key_x + key_w, ymin = key_y[1] - 0.32,
             ymax = key_y[1] + 0.32, fill = COLORS$slate, alpha = 0.10) +
    annotate("rect", xmin = key_x, xmax = key_x + key_w, ymin = key_y[1] - 0.32,
             ymax = key_y[1] + 0.32, fill = COLORS$slate, alpha = 0.20) +
    annotate("rect", xmin = key_x, xmax = key_x + key_w, ymin = key_y[2] - 0.32,
             ymax = key_y[2] + 0.32, fill = COLORS$slate, alpha = 0.10) +
    annotate("segment", x = key_x, xend = key_x + key_w, y = key_y[3], yend = key_y[3],
             color = COLORS$slate, linewidth = 1.0, linetype = "22") +
    annotate("segment", x = key_x, xend = key_x + key_w, y = key_y[4], yend = key_y[4],
             color = COLORS$aluminum, linewidth = 0.8) +
    annotate("text", x = key_txt, y = key_y, hjust = 0, size = pt(TXT$small),
             color = COLORS$subtitle,
             label = c("Middle half of years, 1981\u20132010",
                       "Middle 80% of years, 1981\u20132010",
                       "Median, 1981\u20132010",
                       paste0("Each gray line is one year, ", first_year, "\u2013",
                              latest_year - 1))) +
    scale_x_continuous(breaks = month_mid, labels = month.abb, minor_breaks = month_start,
                       limits = c(1, 366), expand = c(0.004, 0)) +
    scale_y_continuous(limits = c(0, 21), breaks = seq(0, 20, 5), expand = c(0, 0),
                       name = "Sea ice extent (million km\u00b2)") +
    coord_cartesian(clip = "off") +
    labs(title = paste0("Antarctic sea ice extent through the year, ", first_year, " to ",
                        latest_year),
         x = NULL, caption = SOURCE_NSIDC) +
    theme_slide() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_line(color = COLORS$grid, linewidth = 0.6),
          axis.ticks.length = unit(0, "pt"),
          plot.margin = margin(0.32, 0.85, 0.28, 0.50, unit = "in"))

  save_slide(p, name, out_dir)
  invisible(structure(p, latest = latest_date,
                      records = c(high = as.integer(rec_high), low = as.integer(rec_low))))
}

#' Figure: sea ice compared with its 1981-2010 average, month by month
#'
#' One stripe per month, colored by how far that month's mean extent sat above
#' or below the 1981-2010 mean for the same calendar month. Less ice than usual
#' takes the warm end of the Rams diverging ramp, more ice the cool end, and
#' the scale is symmetric about zero at the largest departure in the record.
#' Months NSIDC marks as missing (December 1987 and January 1988) are left
#' blank.
#'
#' @param date The last month to draw, as any date in it, or `NULL` for the
#'   last month in the cache.
#' @inheritParams fig_extent_daily
#' @return The ggplot object, invisibly, with attribute `limits` (the color
#'   limits).
#' @export
fig_extent_anomaly <- function(date = NULL, out_dir = frazil_paths()$output,
                               name = "extent_anomaly_stripes", first_year = 1979,
                               cache = frazil_paths()$cache) {
  end_date <- if (is.null(date)) NULL else as.Date(date)
  m <- read_monthly_extent(cache) |> filter(year >= first_year) |> monthly_departures()
  last_month <- max(m$date[!is.na(m$extent)])
  if (!is.null(end_date)) last_month <- min(last_month, as.Date(format(end_date, "%Y-%m-01")))
  m <- m |>
    filter(date <= last_month) |>
    mutate(idx = (year - first_year) * 12 + month)   # one raster column per month

  lim <- ceiling(max(abs(m$anom), na.rm = TRUE) * 10) / 10
  message(sprintf("Departures run from %.2f to %+.2f million km2; color limits +/- %.1f",
                  min(m$anom, na.rm = TRUE), max(m$anom, na.rm = TRUE), lim))

  year_breaks <- seq(ceiling(first_year / 5) * 5, max(m$year), by = 5)

  # geom_raster draws the months as one regular grid, so no hairline seams
  # appear between neighboring stripes in the PNG. theme_bri() sets
  # panel.grid.major itself, so it has to be blanked by name; otherwise its
  # lines show through the blank 1987-88 column. ggplot2 draws a colorbar five
  # keys long, so a 1.3 in key gives a 6.5 in bar.
  p <- ggplot(m, aes(x = idx, y = 1, fill = anom)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradientn(
      colours = CHANGE_COLORS, limits = c(-lim, lim), na.value = NA,
      breaks = c(-2, -1, 0, 1, 2),
      labels = c("\u22122\nless ice", "\u22121", "0", "+1", "+2\nmore ice"),
      name = "Difference from the 1981\u20132010 average for that month (million km\u00b2)"
    ) +
    scale_x_continuous(breaks = (year_breaks - first_year) * 12 + 1, labels = year_breaks,
                       expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = paste0("Antarctic sea ice, month by month since ", first_year),
         x = NULL, y = NULL, caption = SOURCE_NSIDC) +
    theme_slide() +
    theme(
      panel.grid.major    = element_blank(),
      panel.grid.minor    = element_blank(),
      axis.text.y         = element_blank(),
      axis.ticks.x        = element_line(color = COLORS$subtitle, linewidth = 0.6),
      axis.ticks.length.x = unit(6, "pt"),
      legend.position     = "bottom",
      legend.justification = "left",
      legend.title.position = "top",
      legend.key.width    = unit(1.3, "in"),
      legend.key.height   = unit(0.28, "in"),
      legend.margin       = margin(t = 10),
      legend.title        = element_text(size = TXT$legend_title, face = "plain",
                                         color = COLORS$subtitle, margin = margin(b = 6))
    )

  save_slide(p, name, out_dir)
  invisible(structure(p, limits = c(-lim, lim), latest = last_month))
}

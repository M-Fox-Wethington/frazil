# --- Sector figures -----------------------------------------------------------

#' Figure: one sector's ice extent compared with its 1981-2010 average
#'
#' From NSIDC's sector extents. Each thin bar is one month's departure from the
#' 1981-2010 mean for that month, warm for less ice and blue for more, as in
#' the month-by-month stripes. The dark line is the 12-month running mean, and
#' the dashed line is the straight-line trend of the yearly means over
#' `trend_years`. Yearly means are built from the monthly departures, so the
#' two missing months of 1987-88 do not bias them.
#'
#' @param sector A sector key from [nsidc_sectors].
#' @param date The last month to draw, as any date in it, or `NULL` for the
#'   last month in the workbook.
#' @param trend_years First and last year of the trend fit.
#' @inheritParams fig_extent_daily
#' @return The ggplot object, invisibly, with attribute `trend` (slope and 95%
#'   range in million km2 per decade, and as a percentage of the baseline
#'   mean).
#' @export
fig_sector_extent <- function(sector = "Weddell", date = NULL, trend_years = c(1979, 2025),
                              out_dir = frazil_paths()$output,
                              name = paste0(tolower(sector), "_extent"),
                              cache = frazil_paths()$cache) {
  r   <- sector_row(sector)
  key <- sector
  end_date <- if (is.null(date)) NULL else as.Date(format(as.Date(date), "%Y-%m-01"))
  sector_m <- read_sector_monthly(cache)
  if (!is.null(end_date)) sector_m <- sector_m[sector_m$date <= end_date, ]
  m  <- add_departures(sector_m) |> filter(sector == key, year >= trend_years[1])
  yr <- yearly_sector(sector_m) |>
    filter(sector == key, year >= trend_years[1], year <= trend_years[2])

  # 12-month running mean; a window that touches a missing month stays empty.
  m <- m |> arrange(date)
  full <- data.frame(date = seq(min(m$date), max(m$date), by = "month")) |>
    left_join(m |> select(date, dep), by = "date")
  full$run12 <- as.numeric(stats::filter(full$dep, rep(1 / 12, 12), sides = 2))

  fit   <- lm(dep ~ year, data = yr)
  slope <- 10 * coef(fit)[["year"]]
  ci    <- 10 * confint(fit)["year", ]
  pct   <- 100 * slope / yr$clim[1]
  message(sprintf("%s 1981-2010 yearly mean extent %.2f million km2", r$name, yr$clim[1]))
  message(sprintf(paste("Trend %d-%d: %+.3f million km2 per decade (95%% range %+.3f to",
                        "%+.3f), %+.1f%% per decade"),
                  trend_years[1], trend_years[2], slope, ci[1], ci[2], pct))
  trend <- data.frame(year = trend_years) |>
    mutate(date = as.Date(sprintf("%d-07-01", year)),
           fit  = predict(fit, newdata = data.frame(year = year)))
  lowest <- m |> slice_min(dep, n = 1)
  message(sprintf("Largest monthly shortfall: %s, %.2f million km2",
                  format(lowest$date, "%B %Y"), lowest$dep))

  lim <- ceiling(max(abs(m$dep)) * 2) / 2
  # The key sits in a band above the tallest bar, so it never crosses the data.
  key_y <- lim + c(0.50, 0.32, 0.14)
  key_x <- as.Date(sprintf("%d-03-01", trend_years[1]))
  x_breaks <- seq(as.Date(sprintf("%d-01-01", ceiling(trend_years[1] / 5) * 5)),
                  as.Date(sprintf("%d-01-01", trend_years[2])), by = "5 years")
  p <- ggplot() +
    geom_col(data = m, aes(date, dep, fill = dep < 0), width = 25, show.legend = FALSE) +
    scale_fill_manual(values = c(`TRUE` = COLORS$orange, `FALSE` = COLORS$print_blue)) +
    geom_hline(yintercept = 0, color = COLORS$subtitle, linewidth = 0.6) +
    geom_line(data = full, aes(date, run12), color = COLORS$title, linewidth = 1.3,
              na.rm = TRUE) +
    geom_line(data = trend, aes(date, fit), color = COLORS$red, linewidth = 1.2,
              linetype = "22") +
    annotate("text", x = key_x, y = key_y[1], hjust = 0, vjust = 0.5,
             label = "Bars: each month's difference from its 1981\u20132010 average",
             size = pt(TXT$small), color = COLORS$subtitle) +
    annotate("text", x = key_x, y = key_y[2], hjust = 0, vjust = 0.5,
             label = "Dark line: 12-month average", size = pt(TXT$small),
             color = COLORS$title, fontface = "bold") +
    annotate("text", x = key_x, y = key_y[3], hjust = 0, vjust = 0.5,
             label = sprintf("Dashed line: trend, %d\u2013%d, %s million km\u00b2 per decade",
                             trend_years[1], trend_years[2], minus(sprintf("%+.2f", slope))),
             size = pt(TXT$small), color = COLORS$red, fontface = "bold") +
    scale_x_date(breaks = x_breaks, date_labels = "%Y", expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(-lim, lim + 0.6), breaks = seq(-lim, lim, by = 0.5),
                       labels = function(x) minus(format(x)),
                       name = "Difference from average (million km\u00b2)") +
    labs(title = paste0(r$name, " ice compared with its 1981\u20132010 average"),
         x = NULL,
         caption = paste0(SOURCE_NSIDC, ", ", r$name, " sector (", sector_bounds_text(sector), ")")) +
    theme_slide() +
    theme(panel.grid.major.x = element_blank())

  save_slide(p, name, out_dir)
  invisible(structure(p, trend = c(slope = slope, lo = ci[[1]], hi = ci[[2]], pct = pct,
                                   clim = yr$clim[1])))
}

#' Figure: the trend in each sector
#'
#' Straight-line trend of each sector's yearly mean extent over `years`, as a
#' percentage of the sector's 1981-2010 mean per decade, so sectors of very
#' different size can be compared. The thin lines show each trend's 95% range;
#' a range that crosses zero means the direction of change is not clear from
#' this record. The whole-Antarctic total is shown for reference, and one
#' sector is highlighted in the focal orange. Sectors run east from the Weddell
#' Sea, as around the continent.
#'
#' @param years First and last year of the fit.
#' @param highlight The sector drawn in orange, or `NULL`.
#' @inheritParams fig_extent_daily
#' @return The ggplot object, invisibly, with attribute `trends`, the table
#'   from [sector_trends()].
#' @export
fig_sector_trends <- function(years = c(1979, 2025), highlight = "Weddell",
                              out_dir = frazil_paths()$output, name = "sector_trends",
                              cache = frazil_paths()$cache) {
  hl <- if (is.null(highlight)) "" else highlight
  trends <- sector_trends(read_sector_monthly(cache), years = years) |>
    mutate(role = case_when(sector == "Total" ~ "total",
                            sector == hl ~ "highlight",
                            TRUE ~ "other"))
  message(paste(utils::capture.output(print(as.data.frame(
    trends |> select(sector, slope, lo, hi, clim, pct, pct_lo, pct_hi)), digits = 3)),
    collapse = "\n"))

  fmt_pct <- function(x) minus(sprintf("%+.1f%%", x))
  p <- ggplot(trends, aes(label, pct)) +
    geom_hline(yintercept = 0, color = COLORS$subtitle, linewidth = 0.6) +
    geom_col(aes(fill = role), width = 0.62, show.legend = FALSE) +
    geom_errorbar(aes(ymin = pct_lo, ymax = pct_hi), width = 0.14, linewidth = 0.7,
                  color = COLORS$title) +
    geom_text(aes(y = ifelse(pct >= 0, pct_hi, pct_lo), label = fmt_pct(pct),
                  vjust = ifelse(pct >= 0, -0.6, 1.5)),
              size = pt(TXT$label), fontface = "bold", color = COLORS$title) +
    scale_fill_manual(values = c(highlight = COLORS$orange, other = COLORS$slate,
                                 total = COLORS$aluminum)) +
    scale_y_continuous(labels = function(x) minus(paste0(x, "%")),
                       expand = expansion(mult = c(0.18, 0.18)),
                       name = "Change in yearly average extent,\nper decade") +
    labs(title = sprintf("Change in sea ice extent by sector, %d to %d", years[1], years[2]),
         x = NULL, caption = paste0(SOURCE_NSIDC, ", sector extents")) +
    theme_slide() +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size = TXT$axis_text, color = COLORS$title,
                                     lineheight = 0.95))

  save_slide(p, name, out_dir)
  invisible(structure(p, trends = trends))
}

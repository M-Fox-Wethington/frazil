# --- Sector extents -----------------------------------------------------------

#' NSIDC's five Antarctic sectors
#'
#' NSIDC divides Antarctic sea ice into five sectors by longitude. The Weddell
#' Sea runs from 60W to 20E, the Indian Ocean from 20E to 90E, the Western
#' Pacific from 90E to 160E, the Ross Sea from 160E to 130W, and the
#' Bellingshausen-Amundsen Seas from 130W to 60W.
#'
#' @format A data frame with five rows: `sector`, the key used in NSIDC's
#'   regional workbook and throughout this package; `name`; `label`, the name
#'   broken over two lines for an axis; and `west` and `east`, the bounds in
#'   degrees of longitude with east positive.
#' @export
nsidc_sectors <- data.frame(
  sector = c("Weddell", "Indian", "Pacific", "Ross", "Bell-Amundsen"),
  name   = c("Weddell Sea", "Indian Ocean", "Western Pacific", "Ross Sea",
             "Bellingshausen-Amundsen Seas"),
  label  = c("Weddell Sea", "Indian Ocean", "Western Pacific", "Ross Sea",
             "Bellingshausen-\nAmundsen Seas"),
  west   = c(-60, 20, 90, 160, -130),
  east   = c(20, 90, 160, -130, -60),
  stringsAsFactors = FALSE
)

sector_row <- function(sector) {
  i <- match(sector, nsidc_sectors$sector)
  if (is.na(i)) {
    stop("Unknown sector '", sector, "'. Use one of: ",
         paste(nsidc_sectors$sector, collapse = ", "), call. = FALSE)
  }
  nsidc_sectors[i, ]
}

# "60\u00b0W"
fmt_lon <- function(lon) sprintf("%g\u00b0%s", abs(lon), ifelse(lon < 0, "W", "E"))

# "60\u00b0W to 20\u00b0E"
sector_bounds_text <- function(sector) {
  r <- sector_row(sector)
  sprintf("%s to %s", fmt_lon(r$west), fmt_lon(r$east))
}

#' Monthly mean extent by sector
#'
#' Reads NSIDC's regional workbook
#' `S_Sea_Ice_Index_Regional_Monthly_Data_G02135_v4.0.xlsx`, which has one
#' sheet per sector and measure with a row per year and an extent column and a
#' rank column per month.
#'
#' @inheritParams read_daily_extent
#' @return A data frame with `sector`, `label`, `year`, `month`, `extent`
#'   (million km2) and `date`, sorted by sector and date.
#' @export
read_sector_monthly <- function(cache = frazil_paths()$cache) {
  f <- cache_file(cache, "seaice_analysis/S_Sea_Ice_Index_Regional_Monthly_Data_G02135_v4.0.xlsx")
  lapply(seq_len(nrow(nsidc_sectors)), function(i) {
    x <- suppressMessages(readxl::read_excel(
      f, sheet = paste0(nsidc_sectors$sector[i], "-Extent-km^2"),
      col_names = FALSE, col_types = "text"))
    year <- suppressWarnings(as.integer(x[[1]]))
    rows <- which(!is.na(year))
    ext  <- sapply(1:12, function(m) suppressWarnings(as.numeric(x[[2 * m]][rows])))
    data.frame(sector = nsidc_sectors$sector[i], label = nsidc_sectors$label[i],
               year = rep(year[rows], 12), month = rep(1:12, each = length(rows)),
               extent = as.vector(ext) / 1e6)
  }) |>
    bind_rows() |>
    filter(!is.na(extent), extent > 0) |>
    mutate(date = as.Date(sprintf("%04d-%02d-01", year, month))) |>
    arrange(sector, date)
}

#' Sector departures from the baseline mean
#'
#' Adds each month's departure from the baseline mean for that sector and
#' calendar month.
#'
#' @param m The table from [read_sector_monthly()].
#' @param baseline First and last year of the baseline.
#' @return `m` with `base` and `dep` columns.
#' @export
add_departures <- function(m, baseline = c(1981, 2010)) {
  base <- m |>
    filter(year >= baseline[1], year <= baseline[2]) |>
    group_by(sector, month) |>
    summarise(base = mean(extent), .groups = "drop")
  m |> left_join(base, by = c("sector", "month")) |> mutate(dep = extent - base)
}

#' Yearly mean extent by sector
#'
#' The yearly mean is built from the yearly mean of the monthly departures, so
#' a year with a missing month is not biased by the seasonal cycle.
#'
#' @inheritParams add_departures
#' @return A data frame with `sector`, `label`, `year`, `n_months`, `dep`,
#'   `clim` (the baseline yearly mean) and `extent`.
#' @export
yearly_sector <- function(m, baseline = c(1981, 2010)) {
  m <- add_departures(m, baseline)
  clim <- m |> distinct(sector, month, base) |> group_by(sector) |>
    summarise(clim = mean(base), .groups = "drop")
  m |> group_by(sector, label, year) |>
    summarise(n_months = n(), dep = mean(dep), .groups = "drop") |>
    left_join(clim, by = "sector") |>
    mutate(extent = clim + dep)
}

#' Straight-line trends in yearly sector extent
#'
#' Ordinary least squares fits of each sector's yearly mean extent over
#' `years`, with 95% ranges from the fit, as million km2 per decade and as a
#' percentage of the sector's baseline mean per decade. The whole-Antarctic
#' total, the sum of the five sectors in months when all five have a value, is
#' added as sector `"Total"`.
#'
#' @param m The table from [read_sector_monthly()].
#' @param years First and last year of the fit.
#' @param total Add the whole-Antarctic total.
#' @param baseline First and last year of the baseline.
#' @return A data frame with one row per sector: `sector`, `label`, `slope`,
#'   `lo`, `hi` (million km2 per decade), `clim` and `pct`, `pct_lo`, `pct_hi`
#'   (percent of the baseline mean per decade).
#' @export
sector_trends <- function(m = read_sector_monthly(), years = c(1979, 2025), total = TRUE,
                          baseline = c(1981, 2010)) {
  if (total) {
    total_m <- m |> group_by(year, month, date) |>
      summarise(n_months = n(), extent = sum(extent), .groups = "drop") |>
      filter(n_months == nrow(nsidc_sectors)) |>
      mutate(sector = "Total", label = "All of\nAntarctica") |>
      select(-n_months)
    m <- bind_rows(m, total_m)
  }
  yr <- yearly_sector(m, baseline) |> filter(year >= years[1], year <= years[2])
  yr |>
    group_by(sector, label) |>
    group_modify(~ {
      f  <- lm(dep ~ year, data = .x)
      ci <- confint(f)["year", ]
      data.frame(slope = 10 * coef(f)[["year"]], lo = 10 * ci[1], hi = 10 * ci[2],
                 clim = .x$clim[1])
    }) |>
    ungroup() |>
    mutate(pct = 100 * slope / clim, pct_lo = 100 * lo / clim, pct_hi = 100 * hi / clim,
           label = factor(label, levels = c(nsidc_sectors$label, "All of\nAntarctica"))) |>
    arrange(label)
}

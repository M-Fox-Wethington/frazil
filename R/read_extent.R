# --- Extent tables ------------------------------------------------------------

#' Daily sea ice extent
#'
#' Reads `S_seaice_extent_daily_v4.0.csv` from the cache: the Southern
#' Hemisphere extent in million km2 for every day from 26 October 1978. The
#' record before August 1987 has a value every other day, and there is a gap
#' from early December 1987 to mid-January 1988.
#'
#' @param cache The folder [nsidc_download()] filled.
#' @return A data frame with `date`, `year`, `extent` (million km2), `missing`
#'   and `source`, sorted by date.
#' @export
read_daily_extent <- function(cache = frazil_paths()$cache) {
  f <- cache_file(cache, "daily/data/S_seaice_extent_daily_v4.0.csv")
  readr::read_csv(f, skip = 2,
                  col_names = c("year", "month", "day", "extent", "missing", "source"),
                  col_types = "iiiddc", trim_ws = TRUE, progress = FALSE) |>
    filter(!is.na(extent)) |>
    mutate(date = as.Date(sprintf("%04d-%02d-%02d", year, month, day))) |>
    select(date, year, extent, missing, source) |>
    arrange(date)
}

#' Five-day trailing mean of daily extent
#'
#' The smoothing NSIDC uses when it reports daily extent and announces the
#' annual maximum and minimum. Each value averages the days with data in the
#' five calendar days ending on that date, so an every-other-day stretch
#' averages two or three values and nothing is carried across a gap.
#'
#' @param d The table from [read_daily_extent()], or any data frame with `date`
#'   and `extent` columns sorted by date.
#' @param k Window length in days.
#' @return `d` with an `extent5` column.
#' @export
add_trailing_mean <- function(d, k = 5) {
  t  <- as.numeric(d$date)
  cs <- c(0, cumsum(d$extent))
  lo <- findInterval(t - k + 0.5, t) + 1
  i  <- seq_along(t)
  d$extent5 <- (cs[i + 1] - cs[lo]) / (i - lo + 1)
  d
}

#' Split each year's line at gaps in the record
#'
#' Adds a `line_id` that changes wherever the record skips more than `max_gap`
#' days, so a gap is drawn as a gap.
#'
#' @param d A data frame with `date` and `year`.
#' @param max_gap Longest gap in days drawn as a continuous line.
#' @return `d` with `piece` and `line_id` columns.
#' @export
add_line_groups <- function(d, max_gap = 3) {
  d |>
    arrange(date) |>
    group_by(year) |>
    mutate(piece = cumsum(c(TRUE, diff(as.numeric(date)) > max_gap))) |>
    ungroup() |>
    mutate(line_id = paste(year, piece, sep = "_"))
}

#' Day-of-year climatology of daily extent, 1981 to 2010
#'
#' Reads `S_seaice_extent_climatology_1981-2010_v4.0.csv`: for each day of the
#' year, the mean, standard deviation and the 10th, 25th, 50th, 75th and 90th
#' percentiles of extent over 1981 to 2010.
#'
#' @inheritParams read_daily_extent
#' @return A data frame with `doy`, `mean`, `sd`, `p10`, `p25`, `p50`, `p75`
#'   and `p90`.
#' @export
read_climatology <- function(cache = frazil_paths()$cache) {
  f <- cache_file(cache, "daily/data/S_seaice_extent_climatology_1981-2010_v4.0.csv")
  readr::read_csv(f, skip = 2,
                  col_names = c("doy", "mean", "sd", "p10", "p25", "p50", "p75", "p90"),
                  col_types = "iddddddd", trim_ws = TRUE, progress = FALSE)
}

#' Monthly mean sea ice extent
#'
#' Reads the twelve `S_MM_extent_v4.0.csv` files, one row per month since
#' November 1978. NSIDC marks missing months (December 1987 and January 1988)
#' with -9999, which become `NA`.
#'
#' @inheritParams read_daily_extent
#' @return A data frame with `year`, `month`, `source`, `region`, `extent`,
#'   `area` and `date` (the first of the month), sorted by date.
#' @export
read_monthly_extent <- function(cache = frazil_paths()$cache) {
  files <- vapply(sprintf("monthly/data/S_%02d_extent_v4.0.csv", 1:12),
                  function(rel) cache_file(cache, rel), character(1))
  lapply(files, \(f) readr::read_csv(
    f, skip = 1, col_names = c("year", "month", "source", "region", "extent", "area"),
    col_types = "iiccdd", trim_ws = TRUE, progress = FALSE)) |>
    bind_rows() |>
    mutate(extent = if_else(extent < 0, NA_real_, extent),
           area   = if_else(area < 0, NA_real_, area),
           date   = as.Date(sprintf("%04d-%02d-01", year, month))) |>
    arrange(date)
}

#' Monthly departures from the 1981 to 2010 mean
#'
#' Adds each month's departure from the mean extent for the same calendar
#' month over the baseline years.
#'
#' @param m The table from [read_monthly_extent()].
#' @param baseline First and last year of the baseline.
#' @return `m` with `base` and `anom` columns (million km2).
#' @export
monthly_departures <- function(m, baseline = c(1981, 2010)) {
  base <- m |>
    filter(year >= baseline[1], year <= baseline[2]) |>
    group_by(month) |>
    summarise(base = mean(extent, na.rm = TRUE), .groups = "drop")
  m |> left_join(base, by = "month") |> mutate(anom = extent - base)
}

# A cached NSIDC file, with a message that says how to get it when it is absent.
cache_file <- function(cache, rel) {
  f <- file.path(cache, rel)
  if (!file.exists(f)) {
    stop(sprintf("%s is not in the cache at %s. Run nsidc_download() first.",
                 basename(rel), cache), call. = FALSE)
  }
  f
}

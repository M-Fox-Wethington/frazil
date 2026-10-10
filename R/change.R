# --- Change between two periods ----------------------------------------------

#' Change in monthly ice cover between two periods
#'
#' For each 25 km cell, the mean monthly ice cover over the `recent` years
#' minus the mean over the `early` years, in percentage points, from NSIDC's
#' monthly mean grids. Flags (coast, land, missing) are left out of the means,
#' and cells with under `min_cover` percent ice in both periods become `NA`,
#' so they are drawn as open water.
#'
#' Needs every monthly grid for the two periods: run
#' `nsidc_download(monthly_all = TRUE)` first.
#'
#' @param month Calendar month, 1 to 12.
#' @param early,recent The years of the two periods.
#' @param files The table from [monthly_conc_files()].
#' @param min_cover Cover in percent below which a cell is open water.
#' @param cache The folder [nsidc_download()] filled.
#' @return A list with `change`, a SpatRaster on the grid, and `raw`, a
#'   reference grid for [change_summary()].
#' @export
cover_change <- function(month, early = 1979:1988, recent = 2016:2025,
                         files = monthly_conc_files(cache), min_cover = 10,
                         cache = frazil_paths()$cache) {
  mean_cover <- function(years) {
    p <- files$path[files$month == month & files$year %in% years]
    if (length(p) < length(years)) {
      stop(sprintf(paste("Only %d of %d monthly grids for %s %d-%d are in the cache.",
                         "Run nsidc_download(monthly_all = TRUE). NSIDC has no grid",
                         "for December 1987 or January 1988."),
                   length(p), length(years), month.name[month], min(years), max(years)),
           call. = FALSE)
    }
    v <- sapply(p, function(f) {
      x <- terra::values(read_conc_raw(f), mat = FALSE)
      x[x > 1000] <- NA
      x / 10
    })
    out <- read_conc_raw(p[1])
    terra::values(out) <- rowMeans(v, na.rm = TRUE)
    out
  }
  e <- mean_cover(early)
  r <- mean_cover(recent)
  d <- r - e
  d <- terra::ifel(e < min_cover & r < min_cover, NA, d)
  names(d) <- "change"
  list(change = d, raw = read_conc_raw(files$path[files$month == month & files$year %in% recent][1]))
}

# Adds the ice year to a table of daily grids. An ice year runs from 1 March,
# around the summer low, to the end of the following February.
ice_year_of <- function(dates) {
  as.integer(format(dates, "%Y")) - (as.integer(format(dates, "%m")) < 3)
}

# Days with at least 15% ice in one ice year, per cell, scaled from the days
# that have a grid to the length of the year. NULL when the year's longest gap
# is over `max_gap` days.
ice_days_one <- function(files, iy, max_gap = 10) {
  f <- files[ice_year_of(files$date) == iy, ]
  start <- as.Date(sprintf("%d-03-01", iy))
  end   <- as.Date(sprintf("%d-03-01", iy + 1))
  n_days  <- as.numeric(end - start)
  max_gap_seen <- max(diff(c(start - 1, sort(f$date), end))) - 1
  if (nrow(f) == 0 || max_gap_seen > max_gap) {
    message(sprintf("  ice year %d-%02d left out: %d grids, longest gap %d days",
                    iy, (iy + 1) %% 100, nrow(f), max_gap_seen))
    return(NULL)
  }
  cnt <- integer(0)
  for (p in f$path) {
    v <- terra::values(read_conc_raw(p), mat = FALSE)
    ice <- as.integer(v >= 150 & v <= 1000)
    cnt <- if (length(cnt)) cnt + ice else ice
  }
  cnt / nrow(f) * n_days
}

#' Change in the number of days a year with sea ice
#'
#' For each 25 km cell, the mean number of days per ice year with at least 15%
#' ice cover over the `recent` ice years minus the mean over the `early` ice
#' years. An ice year runs from 1 March to the end of the following February.
#' The early record has a grid every other day, so each year's count is scaled
#' from the days that have a grid to the length of the year. A year with a gap
#' longer than `max_gap` days (1987-88, which loses 3 December to 12 January)
#' is left out of its period. Cells with fewer than `min_days` ice days a year
#' in both periods become `NA`.
#'
#' The per-period means are cached in the terrain cache as
#' `seaice_icedays_{first}_{last}.rds`, so a rerun skips the count. Needs the
#' daily grids for every year of both periods: run
#' `nsidc_download(daily_years = c(early, recent + 1))` first (the last ice
#' year runs into the following calendar year).
#'
#' @param early,recent The ice years of the two periods, named by the calendar
#'   year in which each begins.
#' @param files The table from [daily_conc_files()].
#' @param max_gap Longest gap in days a year may have and still count.
#' @param min_days Ice days per year below which a cell is open water.
#' @param terrain_cache Folder for the cached per-period counts.
#' @param cache The folder [nsidc_download()] filled.
#' @return A list with `change`, a SpatRaster on the grid; `raw`, a reference
#'   grid; and `early` and `recent`, the per-period mean counts per cell.
#' @export
season_change <- function(early = 1979:1988, recent = 2016:2025,
                          files = daily_conc_files(cache), max_gap = 10, min_days = 5,
                          terrain_cache = frazil_paths()$terrain_cache,
                          cache = frazil_paths()$cache) {
  period_mean <- function(years) {
    dir.create(terrain_cache, recursive = TRUE, showWarnings = FALSE)
    cached <- file.path(terrain_cache,
                        sprintf("seaice_icedays_%d_%d.rds", min(years), max(years)))
    if (file.exists(cached)) return(readRDS(cached))
    d <- lapply(years, function(iy) ice_days_one(files, iy, max_gap))
    keep <- !vapply(d, is.null, logical(1))
    if (!any(keep)) stop("No usable ice years in ", min(years), "-", max(years), call. = FALSE)
    message(sprintf("  %d-%d: %d ice years used", min(years), max(years), sum(keep)))
    out <- rowMeans(do.call(cbind, d[keep]))
    saveRDS(out, cached)
    out
  }
  message("Counting ice days ...")
  e <- period_mean(early)
  r <- period_mean(recent)

  raw_ref <- read_conc_raw(tail(files$path, 1))
  flags   <- terra::values(raw_ref, mat = FALSE) > 1000
  change  <- r - e
  change[(e < min_days & r < min_days) | flags] <- NA
  change_r <- terra::setValues(terra::rast(raw_ref), change)
  names(change_r) <- "change"
  list(change = change_r, raw = raw_ref, early = e, recent = r)
}

#' Summarize a change raster over a sector and overall
#'
#' @param change A SpatRaster on the grid, from [cover_change()] or
#'   [season_change()].
#' @param raw A reference grid, for the cell longitudes.
#' @param sector An NSIDC sector key.
#' @param threshold Change below which a cell counts as a large loss.
#' @return A list with the sector and overall mean change, the number of cells
#'   with a value, and the number of cells below `threshold`.
#' @export
change_summary <- function(change, raw, sector = "Weddell", threshold = -30) {
  v <- terra::values(change, mat = FALSE)
  s <- in_sector(raw, sector)
  list(sector = sector,
       sector_mean = mean(v[s], na.rm = TRUE), sector_cells = sum(!is.na(v[s])),
       all_mean = mean(v, na.rm = TRUE), all_cells = sum(!is.na(v)),
       sector_lost = sum(v[s] < threshold, na.rm = TRUE),
       all_lost = sum(v < threshold, na.rm = TRUE))
}

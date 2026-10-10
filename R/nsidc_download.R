# --- Download the NSIDC Sea Ice Index -----------------------------------------

NSIDC_BASE <- "https://noaadata.apps.nsidc.org/NOAA/G02135"

# NSIDC serves plain index pages, one entry per line:
#   <a href="NAME">NAME</a>     07-Oct-2026 10:27     606437
# The time is parsed by hand so the result does not depend on the locale.
parse_listing_time <- function(x) {
  d <- sub("^(\\d{2})-([A-Za-z]{3})-(\\d{4}) (\\d{2}):(\\d{2})$",
           "\\3-\\2-\\1 \\4:\\5", x)
  for (i in seq_along(month.abb)) {
    d <- sub(paste0("-", month.abb[i], "-"), sprintf("-%02d-", i), d, fixed = TRUE)
  }
  as.POSIXct(d, format = "%Y-%m-%d %H:%M", tz = "UTC")
}

parse_listing <- function(html, rel_dir) {
  rx <- '<a href="([^"]+)">[^<]*</a>\\s+(\\d{2}-[A-Za-z]{3}-\\d{4} \\d{2}:\\d{2})\\s+(\\S+)'
  m <- regmatches(html, regexec(rx, html))
  m <- m[lengths(m) == 4]
  data.frame(
    rel_dir  = rep_len(rel_dir, length(m)),
    name     = vapply(m, `[`, "", 2),
    modified = parse_listing_time(vapply(m, `[`, "", 3)),
    size     = suppressWarnings(as.numeric(vapply(m, `[`, "", 4))),
    stringsAsFactors = FALSE
  )
}

list_dir <- function(rel_dir, base_url = NSIDC_BASE) {
  html <- readLines(paste0(base_url, "/", rel_dir), warn = FALSE)
  parse_listing(html, rel_dir)
}

files_in <- function(rel_dir, pattern, base_url = NSIDC_BASE) {
  x <- list_dir(rel_dir, base_url)
  x[!grepl("/$", x$name) & grepl(pattern, x$name), ]
}

month_dir <- function(m) sprintf("%02d_%s", m, month.abb[m])

#' Mirror the NSIDC Sea Ice Index, Version 4, for the Southern Hemisphere
#'
#' Copies the files the package reads into `cache`, keeping NSIDC's folder
#' layout and file names, so every figure rebuilds from disk with no internet
#' connection. A file is fetched only when it is missing locally, or when
#' NSIDC's listing shows a newer timestamp or a different size. Downloads run
#' in parallel through curl, in batches, because each file being written holds
#' an R connection and R allows only 128 at a time.
#'
#' @details
#' Always fetched:
#' * `daily/data/`: daily extent and the 1981-2010 daily percentiles.
#' * `monthly/data/`: monthly extent, one CSV per calendar month.
#' * `monthly/shapefiles/shp_median/`: the 1981-2010 median ice edge, one zip
#'   per month.
#' * `seaice_analysis/`: NSIDC's sector extents, daily and monthly.
#' * `daily/geotiff/YYYY/MM_Mon/`: the daily concentration grids for the last
#'   `days_back` days.
#' * `monthly/geotiff/MM_Mon/`: the monthly concentration grids for the last 12
#'   complete months.
#'
#' The data set is at `https://noaadata.apps.nsidc.org/NOAA/G02135/`, served
#' without a login. With no options, a rerun costs only the new daily grids
#' (about 0.6 MB each) and the small files NSIDC rewrites every day.
#'
#' @param cache Folder to mirror into.
#' @param days_back How many days of daily grids to keep current, counted back
#'   from the latest grid NSIDC has. 366 covers the twelve-month animation.
#' @param monthly_all Fetch every monthly concentration grid since 1978 (about
#'   350 MB), which [cover_change()] needs.
#' @param daily_years Calendar years whose every daily grid is fetched (about
#'   0.6 MB per day), which [season_change()] needs. `NULL` fetches none.
#' @param batch Files per curl batch.
#' @param base_url The NSIDC folder.
#' @return A data frame of the files that were fetched or that failed, with
#'   `dest`, `url`, `size` and `ok`, invisibly. A log of the run is written to
#'   `download_log.txt` in the cache.
#' @examples
#' \dontrun{
#' nsidc_download()
#' nsidc_download(monthly_all = TRUE, daily_years = c(1979:1989, 2016:2026))
#' }
#' @export
nsidc_download <- function(cache = frazil_paths()$cache, days_back = 366, monthly_all = FALSE,
                           daily_years = NULL, batch = 100, base_url = NSIDC_BASE) {
  old <- options(timeout = 300)
  on.exit(options(old), add = TRUE)
  daily_years <- as.integer(daily_years)

  message("Reading NSIDC folder listings ...")
  wanted <- rbind(
    files_in("south/daily/data/", "\\.csv$", base_url),
    files_in("south/monthly/data/", "\\.csv$", base_url),
    files_in("south/monthly/shapefiles/shp_median/", "\\.zip$", base_url),
    files_in("seaice_analysis/", "^S_Sea_Ice_Index_Regional_.*\\.xlsx$", base_url)
  )

  # Latest daily grid, then the daily window back from it
  years  <- list_dir("south/daily/geotiff/", base_url)
  years  <- sort(as.integer(sub("/$", "", years$name[grepl("^\\d{4}/$", years$name)])))
  months <- list_dir(sprintf("south/daily/geotiff/%d/", max(years)), base_url)
  months <- sort(months$name[grepl("^\\d{2}_[A-Za-z]{3}/$", months$name)])
  latest_files <- files_in(sprintf("south/daily/geotiff/%d/%s", max(years), tail(months, 1)),
                           "^S_\\d{8}_concentration_v4\\.0\\.tif$", base_url)
  latest_day <- max(as.Date(substr(latest_files$name, 3, 10), "%Y%m%d"))
  first_day  <- latest_day - days_back + 1
  message("Latest daily grid on NSIDC: ", format(latest_day))

  daily_months <- unique(format(seq(first_day, latest_day, by = "day"), "%Y-%m"))
  extra_months <- if (length(daily_years)) {
    as.vector(outer(sprintf("%04d", intersect(daily_years, years)), sprintf("%02d", 1:12),
                    paste, sep = "-"))
  } else character(0)
  for (ym in sort(unique(c(daily_months, extra_months)))) {
    y <- as.integer(substr(ym, 1, 4)); m <- as.integer(substr(ym, 6, 7))
    if (as.Date(sprintf("%s-01", ym)) > latest_day) next
    x <- tryCatch(files_in(sprintf("south/daily/geotiff/%d/%s/", y, month_dir(m)),
                           "^S_\\d{8}_concentration_v4\\.0\\.tif$", base_url),
                  error = function(e) NULL)
    if (is.null(x) || !nrow(x)) next
    d <- as.Date(substr(x$name, 3, 10), "%Y%m%d")
    keep <- (d >= first_day & d <= latest_day) |
      (as.integer(format(d, "%Y")) %in% daily_years)
    wanted <- rbind(wanted, x[keep, ])
  }

  # Monthly grids: the last 12 complete months, or every month
  last_full <- as.Date(format(latest_day, "%Y-%m-01")) - 1
  recent12  <- format(seq(as.Date(format(last_full, "%Y-%m-01")), by = "-1 month",
                          length.out = 12), "%Y%m")
  for (m in 1:12) {
    x <- files_in(sprintf("south/monthly/geotiff/%s/", month_dir(m)),
                  "^S_\\d{6}_concentration_v4\\.0\\.tif$", base_url)
    if (!monthly_all) x <- x[substr(x$name, 3, 8) %in% recent12, ]
    wanted <- rbind(wanted, x)
  }

  # Local paths drop the hemisphere folder, so the cache keeps the layout the
  # readers expect (daily/..., monthly/..., seaice_analysis/...).
  wanted$dest <- file.path(cache, sub("^south/", "", wanted$rel_dir), wanted$name)
  wanted$url  <- paste0(base_url, "/", wanted$rel_dir, wanted$name)
  have  <- file.exists(wanted$dest)
  stale <- !have
  stale[have] <- (file.size(wanted$dest[have]) != wanted$size[have] &
                    !is.na(wanted$size[have])) |
                 (file.mtime(wanted$dest[have]) < wanted$modified[have])
  todo <- wanted[stale, ]
  message(sprintf("%d files wanted, %d already current, %d to fetch (%.0f MB)",
                  nrow(wanted), sum(!stale), nrow(todo), sum(todo$size, na.rm = TRUE) / 1e6))

  todo$ok <- logical(nrow(todo))
  if (nrow(todo)) {
    for (d in unique(dirname(todo$dest))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    part <- paste0(todo$dest, ".part")
    ok <- logical(nrow(todo))
    for (b in split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / batch))) {
      res <- curl::multi_download(todo$url[b], part[b], progress = FALSE, multi_timeout = Inf)
      ok[b] <- res$success & res$status_code == 200
      message(sprintf("  %d / %d files", max(b), nrow(todo)))
    }
    ok[is.na(ok)] <- FALSE
    file.rename(part[ok], todo$dest[ok])
    Sys.setFileTime(todo$dest[ok], todo$modified[ok])
    unlink(part[!ok])
    todo$ok <- ok
    if (any(!ok)) {
      warning("Failed: ", paste(head(basename(todo$dest[!ok]), 10), collapse = ", "),
              if (sum(!ok) > 10) " ...", call. = FALSE)
    }
    message(sprintf("Fetched %d files (%.0f MB), %d failed.", sum(ok),
                    sum(file.size(todo$dest[ok])) / 1e6, sum(!ok)))
  }

  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    paste("source:", base_url),
    paste("checked:", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    paste("latest_daily_grid:", format(latest_day)),
    paste("monthly_all:", monthly_all),
    paste("daily_years:", if (length(daily_years)) paste(range(daily_years), collapse = "-")
                          else "none")
  ), file.path(cache, "download_log.txt"))

  invisible(todo[, c("dest", "url", "size", "ok")])
}

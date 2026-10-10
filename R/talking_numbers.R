#' The numbers behind the talking points
#'
#' Prints, and optionally writes to a file, every number the speaking notes of
#' the shipboard talk cite, recomputed from the cached NSIDC files: the latest
#' day's extent and rank, the records under the AMSR2 rule, the last twelve
#' months, the monthly departures, the ice edge along chosen meridians against
#' its usual position, and the grid value at each site. The sector, trend and
#' change-map numbers are reported by their own figure functions.
#'
#' @param date The day to report on, or `NULL` for the latest in the cache.
#' @param file Path to write the report to as well, or `NULL`.
#' @param sites A data frame with `label`, `lon` and `lat`, or `NULL`.
#' @param meridians Longitudes, east positive, along which the ice edge is
#'   measured against the month's median edge.
#' @param first_year First year of the record used for ranks.
#' @inheritParams record_year
#' @param paths The list from [frazil_paths()].
#' @return The report lines, invisibly.
#' @export
talking_numbers <- function(date = NULL, file = NULL, sites = bri_sites,
                            meridians = c(-75, -70, -65, -50, -45), first_year = 1979,
                            amsr2_start = 2025, offset = 0.15, paths = frazil_paths()) {
  cache <- paths$cache
  end_date <- if (is.null(date)) NULL else as.Date(date)
  lines <- character(0)
  say <- function(...) {
    s <- sprintf(...)
    lines <<- c(lines, s)
    cat(s, "\n", sep = "")
  }

  d <- read_daily_extent(cache) |> add_trailing_mean() |> filter(year >= first_year)
  if (!is.null(end_date)) d <- d[d$date <= end_date, ]
  d <- d |> mutate(doy = as.integer(format(date, "%j")), md = format(date, "%m-%d"))
  clim <- read_climatology(cache)
  last <- tail(d, 1)
  cm   <- clim[clim$doy == min(last$doy, 365), ]

  say("== Latest day: %s", format(last$date))
  say("Extent: single day %.3f, five-day mean %.3f million km2", last$extent, last$extent5)
  say("1981-2010 median for the date %.3f; below it by %.3f (single) / %.3f (five-day)",
      cm$p50, cm$p50 - last$extent, cm$p50 - last$extent5)
  same <- lapply(first_year:last$year, function(y) {
    target <- as.Date(sprintf("%d-%s", y, last$md))
    x <- d |> filter(abs(as.numeric(date - target)) <= 1) |>
      slice_min(abs(as.numeric(date - target)), n = 1, with_ties = FALSE)
    if (nrow(x)) data.frame(year = y, extent5 = x$extent5) else NULL
  }) |> bind_rows() |> arrange(extent5)
  say("Rank on this date: %d of %d years (1 = lowest); lowest five: %s",
      which(same$year == last$year), nrow(same),
      paste(sprintf("%d (%.2f)", same$year[1:5], same$extent5[1:5]), collapse = ", "))
  if (same$year[1] == last$year) {
    verdict <- is_record(same$extent5[1], last$year, same$extent5[2], lowest = TRUE,
                         amsr2_start = amsr2_start, offset = offset)
    say("Margin below the next lowest year (%d): %.3f; offset rule %s", same$year[2],
        same$extent5[2] - same$extent5[1],
        if (verdict == "record") "met, a record" else "not met, a tie")
  }

  ann <- d |> group_by(year) |>
    summarise(max5 = max(extent5), max_date = date[which.max(extent5)],
              min5 = min(extent5), min_date = date[which.min(extent5)], .groups = "drop")
  full <- ann |> filter(year < last$year)
  hi <- full |> slice_max(max5, n = 1)
  lo <- ann |> slice_min(min5, n = 1)
  lomax <- full |> slice_min(max5, n = 1)
  say("\n== Records (five-day means)")
  say("Record high: %d, %.2f on %s", hi$year, hi$max5, hi$max_date)
  say("Record low minimum: %d, %.2f on %s", lo$year, lo$min5, lo$min_date)
  say("Lowest maximum: %d, %.2f on %s", lomax$year, lomax$max5, lomax$max_date)
  say("Under the AMSR2 rule: record high year %d, record low year %d",
      as.integer(record_year(full$max5, full$year, lowest = FALSE, amsr2_start = amsr2_start,
                             offset = offset, quiet = TRUE)),
      as.integer(record_year(ann$min5, ann$year, lowest = TRUE, amsr2_start = amsr2_start,
                             offset = offset, quiet = TRUE)))
  cur <- ann |> filter(year == last$year)
  pre <- ann |> filter(year < amsr2_start)
  say("%d peak so far: %.2f on %s, rank %d of %d (1 = lowest); pre-%d peaks below it plus the offset: %s",
      cur$year, cur$max5, cur$max_date, rank(ann$max5)[ann$year == cur$year], nrow(ann),
      amsr2_start, paste(pre$year[pre$max5 < cur$max5 + offset], collapse = ", "))
  say("%d minimum: %.2f on %s, rank %d of %d", cur$year, cur$min5, cur$min_date,
      rank(ann$min5)[ann$year == cur$year], nrow(ann))
  say("Median curve: low %.2f (day %d), high %.2f (day %d)", min(clim$p50),
      clim$doy[which.min(clim$p50)], max(clim$p50), clim$doy[which.max(clim$p50)])

  win <- d |> filter(date > last$date - 366) |> mutate(doy365 = pmin(doy, 365)) |>
    left_join(clim |> select(doy, p50), by = c("doy365" = "doy"))
  say("\n== Last 12 months (single-day values)")
  say("Smallest %.2f on %s, largest %.2f on %s; %d of %d days below the 1981-2010 median",
      min(win$extent), win$date[which.min(win$extent)], max(win$extent),
      win$date[which.max(win$extent)], sum(win$extent < win$p50), nrow(win))

  m <- read_monthly_extent(cache) |> filter(year >= first_year) |> monthly_departures() |>
    filter(!is.na(anom))
  if (!is.null(end_date)) m <- m[m$date <= end_date, ]
  lastm  <- tail(m, 1)
  since  <- m |> filter(date >= as.Date("2016-09-01"))
  before <- m |> filter(date < as.Date("2016-09-01"))
  say("\n== Monthly departures from the 1981-2010 mean")
  say("Since Sep 2016: %d of %d months below; before: %.0f%% below",
      sum(since$anom < 0), nrow(since), 100 * mean(before$anom < 0))
  say("2008-2015: %d of %d months above",
      sum(m$anom[m$year %in% 2008:2015] > 0), sum(m$year %in% 2008:2015))
  say("Largest shortfall: %s %d, %.2f; largest surplus: %s %d, %+.2f",
      month.name[m$month[which.min(m$anom)]], m$year[which.min(m$anom)], min(m$anom),
      month.name[m$month[which.max(m$anom)]], m$year[which.max(m$anom)], max(m$anom))
  same_m <- m |> filter(month == lastm$month) |> arrange(extent)
  say("Latest month %s %d: %.2f, %+.2f from average, rank %d of %d; pre-%d below it plus the offset: %s",
      month.name[lastm$month], lastm$year, lastm$extent, lastm$anom,
      which(same_m$year == lastm$year), nrow(same_m), amsr2_start,
      paste(same_m$year[same_m$year < amsr2_start & same_m$extent < lastm$extent + offset],
            collapse = ", "))

  if (length(meridians)) {
    say("\n== Ice edge along meridians on the latest day, against the month's usual edge")
    g    <- daily_grid(last$date, cache)
    raw  <- g$raw
    edge <- read_median_edge(as.integer(format(last$date, "%m")), cache)
    crs_g <- sf::st_crs(terra::crs(raw))
    for (lon in meridians) {
      lats <- seq(-69, -55, by = 0.05)
      pts  <- sf::st_transform(sf::st_as_sf(data.frame(lon = lon, lat = lats),
                                            coords = c("lon", "lat"), crs = 4326), crs_g)
      v <- terra::extract(raw, terra::vect(pts))[[2]]
      ice <- v >= 150 & v <= 1000
      mer <- sf::st_transform(sf::st_sfc(sf::st_linestring(cbind(lon, c(-75, -50))),
                                         crs = 4326), crs_g)
      hit <- sf::st_intersection(mer, sf::st_union(edge))
      hit_lat <- if (length(hit) && !sf::st_is_empty(hit))
        min(sf::st_coordinates(sf::st_transform(hit, 4326))[, "Y"]) else NA
      north <- if (any(ice)) max(lats[ice]) else NA
      say("%s: northernmost ice %.2fS, usual edge %.2fS, %+.0f km", fmt_lon(lon), -north,
          -hit_lat, (north - hit_lat) * 111.2)
    }
    if (!is.null(sites)) {
      vals <- terra::extract(raw, terra::vect(sites_sf(sites, crs_g)))[[2]]
      say("\n== Grid value at each site (0-1000 tenths of a percent; 2530 coast cell)")
      say("%s", paste(sprintf("%s: %d", sites$label, vals), collapse = "; "))
    }
  }

  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, file)
    message("Written ", file)
  }
  invisible(lines)
}

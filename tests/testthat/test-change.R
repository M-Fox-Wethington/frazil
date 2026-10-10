small_grid <- function(values) {
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 50000, ymin = 0, ymax = 50000,
                   crs = "EPSG:3412")
  terra::values(r) <- values
  names(r) <- "raw"
  r
}

# Writes one daily grid per date into a cache layout and returns the file table.
write_daily <- function(cache, dates, value_fn) {
  for (i in seq_along(dates)) {
    d <- dates[i]
    dir <- file.path(cache, "daily/geotiff", format(d, "%Y"),
                     sprintf("%02d_%s", as.integer(format(d, "%m")), month.abb[as.integer(format(d, "%m"))]))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    terra::writeRaster(small_grid(value_fn(i)),
                       file.path(dir, sprintf("S_%s_concentration_v4.0.tif", format(d, "%Y%m%d"))),
                       datatype = "INT2U", overwrite = TRUE)
  }
  daily_conc_files(cache)
}

test_that("ice years run from March to February", {
  expect_equal(frazil:::ice_year_of(as.Date(c("2001-03-01", "2002-02-28", "2002-03-01"))),
               c(2001, 2001, 2002))
})

test_that("ice days are scaled from the days with a grid, and a long gap drops the year", {
  cache <- withr::local_tempdir()
  # A grid every ten days through ice year 2001: cell 1 always iced, cell 2
  # never, cell 3 iced on even grids, cell 4 a land flag.
  dates <- seq(as.Date("2001-03-01"), as.Date("2002-02-28"), by = 10)
  files <- write_daily(cache, dates, function(i) c(1000, 0, if (i %% 2 == 0) 800 else 100, 2540))
  days <- frazil:::ice_days_one(files, 2001, max_gap = 10)
  expect_equal(days[1], 365)
  expect_equal(days[2], 0)
  expect_equal(days[3], 365 * sum(seq_along(dates) %% 2 == 0) / length(dates))
  expect_equal(days[4], 0)
  # Remove a grid to open a 20-day gap: the year is left out.
  unlink(files$path[3])
  files <- daily_conc_files(cache)
  expect_message(dropped <- frazil:::ice_days_one(files, 2001, max_gap = 10), "left out")
  expect_null(dropped)
})

test_that("season_change subtracts the two periods and masks open water and flags", {
  cache <- withr::local_tempdir()
  # Two ice years of 365 days each (2011-12 would run into the 2012 leap day
  # and count 366).
  dates <- c(seq(as.Date("2001-03-01"), as.Date("2002-02-28"), by = 10),
             seq(as.Date("2013-03-01"), as.Date("2014-02-28"), by = 10))
  files <- write_daily(cache, dates, function(i) {
    early <- i <= 37
    c(1000, 0, if (early) 1000 else 0, 2540)
  })
  sc <- suppressMessages(season_change(early = 2001, recent = 2013, files = files,
                                       terrain_cache = file.path(cache, "terrain")))
  v <- terra::values(sc$change, mat = FALSE)
  expect_equal(v[1], 0)          # iced in both periods
  expect_true(is.na(v[2]))       # open water in both
  expect_equal(v[3], -365)       # lost a full year of ice
  expect_true(is.na(v[4]))       # land flag
  expect_true(file.exists(file.path(cache, "terrain", "seaice_icedays_2001_2001.rds")))
})

test_that("cover_change averages monthly grids and masks cells under min_cover", {
  cache <- withr::local_tempdir()
  write_monthly <- function(year, month, values) {
    dir <- file.path(cache, "monthly/geotiff", sprintf("%02d_%s", month, month.abb[month]))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    terra::writeRaster(small_grid(values),
                       file.path(dir, sprintf("S_%04d%02d_concentration_v4.0.tif", year, month)),
                       datatype = "INT2U", overwrite = TRUE)
  }
  write_monthly(1980, 2, c(800, 50, 900, 2540))
  write_monthly(1981, 2, c(600, 50, 700, 2540))
  write_monthly(2020, 2, c(400, 50, 900, 2540))
  write_monthly(2021, 2, c(200, 50, 700, 2540))
  files <- monthly_conc_files(cache)
  cc <- cover_change(2, early = 1980:1981, recent = 2020:2021, files = files)
  v <- terra::values(cc$change, mat = FALSE)
  expect_equal(v[1], 30 - 70)
  expect_true(is.na(v[2]))       # 5% in both periods
  expect_equal(v[3], 0)
  expect_true(is.na(v[4]))       # land flag
  expect_error(cover_change(9, early = 1980:1981, recent = 2020:2021, files = files),
               "monthly grids")
  s <- change_summary(cc$change, cc$raw, "Weddell")
  expect_equal(s$all_cells, 2)
})

# A small grid in the NSIDC projection, 40 by 40 cells of 25 km around the pole.
small_grid <- function(values = 0) {
  r <- terra::rast(nrows = 40, ncols = 40, xmin = -500000, xmax = 500000,
                   ymin = -500000, ymax = 500000, crs = "EPSG:3412")
  terra::values(r) <- values
  names(r) <- "raw"
  r
}

test_that("conc_percent keeps 15% to 100% and drops flags", {
  r <- small_grid(c(0, 100, 150, 500, 1000, 2530, 2540, 2550, rep(0, 1592)))
  v <- terra::values(conc_percent(r), mat = FALSE)[1:8]
  expect_equal(v, c(NA, NA, 15, 50, 100, NA, NA, NA))
  expect_equal(names(conc_percent(r)), "conc")
})

test_that("no_data_cells marks coast, land and missing cells only", {
  r <- small_grid(c(0, 1000, 2530, 2540, 2550, rep(0, 1595)))
  v <- terra::values(frazil:::no_data_cells(r), mat = FALSE)[1:5]
  expect_equal(v, c(NA, NA, 1, 1, 1))
})

test_that("in_sector assigns cells by longitude, including across the dateline", {
  r <- small_grid(0)
  lon <- terra::values(frazil:::grid_longitude(r), mat = FALSE)
  w <- in_sector(r, "Weddell")
  expect_equal(w, lon >= -60 & lon <= 20)
  ross <- in_sector(r, "Ross")
  expect_equal(ross, lon >= 160 | lon <= -130)
  expect_true(any(ross))
  all_sectors <- sapply(nsidc_sectors$sector, function(s) in_sector(r, s))
  # Every cell falls in at least one sector; boundary cells may fall in two.
  expect_true(all(rowSums(all_sectors) >= 1))
  expect_error(in_sector(r, "Atlantic"), "Unknown sector")
})

test_that("daily_conc_files finds and dates the grids in a cache layout", {
  cache <- withr::local_tempdir()
  d <- file.path(cache, "daily/geotiff/2026/10_Oct")
  dir.create(d, recursive = TRUE)
  for (day in c("20261007", "20261005", "20261006")) {
    terra::writeRaster(small_grid(0),
                       file.path(d, sprintf("S_%s_concentration_v4.0.tif", day)))
  }
  f <- daily_conc_files(cache)
  expect_equal(nrow(f), 3)
  expect_equal(f$date, as.Date(c("2026-10-05", "2026-10-06", "2026-10-07")))
  g <- daily_grid(NULL, cache)
  expect_equal(g$date, as.Date("2026-10-07"))
  expect_equal(names(g$raw), "raw")
  expect_error(daily_grid("2026-10-08", cache), "No daily grid")
  expect_error(daily_grid(NULL, file.path(cache, "empty")), "No daily concentration grids")
})

test_that("read_conc_raw drops the color table and keeps the values", {
  tmp <- withr::local_tempfile(fileext = ".tif")
  r <- small_grid(seq_len(1600) %% 1001)
  terra::writeRaster(r, tmp, datatype = "INT2U")
  x <- read_conc_raw(tmp)
  expect_equal(terra::values(x, mat = FALSE), terra::values(r, mat = FALSE))
  expect_equal(as.character(terra::crs(x, describe = TRUE)$code), "3412")
})

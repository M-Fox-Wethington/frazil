test_that("NSIDC folder listings are parsed", {
  html <- c(
    "<html><body><pre>",
    '<a href="../">Parent Directory</a>                             -',
    '<a href="S_20261006_concentration_v4.0.tif">S_20261006_concentration_v4.0.tif</a>     06-Oct-2026 09:12     606201',
    '<a href="S_20261007_concentration_v4.0.tif">S_20261007_concentration_v4.0.tif</a>     07-Oct-2026 10:27     606437',
    '<a href="2026/">2026/</a>                                      07-Oct-2026 10:27     -',
    "</pre></body></html>")
  x <- frazil:::parse_listing(html, "south/daily/geotiff/2026/10_Oct/")
  expect_equal(nrow(x), 3)
  expect_equal(x$name[2], "S_20261007_concentration_v4.0.tif")
  expect_equal(x$size[2], 606437)
  expect_true(is.na(x$size[3]))
  expect_equal(format(x$modified[2], "%Y-%m-%d %H:%M", tz = "UTC"), "2026-10-07 10:27")
  expect_equal(unique(x$rel_dir), "south/daily/geotiff/2026/10_Oct/")
})

test_that("listing times parse in any locale", {
  t <- frazil:::parse_listing_time(c("07-Oct-2026 10:27", "01-Jan-1979 00:00"))
  expect_equal(format(t, "%Y-%m-%d %H:%M", tz = "UTC"), c("2026-10-07 10:27", "1979-01-01 00:00"))
})

test_that("month folders follow NSIDC's naming", {
  expect_equal(frazil:::month_dir(c(1, 10)), c("01_Jan", "10_Oct"))
})

synthetic_sectors <- function(years = 1979:2025, missing = NULL) {
  g <- expand.grid(month = 1:12, year = years, sector = nsidc_sectors$sector,
                   stringsAsFactors = FALSE)
  g$label  <- nsidc_sectors$label[match(g$sector, nsidc_sectors$sector)]
  g$extent <- 3 + sin(2 * pi * g$month / 12) + 0.01 * (g$year - 1979)
  g$date   <- as.Date(sprintf("%04d-%02d-01", g$year, g$month))
  if (!is.null(missing)) g <- g[!(g$date %in% as.Date(missing)), ]
  g
}

test_that("a missing month does not bias the yearly mean", {
  # The missing months sit outside the 1981-2010 baseline, so the baseline
  # itself is unchanged and the yearly mean should be the trend alone.
  # September and April sit at the trough and near the crest of the synthetic
  # seasonal cycle, so a plain mean of the remaining months would be biased.
  m  <- synthetic_sectors(missing = c("2015-09-01", "2016-04-01"))
  yr <- yearly_sector(m)
  w  <- yr[yr$sector == "Weddell", ]
  expect_equal(w$n_months[w$year == 2015], 11)
  expect_equal(w$n_months[w$year == 2016], 11)
  expect_equal(w$n_months[w$year == 2014], 12)
  expect_equal(w$extent[w$year == 2015], 3 + 0.01 * (2015 - 1979), tolerance = 1e-8)
  expect_equal(w$extent[w$year == 2016], 3 + 0.01 * (2016 - 1979), tolerance = 1e-8)
  plain <- mean(m$extent[m$sector == "Weddell" & m$year == 2015])
  expect_gt(abs(plain - (3 + 0.36)), 0.05)
})

test_that("sector_trends recovers the slope and adds the total", {
  tr <- sector_trends(synthetic_sectors(), years = c(1979, 2025))
  expect_true("Total" %in% tr$sector)
  expect_equal(nrow(tr), 6)
  expect_equal(tr$slope[tr$sector == "Weddell"], 0.1, tolerance = 1e-8)
  expect_equal(tr$slope[tr$sector == "Total"], 0.5, tolerance = 1e-8)
  expect_true(all(tr$lo <= tr$slope & tr$slope <= tr$hi))
})

test_that("nsidc_sectors covers the full circle once", {
  expect_equal(nrow(nsidc_sectors), 5)
  spans <- ifelse(nsidc_sectors$east > nsidc_sectors$west,
                  nsidc_sectors$east - nsidc_sectors$west,
                  nsidc_sectors$east - nsidc_sectors$west + 360)
  expect_equal(sum(spans), 360)
})

test_that("the five-day trailing mean averages the days with data", {
  d <- data.frame(date = as.Date("2000-01-01") + 0:9, extent = 1:10)
  d <- add_trailing_mean(d)
  expect_equal(d$extent5[1], 1)
  expect_equal(d$extent5[3], mean(1:3))
  expect_equal(d$extent5[5], mean(1:5))
  expect_equal(d$extent5[10], mean(6:10))
})

test_that("an every-other-day record averages two or three values", {
  d <- data.frame(date = as.Date("1985-01-01") + seq(0, 18, by = 2), extent = 1:10)
  d <- add_trailing_mean(d)
  # Day 9 (value 5): the five calendar days ending on it hold days 5, 7 and 9.
  expect_equal(d$extent5[5], mean(3:5))
})

test_that("nothing is carried across a gap", {
  d <- data.frame(date = as.Date(c("1987-11-28", "1987-11-30", "1988-01-13", "1988-01-14")),
                  extent = c(10, 10, 4, 4))
  d <- add_trailing_mean(d)
  expect_equal(d$extent5[3], 4)
  expect_equal(d$extent5[4], 4)
})

test_that("lines are split at gaps longer than max_gap days", {
  d <- data.frame(date = as.Date(c("1987-11-28", "1987-11-30", "1987-12-02", "1988-01-13",
                                   "1988-01-14")),
                  year = c(1987, 1987, 1987, 1988, 1988), extent = 1)
  g <- add_line_groups(d, max_gap = 3)
  expect_equal(g$piece, c(1, 1, 1, 1, 1))
  d2 <- data.frame(date = as.Date(c("1990-01-01", "1990-01-02", "1990-01-10")),
                   year = 1990, extent = 1)
  expect_equal(add_line_groups(d2)$piece, c(1, 1, 2))
})

test_that("monthly departures use the baseline mean for the same calendar month", {
  m <- expand.grid(year = 1979:2012, month = 1:12)
  m$extent <- m$month + ifelse(m$year > 2010, 1, 0)
  m$date <- as.Date(sprintf("%04d-%02d-01", m$year, m$month))
  a <- monthly_departures(m, baseline = c(1981, 2010))
  expect_equal(unique(a$anom[a$year <= 2010]), 0)
  expect_equal(unique(a$anom[a$year > 2010]), 1)
})

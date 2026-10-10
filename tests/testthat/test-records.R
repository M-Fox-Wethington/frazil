test_that("an earlier record stands against a later value within the AMSR2 offset", {
  r <- record_year(c(2.1, 1.85, 1.80), c(2022, 2023, 2025), lowest = TRUE, quiet = TRUE)
  expect_equal(as.integer(r), 2023)
  expect_equal(attr(r, "close"), 2025)
})

test_that("a later value that beats the earlier record by more than the offset takes it", {
  r <- record_year(c(2.1, 1.85, 1.65), c(2022, 2023, 2025), lowest = TRUE, quiet = TRUE)
  expect_equal(as.integer(r), 2025)
  expect_length(attr(r, "close"), 0)
})

test_that("the rule works for record highs", {
  expect_equal(as.integer(record_year(c(19.5, 20.16, 20.25), c(2013, 2014, 2026),
                                      lowest = FALSE, quiet = TRUE)), 2014)
  expect_equal(as.integer(record_year(c(19.5, 20.16, 20.40), c(2013, 2014, 2026),
                                      lowest = FALSE, quiet = TRUE)), 2026)
})

test_that("a margin just under the offset is not enough, and just over it is", {
  expect_equal(as.integer(record_year(c(1.85, 1.705), c(2023, 2025), lowest = TRUE,
                                      quiet = TRUE)), 2023)
  expect_equal(as.integer(record_year(c(1.85, 1.695), c(2023, 2025), lowest = TRUE,
                                      quiet = TRUE)), 2025)
})

test_that("years before the AMSR2 switch are ranked plainly", {
  expect_equal(as.integer(record_year(c(3.1, 2.2, 2.9), c(1990, 2000, 2010), lowest = TRUE,
                                      quiet = TRUE)), 2000)
  expect_equal(as.integer(record_year(c(3.1, 2.2, 2.9), c(2025, 2026, 2027), lowest = TRUE,
                                      quiet = TRUE)), 2026)
})

test_that("a close year is reported in a message", {
  expect_message(record_year(c(1.85, 1.80), c(2023, 2025), lowest = TRUE), "2025 passes")
  expect_no_message(record_year(c(1.85, 1.80), c(2023, 2025), lowest = TRUE, quiet = TRUE))
})

test_that("NA values are dropped and duplicate years are refused", {
  expect_equal(as.integer(record_year(c(NA, 1.85, 1.80), c(2022, 2023, 2025), quiet = TRUE)),
               2023)
  expect_error(record_year(c(1, 2), c(2023, 2023)), "only once")
})

test_that("is_record classifies a single comparison", {
  expect_equal(is_record(16.71, 2026, best_before = 16.72, lowest = TRUE), "tie")
  expect_equal(is_record(16.50, 2026, best_before = 16.72, lowest = TRUE), "record")
  expect_equal(is_record(16.80, 2026, best_before = 16.72, lowest = TRUE), "no")
  expect_equal(is_record(16.71, 2020, best_before = 16.72, lowest = TRUE), "record")
  expect_equal(is_record(20.20, 2026, best_before = 20.16, lowest = FALSE), "tie")
})

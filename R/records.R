#' The year that holds a record, under the AMSR2 record rule
#'
#' Since 1 January 2025 the NSIDC Sea Ice Index has been built from the
#' Japanese AMSR2 sensor. NSIDC reports that AMSR2 places the ice edge slightly
#' closer to the continent, so Southern Hemisphere extent from 2025 on reads up
#' to 0.15 million km2 lower than the SSMIS sensors of the earlier record would
#' have given. A value from `amsr2_start` on therefore takes a record only when
#' it beats the earlier record by more than `offset`. A value that passes the
#' earlier record by less than that is reported as a tie.
#'
#' @param values Numeric vector with one value per year, for example each
#'   year's maximum or minimum five-day mean extent in million km2.
#' @param years Integer vector of the same length.
#' @param lowest `TRUE` for the record low, `FALSE` for the record high.
#' @param amsr2_start First year of the AMSR2 record.
#' @param offset The margin a later value has to beat the earlier record by, in
#'   the units of `values`.
#' @param quiet If `FALSE`, a message names any year from `amsr2_start` on that
#'   passed the earlier record by less than the margin.
#' @return The year that holds the record, with the attribute `close` holding
#'   the years that passed the earlier record by less than the margin.
#' @examples
#' # 2023 holds the record low minimum; 2025 is lower by only 0.05, so it ties.
#' record_year(c(2.1, 1.85, 1.80), c(2022, 2023, 2025), lowest = TRUE)
#' # A later value 0.2 below the earlier record takes it.
#' record_year(c(2.1, 1.85, 1.65), c(2022, 2023, 2025), lowest = TRUE)
#' @export
record_year <- function(values, years, lowest = TRUE, amsr2_start = 2025, offset = 0.15,
                        quiet = FALSE) {
  stopifnot(length(values) == length(years))
  ok <- !is.na(values) & !is.na(years)
  values <- values[ok]
  years  <- years[ok]
  if (!length(values)) stop("No values to rank.")
  if (anyDuplicated(years)) stop("Each year may appear only once.")
  sgn <- if (lowest) 1 else -1
  early <- years < amsr2_start
  if (!any(early)) {
    return(structure(years[which.min(sgn * values)], close = integer(0)))
  }
  holder <- years[early][which.min(sgn * values[early])]
  best_early <- values[years == holder]
  late <- years >= amsr2_start & sgn * (best_early - values) > offset
  if (any(late)) holder <- years[late][which.min(sgn * values[late])]
  close <- years >= amsr2_start & sgn * (values - best_early) < 0 & !late
  if (any(close) && !quiet) {
    message(paste(years[close], collapse = ", "), " passes the ", holder,
            " record by less than the AMSR2 offset of ", offset,
            ", so it is not labeled a record.")
  }
  structure(holder, close = years[close])
}

#' Does a value count as a record under the AMSR2 record rule?
#'
#' A companion to [record_year()] for one comparison: the value for a date or a
#' month against the lowest or highest earlier value for the same date or
#' month.
#'
#' @param value The value being tested.
#' @param year Its year.
#' @param best_before The best earlier value, lower for `lowest = TRUE`.
#' @inheritParams record_year
#' @return `"record"` when the value takes the record, `"tie"` when it passes
#'   the earlier value by less than the margin, and `"no"` otherwise.
#' @examples
#' is_record(16.71, 2026, best_before = 16.72, lowest = TRUE)
#' is_record(16.50, 2026, best_before = 16.72, lowest = TRUE)
#' @export
is_record <- function(value, year, best_before, lowest = TRUE, amsr2_start = 2025,
                      offset = 0.15) {
  sgn <- if (lowest) 1 else -1
  margin <- sgn * (best_before - value)
  if (margin <= 0) return("no")
  if (year < amsr2_start || margin > offset) "record" else "tie"
}

#' @name period_fmt
#' @title Format a numeric time period into readable text
#'
#' @description Converts a numeric period (e.g. a decimal number of years) into a
#'   human-readable string, automatically choosing the largest time unit
#'   (years, months, weeks, days, hours, minutes, or seconds) for which the
#'   value is at least 1. Avoids awkward output such as "0.417 weeks" or
#'   "70.128 hours" by picking a more natural unit. Vectorised over `period`
#'   and `unit`.
#'
#' @param period Numeric vector of time periods to format, expressed in `unit`.
#' @param unit Character vector giving the unit `period` is expressed in.
#'   Recycled to the length of `period`. One of `"seconds"`, `"minutes"`,
#'   `"hours"`, `"days"`, `"weeks"`, `"months"`, or `"years"`. Default `"years"`.
#' @param digits Integer number of decimal places to show in the formatted
#'   value. Default `1`.
#'
#' @return A character vector the same length as `period`, giving each value
#'   formatted as `"<number> <unit>"` (e.g. `"2.9 days"`), with the unit
#'   singularised only when `digits = 0` and the rounded value is exactly 1
#'   (e.g. `"1 month"`). At non-zero `digits`, the plural is always used
#'   (e.g. `"1.0 years"`), since a decimal value reads more naturally as plural.
#'
#' @examples
#' period_fmt(0.008, unit = "years")
#' # "2.9 days"
#'
#' period_fmt(c(0.09, 27.65, 1.2), unit = "months", digits = 0)
#' # "3 days"  "2 years" "1 month"
#'
#' period_fmt(1, unit = "years", digits = 1)
#' # "1.0 years"
#'
#' period_fmt(1, unit = "years", digits = 0)
#' # "1 year"
#'
#' @export
period_fmt <- function(period, unit = "years", digits = 1) {
  # Seconds-per-unit lookup, used to convert any input unit to a common
  # base (seconds) before choosing the most readable output unit
  secs_per_unit <- c(seconds = 1,
                     minutes = 60,
                     hours   = 3600,
                     days    = 86400,
                     weeks   = 86400 * 7,
                     months  = 86400 * (365.25 / 12),
                     years   = 86400 * 365.25)
  if (!all(unit %in% names(secs_per_unit))) {
    stop("unit must be one of: ", paste(names(secs_per_unit), collapse = ", "))
  }
  # Recycle unit to match length of period
  n <- length(period)
  unit <- rep(unit, length.out = n)
  # Candidate output units, largest to smallest - first one where the
  # converted value is >= 1 is chosen
  candidate_units <- c("years", "months", "weeks", "days", "hours", "minutes", "seconds")
  purrr::map2_chr(period, unit, function(p, u) {
    total_secs <- p * secs_per_unit[[u]]
    chosen <- NULL
    for (cu in candidate_units) {
      val <- total_secs / secs_per_unit[[cu]]
      if (val >= 1) {
        chosen <- cu
        break
      }
    }
    # Fall back to seconds if period is smaller than every candidate unit
    if (is.null(chosen)) chosen <- "seconds"
    val <- total_secs / secs_per_unit[[chosen]]
    # Only singularise when digits = 0 and the rounded value is exactly 1 -
    # a decimal like "1.0" should stay plural ("1.0 years"), since the
    # decimal implies a range rather than a single whole unit
    label <- if (digits == 0 && round(val, digits) == 1) sub("s$", "", chosen) else chosen
    val_fmt <- formatC(val, format = "f", digits = digits)
    paste(val_fmt, label)
  })
}

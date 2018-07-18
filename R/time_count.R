#' Create daynumber or weeknumber with weeks starting on Monday
#'
#' @param dates A vector with dates.
#' @param unit A string giving time-unit ("week" or "day").
#' @return A vector with day/weeknumbers.
#' @importFrom lubridate weeks interval as.period wday days as_date
#' @export
time_count <- function(dates, unit = "week") {
  cnt <- vector(mode = "integer", length(dates))
  min_date <- as_date(min(dates, na.rm = T))
  if(unit == "week") {
    min_wday <- wday(min_date, week_start = 1)
    min_date_mon <- min_date - days(min_wday - 1)
    cnt <- as.numeric(interval(min_date_mon, as_date(dates)),
                      unit = "day") %/% 7
  } else if(unit == "day") {
    cnt <- as.numeric(interval(min_date, lubridate::as_date(dates)), unit = "day")
  }
  return(cnt)
}

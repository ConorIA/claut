#' Daily extremes using the Night and Day Climatological Observing Window
#'
#' Calculates daily maxima and minima after \insertCite{zaknic-catovic_2018_comparison}{claut}
#'
#' @param dat data frame; the data to analyze; the first column must be date times; subsequent columns should contain for which to detect extremes
#' @param lat numeric; the latitude of the observer
#' @param lon numeric; the longitude of the observer
#' @param tz integer; the number of hours shifted from GMT, e.g. for GMT-5, pass \code{-5}
#' @param stimes Boolean; whether the sunrise and sunset should be included in the results table
#' @param ... additional parameters to pass to \code{\link{max}} and \code{\link{min}}, e.g. \code{na.rm = TRUE}
#'
#' @author Conor I. Anderson
#'
#' @source \insertRef{zaknic-catovic_2018_comparison}{claut}
#'
#' @return A table of class \code{\link{tbl_df}}.
#'
#' @importFrom dplyr %>% bind_cols case_when filter inner_join lead left_join mutate select summarize
#' @importFrom lubridate date days hours
#' @importFrom rlang .data
#' @importFrom tibble tibble
#'
#' @export
#'

solar_daily <- function(dat, lat, lon, tz, stimes = FALSE, ...) {

  if (names(dat)[1] != "DateTime") names(dat)[1] <- "DateTime"

  periods <- tibble(Date = date(dat$DateTime) %>% unique() %>% sort()) %>%
    mutate(start = suntimes(lat, lon, tz, .data$Date, "sunrise") + hours(1),
           middle = suntimes(lat, lon, tz, .data$Date, "sunset") + hours(1),
           end = lead(.data$start))

  all_periods <- inner_join(
    select(dat, .data$DateTime) %>% mutate(Date = date(.data$DateTime)),
    periods,
    by = "Date"
  )

  all_periods <- mutate(all_periods, Period = case_when(
    .data$start <= .data$DateTime & .data$middle >= .data$DateTime ~ "Day",
    .data$middle <= .data$DateTime & .data$end >= .data$DateTime ~ "Night",
    TRUE ~ as.character(NA)))

  all_periods$Date[is.na(all_periods$Period) & all_periods$Date != max(all_periods$Date)] <- all_periods$Date[is.na(all_periods$Period) & all_periods$Date != max(all_periods$Date)] - days(1)
  all_periods$Period[is.na(all_periods$Period) & all_periods$Date != max(all_periods$Date)] <- "Night"
  all_periods$Date[is.na(all_periods$Period) & all_periods$DateTime <= max(all_periods$end, na.rm = TRUE)] <- all_periods$Date[is.na(all_periods$Period) & all_periods$DateTime <= max(all_periods$end, na.rm = TRUE)] - days(1)
  all_periods$Period[is.na(all_periods$Period) & all_periods$DateTime <= max(all_periods$end, na.rm = TRUE)] <- "Night"

  vals <- bind_cols(
    select(all_periods, .data$Date, .data$Period),
    select(dat, -.data$DateTime))

  maxs <- vals %>% filter(.data$Period == "Day") %>%
    group_by(.data$Date, .data$Period) %>%
    summarize_all(max, ...) %>% select(-.data$Period)
  names(maxs)[2:ncol(maxs)] <- paste0("Max", names(maxs)[2:ncol(maxs)])

  mins <- vals %>% filter(.data$Period == "Night") %>%
    group_by(.data$Date, .data$Period) %>%
    summarize_all(min, ...) %>% select(-.data$Period)
  names(mins)[2:ncol(mins)] <- paste0("Min", names(mins)[2:ncol(mins)])
  mins <- mins[-1,]

  means <- vals %>% select(-.data$Period) %>%
    group_by(.data$Date) %>%
    summarize_all(mean, ...)
  names(means)[2:ncol(means)] <- paste0("Mean", names(means)[2:ncol(means)])
  means <- means[c(-1,-nrow(means)),]

  period_tab <- if (isTRUE(stimes)) {
    select(periods, .data$Date, Sunrise = .data$start, Sunset = .data$middle) %>%
      mutate(Sunrise = .data$Sunrise - hours(1),
             Sunset = .data$Sunset - hours(1))
  } else {
    select(periods, .data$Date)
  }

  left_join(period_tab,
            left_join(maxs, mins, by = "Date") %>%
              left_join(., means, by = "Date"), by = "Date")
}

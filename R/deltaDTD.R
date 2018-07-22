##' @title Calculate deltaDTD on any time scale
##'
##' @description A function that calculates DTD, SD, G, and deltaDTD on daily, monthly, seasonal, or annual time scales. It automatically eliminates aggregate values with higher than 20\% missing values. This function requres a datain frame with dates in column 1 called 'Date', Tmax in column 2 called 'MaxTemp', Tmin in column 3 called 'MinTemp'. No other datain is necessary Note that the datain aquired though the \code{canadaHCD} package meets these needs.
##'
##' @param datain datain.frame; define the datain that should be analyzed
##' @param period character; The period for the output datain. One of "daily", "monthly", "seasonal", or "annual".
##' @param max_NA numeric; The maximum proportion of missing data to allow (e.g. 0.2 for 20\% missing values)
##'
##' @author Conor I. Anderson
##'
##' @importFrom zoo as.yearmon as.yearqtr
##' @importFrom rlang .data
##' @importFrom stats aggregate sd
##' @importFrom dplyr %>% mutate group_by group_indices select summarize
##'
##' @export
##'
##' @examples
##' \dontrun{deltaDTD(tor_dly, "annual")}

deltaDTD <- function(datain, period = "z", max_NA = 0.2) {

  if(period != "annual" & period != "seasonal" & period != "monthly" & period != "daily") {
    stop("I don't recognize the period you specified.")
  }

  # Calculate the deltaDTD value.
  dat <- mutate(datain, DTD_Tmax = abs(datain$MaxTemp - c(NA, datain$MaxTemp[1:(nrow(datain)-1)])), DTD_Tmin = abs(datain$MinTemp - c(NA, datain$MinTemp[1:(nrow(datain)-1)])), deltaDTD = round(DTD_Tmax - DTD_Tmin, 3))

  # Drop the data we don't need form the table.
  dat <- select(dat, Date:MinTemp, DTD_Tmax:deltaDTD)

  ## Stop here for daily data
  if(period == "daily") return(dat)

  # Calculate number of missing values
  tests <- dat %>% group_by(Yearmon = as.yearmon(.data$Date)) %>%
    summarize(Missing = (sum(is.na(.data$deltaDTD)) / sum(is.na(.data$deltaDTD), !is.na(.data$deltaDTD))))
  # Take note of those months that are missing more than 20% of the data
  badrows <- which(tests$Missing > max_NA)

  if(period == "monthly"){

    # Now we stick everything together in a new data frame.
    dat <- dat %>% group_by(Yearmon = as.yearmon(.data$Date)) %>%
      summarize(Tmax = mean(.data$MaxTemp, na.rm = TRUE),
                Tmin = mean(.data$MinTemp, na.rm = TRUE),
                DTD_Tmax = mean(.data$DTD_Tmax, na.rm = TRUE),
                DTD_Tmin = mean(.data$DTD_Tmin, na.rm = TRUE),
                deltaDTD = mean(.data$deltaDTD, na.rm = TRUE),
                SD_Tmax = sd(.data$MaxTemp, na.rm = TRUE),
                SD_Tmin = sd(.data$MinTemp, na.rm = TRUE))

    # Wipe out the months with too much missing data.
    dat[badrows, 2:ncol(dat)] <- NA

    # Calculate G value and deltaDTD
    dat <- mutate(dat, G_Tmax = .data$DTD_Tmax / .data$SD_Tmax,
                  G_Tmin = .data$DTD_Tmin / .data$SD_Tmin)

    ## Stop here for monthly data
    if (period == "monthly") return(dat)

  } else {

    # Wipe out the bad months
    dat[!is.na(match(dat %>% group_indices(Yearmon = as.yearmon(.data$Date)), badrows)),2:6] <- NA

    dat <- mutate(dat, Year = as.integer(format(.data$Date, format = "%Y")),
                  Month = as.integer(format(.data$Date, format = "%m")))
    dat$Year[dat$Month == 12] <- dat$Year[dat$Month == 12] + 1
    dat$Season[dat$Month == 1 | dat$Month == 2 | dat$Month == 12] <- 1
    dat$Season[dat$Month == 3 | dat$Month == 4 | dat$Month == 5] <- 2
    dat$Season[dat$Month == 6 | dat$Month == 7 | dat$Month == 8] <- 3
    dat$Season[dat$Month == 9 | dat$Month == 10 | dat$Month == 11] <- 4

    tests <- dat %>% group_by(Yearqtr = as.yearqtr(paste(.data$Year, .data$Season, sep = "-"))) %>%
      summarize(Total = sum(is.na(.data$deltaDTD), !is.na(.data$deltaDTD)),
                Missing = (sum(is.na(.data$deltaDTD)) / .data$Total))
    badrows <- c(which(tests$Total < 90), which(tests$Missing > max_NA))

    ## Take this route for seasonal data
    if (period == "seasonal") {

      # Now we stick everything together in a new data frame.
      dat <- dat %>% group_by(Yearqtr = as.yearqtr(paste(Year, Season, sep = "-"))) %>%
        summarize(Tmax = mean(.data$MaxTemp, na.rm = TRUE),
                  Tmin = mean(.data$MinTemp, na.rm = TRUE),
                  DTD_Tmax = mean(.data$DTD_Tmax, na.rm = TRUE),
                  DTD_Tmin = mean(.data$DTD_Tmin, na.rm = TRUE),
                  deltaDTD = mean(.data$deltaDTD, na.rm = TRUE),
                  SD_Tmax = sd(.data$MaxTemp, na.rm = TRUE),
                  SD_Tmin = sd(.data$MinTemp, na.rm = TRUE))

      # Wipe out the months with too much missing data.
      dat[badrows,2:ncol(dat)] <- NA

      # Calculate G value
      dat <- mutate(dat, G_Tmax = .data$DTD_Tmax / .data$SD_Tmax,
                    G_Tmin = .data$DTD_Tmin / .data$SD_Tmin)

      ## Stop here for seasonal data
      return(dat)

    } else {

      # Wipe out the bad quarters FIXME: Is there a way to vectorize this?
      dat[!is.na(match(dat %>%
                         group_indices(Yearqtr = as.yearqtr(paste(.data$Year,
                                                                  .data$Season,
                                                                  sep = "-"))),
                       badrows)),2:6] <- NA

      # Take this route for annual data

      tests <- dat %>% group_by(Year = format(.data$Date, format = "%Y")) %>%
        summarize(Missing = (sum(is.na(.data$deltaDTD)) / sum(is.na(.data$deltaDTD), !is.na(.data$deltaDTD))))
      badrows <- which(tests$Missing > max_NA)

      # Now we stick everything together in a new data frame.
      dat <- dat %>% group_by(Year = format(.data$Date, format = "%Y")) %>%
        summarize(Tmax = mean(.data$MaxTemp, na.rm = TRUE),
                  Tmin = mean(.data$MinTemp, na.rm = TRUE),
                  DTD_Tmax = mean(.data$DTD_Tmax, na.rm = TRUE),
                  DTD_Tmin = mean(.data$DTD_Tmin, na.rm = TRUE),
                  deltaDTD = mean(.data$deltaDTD, na.rm = TRUE),
                  SD_Tmax = sd(.data$MaxTemp, na.rm = TRUE),
                  SD_Tmin = sd(.data$MinTemp, na.rm = TRUE))

      # Wipe out the months with too much missing data.
      dat[badrows,2:ncol(dat)] <- NA

      # Calculate G value and deltaDTD
      dat <- mutate(dat, G_Tmax = .data$DTD_Tmax / .data$SD_Tmax,
                    G_Tmin = .data$DTD_Tmin / .data$SD_Tmin)

      ## Stop here for annual data
      return(dat)
    }
  }
}

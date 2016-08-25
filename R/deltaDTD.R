##' @title Calculate deltaDTD on any time scale
##'
##' @description "A function that calculates DTD, SD, G, and deltaDTD on daily, monthly, seasonal, or annual time scales. It automatically eliminates aggregate values with higher than 20\% missing values. This function requres a data frame with dates in column 1 called 'Date', Tmax in column 2 called 'MaxTemp', Tmin in column 3 called 'MinTemp'. No other data is necessary Note that the data aquired though the \code{canadaHCD} package meets these needs."
##'
##' @param data data.frame; define the data that should be analyzed
##' @param period character; The period for the output data. One of "daily", "monthly", "seasonal", or "annual".
##' @param QA logical; whether to filter those values that are missing 20\% of observations
##'
##' @author Conor I. Anderson
##'
##' @importFrom zoo as.yearmon as.yearqtr
##' @importFrom stats aggregate sd
##'
##' @export
##'
##' @examples
##' \dontrun{deltaDTD(tor_dly, "annual")}

deltaDTD <- function(data, period = "z", QA = TRUE) {

  if(period != "annual" & period != "seasonal" & period != "monthly" & period != "daily") {
    stop("I don't recognize the period you specified.")
  }

  # Make sure the Year, Month, and Day are at the start of the table.
  data$Date <- as.Date(data$Date)
  Year <- format(data$Date, format = "%Y")
  Month <- format(data$Date, format = "%m")
  Day <- format(data$Date, format = "%d")
  data <- cbind(Year, Month, Day, data)

  # Make a copy of the data to modify
  dat <- data

  # Calculate the DTD value.
  dat$DTD_Tmax <- dat$MaxTemp - c(NA, dat$MaxTemp[1:(nrow(dat)-1)])
  dat$DTD_Tmin <- dat$MinTemp - c(NA, dat$MinTemp[1:(nrow(dat)-1)])

  # Calculate deltaDTD
  dat$deltaDTD <- dat$DTD_Tmax-dat$DTD_Tmin

  # Drop the data we don't need form the table.
  dat <- cbind(dat[,1:6], dat[,(ncol(dat)-2):ncol(dat)])

  ## Stop here for daily data
  if(period == "daily") return(dat)

  if(period == "monthly"){
    if(QA) {
      # Count NA values for each variable
      dat$Tmax_test <- is.na(dat$MaxTemp)
      dat$Tmin_test <- is.na(dat$MinTemp)
      dat$period_test <- rep(1, nrow(dat))
      # Calculate number of missing values
      Tmax_test <- stats::aggregate(Tmax_test ~ Month + Year , dat , sum, drop = FALSE)
      Tmin_test <- stats::aggregate(Tmin_test ~ Month + Year , dat , sum, drop = FALSE)
      # Total number of days in each month
      period_test <- stats::aggregate(period_test ~ Month + Year , dat , sum, drop = FALSE)
      # Take note of those months that are missing more than 20% of the data
      Tmax_test <- Tmax_test$Tmax_test > (0.2 * period_test$period_test)
      Tmin_test <- Tmin_test$Tmin_test > (0.2 * period_test$period_test)
    }

    # Agregate dat to monthly values.
    Tmax <- stats::aggregate(MaxTemp ~ Month + Year , dat , mean, drop = FALSE)
    Tmin <- stats::aggregate(MinTemp ~ Month + Year , dat , mean, drop = FALSE)
    DTD_Tmax <- stats::aggregate(DTD_Tmax ~ Month + Year , dat , mean, drop = FALSE)
    DTD_Tmin <- stats::aggregate(DTD_Tmin ~ Month + Year , dat , mean, drop = FALSE)

    # Calculate Standard Deviation
    SD_Tmax <- stats::aggregate(MaxTemp ~ Month + Year , dat , stats::sd, drop = FALSE)
    SD_Tmin <- stats::aggregate(MinTemp ~ Month + Year , dat , stats::sd, drop = FALSE)

    # Now we stick everything together in a new data frame.
    dat <- Tmax
    dat$MinTemp <- Tmin$MinTemp
    dat$DTD_Tmax <- DTD_Tmax$DTD_Tmax
    dat$DTD_Tmin <- DTD_Tmin$DTD_Tmin
    dat$SD_Tmax <- SD_Tmax$MaxTemp
    dat$SD_Tmin  <- SD_Tmin$MinTemp
    if(QA){
      # Wipe out the months with too much missing data.
      dat[Tmax_test,c(3,5,7)] <- NA
      dat[Tmin_test,c(4,6,8)] <- NA
    }
    # Calculate G value
    dat$G_Tmax <- dat$DTD_Tmax/dat$SD_Tmax
    dat$G_Tmin <- dat$DTD_Tmin/dat$SD_Tmin

    # Calculate deltaDTD
    dat$deltaDTD <- dat$DTD_Tmax-dat$DTD_Tmin

    # Add Yr-Mon and organize the table
    dat$Yr.Mon <- zoo::as.yearmon(paste(dat$Year, dat$Month, sep = "-"))
    dat <- cbind(dat[2],dat[1],dat[ncol(dat)],dat[3:(ncol(dat)-1)])

    ## Stop here for monthly data
    return(dat)

  } else {
    ## Take this route for seasonal data
    if (period == "seasonal") {
      ## Shift Decembers up one year (but first, convert factors to numbers)
      dat$Month <- as.numeric(as.character(dat$Month))
      dat$Year <- as.numeric(as.character(dat$Year))
      dat$Year[dat$Month == 12] <- dat$Year[dat$Month == 12] + 1
      ## Convert Months to Seasons
      dat$Month[dat$Month == 1 | dat$Month == 2 | dat$Month == 12] <- 1
      dat$Month[dat$Month == 3 | dat$Month == 4 | dat$Month == 5] <- 2
      dat$Month[dat$Month == 6 | dat$Month == 7 | dat$Month == 8] <- 3
      dat$Month[dat$Month == 9 | dat$Month == 10 | dat$Month == 11] <- 4
      names(dat)[2] <- "Season"
      if(QA) {
        # Count NA values for each variable
        dat$Tmax_test <- is.na(dat$MaxTemp)
        dat$Tmin_test <- is.na(dat$MinTemp)
        dat$period_test <- rep(1, nrow(dat))
        # Calculate number of missing values
        Tmax_test <- stats::aggregate(Tmax_test ~ Season + Year, dat , sum, drop = FALSE)
        Tmin_test <- stats::aggregate(Tmin_test ~ Season + Year, dat , sum, drop = FALSE)
        # Total number of days in each season
        period_test <- stats::aggregate(period_test ~ Season + Year, dat , sum, drop = FALSE)
        # Take note of those seasons that are missing 20% of data
        Tmax_test <- Tmax_test$Tmax_test > (0.2 * period_test$period_test)
        Tmin_test <- Tmin_test$Tmin_test > (0.2 * period_test$period_test)
        # Take note of any seasons that have less than 90 days e.g. first and last
        period_test <- period_test$period_test < 90
      }

      # Agregate dat to seasonal values.
      Tmax <- stats::aggregate(MaxTemp ~ Season + Year, dat , mean, drop = FALSE)
      Tmin <- stats::aggregate(MinTemp ~ Season + Year, dat , mean, drop = FALSE)
      DTD_Tmax <- stats::aggregate(DTD_Tmax ~ Season + Year, dat , mean, drop = FALSE)
      DTD_Tmin <- stats::aggregate(DTD_Tmin ~ Season + Year, dat , mean, drop = FALSE)
      SD_Tmax <- stats::aggregate(MaxTemp ~ Season + Year, dat , stats::sd, drop = FALSE)
      SD_Tmin <- stats::aggregate(MinTemp ~ Season + Year, dat , stats::sd, drop = FALSE)

      # Now we stick everything together in a new data frame.
      dat <- Tmax
      dat$MinTemp <- Tmin$MinTemp
      dat$DTD_Tmax <- DTD_Tmax$DTD_Tmax
      dat$DTD_Tmin <- DTD_Tmin$DTD_Tmin
      dat$SD_Tmax <- SD_Tmax$MaxTemp
      dat$SD_Tmin  <- SD_Tmin$MinTemp
      if(QA){
        # Trim tests if they overshoot the stats::aggregate data
        if(length(Tmax_test) > nrow(dat)) Tmax_test <- Tmax_test[1:nrow(dat)]
        if(length(Tmin_test) > nrow(dat)) Tmin_test <- Tmin_test[1:nrow(dat)]
        if(length(period_test) > nrow(dat)) period_test <- period_test[1:nrow(dat)]
        # Wipe out the seasons with too much missing data.
        dat[Tmax_test,c(3,5,7)] <- NA
        dat[Tmin_test,c(4,6,8)] <- NA
        # Trim incomplete months at start and end of data set
        dat[period_test,3:8] <- NA
        # Get rid of extra year on the end if it got added
        if (max(as.numeric(as.character(dat$Year))) >
            max(as.numeric(as.character(data$Year)))) {
            dat <- dat[-which(as.numeric(as.character(dat$Year)) >
            max(as.numeric(as.character(data$Year)))),]
        }
      }

      # Calculate G value
      dat$G_Tmax <- dat$DTD_Tmax/dat$SD_Tmax
      dat$G_Tmin <- dat$DTD_Tmin/dat$SD_Tmin

      # Calculate deltaDTD
      dat$deltaDTD <- dat$DTD_Tmax-dat$DTD_Tmin

      # Add Yr-Season, and sort the table
      dat$Yr.S <- zoo::as.yearqtr(paste(dat$Year, dat$Season, sep = "-"))
      dat <- cbind(dat[2],dat[1],dat[ncol(dat)],dat[3:(ncol(dat)-1)])

      ## Stop here for seasonal data
      return(dat)

    } else {
      ## Take this route for annual data
      if(QA) {
        # Count NA values for each variable
        dat$Tmax_test <- is.na(dat$MaxTemp)
        dat$Tmin_test <- is.na(dat$MinTemp)
        dat$period_test <- rep(1, nrow(dat))
        # Calculate number of missing values
        Tmax_test <- stats::aggregate(Tmax_test ~ Year , dat , sum, drop = FALSE)
        Tmin_test <- stats::aggregate(Tmin_test ~ Year , dat , sum, drop = FALSE)
        # Total number of days in each year
        period_test <- stats::aggregate(period_test ~ Year , dat , sum, drop = FALSE)
        # Take note of those years that are missing 20% of data
        Tmax_test <- Tmax_test$Tmax_test > (0.2 * period_test$period_test)
        Tmin_test <- Tmin_test$Tmin_test > (0.2 * period_test$period_test)
      }

      # Agregate dat to annual values.
      Tmax <- stats::aggregate(MaxTemp ~ Year , dat , mean, drop = FALSE)
      Tmin <- stats::aggregate(MinTemp ~ Year , dat , mean, drop = FALSE)
      DTD_Tmax <- stats::aggregate(DTD_Tmax ~ Year , dat , mean, drop = FALSE)
      DTD_Tmin <- stats::aggregate(DTD_Tmin ~ Year , dat , mean, drop = FALSE)

      # Calculate standard deviation
      SD_Tmax <- stats::aggregate(MaxTemp ~ Year , dat , stats::sd, drop = FALSE)
      SD_Tmin <- stats::aggregate(MinTemp ~ Year , dat , stats::sd, drop = FALSE)

      # Now we stick everything together in a new data frame.
      dat <- Tmax
      dat$MinTemp <- Tmin$MinTemp
      dat$DTD_Tmax <- DTD_Tmax$DTD_Tmax
      dat$DTD_Tmin <- DTD_Tmin$DTD_Tmin
      dat$SD_Tmax <- SD_Tmax$MaxTemp
      dat$SD_Tmin  <- SD_Tmin$MinTemp
      if(QA){
        # Trim cells with >20% missing values
        dat[Tmax_test,c(2,4,6)] <- NA
        dat[Tmin_test,c(3,5,7)] <- NA
      }

      # Calculate G value
      dat$G_Tmax <- dat$DTD_Tmax/dat$SD_Tmax
      dat$G_Tmin <- dat$DTD_Tmin/dat$SD_Tmin

      # Calculate deltaDTD
      dat$deltaDTD <- dat$DTD_Tmax-dat$DTD_Tmin

      ## Stop here for annual data
      return(dat)
    }
  }
}

#' Perform the mass elimination of consecutive daily values from a monthly data set
#'
#' @param month data.frame; a data.frame with a single year-month of data with no missing values
#' @param csv character; path to a .csv file containing a single year-month of data with no missing values
#' @param NAs numeric; a vector of the number of NA values to test
#' @param variables character; the names of the variables to test (we will try to auto-idenify the column number)
#' @param simplify Boolean; whether to return simplified results
#' @param interpolate Boolean; whether to use linear interpolation to approximate the missing values
#'
#' @importFrom stats sd
#' @importFrom utils read.csv
#' @importFrom zoo na.approx
#'
#' @export
#'

dataEliminatorThorough <- function(month, csv, NAs, variables = c("max", "min", "mean"), simplify = FALSE, interpolate = FALSE) {

  ## This version of the function is for consecutive sampling, and will test all possible
  ## permutations of a month with `1` missing values

  ## First, I hold the users hand, making sure that we have all of the information we need
  ## to perform the test.

  # Set up the data.frame `month`
  if (missing(month) && missing(csv)) csv <- readline(prompt = "Please specify csv file. ")
  if (missing(month) && !missing(csv)) month <- read.csv(csv)

  # Get the range of `k` and sampling type
  if (missing(NAs)) NAs <- as.numeric(readline(prompt = "Enter vector of values of `k` to test: "))

  # Find out which variables we are interested in
  for (var in seq_along(variables)) {
    variables[var] <- as.numeric(grep(variables[var], names(month), ignore.case = TRUE))
  }
  variables <- as.numeric(variables)

  # We make an empty data frame where we will store our results
  df2 <- df3 <- data.frame(stringsAsFactors = FALSE)

  # Now we start an outer loop, for each value of `k`
  for (k in NAs) {

    df <- data.frame(stringsAsFactors = FALSE)

    # Now we start an inner loop, which will loop through dates from the 1st to the (n+1-k)th
    # Note, please ignore the fact that my loop function variables are different letters than above!
    for (days in 1:(nrow(month)+1-k)) {

      # Choose which days to eliminate
      NAs <- seq(days, days-1+k)

      # We do the nullifying
      monthMod <- month
      monthMod[NAs,] <- NA

      # Now we start an inner loop that does all the calculations and builds the results table
      for (var in variables) {
        colname <- colnames(month[var])
        month[[var]] <- as.numeric(month[[var]])
        mean <- mean(month[[var]], na.rm = TRUE)
        SD <- sd(month[[var]], na.rm = T)
        meanMod <- mean(monthMod[[var]], na.rm = TRUE)
        deltaMean <- abs(meanMod-mean)
        prop <- deltaMean/SD

        if (interpolate) {
          monthApprox <- na.approx(monthMod[[var]])
          prop <- c(prop, abs(mean(monthApprox, na.rm = TRUE)-mean)/SD)
        }

        # Then we stick it all together in a table.
        row <- c(colname, k, NAs[1], NAs[length(NAs)], mean, SD, meanMod,deltaMean, prop)
        df <- rbind(df, row, stringsAsFactors = FALSE)
      }
    }
    if (interpolate) {
      names(df) <- c("Variable", "No.NA", "StartDate", "EndDate", "Mean", "StDev", "Mod.Mean", "Abs.Diff", "Mean.Error", "Mean.Error.Approx")
    } else {
      names(df) <- c("Variable", "No.NA", "StartDate", "EndDate", "Mean", "StDev", "Mod.Mean", "Abs.Diff", "Mean.Error")
    }
    df3 <- rbind(df3, df)
    if (interpolate) {
      names(df3) <- c("Variable", "No.NA", "StartDate", "EndDate", "Mean", "StDev", "Mod.Mean", "Abs.Diff", "Mean.Error", "Mean.Error.Approx")
    } else {
      names(df3) <- c("Variable", "No.NA", "StartDate", "EndDate", "Mean", "StDev", "Mod.Mean", "Abs.Diff", "Mean.Error")
    }

    # Yet another loop (Optimization be damned!) that takes the summary stats for each variable, in
    # case we want a simple summary.
    for (var in variables) {
      colname <- colnames(month[var])
      index <- which(df$Variable == colname)
      if (interpolate) {
        sta <- c(mean(as.numeric(df$Mean.Error[index])), mean(as.numeric(df$Mean.Error.Approx[index])))
      } else {
        sta <- summary(as.numeric(df$Mean.Error[index]))
      }
      row <- c(colname, k, length(1:(nrow(month)+1-k)), sta)
      df2 <- rbind(df2, row, stringsAsFactors = FALSE)
    }
  if (interpolate) {
    names(df2) <- c("Variable", "No.NA", "No.Reps", "Mean.Error", "Mean.Error.Approx")
    } else {
      names(df2) <- c("Variable", "No.NA", "No.Reps", "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
    }
  }

  # If we chose the simplify option, we just get the summary results.
  if (simplify) {
    return(df2)
  } else {
    return(df3)
  }
}

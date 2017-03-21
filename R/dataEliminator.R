#' Eliminate daily values from a monthly data set
#'
#' @param month data.frame; a data.frame with a single year-month of data with no missing values
#' @param csv character; path to a .csv file containing a single year-month of data with no missing values
#' @param NAs numeric; a vector of the number of NA values to test
#' @param sampling character; the type of sampling to use: (r)andom, (c)onsecutive
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

dataEliminator <- function(month, csv, NAs, sampling = "z", variables = c("max", "min", "mean"), simplify = FALSE, interpolate = FALSE) {

  ## First, I hold the users hand, making sure that we have all of the information we need to perform the test.

  # Set up the data.frame `month`
  if (missing(month)) {
    if (missing(csv)) csv <- readline(prompt = "Please specify csv file. ")
    month <- read.csv(csv)
  }

  # Get the range of `k` and sampling type
  if (missing(NAs)) NAs <- as.numeric(readline(prompt = "Enter vector of values of `k` to test: "))
  while (sampling != "c" & sampling != "r") {
    sampling <- readline(prompt = "Do you want NAs to be generated (r)andomly, or (c)onsecutively? ")
    if (sampling != "c" & sampling != "r") print("You must choose either (c) or (r).")
  }

  for (var in seq_along(variables)) {
    variables[var] <- as.numeric(grep(variables[var], names(month), ignore.case = TRUE))
  }
  variables <- as.numeric(variables)

  # We make an empty data frame where we will store our results
  df <- data.frame(stringsAsFactors = FALSE)

  # Now we start an outer loop, for each value of `k`
  for (k in NAs) {

    # First we generate our random data points that will be nullified
    if (sampling == "r") {
      NAvec <- sample(1:nrow(month), size = k, replace = FALSE)
    }
    if (sampling == "c") {
      NAvec <- sample(1:(nrow(month)+1-k), size = 1, replace = FALSE)
      NAvec <- seq(NAvec, NAvec-1+k)
    }

    # We do the nullifying
    monthMod <- month
    monthMod[NAvec,] <- NA

    # Now we start an inner loop that does all the calculations and builds the results table
    for (var in variables) {
      colname <- colnames(month[var])
      mean <- mean(month[[var]], na.rm = T)
      SD <- sd(month[[var]], na.rm = T)

      meanMod <- mean(monthMod[[var]], na.rm = T)
      deltaMean <- abs(meanMod-mean)
      prop <- deltaMean/SD

      if (interpolate) {
        monthApprox <- na.approx(monthMod[[var]])
        prop <- c(prop, abs(mean(monthApprox, na.rm = T)-mean)/SD)
      }

      # Then we stick it all together in a table.
      row <- c(colname, k, mean, SD, meanMod,deltaMean, prop)

      df <- rbind(df, row, stringsAsFactors = FALSE)
    }
  }
  if (interpolate) {
    names(prop) <- c("Mean.Error", "Mean.Error.Approx")
    names(df) <- c("Variable", "No.NA", "Mean", "StDev", "Mod.Mean", "Abs.Diff", "Mean.Error", "Mean.Error.Approx")
  } else {
    names(df) <- c("Variable", "No.NA", "Mean", "StDev", "Mod.Mean", "Abs.Diff", "Mean.Error")
  }

  # If we chose the simplify option, we just get the binary result. This is useful for running the test a massive number of times, but only for a single variable. See the appendix!
  if (simplify) {
    return(prop)
  } else {
    return(df)
  }
}

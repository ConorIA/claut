#' Perform the mass elimination of daily values from a monthly data set
#'
#' @param numberTests integer; the number of times to repeat the test
#' @param month data.frame; a data.frame with a single year-month of data with no missing values
#' @param csv character; path to a .csv file containing a single year-month of data with no missing values
#' @param NAs numeric; a vector of the number of NA values to test
#' @param sampling character; the type of sampling to use: (r)andom, (c)onsecutive
#' @param variable character; the name of the variable to test (we will try to auto-idenify the column number)
#' @param verbose Boolean; whether the function should be verbose
#' @param interpolate Boolean; whether to use linear interpolation to approximate the missing values
#'
#' @importFrom utils read.csv
#'
#' @export
#'

dataEliminatorMassive <- function(numberTests, month, csv, NAs, sampling = "z", variable = c("max", "min", "mean"), verbose = FALSE, interpolate = FALSE) {
  #source("dataEliminator.R")

  # We again hold the users hand to come up with all the info we need
  if (missing(numberTests)) numberTests <- as.numeric(readline(prompt = "How many times should I repeat the test? "))
  if (missing(month) && missing(csv)) csv <- readline(prompt = "Please specify csv file. ")
  if (missing(month) && !missing(csv)) month <- read.csv(csv)
  if (missing(NAs)) NAs <- as.numeric(readline(prompt = "Enter vector of values of `k` to test: "))
  while (sampling != "c" & sampling != "r") {
    sampling <- readline(prompt = "Do you want NAs to be generated (r)andomly, or (c)onsecutively? ")
    if (sampling != "c" & sampling != "r") print("You must choose either (c) or (r).")
  }
  while (length(variable) > 1 | (variable != "mean" && variable != "max" && variable != "min")) {
    print("Please choose the variable to test.")
    variable <- readline(prompt = "Enter one of \"max\", \"min\", or \"mean\". ")
  }

  # We create an empty data frame to put our results
  df <- data.frame(stringsAsFactors = FALSE)
  colname <- colnames(month[grep(variable, names(month), ignore.case = TRUE)])

  # We loop through the values of `k`
  for (k in NAs) {
    if (verbose == TRUE) print(paste("Running", numberTests, "tests with", k, "NAs."))

    # We now run the original dataEliminator script, `numberTests` times, using the `simplify argument`.
    result <- replicate(numberTests, dataEliminator(month = month, NAs = k, sampling = sampling, variables = variable, simplify = TRUE, interpolate = interpolate))

    # Then we make a table of the results, proportion of tests passed overall, and broken down by test type.
    if (interpolate) {
      sta <- c(mean(result[1,]), mean(result[2,]))
    } else {
      sta <- summary(result)
    }
    row <- c(colname, k, numberTests, sta)
    df <- rbind(df, row, stringsAsFactors = FALSE)
  }
  if (interpolate) {
    names(df) <- c("Variable", "No.NA", "No.Reps", "Mean.Error", "Mean.Error.Approx")
  } else {
    names(df) <- c("Variable", "No.NA", "No.Reps", "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
  }
  return(df)
}

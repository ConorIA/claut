#' Eliminate daily values within months of a data set and report the results
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

missing_data_lab <- function(monthin, NAs, sampling = "z", variables = c("max", "min", "mean")) {

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

  no_of_tests <- 10

  # First we generate our random data points that will be nullified
  NAvectors <- foreach(k=ks, .combine = "append") %do% {
    if (sampling == "r") {
      NAvec <- replicate(no_of_tests, list(sample(1:nrow(monthin), size = k, replace = FALSE)))
    }
    if (sampling == "c") {
      if (nrow(monthin) - k + 1 < no_of_tests) {
        NAvec <- lapply(1:(nrow(monthin) - k + 1), function (x) eval(x:(x-1+k)))
      } else {
        samp_f <- function() {
          samp <- sample(1:(nrow(monthin)+1-k), size = 1, replace = FALSE)
          list(seq(samp, samp-1+k))
        }
        NAvec <- replicate(no_of_tests, samp_f())
      }
    }
    NAvec
  }

  registerDoParallel(cores = detectCores())

  results <- foreach(NAs=NAvectors) %dopar% {

    NAs <- unlist(NAs)

    month <- foreach(var=variables, .combine = rbind) %do% {
      month <- monthin[,c(1, var)]
      mod_var <- month[[2]]
      mod_var[NAs] <- NA
      month <- tibble(Date = month[[1]], var = colnames(month)[2], Intact = month[[2]], wMiss = mod_var, LinInt = na.approx(mod_var, na.rm = FALSE), SplInt = na.spline(mod_var))
      month
    }

    summary <- month %>%
      melt(id.vars = c("Date", "var"), variable.name = "treatment") %>%
      group_by(yearmon = as.yearmon(Date, format = "%Y-%m-%d"), var, treatment) %>%
      select(-`Date`) %>%
      summarize_all(c(mean = mean.default, sd = sd), na.rm = TRUE) %>%
      mutate(k = length(NAs), days = list(NAs))

    summary <- summary %>%
      left_join(., subset(summary, treatment == "Intact", select = c(-`treatment`, -`k`, -`days`)), by = c("yearmon", "var")) %>%
      mutate(err = abs(mean.x - mean.y), prop = err / sd.y) %>%
      select(k, days, treatment, mean = mean.x, sd = sd.x, -`mean.y`, -`sd.y`, err, prop)

    summary <- summary %>%
      left_join(., subset(summary, treatment == "wMiss", select = c(-`treatment`, -`k`, -`days`)), by = c("yearmon", "var")) %>%
      mutate(change_real = err.y - err.x, change_prop = prop.y - prop.x) %>%
      select(k, days, treatment, mean = mean.x, sd = sd.x, -`mean.y`, -`sd.y`, err = err.x, prop = prop.x, change_real, change_prop)

    list(summary = summary, details = month)
  }

  list(summary = bind_rows(lapply(results, "[[", 1)), details = bind_rows(lapply(results, "[[", 2)))
}

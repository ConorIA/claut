#' Eliminate daily values within months of a data set and report the results
#'
#' @param monthin data.frame; a data.frame with a single year-month of data with no missing values
#' @param NAs numeric; a vector of the number of NA values to test
#' @param no_of_tests integer; the number of tests to run; for consecutive sampling, tops at N - k + 1
#' @param sampling character; the type of sampling to use: (r)andom, (c)onsecutive
#' @param variables character; the names of the variables to test (we will try to auto-idenify the column number)
#' @param cores numeric; the number of cores to parallize over. Defaults to \code{parallel::detectCores() - 1}
#'
#' @importFrom dplyr bind_rows
#' @importFrom iterators icount
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom stats sd
#' @importFrom utils read.csv
#' @importFrom zoo na.approx
#'
#' @export
#'

missing_data_lab <- function(monthin, NAs, sampling = "z", no_of_tests = 1, variables = c("max", "min", "mean"), cores) {

  # Get the range of `k` and sampling type
  if (missing(NAs)) NAs <- as.numeric(readline(prompt = "Enter vector of values of `k` to test: "))
  while (sampling != "c" & sampling != "r") {
    sampling <- readline(prompt = "Do you want NAs to be generated (r)andomly, or (c)onsecutively? ")
    if (sampling != "c" & sampling != "r") print("You must choose either (c) or (r).")
  }

  for (var in seq_along(variables)) {
    variables[var] <- grep(variables[var], names(monthin), ignore.case = TRUE)
  }
  variables <- as.numeric(variables)

  # First we generate our random data points that will be nullified
  NAvectors <- foreach(k=NAs, .combine = "append") %do% {
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

  if (missing(cores)) {
    cores <- detectCores() - 1
  }
  cl <- makeCluster(cores, outfile = "")
  registerDoParallel(cl)
  pb <- txtProgressBar(0, length(NAvectors), style = 3)

  results <- foreach(i = icount(length(NAvectors)), .packages = c("dplyr", "foreach", "tibble", "reshape2", "zoo")) %dopar% {

    NAs <- unlist(NAvectors[i])

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
      select(yearmon, var, k, days, treatment, mean = mean.x, sd = sd.x, -`mean.y`, -`sd.y`, err, prop)

    summary <- summary %>%
      left_join(., subset(summary, treatment == "wMiss", select = c(-`treatment`, -`k`, -`days`)), by = c("yearmon", "var")) %>%
      mutate(change_real = err.y - err.x, change_prop = prop.y - prop.x) %>%
      select(yearmon, var, k, days, treatment, mean = mean.x, sd = sd.x, -`mean.y`, -`sd.y`, err = err.x, prop = prop.x, change_real, change_prop)

    setTxtProgressBar(pb, i)
    list(summary = summary, details = month)
  }

  stopCluster(cl)
  suppressWarnings(list(summary = bind_rows(lapply(results, "[[", 1)), details = bind_rows(lapply(results, "[[", 2))))
}

##' @title Trim daily data to a desired start and end year
##'
##' @description Trim a daily data set to a desired start and end year. Originally written for data obtained using the \code{canadaHCD} package.
##'
##' @param data data.frame; define the data that should be trimmed
##' @param start numerical; the first year to include
##' @param end numerical; the last year to include
##'
##' @author Conor I. Anderson
##'
##' @export
##'
##' @examples
##' \dontrun{trimData(tor_dly, 1991, 2010)}

trimData <- function(data, start, end) {

  dat <- data

  # Make sure the Day, Month, and Year are at the start of the table.
  dat$Date <- as.Date(dat$Date)
  Year <- format(dat$Date, format = "%Y")
  dat <- cbind(Year, dat)

  start <- grep(start, dat$Year)
  if (length(start) > 0) {
    start <- min(start)
  } else start <- 1
  end <- grep(end, dat$Year)
  if (length(end) > 0) {
    end <- max(end)
  } else end <- nrow(dat)
  if (start > 1 | end < nrow(dat)) {
    dat <- subset(dat[start:end,2:ncol(dat)])
    return(dat)
  } else return(data)
}

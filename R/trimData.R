##' @title Trim daily data to a desired start and end year
##'
##' @description Trim a daily data set to a desired start and end year. Originally written for data obtained using the \code{canadaHCD} package.
##'
##' @param datain data.frame; define the data that should be trimmed
##' @param start numerical; the first year to include
##' @param end numerical; the last year to include
##' @param column integer or character; the number or the name of the date column
##'
##' @author Conor I. Anderson
##'
##' @export
##'
##' @examples
##' \dontrun{trimData(tor_dly, 1991, 2010)}

trimData <- function(datain, start, end, column = "Date") {

  if (inherits(column, "integer")) {
    col <- column
  } else if (inherits(column, "character")) {
    col <- which(names(datain) == column)
  } else {
    stop("We couldn't identify the right column.")
  }

  dat <- datain

  # Make sure the Day, Month, and Year are at the start of the table.
  dat[[col]] <- as.Date(dat[[col]])

  start <- which(format(dat[[col]], format = "%Y") == start)
  if (length(start) > 0) {
    start <- min(start)
  } else start <- 1
  end <- which(format(dat[[col]], format = "%Y") == end)
  if (length(end) > 0) {
    end <- max(end)
  } else end <- nrow(dat)
  if (start > 1 | end < nrow(dat)) {
    dat <- subset(dat[start:end,])
    return(dat)
  } else return(datain)
}

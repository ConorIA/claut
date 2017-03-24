#' @title Parse gridded ASCII data
#'
#' @description This function was originally written to read in the NOAA GHCN Merged gridded data set "ncdc-merged-sfc-mntp.dat". It has been written such that it \emph{should} be applicable to other gridded data sets in the ASCII format.
#'
#' @param filename character; the name of the data text data file
#' @param dimensions integer; optionally specify the dimensions of each set of data, e.g. \code{c(36, 72)}
#' @param lat_range numeric; a vector of length 2 specifying the latitude of the gridded data, defaults to \code{c(-87.5, 87.5)}
#' @param lon_range numeric; a vector of length 2 specifying the longitude of the gridded data, defaults to \code{c(-177.5, 177.5)}
#' @param gridsize numeric; the size of the grids, in degrees, defaults to \code{5}.
#' @param separator character; the format of the row that is used to label each entry, defaults to \code{c("month", "year")}
#' @param start numeric; the first entry in the separator rows, defaults to \code{c(1, 1880)}
#' @param format Boolean; whether the data should be cleaned, defaults to \code{TRUE}
#' @param missing_label character or numeric; the label used to mark missing values (only used if format is \code{TRUE}), defaults to \code{-9999}
#' @param scaling numeric; the value with which values have been scaled (only used if format is \code{TRUE}), defaults to \code{100}
#' @param label Boolean; whether the resulting matrix should have the \code{dimnames} set, defaults to \code{TRUE}
#'
#' @return matrix
#'
#' @importFrom readr read_table
#' @importFrom utils count.fields setTxtProgressBar txtProgressBar
#' @importFrom zoo as.yearmon
#'
#' @export
#'

parse_ASCII_grid <- function(filename, dimensions,
                             lat_range = c(-87.5, 87.5),
                             lon_range = c(-177.5, 177.5),
                             gridsize = 5,
                             separator = c("month", "year"),
                             start = c(1, 1880),
                             format = TRUE, missing_label = -9999,
                             scaling = 100,
                             label = TRUE) {

  fields <- count.fields(filename)
  daterows <- which(fields == length(separator))

  if (missing(dimensions)) {
    dimensions = c(max(unlist(rle(fields == length(separator)))), max(fields))
  }

  dataout <- array(NA, dim = c(dimensions[1], dimensions[2], length(daterows)))

  print("Parsing data.")
  prog <- txtProgressBar(min = 0, max = length(daterows), style = 3)
  on.exit(close(prog))

  for(i in 1:length(daterows)) {
    datain <- read_table(filename, col_names = FALSE, skip = daterows[i], n_max = 36)

    if (format) {
      for (col in 1:ncol(datain)) {
        badrows <- which(datain[[col]] == missing_label)
        datain[badrows,col] <- NA
      }
      if (scaling > 1L) datain <- datain/scaling
    }
    setTxtProgressBar(prog, value = i)
    dataout[, , i] <- as.matrix(datain)
  }

  if (label) {
    lons <- seq(lon_range[1], lon_range[2], by = gridsize)
    lats <- rev(seq(lat_range[1], lat_range[2], by = gridsize))

    test <- which(separator == "year")
    if (length(test) > 0) {
      # We have yearly data at least
      year <- start[test]
    } else year <- NULL

    test <- which(separator == "month")
    if (length(test) > 0) {
      # We have monthly data it seems
      month <- sprintf("%02d", start[test])
    } else month <- NULL

    test <- which(separator == "day")
    if (length(test) > 0) {
      # We have daily data it seems
      day <- sprintf("%02d", start[test])
    } else day <- NULL

    if (!is.null(day) && !is.null(month) && !is.null(year)) {
      startdate <- as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")
      datelist <- seq(startdate, startdate + length(daterows)-1, by = 1)
    } else {
      if (is.null(day) && !is.null(month) && !is.null(year)) {
        startdate <- as.yearmon(paste(year, month, sep = "-"), format = "%Y-%m")
        datelist <- seq(startdate, startdate + (length(daterows)-1)/12, by = 1/12)
      } else {
        if (is.null(day) && is.null(month) && !is.null(year)) {
          datelist <- seq(year, year + length(daterows)-1, by = 1)
        } else {
          datelist <- seq(1, length(daterows), by = 1)
        }
      }
    }

    dimnames(dataout) <- list(as.character(lats), as.character(lons), as.character(datelist))
  }
  dataout
}

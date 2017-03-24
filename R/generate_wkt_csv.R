#' @title Generate WKT-formatted polygons for gridded parsed ASCII data
#'
#' @description This function creates a \code{.csv} file from parsed gridded ASCII data, with a properly formatted WKT polygon definition for each grid.
#'
#' @param datain object; a matrix of parsed gridded ASCII data generated using \code{\link{parse_ASCII_grid}}
#' @param name character; the name of the matrix (or matrices) to generate \code{.csv} files for, e.g. \code{"Dec 2013"}
#' @param filename character; the name to save the file under
#' @param calc_mean Boolean; if the vector of names has \code{length(name) > 1}, the we will calculate the average for the selected matrices
#' @param remove_missing Boolean; whether we should remove missing values in the mean calculation
#'
#' @export
#'
#' @importFrom gdata unmatrix
#' @importFrom tibble tibble
#' @importFrom readr write_csv
#'
#' @examples
#' # Generate an anomaly maps for Winter 2013-14
#' \dontrun{generate_wkt_csv(dat, c("Dec 2013", "Jan 2014", "Feb 2014"))}

generate_wkt_csv <- function(datain, name, filename, calc_mean = TRUE, remove_missing = TRUE) {

  mat <- which(unlist(dimnames(datain)[3]) %in% name)

  if (length(mat) > 1 && isTRUE(calc_mean)) {
    datain <- apply(simplify2array(datain[,,mat]), c(1, 2), mean, na.rm = remove_missing)
    unmat <- unmatrix(datain)
    unmat[is.nan(unmat)] <- NA
  } else {
    unmat <- unmatrix(datain[,,mat])
  }

  coords <- strsplit(names(unmat), ":")
  lats <- lons <- NULL

  for (i in 1:length(coords)) {
    lats <- c(lats, coords[[i]][1])
    lons <- c(lons, coords[[i]][2])
  }

  tr <- paste((as.numeric(lons)+2.5), (as.numeric(lats)+2.5))
  tl <- paste((as.numeric(lons)-2.5), (as.numeric(lats)+2.5))
  br <- paste((as.numeric(lons)+2.5), (as.numeric(lats)-2.5))
  bl <- paste((as.numeric(lons)-2.5), (as.numeric(lats)-2.5))

  wkt <- paste0("POLYGON((",paste(br, tr, tl, bl, sep = ","),"))")

  if (length(name) > 1) name <- paste(name, collapse = "-")
  if (missing(filename)) filename <- paste0(name, ".csv")

  write_csv(tibble(WKT = wkt, Anomaly = unmat), filename, na = "")

}

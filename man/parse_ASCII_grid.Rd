% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_ASCII_grid.R
\name{parse_ASCII_grid}
\alias{parse_ASCII_grid}
\title{Parse gridded ASCII data}
\usage{
parse_ASCII_grid(filename, dimensions, lat_range = c(-87.5, 87.5),
  lon_range = c(-177.5, 177.5), gridsize = 5, separator = c("month",
  "year"), start = c(1, 1880), format = TRUE, missing_label = -9999,
  scaling = 100, label = TRUE)
}
\arguments{
\item{filename}{character; the name of the data text data file}

\item{dimensions}{integer; optionally specify the dimensions of each set of data, e.g. \code{c(36, 72)}}

\item{lat_range}{numeric; a vector of length 2 specifying the latitude of the gridded data, defaults to \code{c(-87.5, 87.5)}}

\item{lon_range}{numeric; a vector of length 2 specifying the longitude of the gridded data, defaults to \code{c(-177.5, 177.5)}}

\item{gridsize}{numeric; the size of the grids, in degrees, defaults to \code{5}.}

\item{separator}{character; the format of the row that is used to label each entry, defaults to \code{c("month", "year")}}

\item{start}{numeric; the first entry in the separator rows, defaults to \code{c(1, 1880)}}

\item{format}{Boolean; whether the data should be cleaned, defaults to \code{TRUE}}

\item{missing_label}{character or numeric; the label used to mark missing values (only used if format is \code{TRUE}), defaults to \code{-9999}}

\item{scaling}{numeric; the value with which values have been scaled (only used if format is \code{TRUE}), defaults to \code{100}}

\item{label}{Boolean; whether the resulting matrix should have the \code{dimnames} set, defaults to \code{TRUE}}
}
\value{
matrix
}
\description{
This function was originally written to read in the NOAA GHCN Merged gridded data set "ncdc-merged-sfc-mntp.dat". It has been written such that it \emph{should} be applicable to other gridded data sets in the ASCII format.
}

#' Generate table GCM anomaly data
#'
#' @param dir character; path to the directory containing the \code{.nc} files you wish to process
#' @param lat numeric; the latitude of the target cell
#' @param lon numeric; the longitude of the target cell
#' @param timeslice numeric; 1-16: annual (0), monthly (1-12), seasonal (13-16)
#' @param baseline the baseline on which the check the data (defaults to 1971:2000)
#' @param proj list containing vectors of the projection periods (defaults to 2020s, 2050s, and 2080s)
#' @param simple_names Boolean; whether to simplify the projection interval names (ie. 2020s instead of 2011-2040)
#' @param calc_anom Boolean; whether to calculate anomaly values.
#' @param ensemble_average Boolean; whether to average different ensembles for the same variable/model/scenario.
#'
#' @return a class \code{data.frame}
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarize
#' @importFrom RNetCDF open.nc print.nc utcal.nc var.get.nc
#' @importFrom tibble as_tibble tibble
#' @importFrom utils capture.output
#' @importFrom zoo as.yearmon
#'
#' @examples
#' \dontrun{annual <- gcm_anomalies(getwd(), -7.023833, -76.465222, 0, 1971:2000, calc_anoms = TRUE)}

gcm_anomalies <- function(dir = getwd(), lat, lon, timeslice = 0, baseline, proj = list(2011:2040, 2041:2070, 2071:2100), simple_names = FALSE, calc_anom = TRUE, ensemble_average = TRUE) {

  files_list <- dir(dir)
  files_list <- files_list[grep("*.nc", files_list)]

  dat <- as_tibble(matrix(NA, nrow = length(files_list), ncol = 8))

  for (nc_file in seq_along(files_list)) {
    components <- unlist(strsplit(files_list[nc_file], "_"))
    var <- gsub("hist/", "", components[1])
    model <- components[3]
    scenario <- components[4]
    ensemble <- components[5]

    nc_nc <- open.nc(file.path(dir, files_list[nc_file]))

    nc_summary <- capture.output(print.nc(nc_nc))
    time_string <- sub(".*?time:units = \\\"(.*?)\\\".*", "\\1", nc_summary[grep("time:units =", nc_summary)])

    nc_var <- var.get.nc(nc_nc, var)
    nc_time <- as.yearmon(utcal.nc(time_string, var.get.nc(nc_nc, "time"), type="s"))
    nc_lat <- var.get.nc(nc_nc, "lat")
    nc_lon <- var.get.nc(nc_nc, "lon")

    if (min(nc_lon >= 0)) lon <- ifelse(lon <0, 360 + lon, lon)

    lat_step <- (nc_lat[2] - nc_lat[3])/2
    lat_cell <- which(nc_lat + lat_step < lat & nc_lat - lat_step > lat)

    lon_step <- (nc_lon[2] - nc_lon[3])/2
    lon_cell <- which(nc_lon + lon_step < lon & nc_lon - lon_step > lon)

    time_series <- tibble(Time = nc_time, Year = format(as.yearmon(Time), format = "%Y"),
                          Month = format(as.yearmon(Time), format = "%m"),
                          Var = (nc_var[lon_cell, lat_cell, ] - 273.15))

    if (timeslice > 0 & timeslice < 13) {
      time_series <- filter(time_series, Month == sprintf("%02d", timeslice))
    } else if (timeslice > 12 & timeslice < 17) {
      if (timeslice == 13) {
        time_series <- filter(time_series, Month %in% sprintf("%02d", c(1, 2, 12)))
        time_series$Year[time_series$Month == sprintf("%02d", 12)] <- as.numeric(time_series$Year[time_series$Month == sprintf("%02d", 12)]) + 1
      }
      if (timeslice == 14) {
        time_series <- filter(time_series, Month %in% sprintf("%02d", c(3, 4, 5)))
      }
      if (timeslice == 15) {
        time_series <- filter(time_series, Month %in% sprintf("%02d", c(6, 7, 8)))
      }
      if (timeslice == 16) {
        time_series <- filter(time_series, Month %in% sprintf("%02d", c(9, 10, 11)))
      }
    }

    result <- time_series %>% group_by(Year) %>% summarize(Mean = mean(Var))

    result$Year <- as.numeric(result$Year)
    periods <- rep(NA, length(proj))

    if (scenario == "historical") {
      baseln <- mean(result$Mean[result$Year >= min(baseline) & result$Year <= max(baseline)])
    } else {
      baseln <- NA
      for (i in seq_along(proj)) {
        periods[i] <- mean(result$Mean[result$Year >= min(proj[[i]]) & result$Year <= max(proj[[i]])])
      }
    }

    period_names <- rep(NA, length(periods))
    for (i in seq_along(proj)) {
      period_names[i] <- if(simple_names) paste0(mean(proj[[i]]) %/% 10 * 10, "s") else paste0(min(proj[[i]]), "-", max(proj[[i]]))
    }

    col_names <- c("Variable", "Model", "Scenario", "Ensemble", paste0("Baseline ", min(baseline), "-", max(baseline)), period_names)
    row <- list(var, model, scenario, ensemble, baseln)
    row <- c(row, periods)

    names(row) <- col_names

    dat[nc_file,] <- row
    colnames(dat) <- col_names
  }
  if (calc_anom) dat <- calc_anoms(dat)
  if (ensemble_average) dat <- ensemble_means(dat)
  dat
}

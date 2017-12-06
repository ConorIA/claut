#' Generate table GCM anomaly data
#'
#' @param dir character; path to the directory containing the \code{.nc} files you wish to process
#' @param files character; list of filenames to process. Any list item of >1 will be processed sequentially.
#' @param lat numeric; the latitude of the target cell
#' @param lon numeric; the longitude of the target cell
#' @param timeslice numeric; 1-16: annual (0), monthly (1-12), seasonal (13-16)
#' @param baseline the baseline on which the check the data (defaults to 1971:2000)
#' @param start projection start year (defaults to 2011)
#' @param simple_names Boolean; whether to simplify the projection interval names (ie. 2020s instead of 2011-2040)
#' @param calc_anom Boolean; whether to calculate anomaly values.
#' @param ensemble_average Boolean; whether to average different ensembles for the same variable/model/scenario.
#' @param cache_dir character; a directory to cache results (defaults to NULL, i.e. no cache.)
#'
#' @return a class \code{data.frame}
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter group_by summarize
#' @importFrom ncdf4 nc_close nc_open ncvar_get
#' @importFrom ncdf4.helpers get.split.filename.cmip5 nc.get.time.series nc.get.var.subset.by.axes
#' @importFrom storr storr_rds
#' @importFrom tibble as_tibble tibble
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom zoo as.yearmon
#'
#' @examples
#' \dontrun{annual <- gcm_anomalies(getwd(), -7.023833, -76.465222, 0, 1971:2000, calc_anom = TRUE)}

gcm_anomalies <- function(dir = getwd(), files, lat, lon, timeslice = 0, baseline, start = 2011, simple_names = FALSE, calc_anom = TRUE, ensemble_average = TRUE, cache_dir = NULL) {

  if(!is.null(cache_dir)) {
    use_storr = TRUE
    sto <- storr_rds(cache_dir)
  }

  ## Generate projection periods based on baseline length and start year
  number_of_periods <- (2100 - start + 1) %/% length(baseline)
  proj <- list()
  period_names <- rep(NA, number_of_periods)
  for (per in 1:number_of_periods) {
    st <- start + (length(baseline) * (per - 1))
    ed <- start + (length(baseline) * per) - 1
    period <- list(st:ed)
    period_names[per] <- if(simple_names) paste0(mean(unlist(period)) %/% 10 * 10, "s") else paste0(st, "-", ed)
    proj <- c(proj, period)
  }

  ## Prepare a table with the right dimensions
  dat <- as_tibble(matrix(NA, nrow = length(files), ncol = 5 + number_of_periods))

  # Process each NetCDF file
  print("Processing the NetCDF files...")
  ## Set up a progress Bar
  prog <- txtProgressBar(min = 0, max = length(files), style = 3)
  on.exit(close(prog))

  for (file_list in seq_along(files)) {
    fs <- unlist(files[file_list])
    for (f in 1:length(fs)) {

      components <- get.split.filename.cmip5(file.path(dir, fs[f]))
      var <- unname(components['var'])
      model <- unname(components['model'])
      scenario <- unname(components['emissions'])
      ensemble <- unname(components['run'])
      nc_nc <- nc_open(file.path(dir, fs[f]))

      if (f == 1) {
        # Get the bounds of each cell
        lon_bnds <- try(ncvar_get(nc_nc, "lon_bnds"), silent = TRUE)
        if (inherits(lon_bnds, "try-error")) {
          lon_bnds <- ncvar_get(nc_nc, "lon_bounds")
        }
        lat_bnds <- try(ncvar_get(nc_nc, "lat_bnds"), silent = TRUE)
        if (inherits(lat_bnds, "try-error")) {
          lat_bnds <- ncvar_get(nc_nc, "lat_bounds")
        }

        # Convert Western longitudes to degrees East
        lon <- ifelse(lon <0, 360 + lon, lon)

        # Get the grid cell we are interested in
        lat_index <- which(lat_bnds[1,] <= lat & lat_bnds[2,] >= lat)
        lon_index <- which(lon_bnds[1,] <= lon & lon_bnds[2,] >= lon)

        rm(lat_bnds, lon_bnds); gc()
      }

      # Check the cache
      key <- paste(fs[f], paste(lat_index, collapse = "-"), paste(lon_index, collapse = "-"), sep = "_")
      time_tmp <- NULL

      if (use_storr) {
        if (sto$exists(key)) {
        print("Hit the cache")
        time_tmp <- sto$get(key)
        }
      }

      if (is.null(time_tmp)) {
        print("Missed the cache")

        # Get the time
        nc_time <- nc.get.time.series(nc_nc, v = var, time.dim.name = "time")
        nc_time <- as.yearmon(format(nc_time, format = "%Y-%m-%d hh:mm:ss"))

        # Now load only that grid data
        nc_var <- nc.get.var.subset.by.axes(nc_nc, var, axis.indices = list(X = lon_index, Y = lat_index))

        # Close the nc connection
        nc_close(nc_nc); rm(nc_nc)

        if (dim(nc_var)[1] > 1 || dim(nc_var)[2] > 1) {
          warning("We got two latitud or longitude cells. We'll average across them.")
          nc_var <- apply(nc_var, 3, mean)
        } else {
          nc_var <- nc_var[1, 1, ]
        }

       if (var %in% c("tas", "tasmax", "tasmin")) {
          nc_var <- nc_var - 273.15
        } else {
          stop("Sorry, for now I only work with temperature!")
        }
        time_tmp <- tibble(Time = nc_time, Year = format(as.yearmon(Time), format = "%Y"),
                           Month = format(as.yearmon(Time), format = "%m"),
                           Var = nc_var)

        rm(nc_var, nc_time); gc()
        if(use_storr) sto$set(key, time_tmp)
      }

      if (f == 1) {
        time_series <- time_tmp
      } else {
        time_series <- suppressWarnings(bind_rows(time_series, time_tmp))
      }
    }

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

    col_names <- c("Variable", "Model", "Scenario", "Ensemble", paste0("Baseline ", min(baseline), "-", max(baseline)), period_names)
    row <- list(var, model, scenario, ensemble, baseln)
    row <- c(row, periods)

    names(row) <- col_names

    dat[file_list,] <- row
    colnames(dat) <- col_names

    setTxtProgressBar(prog, value = file_list)
  }

  # Compute additional metrics
  if (calc_anom) dat <- calc_anoms(dat)
  if (ensemble_average) dat <- ensemble_means(dat)

  # Return our tibble
  dat %>% arrange(Model, Scenario, Ensemble)
}

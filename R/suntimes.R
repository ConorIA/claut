#' Sunrise, solar noon, and sunset times
#'
#' Calculations are based on NOAA's solar calculator, which is, in turn, based on Astronomical Algorithms, by Jean Meeus.
#'
#' @param lat numeric; the latitude of the observer
#' @param lon numeric; the longitude of the observer
#' @param tz integer; the number of hours shifted from GMT, e.g. for GMT-5, pass \code{-5}
#' @param date date; the date to get the sun times for
#' @param out character; one of "sunrise", "solar_noon", or "sunset". Leaving this parameter blank will retrun a list of all three.
#'
#' @source \insertRef{noaa_esrl_solar}{claut}
#'
#' @return POSIXct date time for selected sun time
#'
#' @importFrom lubridate force_tz
#' @export
#'
#' @examples
#' ## Get today's sunrise time for Toronto Pearson Airport
#' suntimes(lat = 43.68, lon = -79.63, tz = -5, date = Sys.Date())

suntimes <- function(lat, lon, tz, date, out = c("sunrise", "solar_noon", "sunset")) {

  date <- force_tz(date, "GMT")

  deg2rad <- function(deg) {
    deg * (pi/180)
  }

  rad2deg <- function(rad) {
    rad * (180/pi)
  }

  chk2time <- function(chk, tz) {
    remainder <- chk * 24
    hrs <- sprintf("%02d", remainder %/% 1)
    remainder <- (remainder %% 1) * 60
    mins <- sprintf("%02d", remainder %/% 1)
    remainder <- (remainder %% 1) * 60
    secs <- sprintf("%02d", remainder %/% 1)
    as.POSIXct(paste(date,
                     paste(hrs, mins, secs, sep = ":")),
               tz = paste0("Etc/GMT", tz))
  }

  jdate <- julian(date, -2440588)
  jcen <- (jdate - 2451545) / 36525
  geom_mean_long_sun <- (280.46646 + jcen * (36000.76983 + jcen * 0.0003032)) %% 360
  geom_mean_anom_sun <- 357.52911 + jcen * (35999.05029-0.0001537 * jcen)
  eccent_earth_orbit <- 0.016708634 - jcen * (0.000042037 + 0.0000001267 * jcen)
  sun_eq_of_crt <- sin(deg2rad(geom_mean_anom_sun)) * (1.914602 - jcen * (0.004817 + 0.000014 * jcen)) + sin(deg2rad(2 * geom_mean_anom_sun)) * (0.019993 - 0.000101 * jcen) + sin(deg2rad(3 * geom_mean_anom_sun)) * 0.000289
  sun_true_long <- geom_mean_long_sun + sun_eq_of_crt
  sun_app_long <- sun_true_long - 0.00569 - 0.00478*sin(deg2rad(125.04-1934.136 * jcen))
  mean_obliq_eliptic <- 23 + (26 + ((21.448 - jcen * (46.815 + jcen * (0.00059 - jcen * 0.001813)))) / 60) / 60
  obliq_corr <- mean_obliq_eliptic + 0.00256 * cos(deg2rad(125.04 - 1934.136 * jcen))
  var_y <- tan(deg2rad(obliq_corr / 2)) * tan(deg2rad(obliq_corr / 2))
  sun_declin <- rad2deg(asin(sin(deg2rad(obliq_corr)) * sin(deg2rad(sun_app_long))))
  ha_sunrise <- rad2deg(acos(cos(deg2rad(90.833)) / (cos(deg2rad(lat)) * cos(deg2rad(sun_declin))) - tan(deg2rad(lat)) * tan(deg2rad(sun_declin))))
  eq_time_mins <- 4 * rad2deg(var_y * sin(2 * deg2rad(geom_mean_long_sun)) - 2 * eccent_earth_orbit * sin(deg2rad(geom_mean_anom_sun)) + 4 * eccent_earth_orbit * var_y * sin(deg2rad(geom_mean_anom_sun)) * cos(2 * deg2rad(geom_mean_long_sun)) - 0.5 * var_y * var_y * sin(4 * deg2rad(geom_mean_long_sun)) - 1.25 * eccent_earth_orbit * eccent_earth_orbit * sin(2 * deg2rad(geom_mean_anom_sun)))
  solar_noon <- (720 - 4 * lon-eq_time_mins + tz * 60)/1440

  sunrise <- solar_noon - ha_sunrise * 4 / 1440
  sunset <- solar_noon + ha_sunrise * 4 / 1440

  if (length(out) == 1 && out %in% c("sunrise", "solar_noon", "sunset")) {
    chk2time(get(out), tz)
  } else {
    list(sunrise = chk2time(sunrise, tz),
       solar_noon = chk2time(solar_noon, tz),
       sunset = chk2time(sunset, tz))
  }
}

#' Calculate the anomaly values for a table produced by gcm_anomalies()
#'
#' @param datin a \code{tbl_df} produced by gcm_anomalies().
#'
#' @importFrom dplyr filter
#' @importFrom tibble has_name
#' @export

calc_anoms <- function(datin) {
  dat_proj <- filter(datin, Scenario != "historical")
  dat_hist <- filter(datin, Scenario == "historical")

  if (has_name(datin, "Ensembles")) {
    stop("It seems you already averaged the ensembles. This function needs to be run first.")
  }

  for (i in 1:nrow(dat_hist)) {
    dat_proj[dat_proj$Model == dat_hist$Model[i] & dat_proj$Variable == dat_hist$Variable[i] & dat_proj$Ensemble == dat_hist$Ensemble[i], 5] <- unlist(dat_hist[i, 5])
    for (col in 6:ncol(dat_proj)) {
      rows <- which(dat_proj$Model == dat_hist$Model[i] & dat_proj$Variable == dat_hist$Variable[i] & dat_proj$Ensemble == dat_hist$Ensemble[i])
      for (row in rows) {
        dat_proj[row, col] <- dat_proj[row, col] - dat_hist[i, 5]
      }
    }
  }
  dat_proj[!is.na(dat_proj[[5]]),]
}

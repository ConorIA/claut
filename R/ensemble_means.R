#' Calculate the average of all ensembles for the same Variable/Model/Scenario
#'
#' @param datin a \code{tbl_df} produced by gcm_anomalies().
#'
#' @importFrom dplyr group_by left_join select summarize summarize_all
#' @importFrom magrittr %>%
#' @export

ensemble_means <- function(datin) {
  ensembles <- datin %>% group_by(Variable, Model, Scenario) %>% summarize(Ensembles = list(unique(Ensemble)))
  summaries <- datin %>% select(-4) %>% group_by(Variable, Model, Scenario) %>% summarize_all(mean)
  datout <- left_join(ensembles, summaries)
  datout
}

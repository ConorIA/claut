#' Generate a list of GCM files to pass to \code{gcm_anomalies()}
#'
#' @param dir character; the directory containing the files.
#' @param baseline numeric; a vector years that should be covered by history (\code{NULL} to ignore)
#' @param projection numeric; a vector years that should be covered by the models (\code{NULL} to ignore)
#' @param scenario character; the scenario to filter on (optional)
#' @param ensemble character; the ensemble to fulter on (optional)
#'
#' @return a list of filenames
#'
#' @importFrom tibble add_column as_tibble
#' @importFrom dplyr count filter
#' @importFrom magrittr %>%
#' @importFrom ncdf4.helpers get.split.filename.cmip5
#' @importFrom zoo as.yearmon
#'
#' @export
#'
#' @examples
#' \dontrun{get_gcm_files(getwd(), 1971:2000, scenario = c("rcp85", "rcp45", "rcp26", "rcp60"), ensemble = "r1i1p1")}

get_gcm_files <- function(dir, baseline = NULL, projection = NULL, scenario = NULL, ensemble = NULL) {

  # If we want both the baseline and the projections, we'll run this twice, once for each period.
  if (!is.null(baseline) && !is.null(projection)) {
    baseline_files <- get_gcm_files(dir, baseline, NULL, scenario, ensemble)
    projection_files <- get_gcm_files(dir, NULL, projection, scenario, ensemble)
    return(append(baseline_files, projection_files))
  }

  if (is.null(baseline)) period <- projection
  if (is.null(projection)) {
    period <- baseline
    # There are no experiments in the past.
    scenario <- "historical"
  }

  # Get the available files
  filenames <- dir(dir)
  choices <- as_tibble(t(sapply(filenames, get.split.filename.cmip5)))
  names(choices) <- c("Variable", "Period", "Model", "Scenario", "Ensemble", "Range", "Start", "End")
  choices <- choices %>% add_column(Filenames = filenames)

  # Narrow down our choices
  filtered <- choices
  if (!is.null(scenario)) filtered <- filtered %>% filter(Scenario %in% scenario)
  if (!is.null(ensemble)) filtered <- filtered %>% filter(Ensemble %in% ensemble)

  # A lot of useful model output is split into multiple files
  multi_models <- filtered %>% group_by(Scenario, Ensemble) %>% count(Model) %>% filter(n > 1)
  groupings <- rep(list(NA), nrow(multi_models))

  for (row in 1:nrow(multi_models)){
    tab <- filtered %>% filter(Model == multi_models$Model[row], Scenario == multi_models$Scenario[row], Ensemble == multi_models$Ensemble[row])

    # If any file for this model gives us all the data we need, we'll skip this row
    if (nrow(tab[tab$Start <= paste0(period[1], "01") & tab$End >= paste0(period[length(period)], "12"),]) > 0) {
      next
    }

    # If the data starts too late or ends too soon, we'll skip this row
    if (max(tab$End) < paste0(period[length(period)], "12")) {
      next
    }
    if (min(tab$Start) > paste0(period[1], "01")) {
      next
    }

    # If each file starts one year-month after the previous, we're happy!
    for (file in seq((max(which(tab$Start <= paste0(period[1], "01"))) + 1), min(which(tab$End >= paste0(period[length(period)], "12"))))) {
      if (as.yearmon(tab$Start[file], format = "%Y%m") == (as.yearmon(tab$End[file - 1], format = "%Y%m") + 1/12)) {
        if (is.na(groupings[row])) {
          groupings[row] <- list(c(tab$Filenames[file - 1], tab$Filenames[file]))
        } else {
          groupings[row] <- list(c(unlist(groupings[row]), tab$Filenames[file]))
        }
      } else {
        groupings[row] <- NA
      }
    }

  }

  # Spit out the groupings, generated above, as well as any file that has all the data we need.
  append(groupings[!is.na(groupings)], filtered$Filenames[filtered$Start <= paste0(period[1], "01") & filtered$End >= paste0(period[length(period)], "12")])
}

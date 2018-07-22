#' Describe run-length-encoded events
#'
#' Given an object of class \code{rle}, prints a vector of the length of
#' \code{sum(run_lengths$lengths)}, with each length repeated. It is useful to
#' annotate continuous events in a data frame.
#'
#' If passed a function along with an additional vector. The function is applied
#' to the values of the vector when each event is \code{TRUE}.
#'
#' @param run_lengths an object of class \code{rle}, of some criteria for an "event"
#' @param threshold the minimum length of the events to consider (default: 2)
#' @param vec (optional) a vector of data for which \code{f} should be applied
#' @param f (optional) a function to apply to the values of \code{vec} where events are \code{TRUE}.
#' @param ... additional parameters to be passed to \code{f}
#'
#' @return a vector
#' @export
#'
#' @examples
#' # Create some "weather"
#' weather <- rnorm(1000)
#' # Get the lengths of "cold" events where the temperature is below zero for more than 3 days
#' describe_snaps(rle(weather < 0), 3)

describe_snaps <- function(run_lengths, threshold = 2, vec, f = NULL, ...) {
    run_lengths$values[run_lengths$lengths < threshold] <- FALSE

    if (!inherits(f, "function")) {
      unname(unlist(apply(data.frame(lengths = run_lengths$lengths,
                                     values = run_lengths$values),
                          1, function(x) {
                            if (!is.na(x[2]) & x[2]) {
                              rep(x[1], times = x[1])
                            } else {
                              rep(NA, times = x[1])
                            }
                          })))
    } else {
      unname(unlist(apply(data.frame(
        lengths = run_lengths$lengths,
        values = run_lengths$values,
        index = 1:length(run_lengths[[1]])
      ),
      1, function(x) {
        if (!is.na(x[2]) & x[2]) {
          pull <- c(rep(FALSE, sum(run_lengths$lengths[1:(x[3] - 1)])),
                    rep(TRUE, x[1]))
          pull <-
            c(pull, rep(FALSE, length(vec) - length(pull)))
          rep(f(vec[pull]), x[1])
        } else {
          rep(NA, times = x[1])
        }
      })))
    }
  }

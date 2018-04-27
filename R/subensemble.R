#' Find optimal sub-ensembles
#'
#' @param k integer; the sub-ensemble size(s) to test
#' @param datain numeric; a vector or data.frame containing the GCM anomalies for each baseline series
#' @param target numeric; a vector of the baseline averaegs to test against
#' @param stdev numeric; the standard deviation of the annual average baseline value to test against
#' @param model_names Boolean; whether the datain includes a column of model names
#' @param single_result Boolean; whether to return one optimal sub-ensemble size
#' @param parallel Boolean; whether to perform operations in parallel
#' @param no_cores integer; the numer of CPU cores to use for parallel processing
#'
#' @return a data.frame with one row for each sub-ensemble size
#' 
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom lpSolve lp
#' 
#' @export
#'

subensemble = function(k, datain, target, stdev = NULL, model_names = TRUE, single_result = FALSE, parallel = FALSE, no_cores = parallel::detectCores() - 1) {
  
  # If the data is passed as a vector, we coerce it to a data.frame
  if (!inherits(datain, "data.frame")) {
    datain <- as.data.frame(datain)
    model_names <- FALSE
  }
  
  # If `k` is not provided, try all levels
  if (missing(k)) {
    k <- 1:nrow(datain)
  }
  
  # If `k` exceeds the number of models, throw an error
  if (max(k) > nrow(datain)) {
    stop("You have requested more models than are present in the data set.")
  }
  
  # Drop the model names if the column was included
  dat <- if (model_names) datain[2:ncol(datain)] else datain
  
  # Throw an error if we have more variables than we have targets
  if (ncol(dat) != length(target)) {
    stop("The number of columns does not match the number of targets!")
  }
  
  n = nrow(dat)
  
  if (parallel) registerDoParallel(no_cores)
  
  `%infix%` <- ifelse(parallel, `%dopar%`, `%do%`)
  
  result <- foreach(lvl_k = k, .combine = rbind) %infix% {
    objective.in = c(rep(0, n), lvl_k)
    
    for (i in 1:ncol(dat)) {
      if (i == 1) {
        const.mat <- rbind(c(dat[[i]],-1), c(dat[[i]],+1))
      } else {
        const.mat <- rbind(const.mat, c(dat[[i]],-1), c(dat[[i]],+1))
      }
    }
    const.mat <- rbind(const.mat, c(rep(1,n),0))
    const.dir <- c(rep(c("<",">"), ncol(dat)),"=")
    
    const.rhs <- NULL
    for (i in 1:ncol(dat)) {
      const.rhs <- c(const.rhs, target[i]*lvl_k, target[i]*lvl_k)
    }
    const.rhs <- c(const.rhs, lvl_k)
    
    v <- lp("min", objective.in, const.mat, const.dir, const.rhs, binary.vec = 1:n)
    a <- v$solution[1:n]
    
    residuals <- GFs <- NULL
    for (i in 1:ncol(dat)) {
      residual <- sum(dat[i]*a)/sum(a)-target[i]
      residuals <- c(residuals, residual)
      names(residuals)[i] <- paste("Res.", names(dat[i]))
      if(!is.null(stdev)) {
        GF <- abs(residual)/stdev[i]
        GFs <- c(GFs, GF)
        names(GFs)[i] <- paste("GF", names(dat[i]))
      }
    }
    if(ncol(dat) >1 && !is.null(stdev)) {
      avgGF <- mean(GFs)
      names(avgGF)[1] <- paste("Avg. GF")
    } else {
      avgGF <- NULL
    }
    
    row <- as.data.frame(t(c(k = lvl_k, residuals, GFs, avgGF, a = a)))
    row
  }
  
  if (single_result) {
    if ('Avg. GF' %in% colnames(result)) {
      result[which(result$`Avg. GF` == min(result$`Avg. GF`)),]
    } else {
      result[which(result[3] == min(result[3])),]
    }
  } else {
    result
  }
}
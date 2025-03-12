#' Title
#'
#' @param pars 
#' @param funk 
#' @param logobs 
#' @param ... 
#'
#' @returns
#' @export
#'
#' @examples
negativeLL = function (pars, funk, 
                       logobs,
                       # logobsI, logobsA, 
                       params, choice, time, indat, rtn_catch, rtn_npv) {
  npar <- length(pars)
  
  
  logpred <- funk(choice, pars, time, params, 
                  indat=indat, rtn_catch=TRUE, rtn_npv=FALSE)
  
  logpred <- sapply(1:time, function(i) logpred[[1]][i])
  # logpredI <- sapply(1:time, function(i) logpred[["I"]][i])
  # logpredA <- sapply(1:time, function(i) logpred[["A"]][i])
  
  LL <- -sum(dnorm(x = logobs, mean = logpred, sd = exp(pars[npar]),
                   log = TRUE))
  # LLI <- -sum(dnorm(x = logobsI, mean = logpredI, sd = exp(pars[npar]), 
  #                   log = TRUE))
  # LLA <- -sum(dnorm(x = logobsA, mean = logpredA, sd = exp(pars[npar]),
  #                   log = TRUE))
  # LL = LLI+LLA
  
  return(LL)
}

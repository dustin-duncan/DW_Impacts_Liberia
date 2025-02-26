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
negativeLL = function (pars, funk, logobs, params, choice, time, indat, rtn_catch, rtn_npv) {
  npar <- length(pars)
  
  logpred <- funk(choice, pars, time, params, 
                  indat=indat, rtn_catch=TRUE, rtn_npv=FALSE)
  
  logpred <- sapply(1:time, function(i) logpred[[1]][i])
  
  LL <- -sum(dnorm(x = logobs, mean = logpred, sd = exp(pars[npar]), 
                   log = TRUE))
  # LL2 = ifelse(LL == -Inf | LL == Inf | is.na(LL), 0, LL) 
  
  return(LL)
}

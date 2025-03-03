negativeLLcalib = function (pars, funk, logobs, params, fee, time, indat, rtn_catch, rtn_npv) {
  npar <- length(pars)
  
  logpred <- funk(choice=pars, fee, time, params, indat=NULL, 
                  rtn_catch=TRUE, rtn_npv=FALSE, ban=FALSE)
  
  logpred <- sapply(1:time, function(i) logpred[[1]][i])
  
  LL <- -sum(dnorm(x = logobs, mean = logpred, sd = exp(pars[npar]), 
                   log = TRUE))
  # LL2 = ifelse(LL == -Inf | LL == Inf | is.na(LL), 0, LL) 
  
  return(LL)
}
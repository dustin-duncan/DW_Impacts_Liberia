interaction_LL2 = function(choice, state, time, params, indat=NULL, rtn_catch=TRUE, rtn_npv=FALSE, ban=FALSE) {
  
  if(rtn_catch == TRUE & rtn_npv ==TRUE) stop("Error in args `rtn_catch`, `rtn_npv`: cannot specify both as TRUE ")
  
  B=vector(mode="numeric", length=0)
  e=vector(mode="numeric", length=0)
  E=vector(mode="numeric", length=0)
  HS=vector(mode="numeric", length=0)
  HD=vector(mode="numeric", length=0)
  benefits=vector(mode="numeric", length=0)
  PIS=vector(mode="numeric", length=0)
  catch=vector(mode="numeric", length=0)
  
  params = c(params, "fee" = as.numeric(choice))
  
  params = exp(params)
  
  state=exp(state)
  
  with(as.list(c(state, params)), {
    
    B[1] = Bi
    e[1] = ei
    E[1] = Ei
    
    HS[1] = (qS*B[1]*e[1])
    
    PIS[1] = p*HS[1] - (C1*e[1])
    
    HD[1] = qD*B[1]*E[1]
    
    benefits[1] = ((beta)*(PIS[1])) + ((1-beta)*fee*p*HD[1])
    
    catch[1] = HS[1] + HD[1]
    
    for(t in 2:time) {
      
      
      B[t] <- B[t-1] + (0.45*(B[t-1])*(1-((B[t-1])/K)))-(-mu*B[t-1])-(qS*B[t-1]*e[t-1])-(qD*B[t-1]*E[t-1])
      
      e[t] <- (phi*(-((C1*e[t-1])^2)+(p*qS*B[t-1]*e[t-1])) ) + e[t-1] 
      
      if(ban == TRUE) {
        E[t] = 0
      } else {
        E[t] <- (phi*(-((C*E[t-1])^2)+((1-fee)*p*qD*B[t-1]*E[t-1]))) + E[t-1] 
      }
      
      HS[t] = (qS*B[t]*e[t])
      
      PIS[t] <- p*HS[t] - (C1*e[t])
      
      HD[t] <- qD*B[t]*E[t]
      
      benefits[t] <- (beta*PIS[t]) + ((1-beta)*fee*p*HD[t])
      
      catch[t] = HS[t] + HD[t]
    }
    
    t=seq(from=1, to =time, by=1)
    rho = 1/(1+discount)
    pv = (rho^t)*benefits
    npv=sum(pv)
    
    if(rtn_npv == TRUE) {
      return(-npv)
    } else if(rtn_catch == TRUE) {
      return(list(I=log(HD), A=log(HS)))
    } else {
      return(tibble(time=t, B=B, e=e, E=E, HS=HS, HD=HD, PV=pv))
    }
  })
}
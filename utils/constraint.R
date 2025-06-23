#' Title
#'
#' @param choice This is the choice for an optimization function
#' @param time The timeline over which to run the program (one value)
#' @param state Initial values for Biomass in 3 zones, SSF effort in 3 zones and DWF effort in zone 1 
#' @param params Biological parameters: mu, chi,  gamma, v, Economic Parameters: qD, qS, qS, qS, C1, C2, C3, C, W, phi, p, discount 
#' @param indat 
#' @param rtn_catch TRUE if user would like time-series of total catch (used for calibrating the model), FALSE if the user would like a time series of biomass, effort, harvest, and present value 
#' @param rtn_npv TRUE if user would like to optimize Net Present Value of the fishery over time or return negative NPV as value
#' @param ban TRUE if user would like to model banning DWF entirely from fishery (starts in year 2 reflecting finishing current licenses)
#'
#' @returns
#' @export
#'
#' @examples
constraint_fx = function(choice, state, time, params, indat=NULL, rtn_catch=TRUE, rtn_npv=FALSE, ban=FALSE) {
  
  if(rtn_catch == TRUE & rtn_npv ==TRUE) stop("Error in args `rtn_catch`, `rtn_npv`: cannot specify both as TRUE ")
  
  B1=vector(mode="numeric", length=0)
  B2=vector(mode="numeric", length=0)
  B3=vector(mode="numeric", length=0)
  e1=vector(mode="numeric", length=0)
  e2=vector(mode="numeric", length=0)
  e3=vector(mode="numeric", length=0)
  E=vector(mode="numeric", length=0)
  HS=vector(mode="numeric", length=0)
  HS1=vector(mode="numeric", length=0)
  HS2=vector(mode="numeric", length=0)
  HS3=vector(mode="numeric", length=0)
  HD=vector(mode="numeric", length=0)
  benefits=vector(mode="numeric", length=0)
  PIS=vector(mode="numeric", length=0)
  catch=vector(mode="numeric", length=0)
  conb1=vector(mode="numeric", length=0)
  conb2=vector(mode="numeric", length=0)
  conb3=vector(mode="numeric", length=0)
  
  params = exp(params)
  
  # params = c(params, "fee" = as.numeric(choice))
  
  state=exp(state)
  
  with(as.list(c(state, params, choice)), {
    
    B1[1] = B1i
    B2[1] = B2i
    B3[1] = B3i
    e1[1] = e1i
    e2[1] = e2i
    e3[1] = e3i # 102410*0.34
    E[1] = Ei
    
    HS1[1] = (qS*B1[1]*e1[1])
    HS2[1] = (qS*B2[1]*e2[1]) 
    HS3[1] = (qS*B3[1]*e3[1])
    
    HS[1] = HS1[1] + HS2[1] + HS3[1]
    
    PIS[1] = p*HS[1] - ((C1*e1[1])+(C2*e2[1])+(C3*e3[1]))
    
    HD[1] = qD*B1[1]*E[1]
    
    benefits[1] = ((beta)*(PIS[1])) + ((1-beta)*choice*p*HD[1])
    
    catch[1] = HS[1] + HD[1]
    
    conb1[1] = (HS1[1]+HD[1]) - B1[1]
    conb2[1] = HS2[1] - B2[1]
    conb3[1] = HS3[1] - B3[1]
    
    
    for(t in 2:time) {
      
      recruit <- v*B3[t-1]
      
      # recruit = if(recruit < 0) 0 else recruit
      
      B1[t] <- B1[t-1] + (-mu*B1[t-1])-(qS*B1[t-1]*e1[t-1])-(qD*B1[t-1]*E[t-1])+(recruit*(1-chi))
      
      B2[t] <- B2[t-1] + (-mu*B2[t-1])-(qS*B2[t-1]*e2[t-1])+(recruit*chi)
      
      B3[t] <- B3[t-1] + (0.45*(B1[t-1]+B2[t-1])*(ifelse((B1[t-1]+B2[t-1]) > K, 0, (1-((B1[t-1]+B2[t-1])/K))))) - (qS*B3[t-1]*e3[t-1]) - recruit -(mu*B3[t-1])
      # (gamma*(B1[t-1]+B2[t-1])); (gamma*(B1[t-1]+B2[t-1])*(1-((B1[t-1]+B2[t-1])/K))); (1*(B1[t-1]+B2[t-1])*(ifelse((B1[t-1]+B2[t-1]) > K, 0, (1-((B1[t-1]+B2[t-1])/K))))); max(0.45*(spawn_bio)*(1-(spawn_bio/K)), 0)
      
      e1[t] <- (phi*(-((C1*e1[t-1])^2)+(p*qS*B1[t-1]*e1[t-1])) ) + e1[t-1] 
      
      e2[t] <- (phi*(-((C2*e2[t-1])^2)+(p*qS*B2[t-1]*e2[t-1])))  + e2[t-1] 
      
      # if((phi*(-((C1*e1[t-1])^2)+(p*qS*B1[t-1]*e1[t-1]))) < 0) {
      # e2[t] <- (phi*(-((C2*e2[t-1])^2)+(p*qS*B2[t-1]*e2[t-1])))  + e2[t-1] + (-(phi*(-((C1*e1[t-1])^2)+(p*qS*B1[t-1]*e1[t-1]))))
      # } else if((phi*(-((C1*e1[t-1])^2)+(p*qS*B1[t-1]*e1[t-1]))) >= 0) {
      # e2[t] <- (phi*(-((C2*e2[t-1])^2)+(p*qS*B2[t-1]*e2[t-1])))  + e2[t-1]
      # }
      
      e3[t] <- (phi*(-((C3*e3[t-1])^2)+(p*qS*B3[t-1]*e3[t-1]))) + e3[t-1] 
      
      # if(ban == TRUE) {
      #   E[t] = 0
      # } else if(fee > 0.9) {
      #   E[t] = (1*(-((C*E[t-1])^2)+((1-fee)*p*qD*B1[t-1]*E[t-1]))) + E[t-1]
      # } else {
      #   E[t] <- (phi*(-((C*E[t-1])^2)+((1-fee)*p*qD*B1[t-1]*E[t-1]))) + E[t-1]
      # }
      
      if(ban == TRUE) {
        E[t] = 0
      } else {
        E[t] <- (phi*(-((C*E[t-1])^2)+((1-choice)*p*qD*B1[t-1]*E[t-1]))) + E[t-1]
      }
      
      HS1[t] = (qS*B1[t]*e1[t])
      
      HS2[t] = (qS*B2[t]*e2[t]) 
      
      HS3[t] = (qS*B3[t]*e3[t])
      
      HS[t] = HS1[t] + HS2[t] + HS3[t]
      
      PIS[t] <- p*HS[t] - ((C1*e1[t])+(C2*e2[t])+(C3*e3[t]))
      
      HD[t] <- qD*B1[t]*E[t]
      
      benefits[t] <- (beta*PIS[t]) + ((1-beta)*choice*p*HD[t])
      
      catch[t] = HS[t] + HD[t]
      
      conb1[t] = (HS1[t]+HD[t]) - B1[t]
      conb2[t] = HS2[t] - B2[t]
      conb3[t] = (HS3[t]+(v*B3[t])-(0.45*(B1[t-1]+B2[t-1])*(ifelse((B1[t-1]+B2[t-1]) > K, 0, (1-((B1[t-1]+B2[t-1])/K)))))) - B3[t]
    }
    return(c(conb1, conb2, conb3))
  })
}
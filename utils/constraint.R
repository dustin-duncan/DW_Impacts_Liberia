#' Title
#'
#' @param choice This is the choice for an optimization function
#' @param time The timeline over which to run the program (one value)
#' @param state Initial values for Biomass in 3 zones, SSF effort in 3 zones and DWF effort in zone 1 
#' @param params Biological parameters: mu, chi,  gamma, v, Economic Parameters: q1D, q1S, q2, q3, C1, C2, C3, C, W, omega, p, discount 
#' @param indat 
#' @param rtn_catch TRUE if user would like time-series of total catch (used for calibrating the model), FALSE if the user would like a time series of biomass, effort, harvest, and present value 
#' @param rtn_npv TRUE if user would like to optimize Net Present Value of the fishery over time or return negative NPV as value
#'
#' @returns
#' @export
#'
#' @examples
constraint_fx = function(choice, state, time, params, indat=NULL, rtn_catch=TRUE, rtn_npv=FALSE) {
  
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
  conb1=vector(mode="numeric", length=0)
  conb2=vector(mode="numeric", length=0)
  conb3=vector(mode="numeric", length=0)
  
  params = c(params, "fee" = choice)
  
  params = exp(params)
  
  state=exp(state)
  
  with(as.list(c(state, params)), {
    
    B1[1] = B1i
    B2[1] = B2i
    B3[1] = B3i
    e1[1] = e1i
    e2[1] = e2i
    e3[1] = e3i # 102410*0.34
    E[1] = Ei
    
    HS1[1] = (q1S*B1[1]*e1[1])
    HS2[1] = (q2*B2[1]*e2[1]) 
    HS3[1] = (q3*B3[1]*e3[1])
    
    HS[1] = HS1[1] + HS2[1] + HS3[1]
    
    PIS[1] = p*HS[1] - ((C1*e1[1])+(C2*e2[1])+(C3*e3[1]))
    
    HD[1] = q1D*B1[1]*E[1]
    
    benefits[1] = ((W)*(PIS[1])) + ((1-W)*fee*p*HD[1])
    
    catch[1] = HS[1] + HD[1]
    
    conb1[1] = (HS1[1]+HD[1]) - B1[1]
    conb2[1] = HS2[1] - B2[1]
    conb3[1] = HS3[1] - B3[1]
    
    
    for(t in 2:time) {
      
      recruit <- v*B3[t-1]
      
      # recruit = if(recruit < 0) 0 else recruit
      
      B1[t] <- B1[t-1] + (-mu*B1[t-1])-(q1S*B1[t-1]*e1[t-1])-(q1D*B1[t-1]*E[t-1])+(recruit*(1-chi))
      
      B2[t] <- B2[t-1] + (-mu*B2[t-1])-(q2*B2[t-1]*e2[t-1])+(recruit*chi)
      
      B3[t] <- B3[t-1] + (gamma*(B1[t-1]+B2[t-1])) - (q3*B3[t-1]*e3[t-1]) - recruit -(mu*B3[t-1])
      # -(eta*(B3[t-1]^2))
      
      e1[t] <- (omega*(-((C1*e1[t-1])^2)+(p*q1S*B1[t-1]*e1[t-1])) ) + e1[t-1] 
      
      e2[t] <- (omega*(-((C2*e2[t-1])^2)+(p*q2*B2[t-1]*e2[t-1])))  + e2[t-1] 
      
      e3[t] <- (omega*(-((C3*e3[t-1])^2)+(p*q3*B3[t-1]*e3[t-1]))) + e3[t-1] 
      
      E[t] <- (omega*(-((C*E[t-1])^2)+((1-fee)*p*q1D*B1[t-1]*E[t-1]))) + E[t-1] 
      
      HS1[t] = (q1S*B1[t]*e1[t])
      
      HS2[t] = (q2*B2[t]*e2[t]) 
      
      HS3[t] = (q3*B3[t]*e3[t])
      
      HS[t] = HS1[t] + HS2[t] + HS3[t]
      
      PIS[t] <- p*HS[t] - ((C1*e1[t])+(C2*e2[t])+(C3*e3[t]))
      
      HD[t] <- q1D*B1[t]*E[t]
      
      benefits[t] <- (W*PIS[t]) + ((1-W)*fee*p*HD[t])
      
      catch[t] = HS[t] + HD[t]
      
      conb1[t] = (HS1[t]+HD[t]) - B1[t]
      conb2[t] = HS2[t] - B2[t]
      conb3[t] = (HS3[t]+(v*B3[t])-(gamma*(B1[t]+B2[t]))) - B3[t]
    }
    return(c(conb1, conb2, conb3))
  })
}
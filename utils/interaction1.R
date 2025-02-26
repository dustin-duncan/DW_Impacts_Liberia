#' First Interaction Function
#'
#' @param choice This is the choice for an optimization function
#' @param time The timeline over which to run the program (one value)
#' @param state Initial values for Biomass in 3 zones, SSF effort in 3 zones and DWF effort in zone 1 
#' @param parameters Biological parameters: mu, chi,  gamma, v, Economic Parameters: q1D, q1S, q2, q3, C1, C2, C3, C, W, omega, p, discount
#'
#' @returns negative net present value 
#' @export
#'
#' @examples
interaction_ode = function(choice, time, state, parameters) {
  
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
  
  parameters = c(parameters, "fee"=choice)
  
  with(as.list(c(state, parameters)), {
    
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
    
    for(t in 2:time) {
      
      B1[t] <- (-mu*B1[t-1])-(q1S*B1[t-1]*e1[t-1])-(q1D*B1[t-1]*E[t-1])+(v*B3[t-1]*(1-chi))
      
      B2[t] <- (-mu*B2[t-1])-(q2*B2[t-1]*e2[t-1])+(v*B3[t-1]*chi)
      
      B3[t] <- (gamma*(B1[t-1]+B2[t-1]))-(q3*B3[t-1]*e3[t-1])-(v*B3[t-1])-(mu*B3[t-1])
      # -(eta*(B3[t-1]^2))
      
      e1[t] <- e1[t-1] + (omega*((-C1*e1[t-1])+(p*q1S*B1[t-1]*e1[t-1])) )
      
      e2[t] <- e2[t-1] +  (omega*((-C2*e2[t-1])+(p*q2*B2[t-1]*e2[t-1])))
      
      e3[t] <- e3[t-1] + (omega*((-C3*e3[t-1])+(p*q3*B3[t-1]*e3[t-1])))
      
      E[t] <- E[t-1] + (omega*((-C*E[t-1])+((1-fee)*p*q1D*B1[t-1]*E[t-1])))
      
      HS1[t] = (q1S*B1[t]*e1[t])
      
      HS2[t] = (q2*B2[t]*e2[t]) 
      
      HS3[t] = (q3*B3[t]*e3[t])
      
      HS[t] = HS1[t] + HS2[t] + HS3[t]
      
      PIS[t] <- p*HS[t] - ((C1*e1[t])+(C2*e2[t])+(C3*e3[t]))
      
      HD[t] <- q1D*B1[t]*E[t]
      
      benefits[t] <- (W*PIS[t]) + ((1-W)*fee*p*HD[t])
      
    }
    # list(c(dB1, dB2, dB3, de1, de2, de3, dE))
    t=seq(from=1, to =time, by=1)
    rho = 1/(1+discount)
    pv = (rho^t)*benefits
    npv=sum(pv)
    return(-npv)
  })
}
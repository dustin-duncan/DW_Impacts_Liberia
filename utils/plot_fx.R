ddplot = function(df, bmsy=FALSE, rtn_plot=NULL) {
  if(!require("tidyverse")) {
    install.packages(tidyverse)
    library(tidyverse)
  }
  if(!require("gridExtra")) {
    install.packages(gridExtra)
    library(gridExtra)
  }
  dfB = df %>% 
    select(-c(e1, e2, e3, E, HD, HS, PV, HS1, HS2, HS3, PIS, pvPIS)) %>% 
    pivot_longer(cols=c(-time), names_to="Bzone", values_to="Biomass") 
  
  plotB <- ggplot(dfB) +
    geom_line(aes(x = time, y = Biomass, color=Bzone)) + 
    labs(x = "Time", y = "Biomass", color = "Zone")+ 
    theme_bw()
  
  dfBmsy = df %>% 
    select(-c(e1, e2, e3, E, HD, HS, PV, HS1, HS2, HS3, PIS, pvPIS)) %>% 
    pivot_longer(cols=c(-time), names_to="Bzone", values_to="Biomass") %>% 
    group_by(time) %>% 
    summarize(Btot = sum(Biomass, na.rm=TRUE)) %>% 
    mutate(Bbmsy = Btot/26992) # Biomass over Biomass at MSY (originally 16459.968)
  
  plotBmsy <- ggplot(dfBmsy) +
    geom_line(aes(x = time, y = Bbmsy)) + 
    geom_abline(aes(intercept=1, slope = 0), linetype="dashed") + 
    labs(x = "Time", y = "B/BMSY") +
    scale_y_continuous(limits=c(0,5)) +
    theme_bw()
  
  dfE = df %>% 
    select(-c(B1, B2, B3, HD, HS, PV, HS1, HS2, HS3, PIS, pvPIS)) %>% 
    pivot_longer(cols=c(-time), names_to="Ezone", values_to="Effort") 
  plotE <- ggplot(dfE) +
    geom_line(aes(x = time, y = Effort, color=Ezone))+ 
    labs(x = "Time", y = "Effort", color = "Fleet")+ 
    theme_bw()
  dfH = df %>% 
    select(-c(B1, B2, B3, e1, e2, e3, E, PV, HS1, HS2, HS3, PIS, pvPIS)) %>% 
    pivot_longer(cols=c(-time), names_to="Fleet", values_to="Harvest")
  plotH <- ggplot(dfH) +
    geom_line(aes(x = time, y = Harvest, color=Fleet)) + 
    labs(x = "Time", y = "Harvest", color = "Fleet")+ 
    theme_bw()
  plotP <- ggplot(df) + 
    geom_line(aes(x = time, y = PV)) + 
    labs(x = "Time", y = "Present Value of Harvest") +
    theme_bw() 
  
  plot <- grid.arrange(plotB, plotE, plotH, plotP, ncol=2)
  
  if(bmsy == FALSE & is.null(rtn_plot)) {
    return(plot)
  } else if(!is.null(rtn_plot)) {
    return(plotBmsy)
  } else if(bmsy == FALSE & rtn_plot == "B"){
    return(plotB)
  } else if(bmsy == FALSE & rtn_plot == "Bmsy"){
    return(plotBmsy)
  } else if(bmsy == FALSE & rtn_plot == "E"){
    return(plotE)
  } else if(bmsy == FALSE & rtn_plot == "H"){
    return(plotH)
  } else if(bmsy == FALSE & rtn_plot == "P"){
    return(plotP)
  }
  
  
}
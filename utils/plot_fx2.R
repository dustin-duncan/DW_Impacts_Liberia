ddplot2 = function(df, bmsy=FALSE) {
  if(!require("tidyverse")) {
    install.packages(tidyverse)
    library(tidyverse)
  }
  if(!require("gridExtra")) {
    install.packages(gridExtra)
    library(gridExtra)
  }
  p1 <- ggplot(df, aes(x=time, y=B)) + 
    geom_line()
  
  
  dfE <- df %>% 
    pivot_longer(cols = c(e, E), names_to="fleet", values_to="eff")
  
  p2 <- ggplot(dfE, aes(x=time, y=eff, color=fleet)) + 
    geom_line()
  
  dfbmsy <- df %>% 
    mutate(bbmsy = B/16459.968)
  
  p3 <- ggplot(dfbmsy, aes(x = time, y = bbmsy)) + 
    geom_line() + 
    geom_abline(aes(intercept=1, slope=0), linetype="dashed", color="red")
  
  plot <- grid.arrange(p1, p2, ncol=2)
  
  if(bmsy == FALSE) {
    return(plot)
  } else {
    return(p3)
  }
}
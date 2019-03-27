library(ggplot2)
library(RColorBrewer)

windowsFonts(pisava=windowsFont("Consolas"))
windowsFonts(stevila=windowsFont("Yu Gothic Medium"))
tema <- function() {
  
  theme(plot.background=element_rect(fill="gray92")) + #ta pobarva tudi za naslovom ozadje
    theme(panel.grid.major = element_line(colour="Gray78")) +
    
    theme(panel.grid.minor = element_blank()) +
    
    #dodelamo naslove, podnaslove in osi:
    
    theme(plot.title = element_text(family="sans",face="bold",
                                    colour="gray24", size=18)) +
    
    theme(plot.subtitle = element_text(family="sans",colour="Gray30",size=13))+
    
    theme(axis.title.y = element_text(family="sans",face="bold",colour="Gray30",size=16),
          
          axis.text.y=element_text(family="sans",face="bold", color="gray13", size=12)) +
    
    theme(axis.title.x = element_text(family="sans",face="bold",colour="Gray30",size=16),
          
          axis.text.x = element_text(family="sans",face="bold", color="gray13", size=12)) +
    
    theme(legend.background = element_rect(fill="gray92")) + #barva ozadja legende
    
    #odstranimo ticks na oseh:
    
    theme(axis.ticks = element_blank())
  
}

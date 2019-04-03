library(ggplot2)
library(RColorBrewer)

windowsFonts(pisava=windowsFont("Consolas"))
windowsFonts(stevila=windowsFont("Yu Gothic Medium"))
tema <- function() {
  
  theme(plot.background=element_rect(fill="gray85")) + #ta pobarva tudi za naslovom ozadje
    theme(panel.grid.major = element_line(colour="Gray78"),
          panel.background=element_rect(fill="gray85", color="gray85")) +
    
    theme(panel.grid.minor = element_blank()) +
    
    #dodelamo naslove, podnaslove in osi:
    
    theme(plot.title = element_text(family="sans",face="bold", hjust=0.5,
                                    colour="gray24", size=25)) +
    
    theme(plot.subtitle = element_text(family="sans",colour="Gray30",size=13))+
    
    theme(axis.title.y = element_text(family="sans",face="bold",colour="Gray30",size=18),
          
          axis.text.y=element_text(family="sans",face="bold", color="gray13", size=12)) +
    
    theme(axis.title.x = element_text(family="sans",face="bold",colour="Gray30",size=18),
          
          axis.text.x = element_text(family="sans",face="bold", color="gray13", size=12)) +
    
    theme(legend.background = element_rect(fill="gray85")) + #barva ozadja legende
    
    #odstranimo ticks na oseh:
    
    theme(axis.ticks = element_blank()) +
    
    theme(axis.title.x=element_text(margin=margin(12,0,0,0)),
          axis.title.y=element_text(margin=margin(0,16,0,0)),
          plot.title = element_text(margin=margin(0,0,25,0)),
          # velikost praznega prostora okoli grafa
          plot.margin = margin(1, 1, 1, 1, "cm")) + 
    
    theme(legend.key = element_rect(color="gray85"),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 14))
  
  
}


tema_zemljevid <- function() {
  
  theme(plot.background=element_rect(fill="gray85"),  #ta pobarva tudi za naslovom ozadje
        panel.background=element_rect(fill="gray85", color="gray85")) +
    theme(panel.grid.major = element_blank()) +
    
    theme(panel.grid.minor = element_blank()) +
    
    #dodelamo naslove, podnaslove in osi:
    
    theme(plot.title = element_text(family="sans",face="bold",
                                    colour="gray24", size=25)) +
    
    theme(plot.subtitle = element_text(family="sans",colour="Gray30",size=13))+
    
    theme(axis.title = element_blank(),
          
          axis.text = element_blank()) +
    
    theme(legend.background = element_rect(fill="gray85")) + #barva ozadja legende
    
    #odstranimo ticks na oseh:
    
    theme(axis.ticks = element_blank()) +
    
    theme(plot.margin = margin(.8, .5, .5, .5, "cm"))
  
}
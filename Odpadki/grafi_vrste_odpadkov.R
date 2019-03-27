library(ggplot2)

tabela_vrste <- odpadki_vrste %>% filter(Leto==input$leto_vrste & 
                                         Nastanek==input$nastanek_vrste & 
                                         Vrsta==input$vrsta_vrste)

graf_vrste <- ggplot2(tabela_vrste,aes(x=Leto,y=Kolicina_tona,fill=Vrsta))+geom_bar()

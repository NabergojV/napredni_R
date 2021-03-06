
source("lib.R")
source("uvoz/uvoz_podatkov_odpadki.R")
source("tema.R")
source("uvoz/uvozi_zemljevid.R", encoding = "UTF-8")


ui <- fluidPage(theme = shinytheme("superhero"), useShinyjs(),
                
                titlePanel("Prikaz količine odpadkov v Sloveniji skozi leta glede na vrsto in regije"),
                
                tabsetPanel(
                  tabPanel("Odpadki po vrstah", icon = icon('trash'),
                           # h2("blue tab", style='color:blue'),
                           # p("Izberi si parametre po želji in si oglej tabelo ali graf." , style ="font-weight:bold; color:#4682B4"),
                           sidebarLayout(
                             sidebarPanel(width=3,
                               sliderInput("leto_vrste", "Leto:",
                                           min = 2002, max = 2017,
                                           value = c(2010,2015),
                                           step=1),
                               hr(),
                               selectInput("nastanek_vrste", "Izberi nastanek:",
                                           choices = nastanek_odpadkov,
                                           selected = nastanek_odpadkov[1]),
                               hr(),
                               selectInput("vrsta_vrste", "Izberi vrste odpadkov:",
                                           choices = vrste_odpadkov,
                                           selected = vrste_odpadkov[1],
                                           multiple = TRUE),
                               hr(),
                               selectInput("kolicina", "Izberi količino:",
                                           choices = c("Količina odpadkov v tonah",
                                                       "Količina odpadkov na prebivalca (kg)"),
                                           selected = "Količina odpadkov v tonah")
                             ),
                             
                             mainPanel(
                               tabsetPanel(id="delcek",
                                           tabPanel(title="Graf", icon = icon('chart-bar'),
                                                    value="grafgraf",
                                                    plotOutput("graf_vrste")),
                                           tabPanel(title="Tabela", icon = icon('table'),
                                                    value="tabtab",
                                                    style="color: #fff; background-color: #4682B4  ; border-color: #2e6da4",
                                                    DT::dataTableOutput("tabela1"))
                               )
                             )
                               
                             )),
                  
                  tabPanel("Odpadki po regijah", fluid=TRUE, uiOutput("Odpadki po regijah"),
                           icon = icon('globe'))
                  
                )
)


#----------------------------------------------------------------------------------

server <- function(input, output,session) {
  
  # prvi tab

  observeEvent(input$kazalo,
               {
                 if (input$kazalo == "Graf"){
                   updateTabsetPanel(session,"delcek",
                                     selected="grafgraf")
                 } else{
                   updateTabsetPanel(session,"delcek",
                                     selected="tabtab")
                 }
               })
  

  
  output$graf_vrste <- renderPlot({
  
    tabela_vrste <- odpadki_vrste %>% filter(Leto %in% seq(min(input$leto_vrste),max(input$leto_vrste)) &
                                               Nastanek %in% input$nastanek_vrste &
                                               Vrsta %in% c(input$vrsta_vrste))
    
      if (input$kolicina == "Količina odpadkov v tonah"){
        
        ggplot(tabela_vrste,aes(x=Leto,y=Kolicina_tona,fill=Vrsta)) +
          geom_bar(stat = "identity")+
          labs(title = "Količina odpadkov glede na vrsto in nastanek", x="Leto", y="Količina (v tonah)")+
          tema() +
          scale_x_continuous(breaks = min(input$leto_vrste):max(input$leto_vrste))
      }
      
      else
        
        ggplot(tabela_vrste,aes(x=Leto,y=tabela_vrste$"Kolicina_kg/Prebivalec",fill=Vrsta)) +
        geom_bar(stat = "identity")+
        labs(title = "Količina odpadkov glede na vrsto in nastanek na prebivalca", x="Leto", y="Količina (v kg na prebivalca)")+
        tema() +
        scale_x_continuous(breaks = min(input$leto_vrste):max(input$leto_vrste))
  })
  
  output$tabela1 <- DT::renderDataTable({
    
    tabela_vrste <- odpadki_vrste %>% filter(Leto %in% seq(min(input$leto_vrste),max(input$leto_vrste)) &
                                               Nastanek %in% input$nastanek_vrste &
                                               Vrsta %in% c(input$vrsta_vrste))
    
    if (input$kolicina == "Količina odpadkov v tonah"){
      
      tab <- tabela_vrste[order(tabela_vrste$Kolicina_tona, decreasing = TRUE), c(1,2,3,4)]
      tab
      
    }
    
    else
      
      tab <- tabela_vrste[order(tabela_vrste$'Kolicina_kg/Prebivalec', decreasing = TRUE), c(1,2,3,5)]
      tab
    
    DT:::datatable(data=tab,rownames = FALSE,options = list(autoWidth = TRUE)) %>% 
      formatStyle(columns = colnames(tab), background = "#F5F5DC", color = "black")
    
  }) 
  
  
  

  
  #-------------------------------------------------------------------------
  
  # drugi tab
  
  output$zemljevid_regije <- renderPlot({
    
    zem12 <- zemljevid
    
    tabela_leto <- tabela_zemljevid %>% filter(Leto == input$leto1)
    
    zem12$Kolicina_tona <- tabela_leto$Kolicina_tona
    zem12$'Kolicina_kg/Prebivalec' <- tabela_leto$'Kolicina_kg/Prebivalec'
    
    zem12 <- pretvori.zemljevid(zem12)
    
    cnames <- aggregate(cbind(long, lat) ~ NAME_1, data=zem12, 
                        FUN=function(x)mean(range(x)))
    
    if (input$kolicina1 == "Količina odpadkov v tonah"){
      
      ggplot() + geom_polygon(data=zem12, aes(x=long, y=lat, group=group, fill=Kolicina_tona)) + 
        tema_zemljevid() + 
        labs(title = "Prikaz količine odpadkov po regijah v Sloveniji", x = "", y = "") + 
        geom_text(data=cnames, aes(long, lat, label = NAME_1), size=4, colour="white") + 
        labs(fill = "Količina v tonah")
    }
    
    else
      
      ggplot() + geom_polygon(data=zem12, aes(x=long, y=lat, group=group, fill=zem12$"Kolicina_kg/Prebivalec")) + 
      tema_zemljevid() + 
      labs(title = "Prikaz količine odpadkov po regijah v Sloveniji", x = "", y = "") +
      labs(fill = 'Količina v \n kg/prebivalec') + 
      geom_text(data=cnames, aes(long, lat, label = NAME_1), size=4, colour="white")
    
  })
  
  output$tabela_regije <- DT::renderDataTable({
    
    if (input$kolicina1 == "Količina odpadkov v tonah"){
      tab1 <- tabela_zemljevid[order(tabela_zemljevid$Kolicina_tona),c(1,2,3,4)] %>% 
        filter(Leto == input$leto1)
      datatable(tab1) %>%
        formatStyle(columns = colnames(tab1), target = "cell")
    }
    
    else
      tab1 <- tabela_zemljevid[order(tabela_zemljevid$`Kolicina_kg/Prebivalec`),c(1,2,3,5)] %>% 
        filter(Leto == input$leto1)
      datatable(tab1) %>%
        formatStyle(columns = colnames(tab1), target = "cell")
      
      DT:::datatable(data=tab1,rownames = FALSE,options = list(autoWidth = TRUE)) %>% 
        formatStyle(columns = colnames(tab1), background = "#F5F5DC", color = "black")
      
  })
  
  output$graf_regije <- renderPlot({
    
    odpadki_regije1 <- odpadki_regije2 %>% filter(Leto %in% seq(min(input$leto2),max(input$leto2)),
                                                  Regija %in% c(input$regija))
    
    if (input$kolicina2 == "Količina odpadkov v tonah"){
      
      ggplot(odpadki_regije1,aes(x=Leto,y=Kolicina_tona,fill=Regija)) +
        geom_bar(stat = "identity") +
        labs(title = "Količina odpadkov glede na regijo", x="Leto", y="Količina (v tonah)") +
        tema() + 
        scale_x_continuous(breaks = min(input$leto2):max(input$leto2))
    }
    
    else
      ggplot(odpadki_regije1,aes(x=Leto,y=odpadki_regije1$'Kolicina_kg/Prebivalec',fill=Regija)) +
      geom_bar(stat = "identity") +
      labs(title = "Količina odpadkov glede na regijo na prebivalca", x="Leto", y="Količina (v kg na prebivalca)") +
      tema() + 
      scale_x_continuous(breaks = min(input$leto2):max(input$leto2))
    
  }) 
  
  output$"Odpadki po regijah" <- renderUI({
    tabsetPanel(
      tabPanel("Zemljevid", icon = icon('map-marked-alt'),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              sliderInput("leto1", "Leto:",
                                          min = 2012, max = 2017,
                                          value = 2014,
                                          step = 1,
                                          animate = T),
                              hr(),
                              selectInput("kolicina1", "Izberi količino:",
                                          choices = c("Količina odpadkov v tonah",
                                                      "Količina odpadkov na prebivalca (kg)"),
                                          selected = "Količina odpadkov v tonah")
                 ),
                 mainPanel(
                   fluidRow(
                     column(5,DT::dataTableOutput("tabela_regije"),
                            style="color: #fff; background-color: #4682B4; border-color: #2e6da4"),
                     column(6,plotOutput("zemljevid_regije",height = "470px",width = "650"))
                   )
                 ))
               
      ),
      tabPanel("Graf", icon = icon('chart-bar'),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              sliderInput("leto2", "Leto:",
                                          min = 2012, max = 2017,
                                          value = c(2013,2015),
                                          step = 1),
                              hr(),
                              selectInput("kolicina2", "Izberi količino:",
                                          choices = c("Količina odpadkov v tonah",
                                                      "Količina odpadkov na prebivalca (kg)"),
                                          selected = "Količina odpadkov v tonah"),
                              hr(),
                              prettyCheckboxGroup("regija", "Izberi eno ali več regij:",
                                                  choices = regije,
                                                  selected = regije[1],
                                                  shape = "round",
                                                  fill = TRUE,
                                                  status = "warning")
                 ),
                 mainPanel(
                   plotOutput("graf_regije")
                 )
               )))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


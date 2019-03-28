
library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(shinyWidgets)

source("uvoz_podatkov_odpadki.R")
source("tema.R")
source("lib/uvozi_zemljevid.r", encoding = "UTF-8")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
   
   titlePanel("Analitični prikaz odpadkov"),
   
   tabsetPanel(
     tabPanel("Odpadki po vrstah",
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
                  actionButton("gumb1","Graf" , icon("chart-bar"), 
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  actionButton("gumb2","Tabela", icon("table"),
                               style="color: #fff; background-color: orange")
                ),
                
                mainPanel(
                  conditionalPanel(
                    condition='input.gumb1==TRUE',
                    plotOutput("graf_vrste")),
                  conditionalPanel(
                    condition='input.gumb2==TRUE',
                    tableOutput("tabela1"))
              ))),
     
     tabPanel("Odpadki po regijah", fluid=TRUE, uiOutput("Odpadki po regijah"))
     
  )
)




server <- function(input, output) {
  
  # prvi tab
  
  gumb1 <- reactiveValues(gumb1=FALSE)
  gumb2 <- reactiveValues(gumb2=FALSE)
  
  observeEvent(input$gumb1,
               isolate ({gumb1$gumb1=TRUE
               gumb2$gumb2=FALSE})
  )

  observeEvent(input$gumb2,
               isolate ({gumb2$gumb2=TRUE
               gumb1$gumb1=FALSE})
  )
  
  
  output$graf_vrste <- renderPlot({
    
    if(gumb1$gumb1){
    
    tabela_vrste <- odpadki_vrste %>% filter(Leto %in% seq(min(input$leto_vrste),max(input$leto_vrste)) &
                                               Nastanek %in% input$nastanek_vrste &
                                               Vrsta %in% c(input$vrsta_vrste))
    
    
    ggplot(tabela_vrste,aes(x=Leto,y=Kolicina_tona,fill=Vrsta)) +
      geom_bar(stat = "identity")+
      labs(title = "Količina odpadkov glede na vrsto in nastanek", x="Leto", y="Količina (v tonah)")+
      tema()
    }
    else 
      return()
    
  })
  
  output$tabela1 <- renderTable({
    
    if(gumb2$gumb2){
      
    tabela_vrste <- odpadki_vrste %>% filter(Leto %in% seq(min(input$leto_vrste),max(input$leto_vrste)) &
                                               Nastanek %in% input$nastanek_vrste &
                                               Vrsta %in% c(input$vrsta_vrste))
    
    tabela_vrste
    }
    else 
      return()
    
  })
  
  # drugi tab
  
  output$zemljevid_regije <- renderPlot({
    zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                                 "SVN_adm1", encoding = "UTF-8")
    
    zemljevid@data[["NAME_1"]] <- sapply(zemljevid@data[["NAME_1"]], 
                                         function(x) gsub("š", "s", x))
    
    zem12 <- zemljevid
    
    tabela_leto <- tabela_zemljevid %>% filter(Leto == input$leto1)
    
    #zem12 <- preuredi(tabela_leto, zem12, "NAME_1")
    
    zem12$Kolicina_tona <- tabela_leto$Kolicina_tona
    
    zem12 <- pretvori.zemljevid(zem12)
    
    ggplot() + geom_polygon(data=zem12, aes(x=long, y=lat, group=group, fill=Kolicina_tona)) + 
      tema_zemljevid() + 
      labs(title = "Prikaz količine odpadkov po regijah v Sloveniji", x = "", y = "")
    
  })
  
  output$tabela_regije <- renderTable({
    tabela_zemljevid %>% filter(Leto == input$leto1)
  })
  
  output$graf_regije <- renderPlot({
    
    odpadki_regije1 <- odpadki_regije %>% filter(Leto %in% seq(min(input$leto2),max(input$leto2)),
                                                 Regija %in% c(input$regija))
      
    ggplot(odpadki_regije1,aes(x=Leto,y=Kolicina_tona,fill=Regija)) +
      geom_bar(stat = "identity")+
      labs(title = "Količina odpadkov glede na regijo", x="Leto", y="Količina (v tonah)")+
      tema()
  }) 
  
  output$"Odpadki po regijah" <- renderUI({
    tabsetPanel(
      tabPanel("Zemljevid",
                         sidebarLayout(
                           sidebarPanel(width = 3,
                             sliderInput("leto1", "Leto:",
                                         min = 2012, max = 2017,
                                         value = 2014,
                                         step = 1)
                         ),
                         mainPanel(
                           fluidRow(
                            column(7,plotOutput("zemljevid_regije",height = "400px",width = "600")),
                            column(5,tableOutput("tabela_regije"))
                           )
                         ))
                         
                         ),
      tabPanel("Graf",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              sliderInput("leto2", "Leto:",
                                          min = 2012, max = 2017,
                                          value = c(2013,2015),
                                          step = 1),
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


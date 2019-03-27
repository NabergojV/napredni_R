
library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)

source("uvoz_podatkov_odpadki.R")
source("tema.R")
source("lib/uvozi_zemljevid.r", encoding = "UTF-8")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
   
   titlePanel("Analitični prikaz odpadkov"),
   
   tabsetPanel(
     tabPanel("Odpadki po vrstah",
              sidebarLayout(
                sidebarPanel(
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
     
     tabPanel("Odpadki po regijah", fluid=TRUE, uiOutput("Odpadki po regijah") )
     
  )
)




server <- function(input, output) {
  
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
  
  
  output$"Odpadki po regijah" <- renderUI({
    tabsetPanel(id="prvi",
                tabPanel("Zemljevid",
                         output$zemljevidregije <- renderPlot({
                         zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                                                      "SVN_adm1", encoding = "UTF-8")
                         
                         pretvori.zemljevid <- function(zemljevid) {
                           fo <- fortify(zemljevid)
                           data <- zemljevid@data
                           data$id <- as.character(0:(nrow(data)-1))
                           return(inner_join(fo, data, by="id"))
                         }
                         })
                         
                         ),
                tabPanel("Graf"))
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)



library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)

source("uvoz_podatkov_odpadki.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   titlePanel("Analitični prikaz odpadkov"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("leto_vrste", "Leto:",
                    min = 2002, max = 2017,
                    value = c(2010,2015)),
        hr(),
        selectInput("nastanek_vrste", "Izberi nastanek:",
                    choices = nastanek_odpadkov,
                    selected = nastanek_odpadkov[1]),
        hr(),
        selectInput("vrsta_vrste", "Izberi vrste odpadkov:",
                    choices = vrste_odpadkov,
                    selected = vrste_odpadkov[1],
                    multiple = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


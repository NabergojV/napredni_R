sidebarPanel(
  switchInput("kazalo", label = NULL, value = FALSE, onLabel = "Graf",
              offLabel = "Tabela", onStatus ="red", offStatus = "blue",
              size = "default", labelWidth = "auto", handleWidth = "auto",
              inline = FALSE, width ="100px"),
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
              tabPanel(title="Graf",
                       value="grafgraf",
                       plotOutput("graf_vrste")),
              tabPanel(title="Tabela",
                       value="tabtab",
                       tableOutput("tabela1"))
              )
)



#server:

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
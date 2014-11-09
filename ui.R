
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
shinyUI(fluidPage(

  # Application title
  titlePanel("Komunikaty Kolei Mazowieckich"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateInput("date.from",label = "Data od:",value = "2013-09-03",min = "2013-09-03",max = "2014-10-30"),
      dateInput("date.to",label = "Data do:",min = "2013-09-04",value = "2014-10-31",max = ymd("2014-10-31")  
                
      ),
      radioButtons(inputId = "aggregation.level",label="Agregacja (szereg czasowy)",choices = c("Tygodniowa"="week","Dzienna" = "day")),
      radioButtons(inputId = "notification.type",label="Typ komunikatu (czerwony)",choices = c("Awarie" = "awaria","Utrudnienia" = "utrudnienia", "Odwołane" = "odwolane", "Pozostałe" = "pozostale"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("Czas",
                 h5("Szereg czasowy:"),
                 plotOutput("timelinePlot",height = 180),
                 h5("Podział na dni tygodnia:"),
                 plotOutput("dowPlot",height = 450)),
        tabPanel("Kierunki",
                 h5("Komunikaty w wybranych relacjach:"),
                 plotOutput("corrGram", height=500)                 
                 ),
        tabPanel("Tabele",
                 h4("Podział na dni tygodnia:"),
                 dataTableOutput(outputId="table"),
                hr(),
                 textOutput("free")
                 )
      )
      
      
    )
  )
))

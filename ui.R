library(shiny)
shinyUI(fluidPage(

  # Application title
  titlePanel("KM Annoucements"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Please select date range:"),
      dateInput("date.from",label = "From:",value = dmin_txt ,min = dmin_txt ,max = dmax_txt),
      
      dateInput("date.to",label = "To:",min =dmin_txt,value = dmax_txt ,max = dmax_txt),
      
      radioButtons(inputId = "aggregation.level",label="Aggregation level (time series plot)",choices = c("Weekly"="week","Daily" = "day")),
      
      radioButtons(inputId = "notification.type",label="Event type (in red)",choices = c("Breakdowns" = "awaria","Traffic impediments" = "utrudnienia", "Cancellations" = "odwolane", "Unclassified" = "pozostale")),
      br(),
      helpText("Koleje Mazowieckie (KM) Mazovian Province (Poland, Europe) railway provider announces almost all events of the service impairment. Here is the basic exploration of the collected data for almost 2 year period. Selected events are colored in red, all events are in grey."), a("Details are here.", href="http://rpubs.com/przemo/KM_service")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("Time analysis",
                 h5("Time series:"),
                 plotOutput("timelinePlot",height = 180),
                 h5("Weekdays:"),
                 plotOutput("dowPlot",height = 450)),
        tabPanel("Route analysis",
                 h5("Annoucements given the route:"),
                 plotOutput("corrGram", height = 500)                 
                 ),
        tabPanel("Tables",
                 h4("Weekly occurrence:"),
                 dataTableOutput(outputId="table"),
                hr(),
                 textOutput("free")
                 )
      )
      
      
    )
  )
))

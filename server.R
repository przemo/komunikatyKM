
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
ss <- dget("data.R")

shinyServer(function(input, output) {

  output$timelinePlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    
    o<- generateTimeline(ss,type=input$aggregation.level, dfrom = input$date.from, dto = input$date.to, agg = input$notification.type)
    # <- fadithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    pl1 <- ggplot(data=o) + geom_area(aes(x=date,y=wszystkie), fill="grey90", col="grey70")+ geom_line(aes(x=date,y=variable), col="red") + theme_bw() + xlab("Czas")+ ylab("Liczba komunikatów")
    pl1
  })
  
  output$dowPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    
    o<- dayCounts(ss,dfrom = input$date.from, dto = input$date.to, agg = input$notification.type)
    o2 <- mutate(o,v.pc=variable/sum(variable),w.pc=wszystkie/sum(wszystkie))
    o2 <- dplyr::select(o2,dow,v.pc,w.pc)
    o2 <- melt(o2,id.vars = "dow")
    # <- fadithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    pl1 <- ggplot(data=o) + geom_bar(aes(x=dow,y=wszystkie),stat="identity", fill="grey90", col="grey70")+ geom_bar(aes(x=dow,y=variable), fill="red",stat="identity") + theme_bw() + xlab("Dni tygodnia")+ ylab("Liczba komunikatów")
    pl2 <- ggplot(data=o2) + geom_bar(aes(x=dow,y=value, group=factor(variable),fill=factor(variable), col=factor(variable)),stat="identity", position = "dodge") + theme_bw() + xlab("Dni tygodnia")+ ylab("Udział procentowy")+scale_fill_manual(values = c("red","grey90"))+scale_color_manual(values = c("red","grey70")) +theme(legend.position = "none")
    multiplot(pl1,pl2,cols = 1)
  })
  
  output$corrGram <- renderPlot({
    tt <- getDestCounts(df,dfrom = input$date.from, dto = input$date.to, agg = input$notification.type)
    
    pl <- ggplot(data=tt)+geom_tile(aes(y=Var1,x=Var2,fill=pc), col="gray80")+theme_bw()+theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))+scale_fill_gradient("%", low = "grey80",high = "red")+ylab("")+xlab("")+theme(legend.position="right",legend.title=element_text(color = "gray50", vjust = 0))
    pl
  })
  
  output$table <- renderDataTable({
    o<- dayCounts(ss,dfrom = input$date.from, dto = input$date.to, agg = input$notification.type)
    o2 <- mutate(o,v.pc=round(variable/sum(variable),2),w.pc=round(wszystkie/sum(wszystkie),2))
    names(o2) <- c("Dzień tygodnia","l.komunikatów","l.wszystkich","% komunikatów","% wszystkich")
    o2
    
  })

output$free <- renderText({
  a <- freeDaysCount(ss,dfrom = input$date.from, dto = input$date.to, agg = input$notification.type)
  paste("Liczba dni wolnych od komunikatów",a[1],"z",a[3],"możliwych dni w wyznaczonym zakresie. Liczba dni wolnych od komunikatów ogółem: ",a[2], "z",a[3],"dni.")
  })
})

library(shiny)

shinyServer(function(input, output) {
  
 dates <-  reactive({ # validating dates and generating the dataset for plots and tables
    validate(
      need(ymd(input$date.from) < ymd(input$date.to), "Please, make sure the date 'from' is earlier than the date 'to'.")
    )
    o <- filterData(ss, dfrom = ymd(input$date.from), dto = ymd(input$date.to), type = input$notification.type)
    list(from=ymd(input$date.from), to=ymd(input$date.to), data=o) 
  })
  
  output$timelinePlot <- renderPlot({
    
    # generate dataset for the timeline plot
   o <- generateTimeline(dates()[["data"]], agg=input$aggregation.level)

    # create timline plot
    ggplot(data=o) + geom_area(aes(x=date,y=all), fill="grey90", col="grey70")+ geom_line(aes(x=date,y=variable), col="red") + theme_bw() + xlab("Time")+ ylab("# of events")
  })
  
 # generate week days bar plots
  output$dowPlot <- renderPlot({
        
    o <- dayCounts(dates()[["data"]])
    o2 <- mutate(o,v.pc=variable/sum(variable),w.pc=wszystkie/sum(wszystkie))
    o2 <- dplyr::select(o2,dow,v.pc,w.pc)
    o2 <- melt(o2,id.vars = "dow")

    # draw the histogram with the specified number of bins
    pl1 <- ggplot(data=o) + geom_bar(aes(x=dow,y=wszystkie),stat="identity", fill="grey90", col="grey70")+ geom_bar(aes(x=dow,y=variable), fill="red",stat="identity") + theme_bw() + xlab("Week days")+ ylab("# of events")
    
    # second bar plot with percentages
    pl2 <- ggplot(data=o2) + geom_bar(aes(x=dow,y=value, group=factor(variable),fill=factor(variable), col=factor(variable)),stat="identity", position = "dodge") + theme_bw() + xlab("Week days")+ ylab("Per cent")+scale_fill_manual(values = c("red","grey90"))+scale_color_manual(values = c("red","grey70")) +theme(legend.position = "none")
    
    # combine two plots into one image
    multiplot(pl1,pl2,cols = 1)
  })
  
 # generate correlogram of events given a destonation (from a city to a city)
  output$corrGram <- renderPlot({
    tt <- getDestCounts(ss,dfrom = dates()[["from"]], dto = dates()[["to"]], agg = input$notification.type)
    if (!is.null(tt)) {
   
      validate(
        need(try( # sadly this does not work, ggplot returns error but not in an error class
          
          return(ggplot(data=tt)+geom_tile(aes(y=Var1,x=Var2,fill=pc), col="gray80")+theme_bw()+theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))+scale_fill_gradient("%", low = "grey80",high = "red")+ylab("")+xlab("")+theme(legend.position="right",legend.title=element_text(color = "gray50", vjust = 0))
          )
          ), noDataPlot())
      )
      
      
     } else {
      ## here empty plot
    noDataPlot()
    }
  })
  
  output$table <- renderDataTable({
    o<- dayCounts(dates()[["data"]])
    o2 <- mutate(o,v.pc=round(variable/sum(variable),2),w.pc=round(wszystkie/sum(wszystkie),2))
    names(o2) <- c("DoW","# events","# all","% events","% all")
    o2
    
  })

output$free <- renderText({
  a <- freeDaysCount(dates()[["data"]], dfrom = dates()[["from"]], dto = dates()[["to"]])
  paste("Number of specified event free days",a[1],"out of",a[3],"days in chosen period of time. Total number of annoucement free days:",a[2], "out of",a[3],"days.")
  })
})

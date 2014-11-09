source("libraries.R")
### aggregations

mondayInWeek <- function(dates){
  output <- rep(as.Date(NA),length(dates))
  require(lubridate)
  giveMonday <- function(d) {
    if(length(d)>1) stop("Argument has to me of length 1")
    w  <-  week(d)
    y  <-  year(d)
    if (w > 52) w = 52
    as.Date(paste(1,w,y, sep = "-"),'%u-%W-%Y')
  }
  #vapply(dates,giveMonday,as.Date(NA))
  for (i in 1:length(dates)) output[i] <- giveMonday(dates[i])
  output
}

## day
byDay <-  function(df, dfrom="20130903", dto="20141031", agg="awaria", dow=FALSE){
  col.nums <- match(agg,names(df))[1] # wezmie ttlko jeden element bo inaczej sie wywali
    output <- df %>%
      filter(dates >= ymd(dfrom) & dates <= ymd(dto)) %>%
      dplyr::select(date=dates,v=col.nums) %>%
      group_by(date) %>%
      summarise(variable=sum(v), wszystkie=n()) %>%
      arrange(date)
  output
}

# week
byWeek <- function(df, dfrom="20130903", dto="20141031", agg="awaria"){
  col.nums <- match(agg,names(df))[1] # wezmie ttlko jeden element bo inaczej sie wywali
    output <- df %>%
      filter(dates >= ymd(dfrom) & dates <= ymd(dto)) %>%
      dplyr::select(dates,v=col.nums) %>%
      mutate(date= mondayInWeek(dates))%>%
      group_by(date) %>%
      summarise(variable=sum(v), wszystkie=n()) %>%
      arrange(date)
output
}
# month

generateTimeline <- function(df, type="week", ...){
  if (type=="week") { 
    byWeek(df, ...) 
  } else {
    byDay(df, ...)
  }
}
  
dayCounts <- function(df, dfrom="20130903", dto="20141031", agg="awaria") {
  col.nums <- match(agg,names(df))[1] # wezmie ttlko jeden element bo inaczej sie wywali
  output <- df %>%
    filter(dates >= ymd(dfrom) & dates <= ymd(dto)) %>%
    dplyr::select(dates,v=col.nums) %>%
    mutate(dow=wday(dates, TRUE)) %>%
    group_by(dow) %>%
    summarise(variable=sum(v), wszystkie=n()) %>%
    arrange(dow)
  output
}

getDestCounts <- function(df, dfrom="20130903", dto="20141031", agg="awaria") {
  col.nums <- match(agg,names(df))[1] # wezmie ttlko jeden element bo inaczej sie wywali
  o <- df %>%
    dplyr::select(dates,miasto.from,miasto.to,v=col.nums) %>%
    filter(dates >= ymd(dfrom) & dates <= ymd(dto) & ( v > 0))# tu nie mam pewnosci
  tt <- as.data.frame(table(o$miasto.from,o$miasto.to),stringsAsFactors = FALSE) 
  tt <- mutate(tt, pc=Freq/sum(Freq)*100)
  filter(tt, pc>0)
}

freeDaysCount <- function(df, dfrom="20130903", dto="20141031", agg="awaria") {
  col.nums <- match(agg,names(df))[1] # wezmie ttlko jeden element bo inaczej sie wywali
  ddfrom <- ymd(dfrom);ddto <- ymd(dto)
  days.sequence <- seq(from = ddfrom, to=ddto,by = "1 day")
  col.nums <- match(agg,names(df))[1] # wezmie ttlko jeden element bo inaczej sie wywali
  
  output <- df %>%
    dplyr::select(dates,v=col.nums) %>%
    filter(dates >= ymd(dfrom) & dates <= ymd(dto) & ( v > 0)) %>% # tu nie mam pewnosci
    group_by(dates) %>%
    summarise(n=n())
  
  output2 <- df %>%
    dplyr::select(dates) %>%
    filter(dates >= ymd(dfrom) & dates <= ymd(dto)) %>% # tu nie mam pewnosci
    group_by(dates) %>%
    summarise(n=n())
  tot <- length(days.sequence)
  fd <- tot-length(output$dates)#sum(!(days.sequence %in% output$dates))
  fd2 <- tot-length(output2$dates)#sum(!(days.sequence %in% output2$dates))
  
  c(fd,fd2,tot)
}





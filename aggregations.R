## parameters for date range
url <- "https://raw.githubusercontent.com/przemo/komunikatyKM/master/data.R"
ss <<- dget(textConnection(getURL(url,.opts=list(ssl.verifypeer=FALSE),.encoding = "UTF-8")))

dmin <- min(ss$dates)
dmax  <- max(ss$dates)

dmin_txt <- format(dmin, "%Y-%m-%d")
dmax_txt <- format(dmax, "%Y-%m-%d")



### aggregations

filterData <- function(df, type="awaria", dfrom=dmin, dto=dmax, ... ){
  col.nums <- match(type,names(df))[1]
  a <- df %>%
    filter(dates >= dfrom & dates <= dto) %>%
    dplyr::select(date=dates, variable=col.nums)
  A <<- countDates(a); B <<- countDates(filter(a,variable == TRUE))
  return(mutate(a, all=1))
}

generateTimeline <- function(df, agg="week"){
  zo <- read.zoo(df)
  ad <- apply.daily(x=zo,colSums) # daily aggregation
  
  if (agg=="week") { 
    zoa <- apply.weekly(x=zo, colSums) # weekly aggregation
  } else {
    zoa <- ad
  }
  
  zdf <- as.data.frame(zoa)
  zdf$date <- ymd(row.names(zdf))
  zdf
}
  
dayCounts <- function(df) {
  a <- df %>%
    mutate(dow=wday(date, TRUE)) %>%
    group_by(dow) %>%
    summarise(variable=sum(variable), wszystkie=n()) %>%
    arrange(dow)
  return(a)
}

getDestCounts <- function(df, dfrom=dmin, dto=dmax, agg="awaria") {
  col.nums <- match(agg,names(df))[1] # first argument only
  output <- df %>%
    dplyr::select(dates, miasto.from,miasto.to, v=col.nums) %>%
    filter(dates >= dfrom & dates <= dto & ( v > 0))
  if (nrow(output) > 0) {
  tt <- as.data.frame(table(output$miasto.from,output$miasto.to),stringsAsFactors = FALSE) 
  tt <- mutate(tt, pc=Freq/sum(Freq)*100)
  filter(tt, pc>0)
  } 
  else { (return(NULL))}
}

freeDaysCount <- function(df, dfrom=dmin, dto=dmax) {
  tot <- difftime(dto,dfrom,units="days")
  fd <- tot-B
  fd2 <- tot-A
  c(fd,fd2,tot)
}


noDataPlot <- function(){
  par(mar = c(0,0,0,0))
  plot(c(0, .5), c(0, .5), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  
  text(x = 0.1, y = 0.5, paste("No data available or error occured."), 
       cex = 1.5, col = "gray50", family="serif", font=2, adj=0.5)
}


countDates <- function(df) {
  length(apply.daily(x=read.zoo(df),colSums))
}

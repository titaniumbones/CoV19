plot1 <- function(data=c("states","world","italy"), location=NULL, lockdown=NULL){
  data <- match.arg(data)
  data(list=data, package="CoV19" )
  x <- get(data)
  if(data=="states")  x <- subset(x, state==location)
  if(data=="world")  x <- subset(x, Province.State==location | Country.Region==location)
  if(data=="italy")  x <- subset(x, region==location)
  x <- x[order(x$date),]
    x$new.cases <- c(NA,diff(x$positive))
    if(!is.null(x$recovered)) x$active <- x$positive-x$recovered
    if(is.null(x$hospitalized)) x$hospitalized <- x$active*0.15
    if(!all(diff(x$date)==1)){
      cat("Problem. Time series is missing days.\n")
      return(x)
    }

    ylim.prim <- c(0, max(x$positive, na.rm=TRUE))   
    ylim.sec <- c(0, max(x$new.cases, na.rm=TRUE))
    b <- diff(ylim.prim)/diff(ylim.sec)-2
    a <- b*(ylim.prim[1] - ylim.sec[1])
    
    p <- ggplot(x, aes(x=date, y=positive)) +
      geom_col(col="grey",fill="grey") + 
      xlab("")
    if(!is.null(x$active)) p <- p + geom_col(aes(x=date, y=active),fill="yellow", col="yellow") 
    if(!is.null(x$hospitalized)) p <- p + geom_col(aes(x=date, y=hospitalized),fill="blue", col="blue")
    p <- p +
      geom_line(aes(x=date, y=a+b*new.cases)) +
      geom_point(aes(x=date, y=a+b*new.cases))
    p <- p+scale_x_date(date_labels = "%b %d") +
      scale_y_continuous("Cumulative Cases", 
                         sec.axis = sec_axis(~ (. - a)/b, name = "Daily New Cases"))
    if(!is.null(lockdown)){
      p <- p + geom_vline(xintercept = as.Date(lockdown))
    }
    p + ggtitle(location)
}
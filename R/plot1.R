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
    p <- p + geom_vline(xintercept = as.Date(lockdown)) +
    annotate("text", x=as.Date(lockdown), y=max(x$positive), hjust=0, label=" Lockdown", color="red", size=3)
  }
  #legend
  # I know, I know, I should but the data frame in long form and use aes() for legends
  # But I always have to fight ggplot when I do that. It's defaults are never my goal
  xw = (max(x$date)-min(x$date))*.075
  p <- p + 
    geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=0.875*max(x$positive), ymax=0.95*max(x$positive)), fill="grey") +
    annotate("text", x=min(x$date)+xw, y=(0.95-(0.95-0.875)/2)*max(x$positive), label="  Total",hjust=0)
  if(!is.null(x$hospitalized)) p <- p + geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=0.775*max(x$positive), ymax=0.85*max(x$positive)), fill="blue") +
    annotate("text", x=min(x$date)+xw, y=(0.85-(0.85-0.775)/2)*max(x$positive), label="  Hospitalized",hjust=0)
  if(!is.null(x$active)) p <- p + geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=0.675*max(x$positive), ymax=0.75*max(x$positive)), fill="yellow")  +
    annotate("text", x=min(x$date)+xw, y=(0.75-(0.75-0.675)/2)*max(x$positive),hjust=0, label="  Active")
  
  p + ggtitle(location)
}
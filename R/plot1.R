#' Make a plot of positives, active and recovered versus time
#' 
#' Makes a plot of positives, active and recovered versus time. New cases per day is added as a line. 
#' If lockdown date is given, a line is added showing that. Plot is shown for a specific region (state, country, province, etc). Use regions() to see what locations are available. See ?getdatajhu, ?getdataitaly, ?getdatastates to see the data sources.
#' 
#' @param data Name of a data set. "states", "world" or "italy"
#' @param region Name of a region in the region (location) column of the dataset (dataset$region). Might be country, state, or province. Use regions() to see what is available.
#' @param lockdown (optional) Date of the lockdown in "2020-03-11" format.
#' @examples
#' plot1(data="italy", region="Lombardia", lockdown="2020-03-11")
#' plot1(data="states", region="NY", lockdown="2020-03-21")
plot1 <- function(data=c("states","world","italy"), region, lockdown=NULL){
  data <- match.arg(data)
  data(list=data, package="CoV19" )
  x <- get(data)
  if(!region %in% x$region){
    cat("region is not in region column of the dataset. Use regions(), to see the available regions.\n")
    return()
  }
  x <- x[x$region==region, ]
  x <- x[order(x$date),]
  x$new.cases <- c(NA,diff(x$positive))
  if(!is.null(x$recovered)) x$active <- x$positive-x$recovered
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
  y1 <- 0.85
  if(!is.null(x$hospitalized)){
    p <- p + geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=0.775*max(x$positive), ymax=y1*max(x$positive)), fill="blue") +
    annotate("text", x=min(x$date)+xw, y=(y1-0.075/2)*max(x$positive), label="  Hospitalized",hjust=0)
    y1 <- y1-.1
  }
  if(!is.null(x$active)) p <- p + geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=0.675*max(x$positive), ymax=y1*max(x$positive)), fill="yellow")  +
    annotate("text", x=min(x$date)+xw, y=(y1-0.075/2)*max(x$positive),hjust=0, label="  Active")
  
  p + ggtitle(region)
}
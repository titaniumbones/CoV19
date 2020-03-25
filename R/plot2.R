#' Make a plot of positives, active and recovered versus time
#' 
#' Makes a plot of positives, active and recovered versus time. New cases per day is added as a line. 
#' If decorate, then mitigations will be shown (if available). Plot is shown for a specific region (state, country, province, etc). Use regions() to see what locations are available. See ?getdatajhu, ?getdataitaly, ?getdatastates to see the data sources.
#' 
#' @param data Name of a data set. states, world or italy. You can pass in merged or subsetted versions of these too.
#' @param region Name of a region in the region (location) column of the dataset (dataset$region). If it is not passed in, all regions will be shown. Might be country, state, or province. Use regions() to see what is available. You can pass in multiple regions.
#' @param decorate Mitigation times are shown if available in the mitigations data.
#' @examples
#' plot2(italy, region="Lombardia", decorate=FALSE)
#' plot2(states, region=c("NY","WA"))
plot2 <- function(x, region=NULL, decorate=FALSE){
  if(!all(c("date", "region", "positive") %in% colnames(x))) stop("data is missing data, region or positive columns. These are minimally required.")
  if(missing(region)) region <- unique(as.character(x$region))
  if(length(region) > 1) x <- x[x$region %in% region, ]
  if(length(region) == 1) x <- x[stringr::str_detect(x$region, fixed(region)), ]
  x <- x[order(x$region, x$date),]
  if(class(x$region)=="character") x$region <- as.factor(x$region)
  x$region <-droplevels(x$region)
  x$new.cases <- unname(unlist(tapply(x$positive, x$region, function(x){c(NA,diff(x))})))
  x$new.deaths <- unname(unlist(tapply(x$death, x$region, function(x){c(NA,diff(x))})))
  if("recovered" %in% colnames(x)) x$active <- x$positive-x$recovered
  if("hospitalized" %in% colnames(x) && all(is.na(x$hospitalized))) x$hospitalized <- NULL
  if(decorate){
    for(i in colnames(mitigations)[-1]){
      x[,i] <- as.Date(NA)
      for(j in unique(x$region)) x[x$region==j,i] <- mitigations[mitigations$region==j,i]
    }
  }
  
  ylim.prim <- c(0, max(x$positive, na.rm=TRUE))   
  ylim.sec <- c(0, max(x$new.cases, na.rm=TRUE))
  b <- diff(ylim.prim)/diff(ylim.sec)-2
  a <- b*(ylim.prim[1] - ylim.sec[1])
  
  p <- ggplot(x, aes(x=date, y=positive)) +
    geom_col(col="grey",fill="grey") + 
    xlab("")
  if("active" %in% colnames(x)) p <- p + geom_col(aes(x=date, y=active),fill="yellow", col="yellow") 
  if("hospitalized" %in% colnames(x)) p <- p + geom_col(aes(x=date, y=hospitalized),fill="blue", col="blue")
  p <- p + geom_col(aes(x=date, y=death),fill="red", col="red")
  p <- p +
    geom_line(aes(x=date, y=a+b*new.cases)) +
    geom_point(aes(x=date, y=a+b*new.cases)) +
    geom_line(aes(x=date, y=a+b*new.deaths), linetype="dashed") +
    geom_point(aes(x=date, y=a+b*new.deaths), color="blue")
  p <- p+scale_x_date(date_labels = "%b %d") +
    scale_y_continuous("Cumulative Cases", 
                       sec.axis = sec_axis(~ (. - a)/b, name = "Daily New Cases and Deaths"))

  #legend
  # I know, I know, I should but the data frame in long form and use aes() for legends
  # But I always have to fight ggplot when I do that. It's defaults are never my goal
  xw = (max(x$date)-min(x$date))*0.075
  p <- p + 
    geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=0.875*max(x$positive), ymax=0.95*max(x$positive)), fill="grey") +
    annotate("text", x=min(x$date)+xw, y=(0.95-(0.95-0.875)/2)*max(x$positive), label="  Total",hjust=0)
  y1 <- y2 <- y3 <- 0.85 # weirdly ggplot objects change when the variable val changes, so can't just update y1
  if("hospitalized" %in% colnames(x)){
    p <- p + geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=(y1-0.075)*max(x$positive), ymax=y1*max(x$positive)), fill="blue") +
      annotate("text", x=min(x$date)+xw, y=(y1-0.075/2)*max(x$positive), label="  Hospitalized",hjust=0)
    y2 <- y3 <- y1-0.1
  }
  if("active" %in% colnames(x)){
    p <- p + geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=(y2-0.075)*max(x$positive), ymax=y2*max(x$positive)), fill="yellow")  +
      annotate("text", x=min(x$date)+xw, y=(y2-0.075/2)*max(x$positive),hjust=0, label="  Active")
    y3 <- y2-0.1
  }
  p <- p + geom_rect(mapping=aes(xmin=min(x$date), xmax=min(x$date)+xw, ymin=(y3-0.075)*max(x$positive), ymax=y3*max(x$positive)), fill="red")  +
    annotate("text", x=min(x$date)+xw, y=(y3-0.075/2)*max(x$positive),hjust=0, label="  Deaths")
  if(decorate){
    for(i in 2:ncol(mitigations)){
      val <- colnames(mitigations)[i]
      p <- p + geom_vline(xintercept = x[1,val]) +
        annotate("text", x=x[1,val]+0.25, y=.5*max(x$positive), hjust=0, label=val, color="red", size=3, angle=90)
    }
  }
  if(length(levels(x$region))>1){
    p <- p + facet_wrap(~region)
  }else{
    p <- p + ggtitle(x$region[1])
  }
  p
}

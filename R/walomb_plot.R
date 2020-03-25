my_plot <- function(x, reg, lockdown=NULL){
  p<-plot2(x, reg)
  ymax <- max(subset(x, region==reg)$positive, na.rm=TRUE)
  if(!missing(lockdown)){
  p <- p + geom_vline(xintercept = as.Date(lockdown)) +
    annotate("text", x=as.Date(lockdown)+0.5, y=.5*ymax, hjust=0, label=paste(reg, "lockdown"), color="red", size=3, angle=90)
  p <- p + geom_vline(xintercept = as.Date(lockdown)+12, linetype = "dashed") +
    annotate("text", x=as.Date(lockdown)+12+0.5, y=.5*ymax, hjust=0, label="12 days past lockdown", color="red", size=3, angle=90)
  }
  p
}

walomb_plot <- function(){
  my_plot(states, "WA", "2020-03-16")
  my_plot(italy, "Lombardia", "2020-03-10")
}


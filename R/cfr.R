#' SImplify plotting of CFR charts

#' Collect and munge the data
cfr.countries<-c("France", "Canada", "US", "Spain","United Kingdom", "Russia", "Poland","Czechia")
cfr.world<-c();

cfr.iprovinces<-c("Lombardia");
cfr.italy<-c();

cfr.statelist<-c("NY","NJ","WA","CA","LA");
cfr.states<-c();

for(reg in cfr.countries) {
    regdat<-cfr.collector(subset(world, region==reg), reg)
    cfr.world <- rbind(cfr.world,regdat)
}

for(reg in cfr.italy.provinces) {
    regdat<-cfr.collector(subset(italy, region==reg), reg)
    cfr.italy <- rbind(cfr.italy,regdat)
}

for(reg in cfr.statelist){
    regdat<-cfr.collector(subset(states, region==reg), reg)
    cfr.states <- rbind(cfr.states,regdat)
}



# don't write this twice
cfr.smoother<- function(in.val) {
  return(100*round(mean(in.val[(length(in.val)-3):length(in.val)]),digits=3))
  
}

# simplify data collection
cfr.collector<-function (x, reg) {
  all.lags=c()
  for(lag in 0:10){
    tmp <- data.frame(x=x$death[(1+lag):nrow(x)],
                      val= x$death[(1+lag):nrow(x)]/x$positive[1:(nrow(x)-lag)],
                      lag=lag, region=reg)

    tmp$lag <- as.factor(tmp$lag)
    tmp$log.val <- log(tmp$val)
    all.lags <- rbind(all.lags,tmp)
  }
  return(all.lags)
}

# plotfun renamedsemantically
cfr.plotter <- function(dataset, reg, ylims=c(-4,0), lags=c(0,3,5,7,10), xlims=c(100,5000)){
  val1 <- cfr.smoother(subset(dataset, region==reg & lag==5)$val)
  val2 <- cfr.smoother(subset(dataset, region==reg & lag==7)$val)
  p <- ggplot(subset(dataset, region==reg & lag%in%lags), aes(x=x,y=log.val,color=lag)) + geom_line() + geom_point() +
    xlim(xlims) + ggtitle(paste0(reg," ",val1," to ", val2,"%")) +
    xlab("Cumulative Deaths") +
    scale_y_continuous(name="lagged CFR", breaks=ylims[1]:ylims[2], 
                       labels=round(exp(ylims[1]:ylims[2]),digits=2), limits=ylims)
  p
}



cfr.plotter(cfr.world, "Italy", c(-4,0))

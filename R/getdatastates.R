#' Download state data from Covid Tracking
#' 
#' The data cutoff is 4pm EST. So some states are delayed by one day because they report after that time. https://covidtracking.com/api/states/daily
#' 
#' @param json Whether to get data from JSON (updated faster) or csv link.
#' @return Saves a data frame to the data folder.
#' @examples
#' \dontrun{
#' getdatastates()
#' }
getdatastates <- function(json=TRUE){
    if(!json){
    url <- "http://covidtracking.com/api/states/daily.csv"
    statedata <- try(read.csv(url), silent=TRUE)
    }else{
      url <- "https://covidtracking.com/api/states/daily"
      statedata <- try(jsonlite::fromJSON("https://covidtracking.com/api/states/daily"))
    }
    colnames(statedata)[colnames(statedata)=="state"] <- "region"
    colnames(statedata)[colnames(statedata)=="total"] <- "total.tests"
    if(!inherits(statedata, "try-error")){
      statedata <- statedata[,colnames(statedata)!="dateChecked"]
      statedata$date <- as.Date(as.character(statedata$date), "%Y%m%d")
      states <- statedata
      # If nothing has changed then data were not updated
      cols <- c("positive", "negative", "pending", "hospitalized", "death", "total.tests")
      for(reg in state.abb){
        test <- states[states$region == reg & states$date==max(states$date),cols] == states[states$region==reg & states$date==max(states$date)-1,cols]
        if(all(test, na.rm=TRUE)){
          states[states$region == reg & states$date==max(states$date),cols] <- NA
        }
      }
      states <- statedata[,c("date", "region", "positive", "negative", "hospitalized", "death", "total.tests")]
      
      # Fix WA numbers
      # Dates are off
      states$date[states$region=="WA"] <- states$date[states$region=="WA"]-2
      cols <- c("positive", "negative", "hospitalized", "death", "total.tests")
      for(i in c("2020-03-05", "2020-03-04", "2020-03-03"))
        states[states$region=="WA" & states$date==as.Date(i), cols] <- states[states$region=="WA" & (states$date==(as.Date(i)-1)), cols]
      states[states$region=="WA" & states$date==as.Date("2020-03-02"), cols] <- c(28, NA, NA, 9, NA)
      states[states$region=="WA" & states$date==as.Date("2020-03-29"), cols] <- c(5062, NA, NA, NA, NA)
      states[states$region=="WA" & states$date==as.Date("2020-03-30"), cols] <- c(5515, NA, NA, NA, NA)
      if(!any(states$region=="WA" & states$date==as.Date("2020-03-31"))){
        states <- rbind(states, 
                        data.frame(date=as.Date("2020-03-31"), region="WA",
                                   positive=5984, negative=NA, hospitalized=NA,
                                   death=247, total.tests=NA))
      }else{
      states[states$region=="WA" & states$date==as.Date("2020-03-31"), cols] <- c(5984, NA, NA, 247, NA)
      }
      
      states <- states[order(states$region, states$date),]
      
      
      save(states, file="data/states.RData")
      cat("Success! Data downloaded.\n")
      invisible(states)
    }else{
      cat("Server error. Data could not be downloaded.\n")
      return()
    }
}
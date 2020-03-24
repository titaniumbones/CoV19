#' Download state data from Covid Tracking
#' 
#' The data cutoff is 4pm EST. So some states are delayed by one day because they report after that time. https://covidtracking.com/api/states/daily
#' 
#' @param append Whether to append current numbers to an existing data frame (append=TRUE) or to download the whole time series again (append=FALSE).
#' @param json Whether to get data from JSON (updated faster) or csv link.
#' @return Saves a data frame to the data folder.
#' @examples
#' \dontrun{
#' getdatastates()
#' }
getdatastates <- function(append=FALSE, json=TRUE){
  if(append){
    if(max(statedatadate) < Sys.Date()-1){
      cat(paste0("No update because the state data file max date is ", max(statedatadate), " and appending would skip a day.\n"))
      return()
      }
    }
  if(!append){
    if(!json){
    url <- "http://covidtracking.com/api/states/daily.csv"
    statedata <- try(read.csv(url), silent=TRUE)
    }else{
      url <- "https://covidtracking.com/api/states/daily"
      statedata <- try(fromJSON("https://covidtracking.com/api/states/daily"))
    }
    colnames(statedata)[colnames(statedata)=="state"] <- "region"
    colnames(statedata)[colnames(statedata)=="total"] <- "total.tests"
    if(!inherits(statedata, "try-error")){
      statedata <- statedata[,colnames(statedata)!="dateChecked"]
      statedata$date <- as.Date(as.character(statedata$date), "%Y%m%d")
      states <- statedata
      save(states, file="data/states.RData")
      cat("Success! Data downloaded.\n")
      invisible(states)
    }else{
      cat("Server error. Data could not be downloaded.\n")
      return()
    }
  }else{
    load("data/states.RData")
    statedatadate <- as.Date(states$dateChecked)
    if(max(statedatadate) == Sys.Date()){
      ans <- readline("The state data is has the same date as current date. Still update? (y/n)")
      if(ans != "y"){ cat("No update.\n"); return() }
    }
    url <- "https://covidtracking.com/api/states.csv"
    stateday <- read.csv(url)
    colnames(stateday)[colnames(statedata)=="state"] <- "region"
    colnames(stateday)[colnames(statedata)=="total"] <- "total.tests"
    stateday$date <- as.Date(stateday$dateChecked)
    stateday <- stateday[,colnames(stateday)!="dateChecked"]
    daydate <- as.Date(stateday$dateChecked)[1]
    statedata <- states[statedatadate != daydate,]
    statecols <- colnames(statedata)
    statedata <- rbind(stateday[,statecols],statedata)
    states <- statedata
    cat("Success! Data downloaded.\n")
    save(states, file="data/states.RData")
    invisible(states)
  }
}
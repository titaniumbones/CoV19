#' Download state data from Covid Tracking
#' 
#' @param append Whether to append current numbers to an existing data frame (append=TRUE) or to download the whole time series again (append=FALSE).
#' @return Saves a data frame to the data folder.
#' @examples
#' getdatacovidtrackint
getdatacovidtracking <- function(append=TRUE){
  if(append){
    if(max(statedatadate) < Sys.Date()-1){
      cat(paste0("No update because the state data file max date is ", max(statedatadate), " and appending would skip a day.\n"))
      return()
      }
    }
  if(!append){
    url <- "http://covidtracking.com/api/states/daily.csv"
    statedata <- try(read.csv(url), silent=TRUE)
    if(!inherits(statedata, "try-error")){
      statedata <- statedata[,colnames(statedata)!="dateChecked"]
      statedata$date <- as.Date(as.character(statedata$date), "%Y%m%d")
      states <- statedata
      save(states, file="data/states.RData")
    }else{
      cat("Server error. Data could not be downloaded.\n")
      return()
    }
  }else{
    load("data/states.RData")
    statedata <- states
    statedatadate <- as.Date(statedata$dateChecked)
    if(max(statedatadate) == Sys.Date()){
      ans <- readline("The state data is has the same date as current date. Still update? (y/n)")
      if(ans != "y"){ cat("No update.\n"); return() }
    }
    save(statedata, file="data/statesold.RData")
    url <- "https://covidtracking.com/api/states.csv"
    stateday <- read.csv(url)
    stateday$date <- as.Date(stateday$dateChecked)
    stateday <- stateday[,colnames(stateday)!="dateChecked"]
    daydate <- as.Date(stateday$dateChecked)[1]
    statedata <- statedata[statedatadate != daydate,]
    statecols <- colnames(statedata)
    statedata <- rbind(stateday[,statecols],statedata)
    states <- statedata
    save(states, file="data/states.RData")
  }
}
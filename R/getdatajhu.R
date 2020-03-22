#' Download confirmed, deaths and recovered from JHU
#' 
#' @return Saves a data frame with Province.State, Country.Region, date, positive, death, and recovered to the data folder.
#' @examples
#' getdatajhu
getdatajhu <- function(){
  getx <- function(x, val){
    x <- x[, !(colnames(x) %in% c("Lat","Long"))]
    library(tidyr)
    x %>% pivot_longer(cols=starts_with("X"),names_to="date", values_to = val)
  }
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
    worlddataconfirmed <- try(read.csv(url), silent=TRUE)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
    worlddatadeaths <- try(read.csv(url), silent=TRUE)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
    worlddatarecovered <- try(read.csv(url), silent=TRUE)
    
    worlddata <- cbind(getx(worlddataconfirmed,"positive"),death=getx(worlddatadeaths,"death")$death, recovered=getx(worlddatarecovered,"recovered")$recovered)
    
    worlddata$date <- as.Date(str_remove(worlddata$date,"X"),"%m.%d.%y")
    world <- worlddata
    save(world, file="data/world.RData")
}
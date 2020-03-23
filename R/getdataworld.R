#' Download confirmed, deaths and recovered from JHU database
#' 
#' @return Saves a data frame with date, region, positive, death, and recovered to the data folder.
#' @examples
#' \dontrun{
#' getdataworld()
#' }
getdataworld <- function(){
  getx <- function(x, val){
    x <- x[, !(colnames(x) %in% c("Lat","Long"))]
    x %>% tidyr::pivot_longer(cols=starts_with("X"),names_to="date", values_to = val)
  }
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
    worlddataconfirmed <- try(read.csv(url), silent=TRUE)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
    worlddatadeaths <- try(read.csv(url), silent=TRUE)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
    worlddatarecovered <- try(read.csv(url), silent=TRUE)
    if(inherits(worlddataconfirmed, "try-error") || 
       inherits(worlddatadeaths, "try-error") ||
       inherits(worlddatarecovered, "try-error")){
      cat("Server error. Data was not downloaded.\n")
      return()
    }
    
    worlddata <- cbind(getx(worlddataconfirmed,"positive"),death=getx(worlddatadeaths,"death")$death, recovered=getx(worlddatarecovered,"recovered")$recovered)
    
    worlddata$date <- as.Date(stringr::str_remove(worlddata$date,"X"),"%m.%d.%y")
    worlddata$region <- paste(worlddata$Province.State, worlddata$Country.Region)
    worlddata$region <- stringr::str_trim(worlddata$region)
    worlddata <- worlddata[,c("date", "region", "positive", "death", "recovered")]
    world <- worlddata
    cat("Success! Data downloaded.\n")
    save(world, file="data/world.RData")
    invisible(world)
}
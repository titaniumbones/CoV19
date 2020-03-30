#' Download confirmed and deaths from new JHU database
#' 
#' @return Saves a data frame with date, region, positive, and death to the data folder.
#' @examples
#' \dontrun{
#' getdataworld()
#' }
getdataworld <- function(){
  getx <- function(x, val){
    x <- x[, !(colnames(x) %in% c("Lat","Long"))]
    x %>% tidyr::pivot_longer(cols=starts_with("X"),names_to="date", values_to = val)
  }
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    worlddataconfirmed <- try(read.csv(url), silent=TRUE)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    worlddatadeaths <- try(read.csv(url), silent=TRUE)
    if(inherits(worlddataconfirmed, "try-error") || 
       inherits(worlddatadeaths, "try-error")){
      cat("Server error. Data was not downloaded.\n")
      return()
    }
    
    worlddata <- cbind(getx(worlddataconfirmed,"positive"),
                       death=getx(worlddatadeaths,"death")$death)
    
    worlddata$date <- as.Date(stringr::str_remove(worlddata$date,"X"),"%m.%d.%y")
    worlddata$region <- paste(worlddata$Province.State, worlddata$Country.Region)
    worlddata$region <- stringr::str_trim(worlddata$region)
    worlddata$region <- as.factor(worlddata$region)
    worlddata <- worlddata[,c("date", "region", "positive", "death")]
    world <- worlddata
    
    # Create merged set for China
    x <- world %>% 
      subset(str_detect(region, "China")) %>%
      group_by(date) %>%
      summarize_if(is.numeric, sum, na.rm=TRUE)
    # This will not have the region column, so we add that back on
    x$region <- "China"
    world <- rbind(world, x)
    
    cat("Success! Data downloaded.\n")
    save(world, file="data/world.RData")
    invisible(world)
}
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
    worlddataconfirmed <- try(read.csv(url, stringsAsFactor=FALSE), silent=TRUE)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
    tmp <- try(read.csv(url, stringsAsFactor=FALSE), silent=TRUE)
    if(inherits(worlddataconfirmed, "try-error") || 
       inherits(tmp, "try-error")){
      cat("Server error. JH confirmed data was not downloaded.\n")
      return()
    }
    colnames(tmp)[colnames(tmp) == "Long_"] <- "Long"
    colnames(tmp) <- stringr::str_replace_all(colnames(tmp), "_", ".")
    colnames(tmp) <- stringr::str_replace_all(colnames(tmp), "2020", "20")
    tmp$Province.State <- paste(tmp$Admin2, tmp$Province.State)
    tmp <- tmp[, colnames(tmp) %in% colnames(worlddataconfirmed)]
    worlddataconfirmed <- dplyr::bind_rows(worlddataconfirmed, tmp)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    worlddatadeaths <- try(read.csv(url, stringsAsFactor=FALSE), silent=TRUE)
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    tmp <- try(read.csv(url, stringsAsFactor=FALSE), silent=TRUE)
    if(inherits(worlddatadeaths, "try-error") || 
       inherits(tmp, "try-error")){
      cat("Server error. JH deaths data was not downloaded.\n")
      return()
    }
    colnames(tmp)[colnames(tmp) == "Long_"] <- "Long"
    colnames(tmp) <- stringr::str_replace_all(colnames(tmp), "_", ".")
    colnames(tmp) <- stringr::str_replace_all(colnames(tmp), "2020", "20")
    tmp$Province.State <- paste(tmp$Admin2, tmp$Province.State)
    tmp <- tmp[, colnames(tmp) %in% colnames(worlddatadeaths)]
    worlddatadeaths <- dplyr::bind_rows(worlddatadeaths, tmp)

    worlddata <- cbind(getx(worlddataconfirmed,"positive"),
                       death=getx(worlddatadeaths,"death")$death)
    
    worlddata$date <- as.Date(stringr::str_remove(worlddata$date,"X"),"%m.%d.%y")
    worlddata$region <- paste(worlddata$Province.State, worlddata$Country.Region)
    worlddata$region <- stringr::str_trim(worlddata$region)
    worlddata$region <- as.factor(worlddata$region)
    worlddata <- worlddata[,c("date", "region", "positive", "death")]
    world <- worlddata
    
    # Create merged set for China and Canada
    for(reg in c("China", "Canada")){
    x <- world %>% 
      subset(str_detect(region, reg) & !(region==reg)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize_if(is.numeric, function(x){ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))})
    # This will not have the region column, so we add that back on
    x$region <- reg
    world <- rbind(world, x)
    }
    # Create merged set for states
    for(reg in paste(state.name, "US")){
      x <- world %>% 
        subset(str_detect(region, reg) & !(region==reg)) %>%
        dplyr::group_by(date) %>%
        dplyr::summarize_if(is.numeric, function(x){ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))})
      # This will not have the region column, so we add that back on
      x$region <- reg
      world <- rbind(world, x)
    }
    
    cat("Success! Data downloaded.\n")
    save(world, file="data/world.RData")
    invisible(world)
}
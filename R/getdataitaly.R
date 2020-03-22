#' Download confirmed, deaths and recovered from github/pcm-dpc/COVID-19
#' 
#' @return Saves a data frame with region, date, positive, death, recovered, total hospitalized, and ICU to the data folder.
#' @examples
#' getdatajhu
getdataitaly <- function(){
  getx <- function(x, val){
    x <- x[, !(colnames(x) %in% c("Lat","Long"))]
    library(tidyr)
    x %>% pivot_longer(cols=starts_with("X"),names_to="date", values_to = val)
  }
    url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
    italydata <- try(read.csv(url), silent=TRUE)
    italydata$recovered <- italydata$totale_casi-italydata$totale_attualmente_positivi
    #rename
    iname <- c("data", "denominazione_regione", "totale_casi", "deceduti", "recovered", "totale_ospedalizzati", "terapia_intensiva")
    italydata <- italydata[,iname]
    colnames(italydata) <- c("date", "region", "positive", "death", "recovered", "hospitalized", "ICU")

    italydata$date <- as.Date(italydata$date)
    save(italydata, file="data/italy.RData")
  }
}
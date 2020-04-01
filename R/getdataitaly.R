#' Download confirmed, deaths and recovered from Italian CDC
#' 
#' Date is posted on the github site daily at about 3pm EST. github/pcm-dpc/COVID-19
#' 
#' @return Saves a data frame with region, date, positive, death, recovered, total hospitalized, and ICU to the data folder.
#' @examples
#' \dontrun{
#' getdataitaly()
#' }
getdataitaly <- function(){
  getx <- function(x, val){
    x <- x[, !(colnames(x) %in% c("Lat","Long"))]
    x %>% tidyr::pivot_longer(cols=starts_with("X"),names_to="date", values_to = val)
  }
    url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
    italydata <- try(read.csv(url), silent=TRUE)
    if(inherits(italydata, "try-error")){
      cat("Server error. Data was not downloaded.\n")
      return()
    }
    italydata$recovered <- italydata$totale_casi-italydata$totale_positivi
    #rename
    iname <- c("data", "denominazione_regione", "totale_casi", "deceduti", "recovered", "totale_ospedalizzati", "terapia_intensiva")
    italydata <- italydata[,iname]
    colnames(italydata) <- c("date", "region", "positive", "death", "recovered", "hospitalized", "ICU")

    italydata$date <- as.Date(italydata$date)
    italydata <- italydata[,c("date", "region", "positive","hospitalized",
                              "death", "recovered", "ICU")]
    italy <- italydata
    cat("Success! Data downloaded.\n")
    save(italy, file="data/italy.RData")
    invisible(italy)
}
#' Update datasets
#' 
#' Saves data frames to data folder.
#' 
#' @examples
#' \dontrun{
#' getdata()
#' }
getdata <- function(){
  tries <- list()
  tries <- list(tries, try(getdataworld()))
  tries <- list(tries, try(getdataitaly()))
  tries <- list(tries, try(getdatastates()))
  return(unlist(lapply(tries, function(x){!inherits(x, "try-error")})))
}
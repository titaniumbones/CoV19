#' Show the possible regions in the data files
#' 
#' Type regions() and then it will ask what data set you want to use. You can also enter
#' a pattern to reduce the regions. For example, type "WA US" to show all regions with
#' the text "WA US". Use "China" to show all regions in China.
#' 
#' @examples
#' \dontrun{
#' regions()
#' Which dataset? a) states, b) world, c) italy? (a/b/c) b
#' Search pattern? (return to show all) WA US
#' }
regions <- function(){
  ans <- readline("Which dataset? a) states, b) world, c) italy? (a/b/c) ")
  if(!(ans %in% c("a","b","c"))){
    cat("Answer must be a, b, or c. \n")
    return()
  }
  pattern <- readline("Search pattern? (return to show all) ")
  x <- switch(ans,
         a = states$region,
         b = world$region,
         c = italy$region
         )
  locs <- unique(as.character(x))
  return(locs[stringr::str_detect(locs, pattern)])
}
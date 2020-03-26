#' Add a vertical line to ggplot
#' 
#' Adds a line with annotation at a specified date (x-axis)
#' 
#' @param p The plot
#' @param ltext Text to add parallel to the line
#' @param ldate Date on which to put the line
#' 
add_vline <- function(p, ltext, ldate, linetype = "solid"){
  ymax <- layer_scales(p)$y$range$range[2]
  p <- p + geom_vline(xintercept = as.Date(ldate), linetype = linetype) +
    annotate("text", x=as.Date(ldate)+0.5, y=.5*ymax, hjust=0, label=ltext, color="red", size=3, angle=90)
  p
}
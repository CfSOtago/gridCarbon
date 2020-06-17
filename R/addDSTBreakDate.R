#' \code{addDSTBreakDate} adds label for DST break 31/3/2020 where x axis is a date. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addDSTBreakDate <- function(p, yMin, yMax){
  p <- p + annotate("text", x = as.Date("2020-03-29"),
             y = yMin*1.05, # avoids other labels
             label = "DST break 2020") +
    geom_vline(xintercept = as.Date("2020-03-29"),
               yMin = yMin,
               yMax = yMax,
               linetype = "dotted")
  return(p)
}
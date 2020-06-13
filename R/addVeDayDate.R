#' \code{addVeDayDate} adds label for VE day 2020 where x axis is a date. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addVeDayDate <- function(p, yMin, yMax){
  p <- p + annotate("text", x = as.Date("2020-05-08"),
             y = yMax*gcParams$labelPos,
             label = "VE Day 2020")
  return(p)
}
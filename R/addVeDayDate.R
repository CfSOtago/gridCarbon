#' \code{addVeDayDate} adds extended weekend shading and label for VE day 2020 where x axis is a date. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addVeDayDate <- function(p, yMin, yMax){
  p <- p + annotate("rect", xmin = as.Date("2020-05-08"),
           xmax = as.Date("2020-05-11"), # 3 day weekend starting Friday (in UK)
           ymin = yMin, ymax = yMax,
           alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("text", x = as.Date("2020-05-08"),
             y = yMax*gcParams$labelPos,
             label = "VE Day 2020")
  return(p)
}
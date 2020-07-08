#' \code{addVeDayDate} adds extended weekend shading and label for VE day 2020 where x axis is a date. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yLoc the reltative height of the label (so you can stagger them on a plot)
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addVeDayDate <- function(p, yLoc, yMin, yMax){
  p <- p + annotate("rect", xmin = as.Date("2020-05-08"),
           xmax = as.Date("2020-05-09"), # 3 day weekend starting Friday (in UK)
           ymin = yMin, ymax = yMax,
           alpha = 0.5, fill = gcParams$weFill) + #gcParams$weAlpha
    annotate("text", x = as.Date("2020-05-08"),
             y = yLoc * yMax,
             label = "VE Day 2020")
  return(p)
}
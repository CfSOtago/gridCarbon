#' \code{addWhitsunDate} adds extended weekend shading and label for UK Whit Sunday (late spring bank holiday) where x axis is a date. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addWhitsunDate <- function(p, yMin, yMax){
  p <- p + annotate("rect", xmin = as.Date("2020-05-23"),
           xmax = as.Date("2020-05-26"), # 3 day weekend starting Saturday (in UK)
           ymin = yMin, ymax = yMax,
           alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("text", x = as.Date("2020-05-24"),
             y = yMax*gcParams$labelPos,
             label = "Whitsun 2020")
  return(p)
}
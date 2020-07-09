#' \code{addWhitsunDate} adds extended weekend shading and label for UK Whit Sunday (late spring bank holiday) where x axis is a date. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yLoc the reltative height of the label (so you can stagger them on a plot)
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @param fill fill colour (grey50)
#' @param alpha alpha value (0.5)
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addWhitsunDate <- function(p, yLoc, yMin, yMax, fill = "grey50", alpha = 0.5){
  p <- p + annotate("rect", xmin = as.Date("2020-05-25"),
           xmax = as.Date("2020-05-26"), # 3 day weekend starting Saturday (in UK)
           ymin = yMin, ymax = yMax,
           alpha = alpha, fill = fill) +
    annotate("text", x = as.Date("2020-05-25"),
             y = yLoc * yMax,
             label = "Whitsun 2020")
  return(p)
}
#' \code{addWhitsunDate} adds extended weekend shading and label for UK Whit Sunday (late spring bank holiday) 
#' where x axis is a dateTime. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yLoc the reltative height of the label (so you can stagger them on a plot)
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addWhitsunDateTime <- function(p, yLoc, yMin, yMax){
  p <- p + annotate("rect", xmin = as.Date("2020-05-25 00:00:00"),
           xmax = as.Date("2020-05-25 23:59:59"), # 3 day weekend starting Saturday (in UK)
           ymin = yMin, ymax = yMax,
           alpha = 0.5, fill = gcParams$weFill) +
    annotate("text", x = as.Date("2020-05-25"),
             y = yLoc * yMax,
             label = "Whitsun 2020")
  return(p)
}
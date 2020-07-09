#' \code{addWhitsunDate} adds extended weekend shading and label for UK Whit Sunday (late spring bank holiday) 
#' where x axis is a dateTime. Should work in any time-zone
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
addWhitsunDateTime <- function(p, yLoc, yMin, yMax, fill = "grey50", alpha = 0.5){
  p <- p + annotate("rect", xmin = lubridate::as_datetime("2020-05-25 00:00:00"),
           xmax = lubridate::as_datetime("2020-05-25 23:59:59"), # 3 day weekend starting Saturday (in UK)
           ymin = yMin, ymax = yMax,
           alpha = alpha, fill = fill) +
    annotate("text", x = lubridate::as_datetime("2020-05-25 12:00:00"),
             y = yLoc * yMax,
             label = "Whitsun 2020")
  return(p)
}
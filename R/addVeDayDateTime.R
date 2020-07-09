#' \code{addVeDayDateTime} adds extended weekend shading and label for VE day 2020 where x axis is a dateTime. 
#' Should work in any time-zone.
#'
#' @param p the plot to add them to
#' @param yLoc the relative location of the label (% of max plot height)
#' @param yMin the smallest y value
#' @param yMax the largest y value#' 
#' @param fill fill colour (grey50)
#' @param alpha alpha value (0.3)
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addVeDayDateTime <- function(p, yLoc, yMin, yMax,fill = "grey50", alpha = 0.3){
  p <- p + annotate("rect", xmin = lubridate::as_datetime("2020-05-08 00:00:00"),
                    xmax = lubridate::as_datetime("2020-05-10 23:59:59"), # 3 day weekend starting Friday (in the UK)
                    ymin = yMin, ymax = yMax,
                    alpha = alpha, fill = fill) + # VE Day
    annotate("text", x = lubridate::as_datetime("2020-05-08 12:00:00"),
             y = yLoc * yMax,
             label = "VE Day 2020")
  return(p)
}
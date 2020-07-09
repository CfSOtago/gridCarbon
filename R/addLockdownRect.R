#' \code{addLockdownRect} adds rectangles around the periods specified as dates or date times on a plot
#' where the x-axis is a date or datetime. Be sure to specify from & to as dates if the plot x-axis is
#' a date and as dateTimes if it is a dateTime. Otherwise ggplot will throw a date_trans error.
#'
#' @param p the plot to add them to
#' @param from the start date/datetime
#' @param to the end dat dat/datetime
#' @param label the label to display
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @param alpha - alpha value for the rect (0.1)
#' @param fill - fill  for the rect (grey50)
#' @param colour - line colour  for the rect (red)
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family plot
#' @family utils
#'
addLockdownRect <- function(p, from, to, label, yMin, yMax, alpha = 0.1, fill = "grey50", colour = "red"){
  p <- p + annotate("rect", xmin = from,
                    xmax = to, 
                    ymin = yMin-(yMin*0.01), ymax = yMax+(yMax*.01), # jout outside min/ax data values
                    alpha = alpha, 
                    fill = fill, 
                    colour = colour
  )
  # p <- p + annotate("text", x = as.Date((to - from)/2), # half way?
  #            y = yMax*gcParams$labelPos,
  #            label = label)
  return(p)
}
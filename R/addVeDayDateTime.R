#' \code{addVeDayDateTime} adds label for VE day 2020 where x axis is a dateTime. Should work in any time-zone.
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addVeDayDateTime <- function(p, yMin, yMax){
  p <- p + annotate("text", x = lubridate::as_datetime("2020-05-08 12:00:00"),
             y = yMax*gcParams$labelPos,
             label = "VE Day 2020")
  return(p)
}
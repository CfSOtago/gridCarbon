# Functions to add annotations to plots

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
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family plot
#' @family utils
#'
addLockdownRect <- function(p, from, to, label, yMin, yMax){
  p <- p + annotate("rect", xmin = from,
                    xmax = to, 
                    ymin = yMin*0.99, ymax = yMax*1.01, # jout outside min/ax data values
                    alpha = gcParams$myAlpha, 
                    fill = gcParams$vLineCol, 
                    colour = gcParams$vLineCol
  )
  return(p)
}

#' \code{addWeekendRectsDate} adds rectangles around the weekends from March 2020 onwards
#' to plots where the x-axis is a date.
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addWeekendRectsDate <- function(p, yMin, yMax){
  p <- p + annotate("rect", xmin = as.Date("2020-03-07"),
                    xmax = as.Date("2020-03-09"),
                    ymin = yMin, ymax = yMax,
                    alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-03-14"),
             xmax = as.Date("2020-03-16"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-03-21"),
             xmax = as.Date("2020-03-23"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-03-28"),
             xmax = as.Date("2020-03-30"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-04-04"),
             xmax = as.Date("2020-04-06"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-04-10"),
             xmax = as.Date("2020-04-14"), # Easter
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("text", x = as.Date("2020-04-10"),
             y = yMax*gcParams$labelPos,
             label = "Easter 2020") + # Easter
    annotate("rect", xmin = as.Date("2020-04-18"),
             xmax = as.Date("2020-04-20"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-04-25"),
             xmax = as.Date("2020-04-27"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-05-02"),
             xmax = as.Date("2020-05-04"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-05-08"),
             xmax = as.Date("2020-05-11"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("text", x = as.Date("2020-05-08"),
             y = yMax*gcParams$labelPos,
             label = "VE Day 2020") + # VE Day
    annotate("rect", xmin = as.Date("2020-05-16"),
             xmax = as.Date("2020-05-18"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-05-23"),
             xmax = as.Date("2020-05-25"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-05-23"),
             xmax = as.Date("2020-05-25"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = as.Date("2020-05-30"),
             xmax = as.Date("2020-06-01"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill)
    return(p)
}

#' \code{addWeekendRectsDateTime} adds rectangles around the weekends from March 2020 onwards
#' to plots where the x-axis is a dateTime.
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addWeekendRectsDateTime <- function(p, yMin, yMax){
  p <- p + annotate("rect", xmin = lubridate::as_datetime("2020-03-07 00:00:00"),
                    xmax = lubridate::as_datetime("2020-03-08 23:59:59"),
                    ymin = yMin, ymax = yMax,
                    alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-03-14 00:00:00"),
             xmax = lubridate::as_datetime("2020-03-15 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-03-21 00:00:00"),
             xmax = lubridate::as_datetime("2020-03-22 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-03-28 00:00:00"),
             xmax = lubridate::as_datetime("2020-03-29 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-04-04 00:00:00"),
             xmax = lubridate::as_datetime("2020-04-05 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-04-10 00:00:00"),
             xmax = lubridate::as_datetime("2020-04-13 23:59:59"), # Easter
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("text", x = lubridate::as_datetime("2020-04-10 00:00:00"),
             y = yMax*gcParams$labelPos,
             label = "Easter 2020") + # Easter
    annotate("rect", xmin = lubridate::as_datetime("2020-04-18 00:00:00"),
             xmax = lubridate::as_datetime("2020-04-19 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-04-25 00:00:00"),
             xmax = lubridate::as_datetime("2020-04-26 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-05-02 00:00:00"),
             xmax = lubridate::as_datetime("2020-05-03 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-05-08 00:00:00"),
             xmax = lubridate::as_datetime("2020-05-10 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("text", x = lubridate::as_datetime("2020-05-08 00:00:00"),
             y = yMax*gcParams$labelPos,
             label = "VE Day 2020") + # VE Day
    annotate("rect", xmin = lubridate::as_datetime("2020-05-16 00:00:00"),
             xmax = lubridate::as_datetime("2020-05-17 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-05-23 00:00:00"),
             xmax = lubridate::as_datetime("2020-05-24 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill) +
    annotate("rect", xmin = lubridate::as_datetime("2020-05-30 00:00:00"),
             xmax = lubridate::as_datetime("2020-05-31 23:59:59"),
             ymin = yMin, ymax = yMax,
             alpha = gcParams$weAlpha, fill = gcParams$weFill)
  return(p)
}
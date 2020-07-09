#' \code{addWeekendRectsDate} adds rectangles around the weekends from March 2020 onwards
#' to plots where the x-axis is a date. Should work in any time-zone
#'
#' @param p the plot to add them to
#' @param yMin the smallest y value
#' @param yMax the largest y value
#' @param fill fill colour (grey50)
#' @param alpha alpha value (0.3)
#' @param labelPos relative position of label (0.9)
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
addWeekendRectsDate <- function(p, yMin, yMax, fill = "grey50", alpha = 0.3, labelPos = 0.9){
  p <- p + annotate("rect", xmin = as.Date("2020-03-07"),
                    xmax = as.Date("2020-03-09"),
                    ymin = yMin, ymax = yMax,
                    alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-03-14"),
             xmax = as.Date("2020-03-16"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-03-21"),
             xmax = as.Date("2020-03-23"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-03-28"),
             xmax = as.Date("2020-03-30"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-04-04"),
             xmax = as.Date("2020-04-06"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-04-10"),
             xmax = as.Date("2020-04-14"), # Easter
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("text", x = as.Date("2020-04-10"),
             y = yMax*labelPos,
             label = "Easter 2020") + # Easter
    annotate("rect", xmin = as.Date("2020-04-18"),
             xmax = as.Date("2020-04-20"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-04-25"),
             xmax = as.Date("2020-04-27"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-05-02"),
             xmax = as.Date("2020-05-04"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-05-09"),
             xmax = as.Date("2020-05-11"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-05-16"),
             xmax = as.Date("2020-05-18"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-05-23"),
             xmax = as.Date("2020-05-25"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-05-23"),
             xmax = as.Date("2020-05-25"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-05-30"),
             xmax = as.Date("2020-06-01"),
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-06-06"),
             xmax = as.Date("2020-06-08"), # need to be 3 days total to look right
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-06-13"),
             xmax = as.Date("2020-06-15"), # need to be 3 days total to look right
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-06-20"),
             xmax = as.Date("2020-06-22"), # need to be 3 days total to look right
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill) +
    annotate("rect", xmin = as.Date("2020-06-27"),
             xmax = as.Date("2020-06-29"), # need to be 3 days total to look right
             ymin = yMin, ymax = yMax,
             alpha = alpha, fill = fill)
  return(p)
}
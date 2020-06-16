#' Creates a plot by weekday from the data.table given
#'
#' `makeWeekdayTimePlot` returns a plot which shows yVar by weekday, time, compareYear and plotPeriod (assumed to be lockdown). 
#' Assumes this is the date aligned data. Unlike makeWeekdayPlot this plots the proportion of the value (not the absolute value) within
#' the categories. This helps to see proportional change.
#'
#' @param dt the data
#' @param yVar the variable you want to plot
#' @param yLab the label for the y axis
#' @param yDiv the value you want to divide yVar by to make the y axis more sensible. Default = `1`
#' 
#' @return a plot
#' @import ggplot2
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family plot
#'
makeWeekdayTimePlot <- function(dt, yVar, yLab, yDiv){
  # by weekday and hour
  # use proportion to show relative shifts
  # wkdayFixed = obs (they are the same - that was the whole idea!)
  dt <- plotDT[, .(yVals = mean(get(yVar))/yDiv), keyby = .(hms, plotPeriod, compareYear, wkdayFixed)]
  sums <- dt[, .(sum = sum(yVals)), keyby = .(compareYear, plotPeriod, wkdayFixed)]
  setkey(sums, compareYear, plotPeriod, wkdayFixed)
  setkey(dt, compareYear, plotPeriod, wkdayFixed)
  mDT <- sums[dt]
  mDT[, pVal := (yVals/sum)*100]
  p <- ggplot2::ggplot(mDT, aes(x = hms, y = pVal, 
                                colour = compareYear)) + 
    geom_line() +
    scale_x_time(labels = NULL) +
    #scale_x_datetime(breaks=date_breaks('4 hour'),labels=date_format('%H:%M')) +
    theme(legend.position="bottom") +
    scale_color_discrete(name="Year") +
    facet_grid(plotPeriod ~ wkdayFixed ) +
    labs( y = yLab,
          x = "Time")
  return(p)
}
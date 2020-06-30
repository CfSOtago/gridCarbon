#' Creates a plot by weekday from the data.table given
#'
#' `makeWeekdayPlot` returns a box plot which showing yVar by weekday, compareYear and plotPeriod (assumed to be lockdown). 
#' 
#'
#' @param dt the data
#' @param xVar the weekday variable you want to use
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
makeWeekdayPlot <- function(dt, xVar, yVar, yLab, yDiv){
  # by weekday 
  # not proportions - absolute
  # wkdayFixed = obs (they are the same - that was the whole idea!)
  dt[, xVar := get(xVar)]
  #pDT <- dt[, .(yVal = mean(get(yVar))/yDiv), keyby = .(plotPeriod, compareYear, xVar)]
  p <- ggplot2::ggplot(dt, aes(x = xVar, y = get(yVar), 
                               colour = compareYear)) + 
    geom_boxplot() +
    theme(legend.position="bottom") +
    scale_color_discrete(name="Year") +
    facet_grid(. ~ plotPeriod ) +
    labs( y = yLab,
          x = "Weekday")
  return(p)
}
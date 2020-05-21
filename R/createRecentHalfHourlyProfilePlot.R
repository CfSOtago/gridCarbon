#' Creates a half-hourly profile plot by dayfrom the data.table given
#'
#' `createRecentHalfHourlyProfilePlot` returns a plot which plots yVar by time of day for each day. Assumes rDateTimeUTC exists.
#'
#' @param dt the data
#' @param dateTimeVar the dateTime variable to use (allows for aligned dateTimes etc). Default = `rDateTimeUTC`
#' @param yVar the variable you want to plot
#' @param yCap the caption for the y axis
#' @param yDiv the value you want to divide yVar by to make the y axis more sensible. Default = `1`
#' 
#' @import lubridate
#' @import data.table
#' @import hms
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family plot
#'
createRecentHalfHourlyProfilePlot <- function(dt, dateTime = "rDateTimeUTC", yVar, yCap, yDiv = 1){
  # assumes the base gridGen half-hourly data
  # assumes we want mean of half-hourly obs
  dt[, dateTimeVar := get(dateTime)]
  dt[, obsDate := lubridate::date(dateTimeVar)]
  dt[, wkdayObs := lubridate::wday(dateTimeVar, label = TRUE)]
  dt[, rTime := hms::as_hms(dateTimeVar)]
  plotDT <- dt[obsDate <= lubridate::today() & 
                 obsDate >= localParams$recentCutDate # otherwise we get the whole year 
               ]
  plotDT[, yVals := get(yVar)/yDiv] # do this here so min/max work]
  
  p <- ggplot2::ggplot(plotDT, aes(x = rTime, 
                                     y = yVals,
                                     colour = obsDate)) +
    geom_point() +
    #scale_x_datetime(date_breaks = "2 day", date_labels =  "%a %d %b")  +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    labs(x = "Time of day",
         y = yCap) +
    facet_wrap(. ~ wkdayObs) +
    guides(colour=guide_legend(title="Date")) +
    theme(legend.position="bottom")
  
  return(p)
}
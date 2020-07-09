#' Creates a half-hourly plot from the data.table given
#'
#' `createRecentDateTimePlot` returns a plot which plots yVar by dateTime and adds lockdown annotations.  This assumes users pass in the aligned-date data (`dateFixed`).
#'
#' @param dt the data
#' @param dateTime the dateTime variable to use (allows for aligned dateTimes and for other timezones etc etc).
#' @param yVar the variable you want to plot
#' @param yCap the caption for the y axis
#' @param yDiv the value you want to divide yVar by to make the y axis more sensible. Default = `1`
#' @param lockDownStart date for start of lockdown rectangle annotation
#' @param lockDownEnd date for end of lockdown rectangle annotation
#' 
#' @import lubridate
#' @import ggplot2
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family plot
#'
createRecentDateTimePlot <- function(dt, dateTime, yVar, yCap, yDiv = 1, lockDownStart, lockDownEnd, recentCutDate){
  # assumes the base gridGen half-hourly data
  # assumes we want mean of half-hourly obs
  dt[, dateTimeVar := get(dateTime)]
  dt[, obsDate := lubridate::date(dateTimeVar)]
  dt[, wkdayObs := lubridate::wday(dateTimeVar, label = TRUE)]
  plotDT <- dt[obsDate <= lubridate::today() & 
                 obsDate >= recentCutDate # otherwise we get the whole year 
               ]
  plotDT[, yVals := get(yVar)/yDiv] # do this here so min/max work]
  # make plot - adds annotations
  yMin <- min(plotDT$yVals)
  yMax <- max(plotDT$yVals)
  
  p <- ggplot2::ggplot(plotDT, aes(x = dateTimeVar, 
                                     y = yVals,
                                     colour = wkdayObs)
                       ) +
    geom_point() +
    scale_x_datetime(date_breaks = "7 day", date_labels =  "%a %d %b")  +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    labs(x = "Time",
         y = yCap) +
    theme(legend.position = "bottom") + 
    geom_smooth(aes(colour = NULL), method = "loess") +
    scale_color_viridis_d(name = "Day of the week") +
    guides(colour=guide_legend(nrow=2))
  

  p <- addLockdownRect(p, 
                       from = lockDownStart, 
                       to = lockDownEnd,
                       yMin = yMin, 
                       yMax = yMax)
  
  p <- addWeekendRectsDateTime(p, yMin, yMax)
  
  return(p)
}
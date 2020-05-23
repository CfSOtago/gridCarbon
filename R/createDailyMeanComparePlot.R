#' Creates a daily half-hourly mean plot from the data.table given
#'
#' `createDailyMeanComparePlot` returns a plot which calculates the mean of yVar and plots it for 2020 and 2017-2019 to 
#' compare the values for the same date over previous years. This assumes use pass in the aligned-date data. 
#'
#' @param dt the data, assumed to be the aligned data (use alignDates() to do this)
#' @param yVar the variable you want to plot
#' @param yCap the caption for the y axis
#' @param yDiv the value you want to divide yVar by to make the y axis more sensible. Default = 1
#' 
#' @import lubridate
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family plot
#'
createDailyMeanComparePlot <- function(dt, yVar, yCap, yDiv = 1){
  # assumes the dateFixed half-hourly data
  # assumes we want mean of half-hourly obs
  plotDT <- dt[dateFixed <= lubridate::today() & 
                 dateFixed >= localParams$comparePlotCut, # otherwise we get the whole year 
               .(yMean = mean(get(yVar))/yDiv, # do this here so min/max work
                 nObs = .N
               ),
               keyby = .(dateFixed, wkdayFixed, weekDay, compareYear)]
  
  # make plot - adds annotations
  yMin <- min(plotDT$yMean)
  yMax <- max(plotDT$yMean)
  
  p <- ggplot2::ggplot(plotDT, aes(x = dateFixed, 
                                   y = yMean,
                                   shape = weekDay,
                                   colour = compareYear)) +
    geom_point() +
    scale_x_date(date_breaks = "7 day", date_labels =  "%a %d %b")  +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    labs(caption = paste0(localParams$lockdownCap, localParams$weekendCap,
                          "\n", localParams$gamCap),
         x = "Date",
         y = yCap
    ) +
    theme(legend.position = "bottom") + 
    geom_smooth(aes(shape = NULL)) + # will get a smooth line per year not per day
    scale_color_discrete(name = "Period") +
    scale_shape_discrete(name = "Weekday") +
    guides(colour=guide_legend(nrow=2)) +
    guides(shape=guide_legend(nrow=2))
  
  p <- addLockdownRect(p, 
                       from = gcParams$UKlockDownStartDate, 
                       to = gcParams$UKlockDownEndDate, 
                       label = "Phase 1", yMin, yMax)
  
  p <- addWeekendRectsDate(p, 
                           yMin, 
                           yMax
  )
  return(p)
}
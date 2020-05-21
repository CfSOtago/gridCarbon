#' Calculates a 7 day rolling mean for specified variable for the data.table given
#'
#' `createDailyMeanPlot` returns a plot which calculates the mean of yVar and plots it for 2020 and 2017-2019 to 
#' compare the values for the same date over previous years. This assumes the 
#'
#' @param dt a data table with a summary variable you want to average
#' @param avgVar the variable you want to average
#' @param byVars the variables you want to group by (e.g. obsDate and compareYear) as a vector thus: byVars <- c("obsDate", "compareYear")
#' 
#' @import zoo
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family stats
#'
create7DayMovingAverage <- function(dt, avgVar, byVars){
    # assumes half-hourly data (aligned or not)
    # byVars <- c("year", "compareYear")
    dailyDT <- dt[, .(meanVal = mean(get(avgVar)),
                      nObs = .N), 
                  keyby = eval(byVars) # e.g. year or compareYear or whatever. Only accepts one for now
                  ]
    # summary(dailyDT) # nObs should be 48 except for the obs that couldn't be fixed (weird overlaps?)
    
    #dt[, res := cumsum(Sales)/(1:.N), by = Group]
    dailyDT[, movingAvg7Day := zoo::rollmean(meanVal, 7, fill = NA)]
    return(dailyDT) 
}
#' Codes morning/evening peak periods using rDateTimeUTC in the data.table given
#'
#' `setPeakPeriod` adds a factor column `peakPeriod` to the input data.table  
#'
#' @param dt a data table with a summary variable you want to average
#' @param dateTime the variable which is a nice R dateTime for passing to hms::as_hms()
#' 
#' @import hms
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family stats
#'
setPeakPeriod <- function(dt, dateTime = rDateTimeUTC){
  # does not assume hms exists
  dt[, hms := hms::as_hms(get(dateTime))]
  dt[, peakPeriod := NA]
  dt[, peakPeriod := ifelse(hms < amPeakStart, "Early morning", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakStart & hms < amPeakEnd, "Morning peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakEnd & hms < pmPeakStart, "Day time", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakStart & hms < pmPeakEnd, "Evening peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakEnd, "Late evening", peakPeriod)]
  dt[, peakPeriod := forcats::fct_relevel(peakPeriod, 
                                          "Early morning",
                                          "Morning peak",
                                          "Day time",
                                          "Evening peak",
                                          "Late evening")]
  return(dt)
}
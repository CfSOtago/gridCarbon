#' Codes morning/evening peak periods using rDateTimeUTC in the data.table given
#'
#' `setPeakPeriod` adds a factor column `peakPeriod` to the input data.table  
#'
#' @param dt a data table with a summary variable you want to average
#' @param dateTime the variable which is a nice R dateTime for passing to hms::as_hms()
#' @param t1 start of morning peak in 24 hour clock e.g. "07:00" (default)
#' @param t2 end of morning peak in 24 hour clock e.g. "08:30" (default)
#' @param t3 start of morning peak in 24 hour clock e.g. "16:00" (default)
#' @param t4 start of morning peak in 24 hour clock e.g. "20:00" (default)
#' 
#' @import hms
#' @import data.table
#' @import forcats
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family utils
#'
setPeakPeriod <- function(dt, dateTime = rDateTimeUTC, t1 = "07:00", 
                          t2 = "09:00", t3 = "16:00", t4 = "20:00"){
  # does not assume hms exists
  dt[, hms := hms::as_hms(get(dateTime))]
  dt[, peakPeriod := NA]
  dt[, peakPeriod := ifelse(hms < hms::parse_hm(t1), "Early morning", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= hms::parse_hm(t1) & hms < hms::parse_hm(t2), "Morning peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= hms::parse_hm(t2) & hms < hms::parse_hm(t3), "Day time", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= hms::parse_hm(t3) & hms < hms::parse_hm(t4), "Evening peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= hms::parse_hm(t4), "Late evening", peakPeriod)]
  dt[, peakPeriod := forcats::fct_relevel(peakPeriod, 
                                          "Early morning",
                                          "Morning peak",
                                          "Day time",
                                          "Evening peak",
                                          "Late evening")]
  return(dt)
}
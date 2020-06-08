#' Converts the given time periods (TP1 -> TP48, 49. 50) in a dt to hh:mm
#'
#' \code{setEmbeddedGenTimePeriod} converts the given time periods (TP1 -> TP48, 49. 50) to hh:mm. It ignores
#'  TP49 & TP50 as evil incarnations of DST related clock changes (see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/).
#'  We advise NEVER using the months in which this happens as it will hurt your brain.
#'
#' @param dt the data table containing the Time_Period variable
#' @import data.table
#' @import hms
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
setEmbeddedGenTimePeriod <- function(dt){
  # convert the given time periods (TP1 -> TP48, 49. 50) to hh:mm
  dt <- dt[, c("t","tp") := tstrsplit(Time_Period, "P")]
  dt <- dt[, mins := ifelse(as.numeric(tp)%%2 == 0, "45", "15")] # set to q past/to
  dt <- dt[, hours := floor((as.numeric(tp)+1)/2) - 1]
  dt <- dt[, strTime := paste0(hours, ":", mins, ":00")]
  dt <- dt[, rTime := hms::as_hms(strTime)] # this breaks on TP49 and TP50 as it creates 24:15 and 24:45 which are
  # times that do not exist. But we won't remove them yet
  # head(dt)
  dt <- dt[, c("t","tp","mins","hours") := NULL]  #remove these now we're happy
  return(dt)
}
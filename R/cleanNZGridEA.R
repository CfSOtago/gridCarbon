#' \code{cleanNZGridEA} cleans up the raw grid gen file from the NZ EA data website 
#' 
#' These are not in pretty form so we clean them up to a long form file and fix the dateTimes.
#'
#' Note that rDateTime will be NA for the DST breaks which equate to TP49/50. We really dislike DST breaks.
#' 
#' @param dt the data.table to clean up
#' 
#' @import data.table
#' @import lubridate
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family grid 
#' @family NZ
#' 
cleanNZGridEA <- function(dt){
  # cleans & returns a dt
  dtl <- gridCarbon::reshapeGenDT(dt) # make long
  dtl <- gridCarbon::setGridGenTimePeriod(dtl) # set time periods to something intelligible as rTime
  dtl[, rDate := as.Date(Trading_date)] # fix the dates so R knows what they are
  dtl[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime. Parsing failures are TP49/59
  # don't do this here - do it on data load (saves space)
  #dtl[, rDateTimeNZT := lubridate::force_tz(rDateTime, 
  #                                                 tzone = "Pacific/Auckland")] # for safety in case run in another tz!
  # there will be parse errors in the above due to TP49 & TP50
  table(dtl[is.na(rDateTime)]$Time_Period)
  return(dtl)
}
#' \code{cleanNZEmbEA} cleans up the raw embedded gen file from the NZ EA data website 
#' 
#' These are not in pretty form so we clean them up to a long form file and fix the dateTimes.
#' 
#' NB: rDateTime will be NA for TP49/50 which correspond to the DST breaks.
#'
#' @param dt the data.table to clean up
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family embedded 
#' @family NZ
#' 
cleanNZEmbEA <- function(dt){
  # cleans & returns a long form dt
  dtl <- gridCarbon::reshapeEmbeddedGenDT(dt) # make long
  dtl <- gridCarbon::setEmbeddedGenTimePeriod(dtl) # set time periods to something intelligible as rTime
  dtl[, rDate := lubridate::dmy(Trading_date)] # fix the dates so R knows what they are
  dtl[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  #dtl[, rDateTimeNZT := lubridate::force_tz(rDateTime, 
  #                                          tzone = "Pacific/Auckland")] # for safety in case run in another tz!
  # there will be parse errors in the above due to TP49 & TP50
  table(dtl[is.na(rDateTime)]$Time_Period)
  return(dtl)
}
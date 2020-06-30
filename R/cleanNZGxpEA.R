#' \code{cleanNZGxpEA} cleans up the raw GXP grid export file from the NZ EA data website 
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
cleanNZGxpEA <- function(dt){
  # cleans & returns a dt
  #table(dt$UNIT_MEASURE) #should all be kWh
  mDT <- melt(dt,
                     id.vars=c("POC","NWK_Code", "GENERATION_TYPE", "TRADER", "UNIT_MEASURE","FLOW_DIRECTION", "STATUS", "TRADING_DATE"),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy - see https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export
  )
  mDT <- gridCarbon::setGridGenTimePeriod(mDT) # set time periods to something intelligible as rTime. NB sets this to 15 mins past/to for easy plottin
  mDT[, rDate := lubridate::dmy(TRADING_DATE)] # fix the dates so R knows what they are. Yes they really are dmy. 
  # People: please read https://speakerdeck.com/jennybc/how-to-name-files?slide=21
  mDT[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime. Parsing failures are TP49/59
  # don't do this here - do it on data load (saves space)
  #dtl[, rDateTimeNZT := lubridate::force_tz(rDateTime, 
  #                                                 tzone = "Pacific/Auckland")] # for safety in case run in another tz!
  # there will be parse errors in the above due to TP49 & TP50
  # here's proof
  table(mDT[is.na(rDateTime)]$Time_Period)
  return(mDT)
}
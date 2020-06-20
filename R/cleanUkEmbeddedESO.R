#' \code{cleanUkEmbeddedESO} cleans up UK embedded ESO data adding lates update to the historical data
#' 
#'  - adds proper dateTime 
#'  - keeps just solar & wind power
#'  
#'  @import data.table
#'  @import lubridate
#'
#' @param dt the data.table to clean (returned by \code{getUkEmbeddedESO})
#' @param rawPath ehere to find the pre-downloaded historical data (manually downloaded. Don't ask)
#' 
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#'
cleanUkEmbeddedESO <- function(dt, rawPath){
  # get the old data
  # rawPath <- localParams$rawUkEmbeddedGenPath
  oldFiles <- list.files(rawPath, full.names = TRUE) 
  l <- lapply(oldFiles, data.table::fread) # very fast data loading :-)
  oldDT <- data.table::rbindlist(l, fill = TRUE) # rbind them
  oldDT[, rDate := lubridate::dmy(SETTLEMENT_DATE)] # why dmy people? And why as a string?
  dt[, rDate := lubridate::dmy(SETTLEMENT_DATE)] # why dmy people? And why as a string?
  # add the update we just got
  all <- rbind(oldDT, 
               dt, 
               fill = TRUE) # fill just in case, let's hope the varnames stayed the same

  # keep only Actuals
  #table(dt$FORECAST_ACTUAL_INDICATOR)
  #EMBEDDED_WIND_GENERATION EMBEDDED_WIND_CAPACITY EMBEDDED_SOLAR_GENERATION EMBEDDED_SOLAR_CAPACITY
  # anything else?
  keep <- all[, 
              .(rDate, SETTLEMENT_PERIOD,FORECAST_ACTUAL_INDICATOR,
                  EMBEDDED_WIND_GENERATION,EMBEDDED_WIND_CAPACITY,
                  EMBEDDED_SOLAR_GENERATION,EMBEDDED_SOLAR_CAPACITY)
              ]
  keep[, mins := ifelse(as.numeric(SETTLEMENT_PERIOD)%%2 == 0, "30", "00")] # set to half hours to match gridGen
  keep[, hours := floor((as.numeric(SETTLEMENT_PERIOD)+1)/2) - 1]
  keep[, strTime := paste0(hours, ":", mins, ":00")]
  keep[, hms := hms::as_hms(strTime)] # this breaks on TP49 and TP50 as it creates 24:15 and 24:45 which are
  keep[, rDateTime := lubridate::ymd_hms(paste0(rDate, strTime))]
  # NA = times that do not exist. But we won't remove them yet
  # head(dt)
  keep <- keep[, c("mins","hours", "strTime") := NULL]  #remove these now we're happy
  #summary(keep$rDateTime)
  # check
  nrow(keep)
  uniqDT <- unique(keep)
  nrow(uniqDT)
  nrow(uniqDT[is.na(FORECAST_ACTUAL_INDICATOR) | 
                FORECAST_ACTUAL_INDICATOR != "F"])
  return(uniqDT[is.na(FORECAST_ACTUAL_INDICATOR) | 
                  FORECAST_ACTUAL_INDICATOR != "F"]) # drop the forecasts
}
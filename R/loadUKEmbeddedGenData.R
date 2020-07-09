#' Load the pre-downloaded UK ESO embedded generation files in to a data.table
#'
#' \code{loadUKEmbeddedGenData} returns a dt with a proper rDateTimeUTC added & set to London tzone.
#'
#' @param path the folder to look in for the data
#' @param fromYear the year to start from (needs to be in the data file names - you did name them sensibly, yes?)
#' @param toDate up to but not including this date. e.g. toDate = as.Date("2020-06-01") will be up to and including May 2020
#' @import lubridate
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family data
#'
loadUKEmbeddedGenData <- function(path, fromYear, toDate, update){
  # update = dummy used to force re-load
  # lists files within a folder (path) & loads
  # should be only 1 file so just load it
  # path <- repoParams$ukNonGridDataLoc
  path <- paste0(path, "processed/yearly/")
  message("Checking: ", path)
  filesToGet <- list.files(path, ".csv.gz", full.names = TRUE) # get list of files already downloaded & converted to long form
  l <- lapply(filesToGet, data.table::fread) # very fast data loading :-)
  dt <- data.table::rbindlist(l, fill = TRUE) # rbind them
  
  # > fix  data ----
  dt[, DateTime := lubridate::as_datetime(rDateTime)]
  dt[, rDateTimeUTC := lubridate::force_tz(DateTime, 
                                           tzone = "UTC")] # to be sure to be sure
  
  # check
  #h <- head(gridGenDT[, .(DATETIME, year, rDateTimeUTC, GENERATION, CARBON_INTENSITY)])
  #h
  
  # drop any days with incomplete data (this seems to happen on the last day in the data which is cut at 17:00 instead of 23:59. Oh yes. Data doesn't stop flowing at home time chaps)
  dt[, hms := hms::as_hms(rDateTimeUTC)]
  dt[, obsDate := lubridate::as_date(rDateTimeUTC)]
  dt[, year := lubridate::year(rDateTimeUTC)]
  dt <- setPeakPeriod(dt, dateTime = "rDateTimeUTC") # use defaults
  
  t <- dt[, .(nHalfHours = uniqueN(hms)), keyby = .(obsDate)]
  setkey(t, obsDate)
  setkey(dt, obsDate)
  uniqueN(dt$obsDate)
  #nrow(t)
  ok <- t[nHalfHours == 48]
  #nrow(ok)
  dt <- dt[ok] # drops the dates with less than 48 observations
  #uniqueN(dt$obsDate)
  
  return(dt[obsDate < as.Date(toDate)]) # large, possibly very large depending on fromYear & toDate
}
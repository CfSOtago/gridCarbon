#' Load the pre-downloaded yearly NZ EA embedded generation files in to a data.table
#'
#' \code{loadNZEAYearlyEmbeddedGenData} returns a dt with a proper rDateTimeNZT added & set to NZ tzone. The 
#' embedded generation data from XX does not have a fuel type so we cannot calculate carbon intensity. We also
#' assume that I means inflow (to the grid - so we add this to the grid generation) and that X means outflow (to the LV network) which 
#' we don't really care about.
#' 
#' Presumably the sum of X is the sum of consumption across the POCs which happen to have embedded generation. Maybe.
#'
#' @param path the folder to look in for the data
#' @param fromYear the year to start from (needs to be in the data file names - you did name them sensibly, yes?)
#' @import lubridate
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family data
#'
loadNZEAYearlyEmbeddedGenData <- function(path, fromYear, update){
  # update = dummy used to force re-load
  # lists files within a folder (path) & loads fromYear
  # path <- localParams$nzGridDataLoc
  # fromYear <- 2015
  filesToDateDT <- data.table::as.data.table(list.files(path, ".csv.gz")) # get list of files already downloaded & converted to long form
  filesToDateDT[, file := V1]
  filesToDateDT[, c("year", "name") := data.table::tstrsplit(file, split = "_")]
  filesToDateDT[, year := as.numeric(year)]
  filesToDateDT[, fullPath := paste0(path, file)]
  filesToGet <- filesToDateDT[year >= fromYear, # to reduce files loaded
                              fullPath]
  message("Loading files >= ", fromYear)
  l <- lapply(filesToGet, data.table::fread) # very fast data loading :-)
  dt <- data.table::rbindlist(l, fill = TRUE) # rbind them
  
  # > fix dates ----
  dt[, rDateTime := lubridate::as_datetime(rDateTime)] # comes in unformatted in fread()
  dt[, rDateTimeNZT := lubridate::force_tz(rDateTime, 
                                           tzone = "Pacific/Auckland")] # to be sure to be sure
  
  # check
  #h <- head(gridGenDT[, .(DATETIME, year, rDateTimeUTC, GENERATION, CARBON_INTENSITY)])
  #h
  
  # drop any days with incomplete data (this seems to happen on the last day in the data which is cut at 17:00 instead of 23:59. Oh yes. Data doesn't stop flowing at home time chaps)
  
  dt[, obsDate := lubridate::as_date(rDateTimeNZT)]
  dt[, hms := hms::as_hms(rDateTimeNZT)]
  t <- dt[, .(nHalfHours = uniqueN(hms)), keyby = .(obsDate)]
  setkey(t, obsDate)
  setkey(dt, obsDate)
  uniqueN(dt$obsDate)
  #nrow(t)
  ok <- t[nHalfHours == 48]
  nrow(ok)
  dt <- dt[ok] # drops the dates with less than 48 observations
  #uniqueN(dt$obsDate)
  
  # embedded gen does not give us a fuel type, just a Point of Connection (https://www.emi.ea.govt.nz/Search?q=POC)
  # and flow: X outflow, I inflow (we assume)
  # so we can drop the X as we only care about energy flowing in (for now)
  dt[, kWh_n := as.numeric(kWh)]
  # and there's a lot more of it
  dt[, .(mean = mean(kWh_n), sum = sum(kWh_n), nObs = .N), keyby = .(Flow_Direction)]
  dt <- dt[Flow_Direction == "I"]
  # we have no idea what Loss_Code means
  dt[, .(mean = mean(kWh_n), sum = sum(kWh_n), nObs = .N), keyby = .(Loss_Code)]
  dtw <- dcast(dt[!is.na(kWh_n)], # remove NA now so sum works
               rDateTimeNZT ~ ., # each row is a unique dateTime
               value.var = "kWh_n", # what to sum
               fun.aggregate = sum)
  dtw[, kWh := .]
  dtw[, GENERATION_MWh := kWh/1000]
  dtw[, GWh := GENERATION_MWh/1000]
  dtw[, GENERATION_MW := (GENERATION_MWh * 2)] # convert to MW to match UK data
  dtw[, GW := GENERATION_MW/1000]
  
  dtw[, hms := hms::as_hms(rDateTimeNZT)]
  dtw[, year := lubridate::year(rDateTimeNZT)]
  dtw <- setPeakPeriod(dtw, dateTime = "rDateTimeNZT") 
  dtw$`.` <- NULL
  # Carbon intensity
  # We have no info on fuel type
  dtw[, totalC02e_g := NA]
  dtw[, totalC02e_kg := NA]
  dtw[, totalC02e_T := NA]
  
  return(dtw) # large, possibly very large depending on fromYear
}

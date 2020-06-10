#' Load the pre-downloaded yearly NZ EA grid generation files in to a data.table
#'
#' \code{loadNZEAYearlyGenData} returns a dt with a proper rDateTimeNZT added & set to NZ tzone.
#'
#' @param path the folder to look in for the data
#' @param fromYear the year to start from (needs to be in the data file names - you did name them sensibly, yes?)
#' @import lubridate
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family data
#'
loadNZEAYearlyGenData <- function(path, fromYear, update){
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
  
  # > fix grid data ----
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
  #nrow(ok)
  dt <- dt[ok] # drops the dates with less than 48 observations
  #uniqueN(dt$obsDate)
  
  # we need to reshape it so the fuels are columns and then create a 'total gen' column
  # we also need to sum within fuels as we don't the data per site
  # we can do this all at the same time and very fast
  
  dtw <- dcast(dt, rDateTimeNZT ~ Fuel_Code, # each row is a unique dateTime, each col is a Fuel_Code
               value.var = "kWh", # what to sum
               fun.aggregate = sum)
  dtw[, GENERATION := (Coal + Diesel + Gas + Geo + Hydro + Wind + Wood)/1000] # set to MW same as GB data
  dtw[, GW := GENERATION/1000]
  dtw[, hms := hms::as_hms(rDateTimeNZT)]
  dtw[, year := lubridate::year(rDateTimeNZT)]
  dtw <- setPeakPeriod(dtw, dateTime = "rDateTimeNZT") 
  
  # Carbon intensity
  # Needs conversion factors per fuel
  
  # Total CO2e - original grid gen data
  #dt[, totalC02e_g := ((1000*(GENERATION/2)) * CARBON_INTENSITY)]
  #dt[, totalC02e_kg := totalC02e_g/1000]
  #dt[, totalC02e_T := totalC02e_kg/1000 ]
  
  return(dtw) # large, possibly very large depending on fromYear
}
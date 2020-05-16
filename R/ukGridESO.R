# Functions to get and process the UK Grid ESO data

#' \code{getUkGridESO} gets the file from the UK Grid ESO data website. These are not in pretty form
#'
#' @param f the file to get (as a url suitable for data.table::fread())
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
getUkGridESO <- function(f){
  # url has to be explicit for drake to monitor it
  # breaks
  #dt <- data.table::fread(file_in("http://data.nationalgrideso.com/backend/dataset/88313ae5-94e4-4ddc-a790-593554d8c6b9/resource/7b41ea4d-cada-491e-8ad6-7b62f6a63193/download/df_fuel_ckan.csv"))
  dt <- data.table::fread(f)
  return(dt)
}

#' \code{cleanUkGridESO} cleans up UK Grid ESO data:
#' 
#'  - adds proper dateTime 
#'
#' @param dt the data.table to clean (retuirned by \code{getUkGridESO})
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
cleanUkGridESO <- function(dt){
  # cleans & returns a dt
  dt[, rDateTime := lubridate::ymd_hms(DATETIME)] # set full dateTime
  # anything else?
  return(dt)
}

#' \code{saveUkGridESO} saves the UK Grid ESO data to the location you tell it
#'
#' @param dt the data.table to save (returned by \code{cleanUkGridESO})
#' @param path the folder to put it in
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
saveUkGridESO <- function(dt,path){
  # localParams$rawUkEsoDataPath
  of <- paste0(path,"latest_ukGridEsoGen.csv") # we don't add a date to this to prevent bloat
  data.table::fwrite(dt, of)
  cmd <- paste0("gzip -f ", of)
  message("Gzip file: ", of)
  try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
  message("Done ")
}

#' \code{makeUkGridESOYearlyData} saves the UK Grid ESO data as yearly files to the location you tell it
#'
#' @param dt the data.table to save (returned by \code{cleanUkGridESO})
#' @param path the folder to put the yearly files in
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
makeUkGridESOYearlyData <- function(dt,path){
  dt[, year := lubridate::year(rDateTime)]
  t <- dt[, .(nObs = .N), keyby = .(year)]
  years <- t[, year]
  for(y in years){
    yearDT <- dt[year == y]
    # localParams$processedUkEsoDataPath
    of <- paste0(path,"yearly/",y,"_ukGridEsoGen.csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
    message("Done ", y)
  }
  return(yearDT) # return the last year for testing if needed
}

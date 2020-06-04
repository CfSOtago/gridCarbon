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
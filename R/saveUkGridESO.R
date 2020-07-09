#' \code{saveUkGridESO} saves the UK Grid ESO data to the location you tell it
#'
#' @param dt the data.table to save (returned by \code{cleanUkGridESO})
#' @param path the folder to put it in
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family grid
#' @family uk
#' 
saveUkGridESO <- function(dt,path){
  # repoParams$ukGridDataLoc
  of <- paste0(path,"raw/latest_ukGridEsoGen.csv") # we don't add a date to this to prevent bloat
  data.table::fwrite(dt, of)
  cmd <- paste0("gzip -f ", of)
  message("Gzip file: ", of)
  try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
  message("Saved raw file")
  dt[, year := lubridate::year(rDateTime)]
  t <- dt[, .(nObs = .N), keyby = .(year)]
  years <- t[, year]
  for(y in years){ # save as yearly files
    yearDT <- dt[year == y]
    # localParams$processedUkEsoDataPath
    of <- paste0(path,"processed/yearly/",y,"_ukGridEsoGen.csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
    message("Done ", y)
  }
  message("Saved yearly files")
}
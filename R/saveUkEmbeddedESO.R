#' \code{saveUkEmbeddedESO} saves the latest cleaned UK embedded ESO data 
#' 
#' @param dt the data.table to save (returned by \code{cleanUkGridESO})
#' @param processedPath the folder to put it in
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family embedded
#' @family uk
#'
saveUkEmbeddedESO <- function(dt,path){
  # localParams$rawUkEsoDataPath
  
  of <- paste0(path,"raw/latest_ukEmbeddedGen.csv") # we don't add a date to this to prevent bloat
  data.table::fwrite(dt, of) # save as one file (small)
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
    of <- paste0(path,"processed/yearly/",y,"_ukEmbeddedGen.csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
    message("Done ", y)
  }
  message("Saved yearly files")
}
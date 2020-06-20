#' \code{saveUkEmbeddedESO} saves the clean latest UK embedded ESO data 
#' 
#' @param dt the data.table to save (returned by \code{cleanUkGridESO})
#' @param processedPath the folder to put it in
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#'
saveUkEmbeddedESO <- function(dt,processedPath){
  # localParams$rawUkEsoDataPath
  
  of <- paste0(processedPath,"latest_ukEmbeddedGen.csv") # we don't add a date to this to prevent bloat
  data.table::fwrite(dt, of) # save as one file (small)
  cmd <- paste0("gzip -f ", of)
  message("Gzip file: ", of)
  try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
  message("Done ")
}
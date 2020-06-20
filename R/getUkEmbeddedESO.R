#' \code{getUkEmbeddedESO} gets the latest embedded generation update file from the UK Grid ESO data website. This is a modelled forecast
#' since the ESO has no visibility of these. It is embedded in a demand data file for some reason...
#'
#' @param f the file to get (as a url suitable for data.table::fread())
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' 
getUkEmbeddedESO <- function(f, update){
  # we don't do anything with update - if we change it in any way, drake updates :-)
  # url has to be explicit for drake to monitor it
  # breaks
  #f <- "https://demandforecast.nationalgrid.com/efs_demand_forecast/demandupdatedownload"
  dt <- data.table::fread(f)
  return(dt)
}
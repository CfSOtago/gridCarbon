#' \code{getUkEmbeddedESO} gets the latest embedded generation update file from the UK Grid ESO data website. This is a modelled forecast
#' since the ESO has no visibility of these. It is embedded in a demand data file for some reason... We then need to add it to the 'old'
#' embedded data that we have downloaded manually. We do this when we clean it. Yes, at the time of writing it's a bit of a mess.
#'
#' @param f the file to get (as a url suitable for data.table::fread())
#' @param update force an update
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family embedded
#' @family uk
#' 
getUkEmbeddedESO <- function(f, update){
  # we don't do anything with update - if we change it in any way, drake updates :-)
  # url has to be explicit for drake to monitor it
  # breaks
  #f <- "https://demandforecast.nationalgrid.com/efs_demand_forecast/demandupdatedownload"
  dt <- data.table::fread(f)
  return(dt)
}
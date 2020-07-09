#' \code{cleanUkGridESO} cleans up UK Grid ESO data:
#' 
#'  - adds proper dateTime 
#'
#' @param dt the data.table to clean (retuirned by \code{getUkGridESO})
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family grid
#' @family uk
#'
cleanUkGridESO <- function(dt){
  # cleans & returns a dt
  dt[, rDateTime := lubridate::ymd_hms(DATETIME)] # set full dateTime
  # anything else?
  return(dt)
}
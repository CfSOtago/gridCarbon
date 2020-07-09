#' Add season to a data.table depending on hemisphere
#'
#' \code{addSeason} returns a dt with hemisphere season (Winter, Spring, Summer, Autumn) added - assume temperature latitudes
#'
#' @param dt the data table
#' @param dateVar the column in the dt which is a date that lubridate::month() will work on
#' @param h hemisphere: North (N) or South (S)?
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family utils
#'
addSeason <- function(dt,dateVar,h){
  dt <- dt[, tmpM := lubridate::month(get(dateVar))] # sets 1 (Jan) - 12 (Dec). May already exist but we can't rely on it
  if(h == "S"){
    dt <- dt[, season := "Summer"] # easiest to set the default to be the one that bridges years
    dt <- dt[tmpM >= 3 & tmpM <= 5, season := "Autumn"]
    dt <- dt[tmpM >= 6 & tmpM <= 8 , season := "Winter"]
    dt <- dt[tmpM >= 9 & tmpM <= 11, season := "Spring"]
    # re-order to make sense
    dt <- dt[, season := factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))]
  }
  if(h == "N"){
    dt <- dt[, season := "Winter"] # easiest to set the default to be the one that bridges years
    dt <- dt[tmpM >= 3 & tmpM <= 5, season := "Spring"]
    dt <- dt[tmpM >= 6 & tmpM <= 8 , season := "Summer"]
    dt <- dt[tmpM >= 9 & tmpM <= 11, season := "Autumn"]
    # re-order to make sense
    dt <- dt[, season := factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))]
  }
  dt$tmpM <- NULL
  return(dt)
}
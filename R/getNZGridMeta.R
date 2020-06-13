#' \code{getNZGridMeta} generates metadata for a given grid dt 
#' 
#' These are not in pretty form so we clean them up to a long form file and fix the dateTimes forcing to NZT just in case.
#'
#' @param dt the data.table to report
#' @return dt a data.table of info
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family grid 
#' @family NZ
#' 
getNZGridMeta <- function(dt){
  dt <- dt[, month := lubridate::month(rDate)]
  dt <- dt[, year := lubridate::year(rDate)]
  testDT <- dt[, .(nObs = .N,
                   sumMWh = sum(as.numeric(kWh/1000), na.rm = TRUE),
                   meanMWh = mean(as.numeric(kWh/1000), na.rm = TRUE),
                   nFuels = uniqueN(Fuel_Code),
                   dateFrom = min(rDate),
                   dateTo = max(rDate),
                   nDays = uniqueN(rDate)), keyby = .(month,
                                                      year)]
  return(testDT)
}
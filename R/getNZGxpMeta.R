#' \code{getNZGxpMeta} generates metadata for a given  dt 
#' 
#' Creates various summaries for reporting.
#'
#' @param dt the data.table to report
#' @return dt a data.table of info
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family grid 
#' @family gxp 
#' @family NZ
#' 
getNZGxpMeta <- function(dt){
  dt <- dt[, month := lubridate::month(rDate)]
  dt <- dt[, year := lubridate::year(rDate)]
  testDT <- dt[, .(nObs = .N,
                   sumMWh = sum(as.numeric(kWh/1000), na.rm = TRUE),
                   meanMWh = mean(as.numeric(kWh/1000), na.rm = TRUE),
                   nGenTypes = uniqueN(GENERATION_TYPE),
                   nTraders = uniqueN(TRADER),
                   dateFrom = min(rDate),
                   dateTo = max(rDate),
                   nDays = uniqueN(rDate)), keyby = .(month,
                                                      year)]
  return(testDT)
}
#' \code{makeNZYearlyData} reports meta data for a given dt
#' 
#' @param dt data.table to report
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family embedded 
#' @family NZ
#' 
getNZEmbMeta <- function(dt){
  dt <- dt[, month := lubridate::month(rDate)]
  dt <- dt[,year := lubridate::year(rDate)]
  testDT <- dt[, .(nObs = .N,
                   sumMWh = sum(as.numeric(kWh)/1000, na.rm = TRUE),
                   meanMWh = mean(as.numeric(kWh)/1000, na.rm = TRUE),
                   dateFrom = min(rDate),
                   dateTo = max(rDate),
                   nDays = uniqueN(rDate)), keyby = .(month,
                                                      year,
                                                      Flow_Direction)] # inport/export?
  return(testDT)
}
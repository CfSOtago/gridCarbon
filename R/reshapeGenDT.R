# Functions for EA wholesale generation data processing

#' Reshapes an EA wholesale generation data table from wide (yuk) to long form
#'
#' \code{reshapeGenDT} assumes id.vars exist. They might not
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#'
reshapeGenDT <- function(dt){
  # reshape the data as it comes in a rather unhelpful form
  reshapedDT <- melt(dt,
                     id.vars=c("Site_Code","POC_Code","Nwk_Code", "Gen_Code", "Fuel_Code", "Tech_Code","Trading_date"),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy - see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
  )
  return(reshapedDT)
}

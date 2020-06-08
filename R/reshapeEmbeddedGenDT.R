
#' Reshapes an EA embedded generation data table from wide (yuk) to long form
#'
#' \code{reshapeEmbeddedGenDT} assumes id.vars exist. They might not
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
reshapeEmbeddedGenDT <- function(dt){
  # reshape the data as it comes in a rather unhelpful form
  reshapedDT <- melt(dt,
                     id.vars=c("POC","NWK_Code", "Participant_Code", "Loss_Code", "Flow_Direction","Trading_date"),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy - see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
  )
  return(reshapedDT)
}



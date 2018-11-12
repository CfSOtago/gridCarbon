# Functions for EA wholesale generation data processing

#' Reshapes an EA wholesale generation data table from wide (yuk) to long form
#'
#' \code{reshapeEAGenDT} assumes id.vars exist. They might not
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
reshapeEAGenDT <- function(dt){
  # reshape the data as it comes in a rather unhelpful form
  reshapedDT <- melt(dt,
                     id.vars=c("Site_Code","POC_Code","Nwk_Code", "Gen_Code", "Fuel_Code", "Tech_Code","Trading_date"),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy - see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
  )
  return(reshapedDT)
}

#' Converts the given time periods (TP1 -> TP48, 49. 50) in a dt to hh:mm
#'
#' \code{setEAGenTimePeriod} converts the given time periods (TP1 -> TP48, 49. 50) to hh:mm. It ignores
#'  TP49 & TP50 as evil incarnations of DST related clock changes (see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/).
#'  We advise NEVER using the months in which this happens as it will hurt your brain.
#'
#' @param dt the data table containing the Time_Period variable
#' @import data.table
#' @import hms
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
setEAGenTimePeriod <- function(dt){
  # convert the given time periods (TP1 -> TP48, 49. 50) to hh:mm
  dt <- dt[, c("t","tp") := tstrsplit(Time_Period, "P")]
  dt <- dt[, mins := ifelse(as.numeric(tp)%%2 == 0, "45", "15")] # set to q past/to
  dt <- dt[, hours := floor((as.numeric(tp)+1)/2) - 1]
  dt <- dt[, strTime := paste0(hours, ":", mins, ":00")]
  dt <- dt[, rTime := hms::as.hms(strTime)]
  # head(dt)
  dt <- dt[, c("t","tp","mins","hours","strTime") := NULL]  #remove these now we're happy
  return(dt)
}

# Functions for EA embedded generation data processing

#' Reshapes an EA embedded generation data table from wide (yuk) to long form
#'
#' \code{reshapeEAEmbeddedGenDT} assumes id.vars exist. They might not
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#'
reshapeEAEmbeddedGenDT <- function(dt){
  # reshape the data as it comes in a rather unhelpful form
  reshapedDT <- melt(dt,
                     id.vars=c("POC","NWK_Code", "Participant_Code", "Loss_Code", "Flow_Direction","Trading_date"),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy - see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
  )
  return(reshapedDT)
}

#' Creates a daily mean half-hourly % difference plot from the data.table given
#'
#' `createDailyPcComparePlot` returns a plot which calculates the daily mean of yVar and plots the % difference between the value for 2020 and
#'  2017-2019 to compare the values for the same date over previous years. This assumes you pass in the aligned-date data. 
#'
#' @param dt the data, assumed to be the aligned data (use alignDates() to create this)
#' @param yVar the variable you want to plot
#' @param yCap the caption for the y axis
#' 
#' @import lubridate
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family plot
#'
createDailyPcComparePlot <- function(dt, yVar, yCap){
  # assumes the dateFixed half-hourly data
  # assumes we want mean of half-hourly obs
  dt <- dt[dateFixed <= lubridate::today() & 
                 dateFixed >= localParams$comparePlotCutDate] # make this lopnger to get a sense of trend
  dt[, compareVar := get(yVar)]
  baseDT <- dt[compareYear != "2020", .(baseMean = mean(compareVar)), # careful - uses compareYear label!!
                    keyby = .(dateFixed, wkdayFixed, compareYear)]
  testDT <- dt[compareYear == "2020", .(testMean = mean(compareVar)), 
                    keyby = .(dateFixed, wkdayFixed, compareYear)]
  
  setkey(baseDT, dateFixed, wkdayFixed)
  setkey(baseDT, dateFixed, wkdayFixed)
  
  plotDT <- baseDT[testDT] # auto drops non matches to 2020
  plotDT[, pcDiffMean := 100*(testMean - baseMean)/baseMean] # -ve value indicates lower
  plotDT[, pos := ifelse(pcDiffMean > 0 , "Pos", "Neg")] # want to colour the line sections - how?
  # final plot - adds annotations
  # use means for consistency with comparison plots where we use WHO thresholds (means)
  yMin <- min(plotDT$pcDiffMean)
  print(paste0("Max drop %:", round(yMin)))
  yMax <- max(plotDT$pcDiffMean)
  print(paste0("Max increase %:", round(yMax)))
  p <- ggplot2::ggplot(plotDT, aes(x = dateFixed, y = pcDiffMean 
                                   #color = pos, group=NA)
  )
  ) +
    geom_step() +
    scale_x_date(date_breaks = "7 days", date_labels =  "%a %d %b")  +
    theme(axis.text.x=element_text(angle=90, hjust=1, size = 5)) +
    labs(x = "Date",
         y = yCap,
         caption = paste0(localParams$lockdownCap, localParams$weekendCap)) +  
    theme(legend.position="bottom") +
    geom_hline(yintercept = 0, linetype = 3)
  
  p <- addLockdownRect(p, from = gcParams$UKlockDownStartDate, to = gcParams$UKlockDownEndDate, 
                       label = "Phase 1", yMin, yMax)
  p <- addWeekendRectsDate(p, yMin, yMax)
  return(p)
}
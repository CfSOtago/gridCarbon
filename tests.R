library(gridCarbon)
library(ggplot2)
library(data.table)

source("env.R")
repoParams$ukGridDataLoc

localParams <- list()
localParams$fromYear <- 2019
localParams$toDate <- as.Date("2020-06-30")

# should mostly be in R/
makeOutcomes <- function(dt){
  dt[, rGENERATION := GENERATION * 0.92] # 8% losses (Staffel, 2017)
  dt[, consumption_MW := rGENERATION]
  dt[, consGW := consumption_MW/1000]
  dt[, consumption_MWh := consumption_MW/2] # total MWh per half hour = power / 2
  dt[, consGWh := consumption_MWh/1000]
  
  # Total CO2e - original grid gen data
  # CI = g CO2e/kWh
  dt[, C02e_g := (1000*consumption_MWh) * CARBON_INTENSITY]
  dt[, C02e_kg := C02e_g/1000]
  dt[, C02e_T := C02e_kg/1000 ]
  return(dt)
}

gridGenData <- gridCarbon::loadUKESOYearlyGenData(path = repoParams$ukGridDataLoc, # from where?
                                     fromYear = localParams$fromYear, # from what date?
                                     toDate = localParams$toDate # to when?
                                     )
embeddedGenData <- gridCarbon::loadUKEmbeddedGenData(path = repoParams$ukNonGridDataLoc,
                                                    toDate = localParams$toDate,update)
derivedGridGenData <- makeOutcomes(gridGenData)
alignedGridGenData <- gridCarbon::alignDates(derivedGridGenData, 
                                dateTime = "rDateTimeUTC",
                                toDate = localParams$toDate)

recentDateTimeGWhPlot <- createRecentDateTimePlot(derivedGridGenData,
                                  dateTime = "rDateTimeUTC", 
                                  yVar = "consGWh",
                                  yCap = "GWh",
                                  yDiv = 1,
                                  lockDownStart = repoParams$UKlockDownStartDateTime,
                                  lockDownEnd = repoParams$UKlockDownEndDateTime
                                  )


compareDailyGWhPlot <- createDailyMeanComparePlot(alignedGridGenData,
                                                 yVar = "consGWh",
                                                 yCap = "GWh",
                                                 yDiv = 1,
                                                 form = "step", # default
                                                 lockDownStart = repoParams$UKlockDownStartDate,
                                                 lockDownEnd = repoParams$UKlockDownEndDate
                                                 )
compareDailyGWhPlot

tables()

# loads data & runs a report
# https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix

# Load some packages
library(gridCarbon) # load this first - you will need to download & build it locally from this repo

libs <- c("data.table", # data munching
          "drake", # data gets done once (ideally)
          "ggplot2", # plots
          "here", # here. not there
          "hms", # hms obvs
          "lubridate", # dates made easy
          "skimr", # skimming data for fast descriptives
          "zoo" # rolling mean etc
          ) 


gridCarbon::loadLibraries(libs) # should install any that are missing
drake::expose_imports(gridCarbon) # should track our functions

# check

# Parameters ----
localParams <- list()

# > dates ----
localParams$fromYear <- 2016 # a way to limit the number of years of data files loaded
localParams$recentCutDate <- as.Date("2020-03-01")
localParams$comparePlotCutDate <- as.Date("2020-02-01")

# > data paths ----
localParams$gridDataLoc <- paste0(gcParams$ukData, 
                                  "/processed/yearly/")

# > captions ----
localParams$gridCaption <- paste0("Source: UK Electricity System Operator")
localParams$gridURL <- paste0("https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix")
localParams$gamCap <- "Trend line = Generalized additive model (gam) with integrated smoothness estimation"
localParams$lockdownCap <- "\nColoured rectangles = UK covid lockdown periods to date"
localParams$weekendCap <- "\nShaded rectangle = weekends"

# > defn of peak ----
localParams$amPeakStart <- hms::as_hms("07:00:00")
localParams$amPeakEnd <- hms::as_hms("09:00:00")
localParams$pmPeakStart <- hms::as_hms("17:00:00") # source?
localParams$pmPeakEnd <- hms::as_hms("21:00:00") # source?

# Functions ----

# should mostly be in R/


makeReport <- function(f){
  # default = html
  rmarkdown::render(input = paste0(here::here("dataAnalysis/"), f, ".Rmd"),
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(here::here("docs/"), f,".html")
  )
}

# drake plan ----

plan <- drake::drake_plan(
  gridGenData = loadUKESOYearlyGenData(localParams$gridDataLoc, # from where?
                                       localParams$fromYear), # from what date?
  # nonGridData = loadGenData(localParams$nonGridDataLoc, 
  #                        localParams$fromYear)
  alignedGridGenData = alignDates(gridGenData), # fix the dates so they line up
  ## >> GW stuff ----
  recentDateTimeGW_Plot = createRecentDateTimePlot(gridGenData, 
                                                   dateTime = "rDateTimeUTC",
                                                        yVar = "GENERATION", 
                                                        yCap = "Total generation (GW)",
                                                        yDiv = 1000), # GEN is in MW
  recentHalfHourlyProfileGW_Plot = createRecentHalfHourlyProfilePlot(gridGenData, 
                                                                     dateTime = "rDateTimeUTC",
                                                                     yVar = "GENERATION", 
                                                                     yCap = "Total generation (GW)",
                                                                     yDiv = 1000 # GEN is in MW
                                                                     ),
  compareDailyGW_Plot = createDailyMeanComparePlot(alignedGridGenData, 
                                                   yVar = "GENERATION", 
                                                   yCap = "Mean daily half hourly GW",
                                                   yDiv = 1000 # what to divide the y value by
                                                   ),
  compareDailyGWpc_Plot = createDailyPcComparePlot(alignedGridGenData, 
                                                   yVar = "GENERATION", 
                                                   yCap = "% difference"
                                                   ),
  ## >> CI stuff ----
  recentDateTimeCI_Plot = createRecentDateTimePlot(gridGenData, 
                                                   dateTime = "rDateTimeUTC",
                                                   yVar = "CARBON_INTENSITY", 
                                                   yCap = "Carbon intensity",
                                                   yDiv = 1),
  
  recentHalfHourlyProfileCI_Plot = createRecentHalfHourlyProfilePlot(gridGenData, 
                                                                     dateTime = "rDateTimeUTC",
                                                                     yVar = "CARBON_INTENSITY", 
                                                                     yCap = "Carbon intensity",
                                                                     yDiv = 1), 
  compareDailyCI_Plot = createDailyMeanComparePlot(alignedGridGenData, 
                                                   yVar = "CARBON_INTENSITY", 
                                                   yCap = "Mean daily half hourly carbon intensity",
                                                   yDiv = 1 # what to divide the y value by
  ),
  compareDailyCIpc_Plot = createDailyPcComparePlot(alignedGridGenData, 
                                                   yVar = "CARBON_INTENSITY", 
                                                   yCap = "% difference"),
  ## >> CO2e kg stuff ----
  recentDateTimeC02ekg_Plot = createRecentDateTimePlot(gridGenData, 
                                                   dateTime = "rDateTimeUTC",
                                                   yVar = "totalC02e_kg", 
                                                   yCap = "C02e emitted (T)",
                                                   yDiv = 1000), # totalC02e_kg in kg
  
  
  recentHalfHourlyProfileC02ekg_Plot = createRecentHalfHourlyProfilePlot(gridGenData, 
                                                                     dateTime = "rDateTimeUTC",
                                                                     yVar = "totalC02e_kg", 
                                                                     yCap = "C02e emitted (T)",
                                                                     yDiv = 1000), # totalC02e_kg in kg 
  
  
  compareDailyCO2ekg_Plot = createDailyMeanComparePlot(alignedGridGenData, 
                                                   yVar = "totalC02e_kg", 
                                                   yCap = "Mean daily half hourly C02e (T)",
                                                   yDiv = 1000 # what to divide the y value by
                                                   ),

  compareDailyC02ekgpc_Plot = createDailyPcComparePlot(alignedGridGenData, 
                                                   yVar = "totalC02e_kg", 
                                                   yCap = "% difference")
)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

gridGenDT <- drake::readd(gridGenData)
alignedDT <- drake::readd(alignedGridGenData)


# code ----

print("Grid gen loaded")
message("Loaded ", tidyNum(nrow(gridGenDT)), " rows of data")



# > Make report ----
# >> yaml ----
version <- "1.0"
title <- paste0("UK Electricity Generation and Carbon Itensity")
subtitle <- paste0("covid 19 lockdown v", version)
authors <- "Ben Anderson"

# what dates to expect? 
summary(gridGenDT$rDateTimeUTC)

# >> run report ----
rmdFile <- "covidLockdown_UK" # not the full path
makeReport(rmdFile)

# done

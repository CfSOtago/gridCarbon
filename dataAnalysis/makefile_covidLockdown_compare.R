# loads data & runs a report
# https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix



# Load some packages
library(gridCarbon) # load this first - you will need to download & build it locally from this repo
gridCarbon::setup()

libs <- c("data.table", # data munching
          "drake", # data gets done once (ideally)
          "ggplot2", # for the plots via drake
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
update <- "yep" # edit to force data re-load - forces everything re-build :-)

localParams <- list()

# > dates ----
localParams$fromYear <- 2010 # a way to limit the number of years of data files loaded

localParams$recentCutDate <- as.Date("2020-02-01")
localParams$comparePlotCutDate <- as.Date("2020-02-01")

# > data paths ----
localParams$ukGridDataLoc <- paste0(gcParams$ukData, 
                                  "/processed/yearly/")
localParams$nzGridDataLoc <- paste0(gcParams$nzData, 
                                    "/EA_Generation_Data/processed/yearly/")
localParams$nzEmbeddedDataLoc <- paste0(gcParams$nzData, 
                                    "/EA_Embedded_Generation_Data/processed/yearly/")
localParams$nzEmissionsFile <- here::here("publicData", "emission-factors-electricity-2.csv")

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
  ukGridGenData = loadUKESOYearlyGenData(localParams$ukGridDataLoc, # from where?
                                       localParams$fromYear, # from what date?
                                       update), 
  nzGridGenData = loadNZEAYearlyGenData(localParams$nzGridDataLoc, # from where?
                                       localParams$fromYear, # from what date?
                                       update),
  nzEmbeddedGenData = loadNZEAYearlyEmbeddedGenData(localParams$nzEmbeddedDataLoc, # from where?
                                        localParams$fromYear, # from what date?
                                        update),
  # nonGridData = loadGenData(localParams$nonGridDataLoc, 
  #                        localParams$fromYear)
  ukAlignedGridGenData = alignDates(ukGridGenData, "rDateTimeUTC"), # fix the dates so they line up
  nzAlignedGridGenData = alignDates(nzGridGenData, "rDateTimeNZT"), # fix the dates so they line up
  nzAlignedEmbeddedGenData = alignDates(nzEmbeddedGenData, "rDateTimeNZT") # fix the dates so they line up
)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

ukGridGenDT <- drake::readd(ukGridGenData)
ukAlignedDT <- drake::readd(ukAlignedGridGenData)
nzGridGenDT <- drake::readd(nzGridGenData)
nzAlignedDT <- drake::readd(nzAlignedGridGenData)

# test a plot ----
#drake::readd(ukRecentDateTimeGWPlot)
#drake::readd(nzRecentDateTimeGWPlot)

# code ----

print("UK Grid gen loaded")
message("Loaded ", tidyNum(nrow(ukGridGenDT)), " rows of data")

print("NZ Grid gen loaded")
message("Loaded ", tidyNum(nrow(nzGridGenDT)), " rows of data")



# > Make report ----
# >> yaml ----
version <- "1.0"
title <- paste0("UK Electricity Generation and Carbon Itensity")
subtitle <- paste0("covid 19 lockdown v", version)
authors <- "Ben Anderson"

# what dates to expect? 
summary(ukGridGenDT$rDateTimeUTC)
summary(nzGridGenDT$rDateTimeNZT)

# >> run report ----
rmdFile <- "covidLockdown_compare" # not the full path
#makeReport(rmdFile)

# done


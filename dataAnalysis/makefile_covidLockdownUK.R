# loads data & runs a report

# Load some packages
library(gridCarbon) # load this first - you will need to download & build it locally from this repo

libs <- c("data.table", # data munching
          "drake", # data gets done once (ideally)
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

gridCarbon::loadLibraries(libs) # should install any that are missing


# check

# Parameters ----
localParams <- list()

# > dates ----
localParams$fromYear <- 2016 # a way to limit the number of years of data files loaded
localParams$lockDownStart <- as.Date("2020-03-24")
localParams$lockDownEnd <- as.Date("2020-04-24")

# > data paths ----
localParams$gridDataLoc <- paste0(gcParams$GreenGrid, 
                                  "externalData/ukEso/eso_generation/processed/yearly/")

# > captions ----
localParams$gridCaption <- paste0("Source: UK Electricity System Operator")
localParams$gridURL <- paste0("https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix")

# > defn of peak ----
amPeakStart <- hms::as_hms("07:00:00")
amPeakEnd <- hms::as_hms("09:00:00")
pmPeakStart <- hms::as_hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as_hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

# Functions ----

setPeakPeriod <- function(dt){
  # assumes hms exists
  dt[, peakPeriod := NA]
  dt[, peakPeriod := ifelse(hms < amPeakStart, "Early morning", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakStart & hms < amPeakEnd, "Morning peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakEnd & hms < pmPeakStart, "Day time", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakStart & hms < pmPeakEnd, "Evening peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakEnd, "Late evening", peakPeriod)]
  dt[, peakPeriod := forcats::fct_relevel(peakPeriod, 
                                          "Early morning",
                                          "Morning peak",
                                          "Day time",
                                          "Evening peak",
                                          "Late evening")]
  return(dt)
}


makeReport <- function(f){
  # default = html
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(gcParams$repoLoc,"/docs/", title,"_",
                                         subtitle,".html")
  )
}


loadGenData <- function(path, fromYear){
  # lists files within a folder (path) & loads fromYear
  filesToDateDT <- data.table::as.data.table(list.files(path, ".csv.gz")) # get list of files already downloaded & converted to long form
  filesToDateDT[, file := V1]
  filesToDateDT[, c("year", "name") := tstrsplit(file, split = "_")]
  filesToDateDT[, year := as.numeric(year)]
  filesToDateDT[, fullPath := paste0(path, file)]
  filesToGet <- filesToDateDT[year >= fromYear, # to reduce files loaded
                              fullPath]
  message("Loading files >= ", fromYear)
  l <- lapply(filesToGet, data.table::fread) # very fast data loading :-)
  dt <- rbindlist(l, fill = TRUE) # rbind them
  return(dt) # large, possibly very large depending on fromYear
}

# drake plan ----
plan <- drake::drake_plan(
  gridGenData = loadGenData(localParams$gridDataLoc, # from where?
                      localParams$fromYear), # from what date?
  # nonGridData = loadGenData(localParams$nonGridDataLoc, 
  #                        localParams$fromYear)
)
# 
# path <- localParams$gridDataLoc
# fromYear <- localParams$fromYear
# dt <- loadGenData(path, # from where?
#                   fromYear)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

gridGenDT <- drake::readd(gridGenData)


# code ----

# > fix grid data ----
gridGenDT[, rDateTimeUTC := lubridate::as_datetime(DATETIME)]
gridGenDT[, rDateTimeUTC := lubridate::force_tz(rDateTimeUTC, 
                                              tzone = "Europe/London")] # to be sure to be sure
gridGenDT[, rMonth := lubridate::month(rDateTimeUTC, label = TRUE, abbr = TRUE)]

# check
h <- head(gridGenDT[, .(DATETIME, year, rDateTimeUTC, GENERATION, CARBON_INTENSITY)])
h
print("Grid gen loaded")
message("Loaded ", tidyNum(nrow(gridGenDT)), " rows of data")



# > Make report ----
# >> yaml ----
version <- "1.0"
title <- paste0("UK Electricity Generation")
subtitle <- paste0("covid 19 lockdown v", version)
authors <- "Ben Anderson"

# what dates to expect? 
summary(gridGenDT$rDateTimeUTC)

# >> run report ----
rmdFile <- paste0(gcParams$repoLoc, "/dataAnalysis/covidLockdown_UK.Rmd")
makeReport(rmdFile)



# loads data & runs a report
# https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix

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

# > data paths ----
localParams$gridDataLoc <- paste0(gcParams$ukData, 
                                  "/processed/yearly/")

# > captions ----
localParams$gridCaption <- paste0("Source: UK Electricity System Operator")
localParams$gridURL <- paste0("https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix")

# > defn of peak ----
amPeakStart <- hms::as_hms("07:00:00")
amPeakEnd <- hms::as_hms("09:00:00")
pmPeakStart <- hms::as_hms("17:00:00") # source?
pmPeakEnd <- hms::as_hms("21:00:00") # source?

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
  rmarkdown::render(input = paste0(here::here("dataAnalysis/"), f, ".Rmd"),
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(here::here("docs/"), f,".html")
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
  
  # > fix grid data ----
  dt[, rDateTimeUTC := lubridate::as_datetime(DATETIME)]
  dt[, rDateTimeUTC := lubridate::force_tz(rDateTimeUTC, 
                                                  tzone = "Europe/London")] # to be sure to be sure
  dt[, rMonth := lubridate::month(rDateTimeUTC, label = TRUE, abbr = TRUE)]
  
  # check
  #h <- head(gridGenDT[, .(DATETIME, year, rDateTimeUTC, GENERATION, CARBON_INTENSITY)])
  #h
  
  return(dt) # large, possibly very large depending on fromYear
}

fixDates <- function(dt){
  # expects gridGenData from loadGenData as the dt
  # re-use the method from the airQual analysis to align the years
  # so kludgy it should not be allowed

  dt[, obsDate := lubridate::date(rDateTimeUTC)]
  dt[, decimalDate := lubridate::decimal_date(obsDate)] # gives year.% of year
  
  # set to 2020 'dates'
  dt[, date2020 := lubridate::as_date(lubridate::date_decimal(2020 + (decimalDate - year)))] # sets 'year' portion to 2020 so the lockdown annotation works
  dt[, day2020 := lubridate::wday(date2020, label = TRUE)] # 
  
  # 2020 Jan 1st = Weds
  dt2020 <- dt[year == 2020] 
  dt2020[, dateFixed := obsDate] # no need to change
  dt2020[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2020$origDoW, dt2020$fixedDoW)
  # head(dt2020[origDoW != fixedDoW])
  
  # shift to the closest aligning day
  # 2019 Jan 1st = Tues
  dt2019 <- dt[year == 2019] 
  dt2019[, dateFixed := date2020 -1]
  dt2019[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2019$origDoW, dt2019$fixedDoW)
  # head(dt2019[origDoW != fixedDoW])
  
  # 2018 Jan 1st = Mon
  dt2018 <- dt[year == 2018] 
  dt2018[, dateFixed := date2020 - 2]
  dt2018[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2018$origDoW, dt2018$fixedDoW)
  # head(dt2018[origDoW != fixedDoW])
  
  # 2017 Jan 1st = Sat
  dt2017 <- dt[year == 2017] 
  dt2017[, dateFixed := date2020 - 3]
  dt2017[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2017$origDoW, dt2017$fixedDoW)
  # head(dt2017[origDoW != fixedDoW])
  
  fixedDT <- rbind(dt2017, dt2018, dt2019, dt2020) # leave out 2016 for now
  
  fixedDT[, dateFixed := lubridate::as_date(dateFixed)]
  fixedDT[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  
  
  fixedDT[, compareYear := ifelse(year == 2020, "2020",
                                  "2017-2019")
          ]
  fixedDT[, wkdayObs := lubridate::wday(obsDate, label = TRUE)]
  
  return(fixedDT)
}

# drake plan ----
plan <- drake::drake_plan(
  gridGenData = loadGenData(localParams$gridDataLoc, # from where?
                      localParams$fromYear), # from what date?
  # nonGridData = loadGenData(localParams$nonGridDataLoc, 
  #                        localParams$fromYear)
  fixedGenData = fixDates(gridGenData)
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

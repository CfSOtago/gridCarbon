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
localParams$fromYear <- 2009 # a way to limit the number of years of data files loaded
localParams$lockDownStart <- as.Date("2020-03-24")
localParams$lockDownEnd <- as.Date("2020-04-24")

# > data paths ----
localParams$gridDataLoc <- paste0(gcParams$GreenGrid, 
                                  "externalData/EA_Generation_Data/processed/yearly/")
localParams$nonGridDataLoc <- paste0(gcParams$GreenGrid, 
                                     "externalData/EA_Embedded_Generation_Data/processed/yearly/")
# > captions ----
localParams$gridCaption <- paste0("Source: NZ Energy Authority",
                                 "\nhttps://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/")
localParams$nonGridCaption <- paste0("Source: NZ Energy Authority",
                                 "\nhttps://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation")
localParams$jointCaption <- paste0(localParams$gridCaption, "\n", 
                                   localParams$nonGridCaption)

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

getGXPFileList <- function(dPath){
  # check for EA GXP files
  message("Checking for data files")
  all.files <- list.files(path = dPath, pattern = ".csv")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  message("Found ", nrow(dt))
  return(dt)
} # should be in package functions

loadGenData <- function(path, fromYear){
  # lists files within a folder (path) & loads
  filesToDateDT <- data.table::as.data.table(list.files(path, ".csv.gz")) # get list of files already downloaded & converted to long form
  filesToDateDT[, file := V1]
  filesToDateDT[, c("year", "name") := tstrsplit(file, split = "_")]
  filesToDateDT[, year := as.numeric(year)]
  filesToGet <- filesToDateDT[year >= fromYear, # to reduce files loaded
                              file]
  message("Loading files >= ", fromYear)
  dt <- do.call(rbind,
                lapply(filesToGet, # a list
                       function(f)
                         data.table::fread(paste0(path,f))
                ) # decodes .gz on the fly
  )
  return(dt) # large
}

# drake plan ----
plan <- drake::drake_plan(
  gridData = loadGenData(localParams$gridDataLoc, # from where?
                      localParams$fromYear), # from what date?
  nonGridData = loadGenData(localParams$nonGridDataLoc, 
                         localParams$fromYear)
)
# 
# path <- localParams$gridDataLoc
# fromYear <- localParams$fromYear
# dt <- loadGenData(path, # from where?
#                   fromYear)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

origGridDT <- drake::readd(gridData)
origNonGridDT <- drake::readd(nonGridData)

# code ----

# > fix grid data ----
origGridDT[, rTime := hms::as_hms(rTime)]
origGridDT[, rDateTimeOrig := rDateTime] # just in case
origGridDT[, rDateTime := lubridate::as_datetime(rDateTime)]
origGridDT[, rDateTime := lubridate::force_tz(rDateTime, tzone = "Pacific/Auckland")]
origGridDT[, rMonth := lubridate::month(rDateTime, label = TRUE, abbr = TRUE)]

# check
h <- head(origGridDT[, .(rDateTimeOrig, Time_Period, rDateTime)])
h
print("Grid gen loaded")
message("Loaded ", tidyNum(nrow(origGridDT)), " rows of data")
table(origGridDT[is.na(rDateTime)]$Time_Period)
allGridDT <- origGridDT[!is.na(rDateTime) | # removes TP 49 & 50
                          !is.na(kWh)] # removes NA kWh
nrow(allGridDT)
summary(allGridDT) # test

# > non grid data ----
origNonGridDT[, rDateTimeOrig := rDateTime] # just in case
origNonGridDT[, rDateTime := lubridate::as_datetime(rDateTime)]
origNonGridDT[, rTime := hms::as_hms(rDateTime)]
origNonGridDT[, rDateTime := lubridate::force_tz(rDateTime, tzone = "Pacific/Auckland")]
origNonGridDT[, rMonth := lubridate::month(rDateTime, label = TRUE, abbr = TRUE)]
# check
h <- head(origNonGridDT[, .(rDateTimeOrig, Time_Period, rDateTime)])
h
print("Non grid gen loaded")
message("Loaded ", tidyNum(nrow(origNonGridDT)), " rows of data")
table(origNonGridDT[is.na(rDateTime)]$Time_Period)
allEmbeddedDT <- origNonGridDT[!is.na(rDateTime) | # removes TP 49 & 50
                                 !is.na(kWh)] # removes NA kWh
nrow(allEmbeddedDT)


# > Make report ----
# >> yaml ----
version <- "1.0"
title <- paste0("NZ Electricity Generation")
subtitle <- paste0("GHG intensity trends v", version)
authors <- "Ben Anderson, Marilette Lotte and Carsten Dortans"


# >> run report ----
rmdFile <- paste0(gcParams$repoLoc, "/dataAnalysis/ghgIntensityTrends.Rmd")
makeReport(rmdFile)



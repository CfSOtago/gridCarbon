# loads data & runs a report

# Load some packages
library(gridCarbon) # load this first - you will need to download & build it locally from this repo
gridCarbon::setup()

libs <- c("data.table", # data munching
          "drake", # data gets done once (ideally)
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

gridCarbon::loadLibraries(libs) # should install any that are missing


# check

# Parameters ----

update <- "yes" # edit this in any way to trigger drake to reload data
  
localParams <- list()

# > dates ----
localParams$fromYear <- 2017 # a way to limit the number of years of data files loaded
localParams$lockDownStart <- as.Date("2020-03-24")
localParams$lockDownEnd <- lubridate::today()

# > data paths ----
gridDataPath <- paste0(gcParams$nzGridDataLoc, 
                                  "processed/yearly/")
nonGridDataPath <- paste0(gcParams$nzNonGridDataLoc, 
                                     "processed/yearly/")
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
                    output_file = paste0(gcParams$repoLoc,"/docs/nz/", title,"_",
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

loadGenData <- function(path, fromYear, update){
  # lists files within a folder (path) & loads
  # edit fromYear to force a new data load
  filesToDateDT <- data.table::as.data.table(list.files(path, ".csv.gz")) # get list of files already downloaded & converted to long form
  filesToDateDT[, file := V1]
  filesToDateDT[, c("year", "name") := tstrsplit(file, split = "_")]
  filesToDateDT[, year := as.numeric(year)]
  filesToGet <- filesToDateDT[year >= fromYear, # to reduce files loaded
                              file]
  message("Loading files >= ", fromYear)
  l <- lapply(paste0(path, filesToGet), # construct path for each file
              data.table::fread) # mega fast read
  dt <- rbindlist(l, fill = TRUE) # rbind them
  l <- NULL
  # dt <- do.call(rbind,
  #               lapply(filesToGet, # a list
  #                      function(f)
  #                        data.table::fread(paste0(path,f))
  #               ) # decodes .gz on the fly
  # )
  return(dt) # large
}

# 
#gridData <- loadGenData(localParams$gridDataLoc, # from where?
#                        localParams$fromYear) # from what date?

# drake plan ----
plan <- drake::drake_plan(
  gridData = loadGenData(gridDataPath, # from where?
                      localParams$fromYear,  # from what date?
                      update),
  nonGridData = loadGenData(nonGridDataPath, 
                         localParams$fromYear, 
                         update)
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
origGridDT[, rDateTimeOrig := rDateTime] # just in case
origGridDT[, rDateTime := lubridate::as_datetime(rDateTime)]
origGridDT[, rDateTimeNZT := lubridate::force_tz(rDateTime, 
                                                 tzone = "Pacific/Auckland")] # just to be sure
origGridDT[, rTime := hms::as_hms(rDateTimeNZT)]
origGridDT[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]

# check
print("Grid gen loaded")
message("Loaded ", tidyNum(nrow(origGridDT)), " rows of data")
table(origGridDT[is.na(rDateTimeNZT)]$Time_Period)
nrow(origGridDT)
allGridDT <- origGridDT[!is.na(rDateTimeNZT)] # removes TP 49 & 50
allGridDT <- allGridDT[!is.na(kWh)] # removes NA kWh
nrow(allGridDT)
summary(allGridDT$rDateTimeNZT)

# > non grid data ----
origNonGridDT[, rDateTimeOrig := rDateTime] # just in case
origNonGridDT[, rDateTime := lubridate::as_datetime(rDateTime)]
origNonGridDT[, rDateTimeNZT := lubridate::force_tz(rDateTime, 
                                                    tzone = "Pacific/Auckland")]
origNonGridDT[, rTime := hms::as_hms(rDateTimeNZT)]
origNonGridDT[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]

# check
print("Non grid gen loaded")
message("Loaded ", tidyNum(nrow(origNonGridDT)), " rows of data")
table(origNonGridDT[is.na(rDateTime)]$Time_Period)
nrow(origNonGridDT)
allEmbeddedDT <- origNonGridDT[!is.na(rDateTimeNZT)] # removes TP 49 & 50
allEmbeddedDT <- allEmbeddedDT[!is.na(kWh)] # removes NA kWh
nrow(allEmbeddedDT)
summary(allEmbeddedDT$rDateTimeNZT)

# test dates available ----
# allGrid
summary(allGridDT$rDateTimeNZT) # test dates available
# embeddedGrid
summary(allEmbeddedDT$rDateTimeNZT)

# > Make report ----
# >> yaml ----
version <- "1.0"
title <- paste0("NZ Electricity Generation")
subtitle <- paste0("covid 19 lockdown v", version)
authors <- "Ben Anderson, Carsten Dortans and Marilette Lotte"


# >> run report ----
rmdFile <- paste0(gcParams$repoLoc, "/rmd/nz/covidLockdown_NZ.Rmd")

makeReport(rmdFile)



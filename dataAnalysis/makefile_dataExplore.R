# loads data & runs a report

# Load some packages
library(GREENGridEECA)

libs <- c("data.table", # data munching
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

GREENGridEECA::loadLibraries(libs) # should install any that are missing

GREENGridEECA::setup() # set data paths etc

# check

# parameters ----

# Data ----
# Load all data here, not in .Rmd

#> Census data ----
ipfCensusDT <- data.table::fread(paste0("~/Data/NZ_Census/data/processed/2013IpfInput.csv"))

#> GREEN Grid half hourly total dwelling power data ----
hhTotalLoadF <- paste0(repoParams$GreenGridData, "/gridSpy/halfHour/extracts/halfHourImputedTotalDemand.csv.gz")
hhTotalLoadDT <- data.table::fread(hhTotalLoadF)

#> HCS 2015 heat source
hcs2015DT <- data.table::fread(paste0(here::here(), "/data/input/hcs2015HeatSources.csv"))

#> GREEN Grid household survey data ----
hhAttributesF <- paste0(repoParams$GreenGridData,"/survey/ggHouseholdAttributesSafe.csv.gz") 
hhAttributesDT <- data.table::fread(hhAttributesF)
ipfSurveyDT <- data.table::fread(paste0(repoParams$GreenGridData, "/survey/ggIpfInput.csv"))

#> ipf weights data from previous run of model
ipfWeightsDT <- data.table::fread(paste0(repoParams$GreenGridData, "/ipf/nonZeroWeightsAu2013.csv"))

#> GXP data ----
getGXPFileList <- function(dPath){
  # check for EA GXP files
  message("Checking for data files")
  all.files <- list.files(path = dPath, pattern = ".csv")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  message("Found ", nrow(dt))
  return(dt)
} # should be in package functions


loadGXPData <- function(files){
  # load the EA GXP data
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  message("Loading ", nrow(files), " GXP files")
  l <- lapply(files$fullPath, data.table::fread)
  dt <- rbindlist(l)
  setkey(dt, rDateTime)
  try(file.remove("temp.csv")) # side effect
  # fix dates & times ----
  dt <- dt[!is.na(rTime)] # drop the TP49 & TP50
  dt[, rDateTime := lubridate::as_datetime(rDateTime)]
  dt[, rDateTime := lubridate::force_tz(rDateTime, tzone = "Pacific/Auckland")]
  dt[, date := lubridate::date(rDateTime)]
  dt[, month := lubridate::month(rDateTime)]
  dt[, day_of_week := lubridate::wday(rDateTime, label = TRUE)]
  dt[, hms := hms::as.hms(rDateTime)] # set to middle of half-hour
  dt[, halfHour := hms::trunc_hms(hms, 30*60)] # truncate to previous half-hour
  
  # Create factor for weekdays/weekends ----
  dt[, weekdays := "Weekdays"]
  dt[, weekdays := ifelse(day_of_week == "Sat" |
                            day_of_week == "Sun", "Weekends", weekdays)]
  # locate in peak/not peak ----
  dt <- setPeakPeriod(dt)
  
  # load the POC lookup table Vince sent
  f <- paste0(here::here(), "/data/input/gxp-lookup.csv")
  gxpLutDT <- data.table::fread(f)
  
  setkey(gxpLutDT, node)
  setkey(dt, POC)
  
  dt <- gxpLutDT[dt] # merge them - this keeps all the gxpData
  
  # load the Transpower GIS table
  f <- paste0(here::here(), "/data/input/gxpGeolookup.csv")
  gxpGeoDT <- data.table::fread(f)
  # note there are differences
  # in the Transpower data MXLOCATION == node but without the trailing 0331 - whatever this means
  dt[, MXLOCATION := substr(node, start = 1, stop = 3)] # note this produces non-unique locations
  # so presumably some gxps share a location where they feed different networks
  uniqueN(dt$node)
  uniqueN(dt$MXLOCATION)
  dt[, .(nNodes = uniqueN(node)), keyby = .(MXLOCATION)]
  setkey(dt, MXLOCATION)
  setkey(gxpGeoDT, MXLOCATION)
  gxpDataDT <- gxpGeoDT[dt]
  return(dt)
} # should be in package functions

years <- c("2015")

gxpFiles <- getGXPFileList(repoParams$gxpData) # will be empty if never run before so

if(nrow(gxpFiles) == 0){
  message("No data!")
} else {
  message("Yep, we've got (some) data, loading it")
  gxpDataDT <- loadGXPData(gxpFiles)
  # save out fpr future use
  message("Saving processed version for future use...")
  data.table::fwrite(file = paste0(repoParams$gxpData, "gxpDataDT_latest.csv"),
                     gxpDataDT)
}

# remove the date NAs here (DST breaks)
gxpDataDT <- gxpDataDT[!is.na(date)]

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
                    output_file = paste0(repoParams$repoLoc,"/docs/partC_upscalingAdvice_v", version, ".html")
  )
}

# code ----

# > Make report ----
# >> yaml ----
version <- "1.0_Final"
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Analysis (Part C) Upscaling Advice Report v", version)
authors <- "Ben Anderson"


# >> run report ----
rmdFile <- paste0(repoParams$repoLoc, "/reports/partC_upscalingAdvice/upscaling.Rmd")
makeReport(rmdFile)



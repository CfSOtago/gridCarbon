# loads data & runs a report

# Load some packages
library(gridCarbon)

libs <- c("data.table", # data munching
          "drake", # data gets done once (ideally)
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

loadLibraries(libs) # should install any that are missing


# check

# Parameters ----
localParams <- list()

# > data paths
localParams$gridDataLoc <- paste0(gcParams$GreenGrid, 
                                  "externalData/EA_Generation_Data/processed/yearly/")
localParams$nonGridDataLoc <- paste0(gcParams$GreenGrid, 
                                     "externalData/EA_Embedded_Generation_Data/processed/yearly/")
# > captions
localParams$gridCaption <- paste0("Source: NZ Energy Authority",
                                 "\nhttps://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/")
localParams$nonGridCaption <- paste0("Source: NZ Energy Authority",
                                 "\nhttps://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation")
localParams$jointCaption <- paste0(localParams$gridCaption, "\n", 
                                   localParams$nonGridCaption)



# Functions ----
loadData <- function(path){
  # lists files within a folder (path) & loads
  filesToDateDT <- data.table::as.data.table(list.files(path, ".csv.gz")) # get list of files already downloaded & converted to long form
  dt <- do.call(rbind,
                lapply(filesToDateDT[,V1], # V1 = filename without path
                       function(f)
                         data.table::fread(paste0(path,f))
                ) # decodes .gz on the fly
  )
  return(dt) # large
}

# > drake plan ----
plan <- drake::drake_plan(
  gridData = loadData(localParams$gridDataLoc),
  nonGridData = loadData(localParams$nonGridDataLoc)
)


# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

origGridDT <- drake::readd(gridData)
origNonGridDT <- drake::readd(nonGridData)

# fix vars ----
  # fix vars here so we don't get forced to reload the data when we add new fixes
  # > grid data ----
origGridDT[, rTime := hms::as_hms(rTime)]
origGridDT[, rDateTimeOrig := rDateTime] # just in case
origGridDT[, rDateTime := lubridate::as_datetime(rDateTime)]
origGridDT[, rDateTime := lubridate::force_tz(rDateTime, tzone = "Pacific/Auckland")]
origGridDT[, rMonth := lubridate::month(rDateTime, label = TRUE, abbr = TRUE)]

h <- head(origGridDT[, .(rDateTimeOrig, Time_Period, rDateTime)])
h
print("Grid gen loaded")
message("Loaded ", tidyNum(nrow(origGridDT)), " rows of data")

# > non grid data ----
h <- head(origNonGridDT[, .(rDateTimeOrig, Time_Period, rDateTime)])
h
print("Non grid gen loaded")
message("Loaded ", tidyNum(nrow(origNonGridDT)), " rows of data")
  
# GXP data ----
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
                    output_file = paste0(repoParams$repoLoc,"/docs/", title,"_",
                                         subtitle,".html")
  )
}

# code ----

# > Make report ----
# >> yaml ----
version <- "1.0"
title <- paste0("NZ Electricity Generation")
subtitle <- paste0("GHG intensity trends v", version)
authors <- "Ben Anderson, Marilette Lotte and Carsten Dortans"


# >> run report ----
rmdFile <- paste0(gcParams$repoLoc, "/analysis/ghgIntensityTrends.Rmd")
#makeReport(rmdFile)



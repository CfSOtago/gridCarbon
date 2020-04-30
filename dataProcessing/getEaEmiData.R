# Get NZ Generation data: ----
# This is:
# > EA Wholesale generation data - https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
# > EA Embedded Generation data - https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation

# - gets or refreshes the EA generation data
# - saves them as-is 
# - processes to long form & saves as .csv.gz
# - converts to yearly files

# Load libraries ----
library(gridCarbon) # load this first - you will need to download & build it locally from this repo

# Packages needed in this .Rmd file ----
reqLibs <- c("data.table", # data munching
             "curl",  # for data download
             "here", # for here
             "ggplot2", # for fancy graphs
             "readr", # writing to files
             "skimr" # for skim
)
# load them
gridCarbon::loadLibraries(reqLibs)

# Parameters ----
source(paste0(here::here(), "/.Rprofile")) # to set default repo lovel params

localParams <- list() # repo level params are in gcParams

years <- seq(2000, 2020, 1) # change these to restrict or extend the file search
months <- seq(1,12,1) # change these to restrict or extend the file search

refresh <- 0 # set to 1 to try to download all files even if we already have them

localParams$gridGenDataLoc <- paste0(gcParams$GreenGrid, 
                                  "externalData/EA_gridGen_Data/")
localParams$embeddedGenDataLoc <- paste0(gcParams$GreenGrid, 
                                     "externalData/EA_embeddedGen_Data/")

localParams$embDataURL <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation/"
localParams$gridDataURL <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/"

localParams$rawEmbDataPath <- path.expand(paste0(localParams$embeddedGenDataLoc, "raw/"))
localParams$processedEmbDataPath <- path.expand(paste0(localParams$embeddedGenDataLoc, "processed/"))
localParams$rawGridDataPath <- path.expand(paste0(localParams$gridGenDataLoc, "raw/"))
localParams$processedGridDataPath <- path.expand(paste0(localParams$gridGenDataLoc, "processed/"))

# Local functions ----


# > grid data ----
# >> cleaning ----
cleanGridEA <- function(dt){
  # cleans & returns a dt
  dtl <- gridCarbon::reshapeGenDT(dt) # make long
  dtl <- gridCarbon::setGridGenTimePeriod(dtl) # set time periods to something intelligible as rTime
  dtl[, rDate := as.Date(Trading_date)] # fix the dates so R knows what they are
  dtl[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  # there will be parse errors in the above due to TP49 & TP50
  table(dtl[is.na(rDateTime)]$Time_Period)
  return(dtl)
}
# >> grid meta data ----
getGridMeta <- function(dt){
  dt <- dt[, month := lubridate::month(rDate)]
  dt <- dt[,year := lubridate::year(rDate)]
  testDT <- dt[, .(nObs = .N,
                   sumMWh = sum(as.numeric(kWh/1000), na.rm = TRUE),
                   meanMWh = mean(as.numeric(kWh/1000), na.rm = TRUE),
                   nFuels = uniqueN(Fuel_Code),
                   dateFrom = min(rDate),
                   dateTo = max(rDate),
                   nDays = uniqueN(rDate)), keyby = .(month,
                                                      year)]
  return(testDT)
}

# >> get gridGen data ----
# if we already have it, loads from local and processes
# saves out a raw monthly file (as they are monthly)
# saves out a processed (long form) monthly file
# returns metaDT

getGridData <- function(years, months){
  message("Checking what we have already...")
  filesToDateDT <- data.table::as.data.table(list.files(localParams$rawGridDataPath)) # get list of files already downloaded
  metaDT <- data.table::data.table() # stats collector
  for(y in years){
    for(month in months){
      # construct the filename
      if(nchar(month) == 1){
        # need to add 0 as prefix
        m <- paste0("0", month)
      } else {
        m <- month
      }
      lfName <- paste0(y,"_", m,"_Generation_MD.csv") # the raw data file as saved
      # do not add .gz because we use the name below to save the raw data file if it doesn't exsit yet
      print(paste0("Checking ", lfName))
      test <- filesToDateDT[V1 %like% lfName] #should catch .csv.gz too
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so load from local file
        print(paste0("Already have ", lfName, ", loading from local..."))
        # Load so we can update meta
        dt <- data.table::fread(paste0(localParams$rawGridDataPath, lfName, ".gz")) # this loop wants the raw data
        # should probably change this so it doesn't need to run over _all_ the files every time
        dtl <- cleanGridEA(dt) # clean up to a dt - fixes dateTimes etc
        #print(summary(dt))
        gotDT <- TRUE
      } else {
        # Get it
        print(paste0("We don't have or need to refresh ", lfName))
        rfName <- paste0(y, m,"_Generation_MD.csv")
        rFile <- paste0(localParams$gridDataURL,rfName)
        # use curl function to catch errors
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          dt <- data.table::fread(req$content)
          f <- paste0(localParams$rawGridDataPath, lfName)
          print(paste0("Downloaded, saving to ", f))
          data.table::fwrite(dt, f)
          cmd <- paste0("gzip -f ", "'", path.expand(f), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed it")
          dtl <- cleanGridEA(dt) # clean up to a dt - this does all the processing & converts to long form
          gotDT <- TRUE
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
          gotDT <- FALSE
        }
      }
      # at this point we have the long form of the dt unless the download failed
      if(gotDT){
        testDT <- getGridMeta(dtl) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := lfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
        # save it
        mfName <- paste0(y,"_",m,"_gridGen.csv")
        of <- paste0(localParams$processedGridDataPath, "monthly/", mfName)
        message("Converted to long form, saving it to", of)
        data.table::fwrite(dtl, of)
        cmd <- paste0("gzip -f ", "'", path.expand(of), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
        try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
        print("Compressed it")
      } else {
        # skip that
      }
    }
  }
  return(metaDT)
}


# > Embedded data ----
#>> cleaning ----
# these need to vary slightly from the grid data :-(
cleanEmbeddedEA <- function(dt){
  # cleans & returns a long form dt
  dtl <- gridCarbon::reshapeEmbeddedGenDT(dt) # make long
  dtl <- gridCarbon::setEmbeddedGenTimePeriod(dtl) # set time periods to something intelligible as rTime
  dtl[, rDate := lubridate::dmy(Trading_date)] # fix the dates so R knows what they are
  dtl[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  # there will be parse errors in the above due to TP49 & TP50
  table(dtl[is.na(rDateTime)]$Time_Period)
  return(dtl)
}
#>> meta data ----
getEmbeddedMeta <- function(dt){
  dt <- dt[, month := lubridate::month(rDate)]
  dt <- dt[,year := lubridate::year(rDate)]
  testDT <- dt[, .(nObs = .N,
                   sumMWh = sum(as.numeric(kWh)/1000, na.rm = TRUE),
                   meanMWh = mean(as.numeric(kWh)/1000, na.rm = TRUE),
                   dateFrom = min(rDate),
                   dateTo = max(rDate),
                   nDays = uniqueN(rDate)), keyby = .(month,
                                                      year,
                                                      Flow_Direction)] # inport/export?
  return(testDT)
}
# >> get embedded gen data ----
getEmbeddedData <- function(years, months){
  message("Checking what we have already...")
  filesToDateDT <- data.table::as.data.table(list.files(localParams$rawEmbDataPath)) # get list of files already downloaded
  metaDT <- data.table::data.table() # stats collector
  for(y in years){
    for(month in months){
      # construct the filename
      if(nchar(month) == 1){
        # need to add 0 as prefix
        m <- paste0("0", month)
      } else {
        m <- month
      }
      lfName <- paste0(y,"_", m,"_Embedded_generation.csv") # the raw data file as saved
      # do not add .gz because we use the name below to save the raw data file if it doesn't exsit yet
      print(paste0("Checking ", lfName))
      test <- filesToDateDT[V1 %like% lfName] #should catch .csv.gz too
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so load from local file
        print(paste0("Already have ", lfName, ", loading from local..."))
        # Load so we can update meta
        dt <- data.table::fread(paste0(localParams$rawEmbDataPath, lfName, ".gz")) # this loop wants the raw data
        # should probably change this so it doesn't need to run over _all_ the files every time
        dtl <- cleanEmbeddedEA(dt) # clean up to a dt - fixes dateTimes etc
        #print(summary(dt))
        gotDT <- TRUE
      } else {
        # Get it
        print(paste0("We don't have or need to refresh ", lfName))
        rfName <- paste0(y, m,"_Embedded_generation.csv")
        rFile <- paste0(localParams$embDataURL,rfName)
        # use curl function to catch errors
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          dt <- data.table::fread(req$content)
          f <- paste0(localParams$rawEmbDataPath, lfName)
          print(paste0("Downloaded, saving to ", f))
          data.table::fwrite(dt, f)
          cmd <- paste0("gzip -f ", "'", path.expand(f), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed it")
          dtl <- cleanEmbeddedEA(dt) # clean up to a dt - this does all the processing & converts to long form
          gotDT <- TRUE
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
          gotDT <- FALSE
        }
      }
      # at this point we have the long form of the dt if the download didn't fail
      if(gotDT){
        testDT <- getEmbeddedMeta(dtl) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := lfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
        # save it
        mfName <- paste0(y,"_",m,"_embeddedGen.csv")
        of <- paste0(localParams$processedEmbDataPath, "monthly/", mfName)
        message("Converted to long form, saving it to", of)
        data.table::fwrite(dtl, of)
        cmd <- paste0("gzip -f ", "'", path.expand(of), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
        try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
        print("Compressed it")
      }
    }
  }
  return(metaDT)
}

# > convert monthly to yearly data ----
makeYearlyData <- function(genType){ # parameter selects path and thus files
  #genType <- "embeddedGen"
  if(genType == "gridGen"){
    path <- paste0(localParams$gridGenDataLoc)
  } 
  if(genType == "embeddedGen"){
    path <- paste0(localParams$embeddedGenDataLoc)
  }
  iPath <- paste0(path, "processed/monthly/")
  oPath <- paste0(path, "processed/yearly/")
  message("Checking what we have in: ", iPath)
  filesToDateDT <- data.table::as.data.table(list.files(iPath)) # get list of files already downloaded
  filesToDateDT[, year := tstrsplit(V1, split = "_", keep = 1)]
  filesToDateDT[, fullPath := paste0(iPath,V1)]
  years <- unique(filesToDateDT[year != "metaDT.csv",]$year) # avoid the metadata file
  for(y in years){
    fList <- filesToDateDT[year == y, fullPath]
    l <- lapply(fList, data.table::fread) # very fast data loading :-)
    yearDT <- rbindlist(l, fill = TRUE) # rbind them
    of <- paste0(oPath,y,"_", genType, ".csv")
    message("Writing: ", of)
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
    message("Done")
  }
  return(yearDT) # return the last year for testing if needed
}

# Code ----
# Set start time ----
startTime <- proc.time()

# can't use drake as it won't go and get new data

gridMetaDT <- getGridData(years = years, months = months) # returns metadata
makeYearlyData(genType = "gridGen")


embeddedMetaDT <- getEmbeddedData(years = years, months = months) # returns metadata
makeYearlyData(genType = "embeddedGen")


# tests
skimr::skim(embeddedMetaDT)
skimr::skim(gridMetaDT)

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))

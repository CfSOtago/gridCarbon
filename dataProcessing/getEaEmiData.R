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
             "curl",  #for data download
             "ggplot2", # for fancy graphs
             "readr", # writing to files
             "skimr" # for skim
)
# load them
gridCarbon::loadLibraries(reqLibs)

# Parameters ----
localParams <- list() # repo level params are in gcParams

years <- seq(1999, 2020, 1) # change these to restrict or extend the file search
months <- seq(1,12,1) # change these to restrict or extend the file search

refresh <- 0 # set to 1 to try to download all files even if we already have them

localParams$gridDataLoc <- paste0(gcParams$GreenGrid, 
                                  "externalData/EA_Generation_Data/")
localParams$nonGridDataLoc <- paste0(gcParams$GreenGrid, 
                                     "externalData/EA_Embedded_Generation_Data/")

localParams$embDataURL <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation/"
localParams$gridDataURL <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/"

localParams$rawEmbDataPath <- path.expand(paste0(localParams$nonGridDataLoc, "raw/"))
localParams$processedEmbDataPath <- path.expand(paste0(localParams$nonGridDataLoc, "processed/monthly/"))
localParams$rawGridDataPath <- path.expand(paste0(localParams$gridDataLoc, "raw/"))
localParams$processedGridDataPath <- path.expand(paste0(localParams$gridDataLoc, "processed/monthly/"))

# Local functions ----


# > grid gen data  ----
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
      if(lubridate::today() < as.Date(paste0(y, "-",m,"-", "01"))){
        break # clearly there won't be any data for future dates
      }
      eafName <- paste0(y, m,"_Generation_MD.csv") # what we see on the EA EMI
      rawfName <- paste0(y,"_", m,"_gridGen.csv") # for ease of future file filtering
      print(paste0("Checking ", rawfName))
      test <- filesToDateDT[V1 %like% rawfName] # have we got it already?
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so skip
        print(paste0("Have already downloaded: ", test$V1, ", loading..."))
        # Load so we can update meta
        dt <- data.table::fread(paste0(localParams$rawGridDataPath, test)) # this loop wants the raw data
        # should probably change this so it doesn't need to run over _all_ the files every time
        dt <- cleanGridEA(dt) # clean up to a dt - fixes dateTimes etc
        print(summary(dt))
        testDT <- getGridMeta(dt) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := rawfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
      } else {
        # Get it
        rFile <- paste0(localParams$gridDataURL,eafName)
        print(paste0("We don't have or need to refresh ", rawfName))
        # use curl function to catch errors
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          dt <- data.table::fread(req$content)
          message("File downloaded successfully, saving as ", rawfName)
          data.table::fwrite(dt, paste0(localParams$rawGridDataPath, rawfName))
          cmd <- paste0("gzip -f ", "'", path.expand(paste0(localParams$rawGridDataPath, rawfName)), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          message("Compressed it")
          dt <- cleanGridEA(dt) # clean up to a dt - this does all the processing
          testDT <- getGridMeta(dt) # get metaData
          testDT <- testDT[, source := rawfName]
          metaDT <- rbind(metaDT, testDT)
          print("Converted to long form, saving it")
          processedfName <- paste0(y,"_",m,"_gridGen.csv")
          data.table::fwrite(dt, paste0(localParams$processedGridDataPath, processedfName))
          cmd <- paste0("gzip -f ", "'", path.expand(paste0(localParams$processedGridDataPath, processedfName)), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          message("Compressed it")
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
        }
      }
    }
  }
  return(metaDT)
}

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


# > embedded gen data ----
getEmbData <- function(years,months){
  message("Embedded Gen: Checking what we have already...")
  filesToDateDT <- data.table::as.data.table(list.files(localParams$rawEmbDataPath)) # get list of files already downloaded
  metaDT <- data.table::data.table() # stats collector
  
  for(y in years){
    for(mo in months){
      # construct the filename
      if(nchar(mo) == 1){
        # need to add 0 as prefix
        m <- paste0("0", mo)
      } else {
        m <- mo
      }
      if(lubridate::today() < as.Date(paste0(y, "-",m,"-", "01"))){
        break # clearly there won't be any data
      }
      eafName <- paste0(y, m,"_Embedded_generation.csv") # what we see on the EA EMI
      rawfName <- paste0(y,"_", m,"_embeddedGen.csv") # for ease of future file filtering
      print(paste0("Checking ", rawfName))
      test <- filesToDateDT[V1 %like% rawfName] # should catch .csv.gz too
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so skip
        print(paste0("Already have ", rawfName, ", loading from local..."))
        # Load so we can update meta
        #df <- readr::read_csv(paste0(localParams$rawEmbDataPath, rawfName))
        dt <- data.table::fread(paste0(localParams$rawEmbDataPath, paste0(rawfName, ".gz")))
        dt <- cleanEmbEA(dt) # clean up to a dt, TP49 & TP50 will fail to parse
        # print(summary(dt))
        testDT <- getEmbMeta(dt) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := rawfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
      } else {
        # Download it
        rFile <- paste0(localParams$embDataURL,eafName)
        print(paste0("We don't have or need to refresh ", rawfName))
        # use curl function to catch errors
        # currently this breaks if no net - we need to catch that error too!
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          #df <- readr::read_csv(req$content)
          dt <- data.table::fread(req$content)
          print("File downloaded successfully, saving it")
          data.table::fwrite(dt, paste0(localParams$rawEmbDataPath, rawfName))
          cmd <- paste0("gzip -f ", "'", path.expand(paste0(localParams$rawEmbDataPath, rawfName)), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed original file")
          dt <- cleanEmbEA(dt) # clean up to a dt
          testDT <- getEmbMeta(dt) # get metaData
          testDT <- testDT[, source := rawfName]
          metaDT <- rbind(metaDT, testDT)
          print("Converted to long form, saving it")
          lF <- paste0(localParams$processedEmbDataPath, rawfName)
          data.table::fwrite(dt, lF)
          cmd <- paste0("gzip -f ", "'", lF, "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed long form file")
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
        }
      }
    }
  }
  # write out the meta data ----
  data.table::fwrite(metaDT, paste0(localParams$processedEmbDataPath, "metaDT.csv"))
  
  # remove the temp file
  file.remove("temp.csv")
  return(metaDT)
}

# these need to vary slightly from the EA wholesale data :-(
cleanEmbEA <- function(dt){
  # cleans & returns a long form dt
  dtl <- gridCarbon::reshapeEmbeddedGenDT(dt) # make long
  dtl <- gridCarbon::setEmbeddedGenTimePeriod(dtl) # set time periods to something intelligible as rTime
  dtl[, rDate := lubridate::dmy(Trading_date)] # fix the dates so R knows what they are
  dtl[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  # there will be parse errors in the above due to TP49 & TP50
  table(dtl[is.na(rDateTime)]$Time_Period)
  return(dtl)
}

getEmbMeta <- function(dt){
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


# > processes either to yearly ----
makeYearlyData <- function(genType, years){ # parameter selects path and thus files
  if(genType == "gridGen"){
    path <- localParams$gridDataLoc
  } 
  if(genType == "embeddedGen"){
    path <- localParams$nonGridDataLoc
  }
  message("Checking what we have already in: ", paste0(path,"processed/monthly/"))
  filesToDateDT <- data.table::as.data.table(list.files(paste0(path,"processed/monthly/"))
                                             ) # get list of files already downloaded
  filesToDateDT[, year := tstrsplit(V1, split = "_", keep = 1)] #
  filesToDateDT[, fullPath := paste0(path, "processed/monthly/",V1)]
  #years <- unique(filesToDateDT[year != "metaDT.csv",]$year) # avoid the metadata file
  for(y in years){ # use the years passed in as they will be the ones we updated
    fList <- filesToDateDT[year == y, fullPath]
    yearDT <- data.table::data.table()
    yearDT <- do.call(rbind,
                      lapply(fList,
                             function(f)
                               data.table::fread(f) # auto-parses nicely
                      ) # decodes .gz on the fly
    )
    # write out the year file ----
    of <- paste0(path,"processed/yearly/",y,"_", genType, ".csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
    message("Done ", y)
  }
  return(yearDT) # return the last year for testing if needed
}

# Code ----
# > Set start time ----
startTime <- proc.time()

# Get data ----
# can't use drake as it won't go and get new data
# well, we could but...
message("Getting ", years[1], " to ", years[length(years)])

gridMetaDataDT <- getGridData(years = years, months = months) # returns metadata
embMetaDataDT <- getEmbData(years = years, months = months) # returns metadata

gridYearlyResultDT <- makeYearlyData(genType = "gridGen", years = years)
embYearlyResultDT <- makeYearlyData(genType = "embeddedGen", years = years)

gridYearlyResultDT[, rDateTime := lubridate::as_datetime(rDateTime)]
summary(gridYearlyResultDT$rDateTime)

# tests
skimr::skim(embMetaDataDT)
skimr::skim(embYearlyResultDT)


skimr::skim(gridMetaDataDT)
skimr::skim(gridYearlyResultDT)

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))

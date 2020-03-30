# Get NZ Generation data: ----
# This is:
# > EA whoelsale generation data - https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
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
             "drake", # for workflow efficiency
             "ggplot2", # for fancy graphs
             "readr", # writing to files
             "skimr" # for skim
)
# load them
gridCarbon::loadLibraries(reqLibs)

# Parameters ----
myParams <- list() # repo level params are in repoParams

years <- seq(2020, 2020, 1) # change these to restrict or extend the file search
months <- seq(1,12,1) # change these to restrict or extend the file search

local <- 1 # set to 1 for local file storage (see below)
refresh <- 0 # set to 1 to try to download all files even if we already have them

if(local){ # set data storage location.
  myParams$dPath <- "~/Data/NZ_EA_EMI/"
  myParams$embDataPath <- path.expand(paste0(myParams$dPath,"embeddedGen/"))
  myParams$gridDataPath <- path.expand(paste0(myParams$dPath,"gridGen/"))
} else {
  myParams$dPath <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/externalData/"
  myParams$embDataPath <- path.expand(paste0(myParams$dPath,"EA_Embedded_Generation_Data/"))
  myParams$gridDataPath <- path.expand(paste0(myParams$dPath,"EA_Generation_Data/"))
}
myParams$iEmbDataPath <- path.expand(paste0(myParams$embDataPath, "raw/"))
myParams$oEmbDataPath <- path.expand(paste0(myParams$embDataPath, "/processed/monthly/"))
myParams$iGridDataPath <- path.expand(paste0(myParams$gridDataPath, "raw/"))
myParams$oGridDataPath <- path.expand(paste0(myParams$gridDataPath, "/processed/monthly/"))

myParams$embDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation/"
myParams$gridDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/"



# Local functions ----
cleanGridEA <- function(df){
  # takes a df, cleans & returns a dt
  dt <- data.table::as.data.table(df) # make dt
  dt <- gridCarbon::reshapeEAGenDT(dt) # make long
  dt <- gridCarbon::setEAGenTimePeriod(dt) # set time periods to something intelligible as rTime
  dt <- dt[, rDate := as.Date(Trading_date)] # fix the dates so R knows what they are
  dt <- dt[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(dt)
}
# these need to vary slightly from the EA wholesale data :-(
cleanEmbEA <- function(df){
  # takes a df, cleans & returns a dt
  dt <- data.table::as.data.table(df) # make dt
  dt <- gridCarbon::reshapeEAEmbeddedGenDT(dt) # make long
  dt <- gridCarbon::setEAGenTimePeriod(dt) # set time periods to something intelligible as rTime
  dt <- dt[, rDate := lubridate::dmy(Trading_date)] # fix the dates so R knows what they are
  dt <- dt[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(dt)
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


getEmbData <- function(years,months){
  message("Checking what we have already...")
  filesToDateDT <- data.table::as.data.table(list.files(myParams$iEmbDataPath)) # get list of files already downloaded
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
      lfName <- paste0(y,"_", m,"_Embedded_generation.csv") # for ease of future file filtering
      rfName <- paste0(y, m,"_Embedded_generation.csv")
      print(paste0("Checking ", lfName))
      test <- filesToDateDT[V1 %like% lfName] # should catch .csv.gz too
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so skip
        print(paste0("Already have ", lfName, ", loading from local..."))
        # Load so we can update meta
        #df <- readr::read_csv(paste0(myParams$iEmbDataPath, lfName))
        dt <- data.table::fread(paste0(myParams$iEmbDataPath, lfName))
        dt <- cleanEmbEA(dt) # clean up to a dt
        # print(summary(dt))
        testDT <- getEmbMeta(dt) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := lfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
      } else {
        # Get it
        rFile <- paste0(myParams$embDataLoc,rfName)
        print(paste0("We don't have or need to refresh ", lfName))
        # use curl function to catch errors
        # currently this breaks if no net - we need to catch that error too!
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          #df <- readr::read_csv(req$content)
          dt <- data.table::fread(req$content)
          print("File downloaded successfully, saving it")
          data.table::fwrite(dt, paste0(myParams$iEmbDataPath, lfName))
          dt <- cleanEmbEA(dt) # clean up to a dt
          testDT <- getEmbMeta(dt) # get metaData
          testDT <- testDT[, source := rfName]
          metaDT <- rbind(metaDT, testDT)
          print("Converted to long form, saving it")
          lfName <- paste0(y,"_",m,"_Embedded_generation_long.csv")
          data.table::fwrite(dt, paste0(myParams$oEmbDataPath, lfName))
          cmd <- paste0("gzip -f ", "'", path.expand(paste0(myParams$oEmbDataPath, lfName)), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed it")
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
        }
      }
    }
  }
  # write out the meta data ----
  data.table::fwrite(metaDT, paste0(myParams$oEmbDataPath, "metaDT.csv"))
  
  # remove the temp file
  file.remove("temp.csv")
  return(metaDT)
}

getGridData <- function(years, months){
  message("Checking what we have already...")
  filesToDateDT <- data.table::as.data.table(list.files(myParams$i)) # get list of files already downloaded
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
      lfName <- paste0(y,"_", m,"_Generation_MD.csv") # for ease of future file filtering
      rfName <- paste0(y, m,"_Generation_MD.csv")
      print(paste0("Checking ", lfName))
      test <- filesToDateDT[V1 %like% lfName] #should catch .csv.gz too
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so skip
        print(paste0("Already have ", lfName, ", loading from local..."))
        # Load so we can update meta
        dt <- data.table::fread(paste0(myParams$iGridDataPath, lfName)) # this loop wants the raw data
        # should probably change this so it doesn't need to run over _all_ the files every time
        dt <- cleanGridEA(dt) # clean up to a dt - fixes dateTimes etc
        print(summary(dt))
        testDT <- getGridMeta(dt) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := lfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
      } else {
        # Get it
        rFile <- paste0(myParams$gridDataLoc,rfName)
        print(paste0("We don't have or need to refresh ", lfName))
        # use curl function to catch errors
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          dt <- data.table::fread(req$content)
          print("File downloaded successfully, saving it")
          data.table::fwrite(dt, paste0(myParams$iGridDataPath, lfName))
          dt <- cleanGridEA(dt) # clean up to a dt - this does all the processing
          testDT <- getGridMeta(dt) # get metaData
          testDT <- testDT[, source := rfName]
          metaDT <- rbind(metaDT, testDT)
          print("Converted to long form, saving it")
          lfName <- paste0(y,"_",m,"_Generation_MD_long.csv")
          data.table::fwrite(dt, paste0(myParams$oGridDataPath, lfName))
          cmd <- paste0("gzip -f ", "'", path.expand(paste0(myParams$oGridDataPath, lfName)), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed it")
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
        }
      }
    }
  }
  return(metaDT)
}

makeYearlyData <- function(which){ # parameter selects path and thus files
  path <- paste0(myParams$dPath,which, "/processed/monthly/")
  message("Checking what we have already in: ", path)
  filesToDateDT <- data.table::as.data.table(list.files(path)) # get list of files already downloaded
  filesToDateDT[, year := tstrsplit(V1, split = "_", keep = 1)]
  filesToDateDT[, fullPath := paste0(path,V1)]
  years <- unique(filesToDateDT[year != "metaDT.csv",]$year) # avoid the metadata file
  for(y in years){
    fList <- filesToDateDT[year == y, fullPath]
    yearDT <- do.call(rbind,
                      lapply(fList,
                             function(f)
                               data.table::fread(f) # auto-parses nicely
                      ) # decodes .gz on the fly
    )
    # write out the year file ----
    of <- paste0(myParams$dPath, which, "/processed/yearly/",y,"_", which, ".csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd))
  }
  return(yearDT) # return the last year for testing if needed
}

# Parameters ----
# Set start time ----
startTime <- proc.time()

# drake plan ----
plan <- drake::drake_plan(
  embMetaData = getEmbData(years = years, months = months), # returns metadata
  gridMetaData = getGridData(years = years, months = months), # returns metadata
  embYearlyResult = makeYearlyData(which = "embeddedGen"),
  gridYearlyResult = makeYearlyData(which = "gridGen")
)


# test it ----
plan

config <- drake_config(plan)

# do it ----
make(plan)

# tests
skimr::skim(drake::readd(embMetaData))
skimr::skim(drake::readd(gridMetaData))
skimr::skim(drake::readd(embYearlyResult))
skimr::skim(drake::readd(gridYearlyResult))

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))

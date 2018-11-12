# Get NZ EA Wholesale Generation data ----
# Not a function
# Gets or refreshes the EA wholesale generation data from https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
# Saves them as-is and also processes to long form & saves as .csv.gz

# Housekeeping ----
rm(list=ls(all=TRUE)) # remove all objects from workspace


# Load libraries ----
library(GREENGrid) #load this first - you will need to download & install it locally from this repo

# Packages needed in this .Rmd file ----
reqLibs <- c("data.table", # data munching
             "readr", # writing to files
             "curl",  #for data download
             "ggplot2", # for fancy graphs
             "skimr" # for skim
)
# load them
loadLibraries(reqLibs)

# Parameters ----
GREENGrid::setup()

local <- 0 # set to 1 for local file storage (see below)
refresh <- 0 # set to 1 to try to download all files even if we have them

if(local){ # set data storage location
  lDataLoc <- path.expand("~/Data/NZGreenGrid/safe/ea/")
} else {
  lDataLoc <- path.expand("/Volumes/hum-csafe/Research Projects/GREEN Grid/externalData/EA_Generation_Data/")
}

rDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/"
years <- seq(1997, 2018, 1) # change these to restrict or extend the file search
months <- seq(1,12,1) # change these to restrict or extend the file search

# Local functions ----
cleanEA <- function(df){
  # takes a df, cleans & returns a dt
  dt <- data.table::as.data.table(df) # make dt
  dt <- reshapeEAGenDT(dt) # make long
  dt <- setEAGenTimePeriod(dt) # set time periods to something intelligible as rTime
  dt <- dt[, rDate := as.Date(Trading_date)] # fix the dates so R knows what they are
  dt <- dt[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(dt)
}

getMeta <- function(dt){
  dt <- dt[, month := lubridate::month(rDate)]
  dt <- dt[,year := lubridate::year(rDate)]
  testDT <- dt[, .(nObs = .N,
                   sumkWh = sum(as.numeric(kWh), na.rm = TRUE),
                   nFuels = uniqueN(Fuel_Code),
                   dateFrom = min(rDate),
                   dateTo = max(rDate),
                   nDays = uniqueN(rDate)), keyby = .(month,
                                                      year)]
  return(testDT)
}

# Code ----
# Set start time ----
startTime <- proc.time()

filesToDateDT <- data.table::as.data.table(list.files(lDataLoc)) # get list of files already downloaded

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
    test <- filesToDateDT[V1 %like% lfName] # should catch .csv.gz too
    if(nrow(test) > 0 & refresh == 0){
      # Already got it & we don't want to refresh so skip
      print(paste0("Already have ", lfName, ", loading from local..."))
      # Load so we can update meta
      df <- readr::read_csv(paste0(lDataLoc, lfName))
      dt <- cleanEA(df) # clean up to a dt
      print(summary(dt))
      testDT <- getMeta(dt) # get metaData
      print(head(testDT))
      testDT <- testDT[, source := lfName]
      metaDT <- rbind(metaDT, testDT)
      testDT <- NULL
    } else {
      # Get it
      rFile <- paste0(rDataLoc,rfName)
      print(paste0("We don't have or need to refresh ", lfName))
      # use curl function to catch errors
      print(paste0("Trying to download ", rFile))
      req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
      if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
        df <- readr::read_csv(req$content)
        print("File downloaded successfully, saving it")
        data.table::fwrite(df, paste0(lDataLoc, lfName))
        dt <- cleanEA(df) # clean up to a dt
        testDT <- getMeta(dt) # get metaData
        testDT <- testDT[, source := rfName]
        metaDT <- rbind(metaDT, testDT)
        print("Converted to long form, saving it")
        lfName <- paste0(y,"_",m,"_Generation_MD_long.csv")
        data.table::fwrite(dt, paste0(lDataLoc, lfName))
        cmd <- paste0("gzip -f ", "'", path.expand(paste0(lDataLoc, lfName)), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
        try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
        print("Compressed it")
      } else {
        print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
      }
    }
  }
}

# load the meta data as a shortcut for testing ----
# metaDT <- data.table::fread(paste0(lDataLoc, "metaDT.csv"))


figCaption <- paste0("EA Wholesale Generation data ", min(metaDT$dateFrom), " - ", max(metaDT$dateTo),
                     "\nData: ", lDataLoc)

makeCheckPlot <- function(dt,fillVar){
  # makes a tile plot of month by year and fills colour by the indicator chosen
  # serves as a data quality check
  myPlot <- ggplot2::ggplot(dt, aes(x = as.factor(year), y = as.factor(month), fill = get(fillVar))) +
    geom_tile() +
    labs(x = "Year", y = "Month",
         caption = figCaption,
         title = paste0("Data check chart:", eval(fillVar))
         )

  ggplot2::ggsave(lDataLoc,paste0(eval(fillVar),"Plot.pdf"))

  return(myPlot)
}

makeCheckPlot(metaDT, "nDays")
makeCheckPlot(metaDT, "nObs")
makeCheckPlot(metaDT, "sumkWh")
makeCheckPlot(metaDT, "nFuels")

# write out the meta data ----
data.table::fwrite(metaDT, paste0(lDataLoc, "metaDT.csv"))

# remove the temp file
file.remove("temp.csv")

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))

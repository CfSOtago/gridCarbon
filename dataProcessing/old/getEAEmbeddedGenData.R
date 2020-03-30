# Get NZ EA Embedded Generation data ----
# Not a function
# Gets or refreshes the EA wholesale generation data from https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation
# Saves them as-is and also processes to long form & saves as .csv.gz

# Load libraries ----
library(gridCarbon) # load this first - you will need to download & install it locally from this repo

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

local <- 1 # set to 1 for local file storage (see below)
refresh <- 0 # set to 1 to try to download all files even if we have them

if(local){ # set data storage location
  dataPath <- path.expand("~/Data/NZ_EA_EMI/embeddedGen/")
} else {
  dataPath <- path.expand("/Volumes/hum-csafe/Research Projects/GREEN Grid/externalData/EA_Embedded_Generation_Data/")
}
iDataPath <- path.expand(paste0(dataPath, "raw/"))
oDataPath <- path.expand(paste0(dataPath, "/processed/monthly/"))

rDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation/"
years <- seq(2018, 2020, 1) # change these to restrict or extend the file search
months <- seq(1,12,1) # change these to restrict or extend the file search

# Local functions ----
# these need to vary slightly from the EA wholesale data :-(
cleanEA <- function(df){
  # takes a df, cleans & returns a dt
  dt <- data.table::as.data.table(df) # make dt
  dt <- reshapeEAEmbeddedGenDT(dt) # make long
  dt <- setEAGenTimePeriod(dt) # set time periods to something intelligible as rTime
  dt <- dt[, rDate := lubridate::dmy(Trading_date)] # fix the dates so R knows what they are
  dt <- dt[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(dt)
}

getMeta <- function(dt){
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

makeCheckPlot <- function(dt,fillVar, myTitle, myKey){
  # makes a tile plot of month by year and fills colour by the indicator chosen
  # serves as a data quality check
  myPlot <- ggplot2::ggplot(dt, aes(x = as.factor(year), y = as.factor(month), alpha = get(fillVar))) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_alpha_continuous(name = myKey) +
    labs(x = "Year", y = "Month",
         caption = paste0(figCaption), # add parameterised caption
         title = paste0("Check plot: ", myTitle)
    ) +
    facet_wrap(~ . Flow_Direction)
  
  ggplot2::ggsave(paste0(dataPath,"checkPlots/EA_embeddedGen_", eval(fillVar), "_Plot.png"),myPlot)
  return(myPlot)
}

getData <- function(years,months){
  message("Checking what we have already...")
  filesToDateDT <- data.table::as.data.table(list.files(iDataPath)) # get list of files already downloaded
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
        df <- readr::read_csv(paste0(iDataPath, lfName))
        dt <- cleanEA(df) # clean up to a dt
        # print(summary(dt))
        testDT <- getMeta(dt) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := lfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
      } else {
        # Get it
        rFile <- paste0(rDataLoc,rfName)
        print(paste0("We don't have or need to refresh ", lfName))
        # use curl function to catch errors
        # currently this breaks if no net - we need to catch that error too!
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          df <- readr::read_csv(req$content)
          print("File downloaded successfully, saving it")
          data.table::fwrite(df, paste0(iDataPath, lfName))
          dt <- cleanEA(df) # clean up to a dt
          testDT <- getMeta(dt) # get metaData
          testDT <- testDT[, source := rfName]
          metaDT <- rbind(metaDT, testDT)
          print("Converted to long form, saving it")
          lfName <- paste0(y,"_",m,"_Embedded_generation_long.csv")
          data.table::fwrite(dt, paste0(oDataPath, lfName))
          cmd <- paste0("gzip -f ", "'", path.expand(paste0(oDataPath, lfName)), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed it")
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
        }
      }
    }
  }
  # write out the meta data ----
  data.table::fwrite(metaDT, paste0(oDataPath, "metaDT.csv"))
  
  # remove the temp file
  file.remove("temp.csv")
  return(metaDT)
}

makePlots <- function(){
  drake::readd(metaData) # bring it back
  figCaption <- paste0("EA Embedded Generation data ", min(metaDT$dateFrom), " - ", max(metaDT$dateTo),
                       "\nData: ", iDataPath)
  makeCheckPlot(metaDT, "nDays", myTitle = "N days of data by month and year", myKey = "N days")
  makeCheckPlot(metaDT, "nObs", "N observations per month", "N obs")
  makeCheckPlot(metaDT, "sumMWh", "Sum of MWh across all sites per month", "MWh")
  makeCheckPlot(metaDT, "meanMWh", "Mean of half-hourly MWh per site by month", "MWh")
  makeCheckPlot(metaDT, "ORON", "N different ORON codes by month and year", "N ORON")
}
# Parameters ----
# Set start time ----
startTime <- proc.time()

# drake plan ----
plan <- drake::drake_plan(
  metaData = getData(years = years, months = months), # returns metadata
  plots = makePlots()
)


# test it ----
plan

config <- drake_config(plan)
vis_drake_graph(config)

# do it ----
make(plan)


# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))

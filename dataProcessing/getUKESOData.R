# Get UK Generation & carbon intensity data: ----
# This is:
# https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix
# http://data.nationalgrideso.com/backend/dataset/88313ae5-94e4-4ddc-a790-593554d8c6b9/resource/7b41ea4d-cada-491e-8ad6-7b62f6a63193/download/df_fuel_ckan.csv
# - gets or refreshes the data
# - saves it as-is 
# - processes to long form & saves as .csv.gz
# - converts to yearly files

# UK ESO publishes this as 1 big file but there are API options
# for smaller file refreshes

# Load libraries ----
library(gridCarbon) # load this first - you will need to download & build it locally from this repo

# Packages needed in this .Rmd file ----
reqLibs <- c("data.table", # data munching
             "drake", # what's done stays done
             "here", # here
             "skimr" # for skim
)
# load them
gridCarbon::loadLibraries(reqLibs)

# Parameters ----
source(paste0(here::here(), "/.Rprofile")) # gcParams

localParams <- list() # repo level params are in gcParams
localParams$url <- "http://data.nationalgrideso.com/backend/dataset/88313ae5-94e4-4ddc-a790-593554d8c6b9/resource/7b41ea4d-cada-491e-8ad6-7b62f6a63193/download/df_fuel_ckan.csv"
localParams$ukEsoDataLoc <- paste0(gcParams$GreenGrid, 
                                   "externalData/ukeso/eso_generation/")

localParams$rawUkEsoDataPath <- path.expand(paste0(localParams$ukEsoDataLoc, "raw/"))
localParams$processedUkEsoDataPath <- path.expand(paste0(localParams$ukEsoDataLoc, "processed/"))

# Local functions ----

# test url
h <- curlGetHeaders(localParams$url)
# get data
getGridESO <- function(f){
  # url has to be explicit for drake to monitor it
  # breaks
  #dt <- data.table::fread(file_in("http://data.nationalgrideso.com/backend/dataset/88313ae5-94e4-4ddc-a790-593554d8c6b9/resource/7b41ea4d-cada-491e-8ad6-7b62f6a63193/download/df_fuel_ckan.csv"))
  dt <- data.table::fread(f)
  return(dt)
}

# data cleaning - used below
cleanGridESO <- function(dt){
  # cleans & returns a dt
  dt[, rDateTime := lubridate::ymd_hms(DATETIME)] # set full dateTime
  # anything else?
  return(dt)
}

makeYearlyData <- function(dt){
  dt[, year := lubridate::year(rDateTime)]
  t <- dt[, .(nObs = .N), keyby = .(year)]
  years <- t[, year]
  for(y in years){
    yearDT <- dt[year == y]
    # write out the year file ----
    of <- paste0(localParams$processedUkEsoDataPath,"yearly/",y,"_ukEsoGen.csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
    message("Done ", y)
  }
  return(yearDT) # return the last year for testing if needed
}

saveGridESO <- function(dt){
  of <- paste0(localParams$rawUkEsoDataPath,"latest_ukEsoGen.csv")
  data.table::fwrite(dt, of)
  cmd <- paste0("gzip -f ", of)
  message("Gzip file: ", of)
  try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
  message("Done ")
}

# Code ----
# Set start time ----
startTime <- proc.time()

# can't use drake as it won't go and get new data
# drake plan - will only get new data if it has changed (it checks)
# drake plan ----
plan <- drake::drake_plan(
  esoData = getGridESO(localParams$url), # need to make this only happen if updated
  cleanData = cleanGridESO(esoData),
  saveData = saveGridESO(cleanData),
  makeYearlyData(cleanData)
)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

gridGenDT <- drake::readd(cleanData)

# tests
skimr::skim(gridGenDT)

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))
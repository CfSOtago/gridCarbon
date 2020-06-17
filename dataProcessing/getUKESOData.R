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
gridCarbon::setup() # loads env.R

# Packages needed in this .Rmd file ----
reqLibs <- c("data.table", # data munching
             "drake", # what's done stays done
             "here", # here
             "skimr" # for skim
)
# load them
gridCarbon::loadLibraries(reqLibs)

# Parameters ----
#source(paste0(here::here(), "/env.R")) # gcParams - just in case not already loaded by grifCarbon (should be)

localParams <- list() # repo level params are in gcParams
localParams$url <- "http://data.nationalgrideso.com/backend/dataset/88313ae5-94e4-4ddc-a790-593554d8c6b9/resource/7b41ea4d-cada-491e-8ad6-7b62f6a63193/download/df_fuel_ckan.csv"

localParams$rawUkEsoDataPath <- path.expand(paste0(gcParams$ukData, "/raw/"))
localParams$processedUkEsoDataPath <- path.expand(paste0(gcParams$ukData, "/processed/"))

update <- "yep" # doesn't matter what this is but to force an update, edit it :-)

# Local functions ----

# test url
h <- curlGetHeaders(localParams$url)

# Code ----
# Set start time ----
startTime <- proc.time()

# can't use drake as it won't go and get new data
# drake plan - will only get new data if it has changed (it checks)
# drake plan ----
plan <- drake::drake_plan(
  esoData = gridCarbon::getUkGridESO(localParams$url, update), # returns data as data.table
  cleanData = gridCarbon::cleanUkGridESO(esoData), # returns clean data.table
  saveResult = gridCarbon::saveUkGridESO(cleanData, localParams$rawUkEsoDataPath), # doesn't return anything
  latestYear = gridCarbon::makeUkGridESOYearlyData(cleanData,localParams$processedUkEsoDataPath) # returns the most recent year if needed
)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

gridGenDT <- drake::readd(cleanData)

# tests
skimr::skim(gridGenDT)

# latest dates:
message("We now have data up to: ", max(gridGenDT$rDateTime))

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))
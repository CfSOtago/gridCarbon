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
update <- "yes" # doesn't matter what this is but to force an update, edit it :-)

#source(paste0(here::here(), "/env.R")) # gcParams - just in case not already loaded by grifCarbon (should be)

localParams <- list() # repo level params are in gcParams
localParams$gridUrl <- "http://data.nationalgrideso.com/backend/dataset/88313ae5-94e4-4ddc-a790-593554d8c6b9/resource/7b41ea4d-cada-491e-8ad6-7b62f6a63193/download/df_fuel_ckan.csv"

localParams$rawUkGridGenPath <- path.expand(paste0(gcParams$ukData, "/gridGen/raw/"))
localParams$processedUkGridGenPath <- path.expand(paste0(gcParams$ukData, "/gridGen/processed/"))

# "https://data.nationalgrideso.com/demand/daily-demand-update/r/demand_data_update_daily"
# history before 1/5/2020 is at https://demandforecast.nationalgrid.com/efs_demand_forecast/faces/DataExplorer;jsessionid=Wi7TQtk8s4h5K5Z-6uSHOTeTMzAEVBy8W8RvO9fyBax8DALJ68c_!-947165471
# why?
localParams$embeddedUrl <- "https://demandforecast.nationalgrid.com/efs_demand_forecast/demandupdatedownload"
localParams$rawUkEmbeddedGenPath <- path.expand(paste0(gcParams$ukData, "/embeddedGen/raw/"))
localParams$processedUkEmbeddedGenPath <- path.expand(paste0(gcParams$ukData, "/embeddedGen/processed/"))


# Local functions ----

# test urls
hGrid <- curlGetHeaders(localParams$gridUrl)
hEmbedded <- curlGetHeaders(localParams$embeddedUrl)

# Code ----
# Set start time ----
startTime <- proc.time()

# can't use drake as it won't go and get new data
# drake plan - will only get new data if it has changed (it checks)
# drake plan ----
plan <- drake::drake_plan(
  # grid
  gridGenData = gridCarbon::getUkGridESO(localParams$gridUrl, 
                                         update), # returns data as data.table
  cleanGridGenData = gridCarbon::cleanUkGridESO(gridGenData,
                                                path = gcParams$ukData), # returns clean data.table
  saveResultGrid = gridCarbon::saveUkGridESO(cleanGridGenData, 
                                             path = gcParams$ukData), # doesn't return anything
  # embedded
  # if this breaks check the url is still current
  embeddedGenData = gridCarbon::getUkEmbeddedESO(localParams$embeddedUrl, 
                                                 update), # returns latest data update as data.table
  cleanEmbeddedGenData = gridCarbon::cleanUkEmbeddedESO(embeddedGenData, 
                                                        path = gcParams$ukData), 
  saveResultEmbedded = gridCarbon::saveUkEmbeddedESO(cleanEmbeddedGenData,
                                                     path = gcParams$ukData) # adds the latest update to what we already have (removes duplicates obvs)
)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

gridGenDT <- drake::readd(cleanGridGenData)
embeddedGenDT <- drake::readd(cleanEmbeddedGenData)
# tests
# skimr::skim(gridGenDT)
# skimr::skim(embeddedGenDT)
gridGenDT[, rDateTime := lubridate::as_datetime(rDateTime)]
gridGenDT[, rDateTimeUTC := lubridate::with_tz(rDateTime, tzone = "UTC")]
setkey(gridGenDT, rDateTimeUTC)
embeddedGenDT[, rDateTime := lubridate::as_datetime(rDateTime)]
embeddedGenDT[, rDateTimeUTC := lubridate::with_tz(rDateTime, tzone = "UTC")]
setkey(embeddedGenDT, rDateTimeUTC)
allGenDT <- embeddedGenDT[gridGenDT] # keeps all of gridGen (continuous & longer)

# latest dates:
message("We now have gridGen data from, " , min(gridGenDT$rDateTime), 
        " to: ", max(gridGenDT$rDateTime))
message("We now have embeddedGen data from, " , min(embeddedGenDT$rDateTime), 
        " to: ", max(embeddedGenDT$rDateTime))

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))
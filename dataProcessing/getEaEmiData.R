# Get NZ Generation data: ----
# This is:
# > EA Wholesale generation data - https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
# Units are kilowatt hours (kWh).
# > EA Embedded Generation data - https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation

# - gets or refreshes the EA generation data
# - saves them as-is 
# - processes to long form & saves as .csv.gz
# - converts to yearly files

# Load libraries ----
library(gridCarbon) # load this first - you will need to download & build it locally from this repo
gridCarbon::setup()

# Packages needed in this .Rmd file ----
reqLibs <- c("data.table", # data munching
             "curl",  #for data download
             "skimr" # for skim
)
# load them
gridCarbon::loadLibraries(reqLibs)

# Parameters ----
localParams <- list() # repo level params are in gcParams

years <- seq(2015, 2020, 1) # change these to restrict or extend the file search
months <- seq(1,12,1) # change these to restrict or extend the file search

refresh <- 0 # set to 1 to try to download all files even if we already have them

localParams$embDataURL <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation/"
localParams$gridDataURL <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/"

# Local functions ----
# should all be in R/ now

# Code ----
# > Set start time ----
startTime <- proc.time()

# Get data ----
# can't use drake as it won't go and get new data
# well, we could but...
message("Getting ", years[1], " to ", years[length(years)])

gridMetaDataDT <- gridCarbon::getNZGridEA(years = years, 
                                          months = months, 
                                          path = gcParams$nzGridDataLoc) # returns metadata

embMetaDataDT <- gridCarbon::getNZEmbData(years = years, 
                                          months = months,
                                          path = gcParams$nzNonGridDataLoc) # returns metadata

gridCarbon::makeNZYearlyData(genType = "gridGen", 
                                                   years = years,
                                                   path = gcParams$nzGridDataLoc)
gridCarbon::makeNZYearlyData(genType = "embeddedGen", 
                                                   years = years,
                                                   path = gcParams$nzNonGridDataLoc)

# tests
skimr::skim(embMetaDataDT)
skimr::skim(gridMetaDataDT)


# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]


print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))

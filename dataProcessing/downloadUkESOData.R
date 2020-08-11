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


# "https://data.nationalgrideso.com/demand/daily-demand-update/r/demand_data_update_daily"
# history before 1/5/2020 is at https://demandforecast.nationalgrid.com/efs_demand_forecast/faces/DataExplorer;jsessionid=Wi7TQtk8s4h5K5Z-6uSHOTeTMzAEVBy8W8RvO9fyBax8DALJ68c_!-947165471
# why?
localParams$embeddedUrl <- "https://demandforecast.nationalgrid.com/efs_demand_forecast/demandupdatedownload"


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
  cleanGridGenData = gridCarbon::cleanUkGridESO(gridGenData), # returns clean data.table
  saveResultGrid = gridCarbon::saveUkGridESO(cleanGridGenData, 
                                             path = repoParams$ukGridDataLoc), # doesn't return anything
  # embedded
  # if this breaks check the url is still current
  embeddedGenData = gridCarbon::getUkEmbeddedESO(localParams$embeddedUrl, 
                                                 update), # returns latest data update as data.table
  cleanEmbeddedGenData = gridCarbon::cleanUkEmbeddedESO(embeddedGenData, 
                                                        path = repoParams$ukNonGridDataLoc), # needs a path as it uses newly updated data
  saveResultEmbedded = gridCarbon::saveUkEmbeddedESO(cleanEmbeddedGenData,
                                                     path = repoParams$ukNonGridDataLoc) # adds the latest update to what we already have (removes duplicates obvs)
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

# check for gaps
library(ggplot2)
pEmbeddedWind <- ggplot2::ggplot(embeddedGenDT, aes(x = rDate, y = hms, fill = EMBEDDED_WIND_GENERATION/1000)) +
  geom_tile() +
  theme(legend.position = "bottom")
ggplot2::ggsave(paste0(repoParams$ukData, "latestPlots/", "ukEmbeddedWind_tile.png"), pEmbeddedWind, width = 6, units = "in")

pEmbeddedSolar <- ggplot2::ggplot(embeddedGenDT, aes(x = rDate, y = hms, fill = EMBEDDED_SOLAR_GENERATION/1000)) +
  geom_tile() +
  theme(legend.position = "bottom")
ggplot2::ggsave(paste0(repoParams$ukData, "latestPlots/", "ukEmbeddedSolar_tile.png"), pEmbeddedSolar, width = 6, units = "in")

gridGenDT[, rDate := as.Date(rDateTimeUTC)]
gridGenDT[, hms := hms::as_hms(rDateTimeUTC)]
pGrid <- ggplot2::ggplot(gridGenDT, aes(x = rDate, y = hms, fill = GENERATION/1000)) +
  geom_tile() +
  theme(legend.position = "bottom")
ggplot2::ggsave(paste0(repoParams$ukData, "latestPlots/", "ukGridGen_tile.png"), pGrid,width = 6, units = "in")

embSolar <- embeddedGenDT[,.(rDateTimeUTC, EMBEDDED_SOLAR_GENERATION)]
embSolar[, MW := EMBEDDED_SOLAR_GENERATION]
embSolar[, source := "Embedded solar"]
embWind <- embeddedGenDT[,.(rDateTimeUTC, EMBEDDED_WIND_GENERATION)]
embWind[, MW := EMBEDDED_WIND_GENERATION]
embWind[, source := "Embedded wind"]

gridGenDT[, MW := GENERATION]
gridGenDT[, source := "All grid"]

allGenDT <- rbind(embWind[,.(rDateTimeUTC, MW, source)],
                  embSolar[,.(rDateTimeUTC, MW, source)],
                  gridGenDT[,.(rDateTimeUTC, MW, source)]) 
allGenDT[, hms := hms::as_hms(rDateTimeUTC)]
allGenDT[, rDate := lubridate::date(rDateTimeUTC)]

pAllGen <- ggplot2::ggplot(allGenDT, aes(x = rDate, y = hms, fill = MW/1000)) +
  geom_tile() +
  facet_grid(source ~ .) +
  theme(legend.position = "bottom")
ggplot2::ggsave(paste0(repoParams$ukData, "latestPlots", "ukAllGen_tile.png"), pAllGen, width = 6, units = "in")


# latest dates feedback - save it out for ease of reference
gridMeta <- paste0("We now have gridGen data from, " , min(gridGenDT$rDateTimeUTC), 
        " to: ", max(gridGenDT$rDateTimeUTC))
embeddedMeta <- paste0("We now have embeddedGen data from, " , min(embeddedGenDT$rDateTimeUTC), 
        " to: ", max(embeddedGenDT$rDateTimeUTC))

f <- file(paste0(repoParams$ukData,"latest_gridMeta.txt"))
writeLines(gridMeta, f)
close(f)

f <- file(paste0(repoParams$ukData,"latest_embeddedMeta.txt"))
writeLines(embeddedMeta, f)
close(f)

# Finish off ----

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,2), " minutes using ",
             R.version.string, " running on ", R.version$platform))
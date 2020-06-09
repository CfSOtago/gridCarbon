# Keep all the main frequently re-used package parameters in here
# library(gridCarbon) causes .Rprofile to be loaded whin turn sources this file
# But you can source it yourself as well if you forgot to build gridCarbon

# Package we need in this file
library(here)

# Package parameters ----

gcParams <- list() # params holder as a list. Sooooo much easier with tab complete

# Data ----
# attempt to guess the platform & user
gcParams$info <- Sys.info()
gcParams$sysname <- gcParams$info[[1]]
gcParams$nodename <- gcParams$info[[4]]
gcParams$login <- gcParams$info[[6]]
gcParams$user <- gcParams$info[[7]]

# > Set data path ----
if((gcParams$user == "dataknut" | gcParams$user == "carsten" ) & 
   gcParams$sysname == "Linux"){ # Otago CS RStudio server
  
  gcParams$GreenGrid <- path.expand("~/greenGridData/")
  gcParams$GreenGridData <- path.expand("~/greenGridData/cleanData/safe/")
  gcParams$censusData <- path.expand("~/greenGridData/externalData/nzCensus/") # fix for your platform
  gcParams$gxpData <- path.expand("~/greenGridData/externalData/EA_GXP_Data/") # fix for your platform
  gcParams$nzGridDataLoc <- paste0(gcParams$GreenGrid, 
                                 "externalData/EA_Generation_Data/")
  gcParams$nzNonGridDataLoc <- paste0(gcParams$GreenGrid, 
                                    "externalData/EA_Embedded_Generation_Data/")
  gcParams$nzData <- gcParams$GreenGridData
  gcParams$ukData <- path.expand("not set")
}
if(gcParams$user == "ben" & gcParams$sysname == "Darwin"){
  # Ben's laptop
  gcParams$GreenGrid <- path.expand("~/Data/NZ_GREENGrid/")
  gcParams$GreenGridData <- path.expand("~/Data/NZ_GREENGrid/safe/")
  gcParams$censusData <- path.expand("~/Data/NZ_Census/") # fix for your platform
  gcParams$gxpData <- path.expand("~/Data/NZ_EA_EMI/gxp/") # fix for your platform
  gcParams$nzGridDataLoc <- path.expand("~/Data/NZ_EA_EMI/EA_Generation_Data/")
  gcParams$nzNonGridDataLoc <- path.expand("~/Data/NZ_EA_EMI/EA_Embedded_Generation_Data/")
  gcParams$nzData <- gcParams$GreenGridData
}
if(gcParams$user == "carsten.dortans" & gcParams$sysname == "Darwin"){
  # Carsten's laptop
  gcParams$GreenGridData <- path.expand("/Volumes/hum-csafe/Research Projects/GREEN Grid/cleanData/safe/")
  gcParams$nzGridDataLoc <- path.expand(paste0(gcParams$GreenGridData, 
                                               "/EA_Generation_Data/"))
  gcParams$nzNonGridDataLoc <- path.expand(paste0(gcParams$GreenGridData, 
                                                  "/EA_Embedded_Generation_Data/"))
  gcParams$nzData <- gcParams$GreenGridData
}
if(gcParams$user == "ba1e12" & gcParams$sysname == "Linux" & gcParams$nodename == "srv02405"){
  # UoS RStudio server
  gcParams$ukData <- path.expand("/mnt/SERG_data/UK_National_Grid")
  gcParams$ukGridDataLoc <- path.expand(paste0(gcParams$ukData, "/EA_Generation_Data/"))
  gcParams$ukNonGridDataLoc <- path.expand(paste0(gcParams$ukData, "EA_Embedded_Generation_Data/"))
  gcParams$nzData <- path.expand("/mnt/SERG_data/NZ_EA_EMI")
  gcParams$nzGridDataLoc <- path.expand(paste0(gcParams$nzData, "/EA_Generation_Data/"))
  gcParams$nzNonGridDataLoc <- path.expand(paste0(gcParams$nzData, "/EA_Embedded_Generation_Data/"))
}

# > Misc data ----
gcParams$bytesToMb <- 0.000001

# > lockdown dates ----
# set values for annotations
gcParams$UKlockDownStartDate <- as.Date("2020-03-24")
gcParams$UKlockDownStartDateTime <- lubridate::as_datetime("2020-03-23 23:59:00")
gcParams$UKlockDownRelaxDate_1 <- as.Date("2020-05-11") # BJ speech 10th May
gcParams$UKlockDownRelaxDateTime_1 <- lubridate::as_datetime("2020-05-10 23:59:00")
gcParams$UKlockDownEndDate <- lubridate::today() # for plots
gcParams$UKlockDownEndDateTime <- lubridate::now()

gcParams$NZLevel4StartDate <- as.Date("2020-03-26")
gcParams$NZLevel4StartDateTime <- lubridate::as_datetime("2020-03-25 23:59:00")
gcParams$NZLevel3StartDate <- as.Date("2020-04-27")
gcParams$NZLevel3StartDateTime <- lubridate::as_datetime("2020-04-27 23:59:00")
gcParams$NZLevel2StartDate <- as.Date("2020-05-13")
gcParams$NZLevel2StartDateTime <- lubridate::as_datetime("2020-05-13 23:59:00")
gcParams$NZlockDownEndDate <- lubridate::today() # for plots
gcParams$NZlockDownEndDateTime <- lubridate::now()

# For .Rmd ----
# > Default yaml for Rmd ----
gcParams$pubLoc <- "[Centre for Sustainability](http://www.otago.ac.nz/centre-sustainability/), University of Otago: Dunedin"
gcParams$Authors <- "Anderson, B., Dortans, C."

gcParams$myAlpha <- 0.1
gcParams$vLineAlpha <- 0.4
gcParams$vLineCol <- "red" # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
gcParams$myTextSize <- 4

gcParams$weAlpha <- 0.3 # weekend shaded rects on plots
gcParams$weFill <- "grey50"
gcParams$labelPos <- 0.9

# > Rmd includes ----
gcParams$licenseCCBY <- paste0(here::here(), "/includes/licenseCCBY.Rmd")
gcParams$support <- paste0(here::here(), "/includes/supportGeneric.Rmd")
gcParams$history <- paste0(here::here(), "/includes/historyGeneric.Rmd")
gcParams$citation <- paste0(here::here(), "/includes/citationGeneric.Rmd")

message("We're ", gcParams$user, " using " , gcParams$sysname, " on ", gcParams$nodename)
message("NZ data path : ", gcParams$nzData)
message("UK data path : ", gcParams$ukData)

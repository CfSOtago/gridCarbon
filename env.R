# Keep all the main frequently re-used package parameters in here
# library(gridCarbon) causes .Rprofile to be loaded whin turn sources this file
# But you can source it yourself as well if you forgot to build gridCarbon

# Package we need in this file
library(here)

# Package parameters ----

repoParams <- list() # params holder as a list. Sooooo much easier with tab complete

repoParams$repoLoc <- here::here() # try not to use this, use here() instead

# Data ----
# attempt to guess the platform & user
repoParams$info <- Sys.info()
repoParams$sysname <- repoParams$info[[1]]
repoParams$nodename <- repoParams$info[[4]]
repoParams$login <- repoParams$info[[6]]
repoParams$user <- repoParams$info[[7]]

repoParams$nzData <- "No idea, you need to edit env.R so I can find it!"
repoParams$ukData <- "No idea, you need to edit env.R so I can find it!"

# > Set data path ----
if((repoParams$user == "dataknut" | repoParams$user == "carsten" ) & 
   repoParams$sysname == "Linux"){ # Otago CS RStudio server
  
  repoParams$GreenGrid <- path.expand("~/greenGridData/")
  repoParams$GreenGridData <- path.expand("~/greenGridData/cleanData/safe/")
  repoParams$censusData <- path.expand("~/greenGridData/externalData/nzCensus/") # fix for your platform
  repoParams$gxpData <- path.expand("~/greenGridData/externalData/EA_GXP_Data/") # fix for your platform
  repoParams$nzGridDataLoc <- paste0(repoParams$GreenGrid, 
                                 "externalData/EA_Generation_Data/")
  repoParams$nzNonGridDataLoc <- paste0(repoParams$GreenGrid, 
                                    "externalData/EA_Embedded_Generation_Data/")
  repoParams$nzData <- repoParams$GreenGridData
}
if(repoParams$user == "ben" & repoParams$sysname == "Darwin"){
  # Ben's laptop
  repoParams$GreenGrid <- path.expand("~/Data/NZ_GREENGrid/")
  repoParams$GreenGridData <- path.expand("~/Data/NZ_GREENGrid/safe/")
  repoParams$censusData <- path.expand("~/Data/NZ_Census/") # fix for your platform
  repoParams$gxpData <- path.expand("~/Data/NZ_EA_EMI/gxp/") # fix for your platform
  repoParams$nzGridDataLoc <- path.expand("~/Data/NZ_EA_EMI/EA_Generation_Data/")
  repoParams$nzNonGridDataLoc <- path.expand("~/Data/NZ_EA_EMI/EA_Embedded_Generation_Data/")
  repoParams$nzData <- repoParams$GreenGridData
  
  repoParams$ukData <- path.expand("~/Data/UK_National_Grid/")
  repoParams$ukGridDataLoc <- path.expand(paste0(repoParams$ukData, "gridGen/"))
  repoParams$ukNonGridDataLoc <- path.expand(paste0(repoParams$ukData, "embeddedGen/"))
  
}
if(repoParams$user == "carsten.dortans" & repoParams$sysname == "Darwin"){
  # Carsten's laptop
  repoParams$GreenGridData <- path.expand("/Volumes/hum-csafe/Research Projects/GREEN Grid/cleanData/safe/")
  repoParams$nzGridDataLoc <- path.expand(paste0(repoParams$GreenGridData, 
                                               "/EA_Generation_Data/"))
  repoParams$nzNonGridDataLoc <- path.expand(paste0(repoParams$GreenGridData, 
                                                  "/EA_Embedded_Generation_Data/"))
  repoParams$nzData <- repoParams$GreenGridData
}
if(repoParams$user == "ba1e12" & repoParams$sysname == "Linux" & repoParams$nodename == "srv02405"){
  # UoS RStudio server
  repoParams$ukData <- path.expand("/mnt/SERG_data/UK_National_Grid")
  repoParams$ukGridDataLoc <- path.expand(paste0(repoParams$ukData, "/gridGen/"))
  repoParams$ukNonGridDataLoc <- path.expand(paste0(repoParams$ukData, "/embeddedGen/"))
  repoParams$nzData <- path.expand("/mnt/SERG_data/NZ_EA_EMI")
  repoParams$nzGridDataLoc <- path.expand(paste0(repoParams$nzData, "/EA_Generation_Data/"))
  repoParams$nzNonGridDataLoc <- path.expand(paste0(repoParams$nzData, "/EA_Embedded_Generation_Data/"))
  repoParams$nzGxpDataLoc <- path.expand(paste0(repoParams$nzData, "/EA_GXP_Data/"))
}

# > Misc data ----
repoParams$bytesToMb <- 0.000001

# > lockdown dates ----
# set values for annotations
repoParams$UKlockDownStartDate <- as.Date("2020-03-24")
repoParams$UKlockDownStartDateTime <- lubridate::as_datetime("2020-03-23 23:59:00")
repoParams$UKlockDownRelaxDate_1 <- as.Date("2020-05-11") # BJ speech 10th May
repoParams$UKlockDownRelaxDateTime_1 <- lubridate::as_datetime("2020-05-10 23:59:00")
repoParams$UKlockDownEndDate <- lubridate::today() # for plots
repoParams$UKlockDownEndDateTime <- lubridate::now()

repoParams$NZLevel4StartDate <- as.Date("2020-03-26")
repoParams$NZLevel4StartDateTime <- lubridate::as_datetime("2020-03-25 23:59:00")
repoParams$NZLevel3StartDate <- as.Date("2020-04-27")
repoParams$NZLevel3StartDateTime <- lubridate::as_datetime("2020-04-27 23:59:00")
repoParams$NZLevel2StartDate <- as.Date("2020-05-13")
repoParams$NZLevel2StartDateTime <- lubridate::as_datetime("2020-05-13 23:59:00")
repoParams$NZlockDownEndDate <- as.Date("2020-06-08") # for plots
repoParams$NZlockDownEndDateTime <- lubridate::as_datetime("2020-06-08 23:59:00")

# For .Rmd ----
# > Default yaml for Rmd ----
repoParams$pubLoc <- "[Centre for Sustainability](http://www.otago.ac.nz/centre-sustainability/), University of Otago: Dunedin"
repoParams$Authors <- "Anderson, B., Dortans, C."

repoParams$myAlpha <- 0.1
repoParams$vLineAlpha <- 0.4
repoParams$vLineCol <- "red" # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
repoParams$myTextSize <- 4

repoParams$weAlpha <- 0.3 # weekend shaded rects on plots
repoParams$weFill <- "grey50"
repoParams$labelPos <- 0.9

# > Rmd includes ----
repoParams$licenseCCBY <- paste0(here::here(), "/includes/licenseCCBY.Rmd")
repoParams$support <- paste0(here::here(), "/includes/supportGeneric.Rmd")
repoParams$history <- paste0(here::here(), "/includes/historyGeneric.Rmd")
repoParams$citation <- paste0(here::here(), "/includes/citationGeneric.Rmd")
repoParams$citationUoS <- paste0(here::here(), "/includes/citationUoS.Rmd")

message("We're ", repoParams$user, " using " , repoParams$sysname, " on ", repoParams$nodename)
message("NZ data path : ", repoParams$nzData)
message("UK data path : ", repoParams$ukData)

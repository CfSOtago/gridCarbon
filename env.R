# Keep all the main frequently re-used package parameters in here
# library(gridCarbon) causes .Rprofile to be loaded whin turn sources this file
# But you can source it yourself as well if you forgot to build gridCarbon

# Package we need in this file
library(here)

# Package parameters ----

gcParams <<- list() # params holder as a list. Sooooo much easier with tab complete

# > Location of the repo ----
gcParams$repoLoc <- here::here()

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
}
if(gcParams$user == "ben" & gcParams$sysname == "Darwin"){
  # Ben's laptop
  gcParams$GreenGrid <- path.expand("~/Data/NZ_GREENGrid/")
  gcParams$GreenGridData <- path.expand("~/Data/NZ_GREENGrid/safe/")
  gcParams$censusData <- path.expand("~/Data/NZ_Census/") # fix for your platform
  gcParams$gxpData <- path.expand("~/Data/NZ_EA_EMI/gxp/") # fix for your platform
}
if(gcParams$user == "carsten.dortans" & gcParams$sysname == "Darwin"){
  # Carsten's laptop
  gcParams$GreenGridData <- path.expand("/Volumes/hum-csafe/Research Projects/GREEN Grid/cleanData/safe/")
}
if(gcParams$user == "ba1e12" & gcParams$sysname == "Linux" & gcParams$nodename == "srv02405"){
  # UoS RStudio server
  gcParams$GreenGridData <- path.expand("/mnt/SERG_data/NZ_EA_EMI")
  gcParams$ukData <- path.expand("/mnt/SERG_data/UK_National_Grid")
}

# > Misc data ----
gcParams$bytesToMb <- 0.000001

# For .Rmd ----
# > Default yaml for Rmd ----
gcParams$pubLoc <- "[Centre for Sustainability](http://www.otago.ac.nz/centre-sustainability/), University of Otago: Dunedin"
gcParams$Authors <- "Anderson, B., Dortans, C."

# > Rmd includes ----
gcParams$licenseCCBY <- paste0(gcParams$repoLoc, "/includes/licenseCCBY.Rmd")
gcParams$support <- paste0(gcParams$repoLoc, "/includes/supportGeneric.Rmd")
gcParams$history <- paste0(gcParams$repoLoc, "/includes/historyGeneric.Rmd")
gcParams$citation <- paste0(gcParams$repoLoc, "/includes/citationGeneric.Rmd")

message("We're ", gcParams$user, " using " , gcParams$sysname, " on ", gcParams$nodename)
message("NZ data path : ", gcParams$GreenGrid)
message("UK data path : ", gcParams$ukData)

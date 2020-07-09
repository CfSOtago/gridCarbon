# loads data & runs a report

# Load some packages
library(gridCarbon) # load this first - you will need to download & build it locally from this repo
gridCarbon::setup()

libs <- c("data.table", # data munching
          "drake", # data gets done once (ideally)
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

gridCarbon::loadLibraries(libs) # should install any that are missing


# check

# Parameters ----

update <- "yes" # edit this in any way to trigger drake to reload data
  
localParams <- list()

# > dates ----
localParams$fromYear <- 2019 # a way to limit the number of years of data files loaded
localParams$lockDownStart <- as.Date("2020-03-24")
localParams$lockDownEnd <- lubridate::today()

# > data paths ----
gxpDataPath <- paste0(gcParams$nzGxpDataLoc, 
                                  "processed/yearly/")
# > captions ----
localParams$dataSource <- "NZ Energy Authority"
localParams$dataSourceURL <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export"
localParams$gxpCaption <- paste0("Source: ", localParams$dataSource,
                                 "\n", localParams$dataSourceURL)
# > defn of peak ----
amPeakStart <- hms::as_hms("07:00:00")
amPeakEnd <- hms::as_hms("09:00:00")
pmPeakStart <- hms::as_hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as_hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

# Functions ----

makeReport <- function(f){
  # default = html
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(gcParams$repoLoc,"/docs/nz/", title,"_",
                                         subtitle,".html")
  )
}

loadData <- function(path, fromYear, update){
  # lists files within a folder (path) & loads
  # edit fromYear to force a new data load
  filesToDateDT <- data.table::as.data.table(list.files(path, ".csv.gz")) # get list of files already downloaded & converted to long form
  filesToDateDT[, file := V1]
  filesToDateDT[, c("year", "name") := tstrsplit(file, split = "_")]
  filesToDateDT[, year := as.numeric(year)]
  filesToGet <- filesToDateDT[year >= fromYear, # to reduce files loaded
                              file]
  message("Loading files >= ", fromYear)
  l <- lapply(paste0(path, filesToGet), # construct path for each file
              data.table::fread) # mega fast read
  dt <- rbindlist(l, fill = TRUE) # rbind them
  l <- NULL
  # dt <- do.call(rbind,
  #               lapply(filesToGet, # a list
  #                      function(f)
  #                        data.table::fread(paste0(path,f))
  #               ) # decodes .gz on the fly
  # )
  return(dt) # large
}

# add dates etc, do this within drake so it gets done once
processGxpData <- function(dt){
  #fix gxp data ----
  dt[, rDateTimeOrig := rDateTime] # just in case
  dt[, rDateTime := lubridate::as_datetime(rDateTime)]
  dt[, rDateTimeNZT := lubridate::force_tz(rDateTime, 
                                                  tzone = "Pacific/Auckland")] # just to be sure
  dt[, rTime := hms::as_hms(rDateTimeNZT)]
  dt[, rMonth := lubridate::month(rDateTimeNZT, label = TRUE, abbr = TRUE)]
  
  dt[, rDate := lubridate::as_date(rDate)] # in case loads as character
  
  dt <- gridCarbon::setPeakPeriod(dt, dateTime = "rDateTimeNZT")
  dt <- setPeakPeriod(dt, dateTime = "rDateTimeNZT") # use defaults
  dt <- addSeason(dt, dateVar = "rDateTimeNZT", h = "S")
  return(dt)
}


# drake plan ----
plan <- drake::drake_plan(
  rawGxpData = loadData(gxpDataPath, # from where?
                      localParams$fromYear,  # from what date?
                      update),
  gxpData = processGxpData(rawGxpData)
)
# 
# path <- localParams$gridDataLoc
# fromYear <- localParams$fromYear
# dt <- loadGenData(path, # from where?
#                   fromYear)

# > run drake plan ----
plan # test the plan
make(plan) # run the plan, re-loading data if needed

# get the data back
gxpDT <- drake::readd(gxpData)

# load the GXP look up table
gxpLutDT <- data.table::fread(paste0(gcParams$nzGxpDataLoc, 
                                     "Network_supply_points_table_20191021154036.csv"))
  
# code ----

# check
message("Loaded ", tidyNum(nrow(aggGxpDT)), " rows of aggregated GXP data")
cleanGxpDT <- gxpDT[!is.na(rDateTimeNZT)] # removes TP 49 & 50
cleanGxpDT <- cleanGxpDT[!is.na(kWh)] # removes NA kWh
message("Now have ", tidyNum(nrow(cleanGxpDT)), " rows of data after removing NAs")

summary(aggGxpDT$rDateTimeNZT)


# > Make report ----
# >> yaml ----
version <- "1.0"
title <- paste0("NZ Electricity Demand")
subtitle <- paste0("GXP (grid export) data exploration v", version)
authors <- "Ben Anderson & Michael Jack"


# >> run report ----
rmdFile <- paste0(gcParams$repoLoc, "/rmd/nz/gxpExplore_NZ.Rmd")

#makeReport(rmdFile)



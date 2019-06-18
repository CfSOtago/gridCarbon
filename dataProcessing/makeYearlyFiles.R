# Loads the monthly EA Wholesale & Embedded Gen files
# Creates single year files for easier future use

# Load libraries ----
library(gridCarbon) # load this first - you will need to download & install it locally from this repo

# Packages needed in this .Rmd file ----
reqLibs <- c("data.table", # data munching
             "drake", # for workflow efficiency
             "skimr" # for skim
)
# load them
gridCarbon::loadLibraries(reqLibs)

# Parameters ----

local <- 1 # set to 1 for local file storage (see below)
refresh <- 0 # set to 1 to try to download all files even if we have them

if(local){ # set data storage location
  dPath <- path.expand("~/Data/NZ_EA_EMI/")
} else {
  dPath <- path.expand("/Volumes/hum-csafe/Research Projects/GREEN Grid/externalData/")
}

getData <- function(which){ # parameter selects path and thus files
  path <- paste0(dPath,which, "/processed/monthly/")
  message("Checking what we have already in: ", path)
  filesToDateDT <- data.table::as.data.table(list.files(path)) # get list of files already downloaded
  filesToDateDT[, year := tstrsplit(V1, split = "_", keep = 1)]
  filesToDateDT[, fullPath := paste0(path,V1)]
  years <- unique(filesToDateDT[year != "metaDT.csv",]$year) # avoid the metadata file
  for(y in years){
    fList <- filesToDateDT[year == y, fullPath]
    yearDT <- do.call(rbind,
                      lapply(fList,
                             function(f)
                               data.table::as.data.table(readr::read_csv(f)) # auto-parses nicely
                      ) # decodes .gz on the fly
    )
    # write out the year file ----
    of <- paste0(dPath, which, "/processed/yearly/",y,"_", which, ".csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd))
  }
  return(yearDT) # return the last year for testing if needed
}

# Parameters ----
# Set start time ----
startTime <- proc.time()

# drake plan ----
plan <- drake::drake_plan(
  embedded = getData(which = "embeddedGen"),
  grid = getData(which = "gridGen"), 
  trace = TRUE
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

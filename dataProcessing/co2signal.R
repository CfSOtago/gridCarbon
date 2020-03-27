# testing 

library(curl)
library(jsonlite)
library(data.table)
library(ggplot2)

f <- "https://api.co2signal.com/v1/latest?countryCode="
countryCode <- "GB"

source(path.expand("~/myTokens.R")) # not in repo
oPath <- path.expand("~/Data/co2signal/")
  
# curl 'https://api.co2signal.com/v1/latest?countryCode=GB'
# -H 'auth-token: myapitoken'

authRequest <- paste0(f, countryCode, "&auth-token=", myTokens$co2signal) # don't expose this!

# get the data every minute (for now) for 60 minutes as a test. Does it vary?
loops <- 10
sleepTime <- 30
print(paste0("Trying to download ", f))

dataBucket <- data.table::data.table()

for(l in 1:loops){
  message("Fetch # ", l, " of ", loops)
  dateTimeReq <- Sys.time()
  req <- curl::curl_fetch_memory(authRequest) # https://www.rdocumentation.org/packages/curl/versions/4.3/topics/curl_fetch_memory
  if(req$status_code == 200){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
    message("Success :-)")
    data <- jsonlite::fromJSON(rawToChar(req$content))
    dtCI <- data.table::data.table()
    # we don't seem to be getting dateTime
    dtCI$dateTimeRequested <- dateTimeReq
    dtCI$value <- data$data$carbonIntensity
    dtCI$variable <- "carbonIntensity"
    
    dtFFP <- data.table::data.table()
    # we don't seem to be getting dateTime
    dtFFP$dateTimeRequested <- dateTimeReq
    dtFFP$value <- data$data$fossilFuelPercentage
    dtFFP$variable <- "fossilFuelPercentage"
    dt <- rbind(dtCI, dtFFP)
    dt$countryCode <- data$countryCode
    # message("Country: ", data$countryCode)
    # print(dt)
  } else {
    print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
  }
  dataBucket <- rbind(dataBucket, dt)
  message("Processed, waiting for ", sleepTime ," seconds")
  Sys.sleep(sleepTime)
}

message("N rows: ", nrow(dataBucket))
summary(dataBucket)

p <- ggplot2::ggplot(dataBucket, aes(x = dateTimeRequested, y = value, colour = variable)) +
  geom_point() +
  labs(x = "Time requested",
       y = "Value")

ggplot2::ggsave(file = paste0(oPath, "/", 
                              countryCode, "/co2signal_",
                              period, ".png"), 
                p)

period <- paste0(min(dataBucket$dateTimeRequested), "_", max(dataBucket$dateTimeRequested))
data.table::fwrite(dataBucket, file = paste0(oPath, "/", 
                                             countryCode, "/co2signal_",
                                             period, ".csv"))
# see also http://static.electricitymap.org/api/docs/index.html#past-carbon-intensity-history

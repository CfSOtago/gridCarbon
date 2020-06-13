#' \code{getNZEmbData} gets the embedded generation data files from the NZ EA data website 
#' 
#' These are not in pretty form so we clean them up and save them as monthly and yearly files.
#' The code attempts to be clever about not downloading files it already has.
#'
#' @param path the path we may have saved files in before as `path/raw`, `path/processed/monthly` etc
#' @param years the years to get
#' @param months themonths to get
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family grid 
#' @family NZ
#' 

getNZEmbData <- function(path, years,months){
  message("Embedded Gen: Checking what we have already...")
  filesToDateDT <- data.table::as.data.table(list.files(paste0(path, "/raw/"))) # get list of files already downloaded
  metaDT <- data.table::data.table() # stats collector
  
  for(y in years){
    for(mo in months){
      # construct the filename
      if(nchar(mo) == 1){
        # need to add 0 as prefix
        m <- paste0("0", mo)
      } else {
        m <- mo
      }
      if(lubridate::today() < as.Date(paste0(y, "-",m,"-", "01"))){
        break # clearly there won't be any data
      }
      
      #y <- 2020
      #m <- "01"
      eafName <- paste0(y, m,"_Embedded_generation.csv") # what we see on the EA EMI
      rawfName <- paste0(y,"_", m,"_embeddedGen.csv") # for ease of future file filtering
      print(paste0("Checking ", rawfName))
      test <- filesToDateDT[V1 %like% rawfName] # should catch .csv.gz too
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so skip
        print(paste0("Already have ", rawfName, ", loading from local..."))
        # Load so we can update meta
        #df <- readr::read_csv(paste0(localParams$rawEmbDataPath, rawfName))
        dt <- data.table::fread(paste0(path, "/raw/", rawfName , ".gz"))
        dt <- gridCarbon::cleanNZEmbEA(dt) # clean up to a dt, TP49 & TP50 will fail to parse
        # print(summary(dt))
        testDT <- gridCarbon::getNZEmbMeta(dt) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := rawfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
      } else {
        # Download it
        rFile <- paste0(localParams$embDataURL,eafName)
        print(paste0("We don't have or need to refresh ", rawfName))
        # use curl function to catch errors
        # currently this breaks if no net - we need to catch that error too!
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          #df <- readr::read_csv(req$content)
          dt <- data.table::fread(req$content)
          print("File downloaded successfully, saving it")
          wf <- paste0(path, "/raw/", rawfName)
          data.table::fwrite(dt, wf)
          cmd <- paste0("gzip -f ", "'", path.expand(wf), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed original file")
          dt <- gridCarbon::cleanNZEmbEA(dt) # clean up to a dt
          testDT <- gridCarbon::getNZEmbMeta(dt) # get metaData
          testDT <- testDT[, source := rawfName]
          metaDT <- rbind(metaDT, testDT)
          print("Converted to long form, saving it")
          lf <- paste0(path, "/processed/monthly/", rawfName)
          data.table::fwrite(dt, lf)
          cmd <- paste0("gzip -f ", "'", path.expand(lf), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          print("Compressed long form file")
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
        }
      }
    }
  }
  # write out the meta data ----
  data.table::fwrite(metaDT, paste0(localParams$processedEmbDataPath, "metaDT.csv"))
  
  # remove the temp file
  file.remove("temp.csv")
  return(metaDT)
}
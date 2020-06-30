#' \code{getNZGxpEA} gets the grid exit flow data files from the NZ EA data website 
#' 
#' These are not in pretty form so we clean them up and save them as monthly and yearly files.
#' The code attempts to be clever about not downloading files it already has.
#'
#' @param path the path we may have saved files in before as `path/raw`, `path/processed/monthly` etc
#' @param years the years to get
#' @param months themonths to get
#' @param url the EA site base url
#' 
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family grid 
#' @family NZ
#' 
getNZGxpEA <- function(path, years, months, url){
  message("Checking what we have already...")
  # path <- gcParams$nzGridDataLoc
  
  filesToDateDT <- data.table::as.data.table(list.files(paste0(path, "/raw/"))) # get list of files already downloaded
  metaDT <- data.table::data.table() # stats collector
  for(y in years){
    for(month in months){
      # y <- 2020
      # month <- 5
      # construct the filename
      if(nchar(month) == 1){
        # need to add 0 as prefix
        m <- paste0("0", month)
      } else {
        m <- month
      }
      if(lubridate::today() < as.Date(paste0(y, "-",m,"-", "01"))){
        break # clearly there won't be any data for future dates
      }
      eafName <- paste0(y, m,"_Grid_export.csv") # what we see on the EA EMI
      rawfName <- paste0(y,"_", m,"_gxpGridExport.csv") # for ease of future file filtering
      print(paste0("Checking ", rawfName))
      test <- filesToDateDT[V1 %like% rawfName] # have we got it already?
      if(nrow(test) > 0 & refresh == 0){
        # Already got it & we don't want to refresh so skip
        print(paste0("Have already downloaded: ", test$V1, ", loading..."))
        # Load so we can update meta
        dt <- data.table::fread(paste0(path,"/raw/", test)) # this loop wants the raw data
        # should probably change this so it doesn't need to run over _all_ the files every time
        dt <- gridCarbon::cleanNZGxpEA(dt) # clean up to a dt - fixes dateTimes etc
        print(summary(dt))
        testDT <- gridCarbon::getNZGxpMeta(dt) # get metaData
        print(head(testDT))
        testDT <- testDT[, source := rawfName]
        metaDT <- rbind(metaDT, testDT)
        testDT <- NULL
      } else {
        # Get it
        rFile <- paste0(url,eafName)
        print(paste0("We don't have or need to refresh ", rawfName))
        # use curl function to catch errors
        print(paste0("Trying to download ", rFile))
        req <- curl::curl_fetch_disk(rFile, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
        if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
          #dt <- data.table::fread(req$content) # breaks on 2014-11, why?
          df <- readr::read_csv(req$content)
          dt <- data.table::as.data.table(df)
          message("File downloaded successfully, saving as ", rawfName)
          wf <- paste0(path, "/raw/", rawfName)
          data.table::fwrite(dt, wf)
          cmd <- paste0("gzip -f ", "'", path.expand(wf), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          message("Compressed it")
          ldt <- gridCarbon::cleanNZGxpEA(dt) # clean up to a dt - this does all the processing and converts to long
          testDT <- gridCarbon::getNZGxpMeta(ldt) # get metaData
          testDT <- testDT[, source := rawfName]
          metaDT <- rbind(metaDT, testDT)
          print("Converted to long form, saving it")
          lf <- paste0(path, "/processed/monthly/", rawfName)
          data.table::fwrite(ldt, lf)
          cmd <- paste0("gzip -f ", "'", path.expand(lf), "'") # gzip it - use quotes in case of spaces in file name, expand path if needed
          message("Running: ", cmd)
          try(system(cmd)) # in case it fails - if it does there will just be .csv files (not gzipped) - e.g. under windows
          message("Compressed it")
        } else {
          print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
        }
      }
    }
  }
  return(metaDT)
}
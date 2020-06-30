#' \code{makeNZYearlyData} converts monthly to yearly data
#' 
#' @param genType understands `gridGen` or `embeddedGen`
#' @param years which years to loop over
#' @param path the top level data path for this genType
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk} (original)
#' @export
#' @family data
#' @family embedded 
#' @family gxp
#' @family grid 
#' @family NZ
#' 
makeNZYearlyData <- function(genType, years, path){ 
  message("Checking what we have already in: ", paste0(path,"processed/monthly/"))
  filesToDateDT <- data.table::as.data.table(list.files(paste0(path,
                                                               "processed/monthly/")
                                                        )
                                             ) # get list of files already downloaded
  message("We have ", nrow(filesToDateDT), " files to process")
  filesToDateDT[, year := tstrsplit(V1, split = "_", keep = 1)] #
  filesToDateDT[, fullPath := paste0(path, "processed/monthly/",V1)]
  #years <- unique(filesToDateDT[year != "metaDT.csv",]$year) # avoid the metadata file
  for(y in years){ # use the years passed in as they will be the ones we updated
    fList <- filesToDateDT[year == y, fullPath]
    yearDT <- data.table::data.table()
    yearDT <- do.call(rbind,
                      lapply(fList,
                             function(f)
                               data.table::fread(f) # auto-parses nicely
                      ) # decodes .gz on the fly
    )
    # write out the year file ----
    of <- paste0(path,"processed/yearly/",y,"_", genType, ".csv")
    data.table::fwrite(yearDT, of)
    cmd <- paste0("gzip -f ", of)
    message("Gzip file: ", of)
    try(system(cmd)) # seems to throw an error on the CS RStudio server but it still works
    message("Done ", y)
  }
}

#' gzip a file
#'
#' \code{gzipFile} gzips a file
#'
#'    - usually one we've just written out.
#'
#' @param f the file
#'
#' @author Michael Harper, \email{m.harper@@soton.ac.uk}
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family utils
#'
gzipFile <- function(f){
  file <- path.expand(f) # expand path just in case was symlink etc
  
  # Check if input csv exists
  if(!file.exists(file)) stop("Input `f` not found") # refer to unexpanded for security
  
  # Path of output file
  gzipped <- paste0(file, ".gz")
  
  # Rremove old gzip file if exists
  if(file.exists(gzipped)){
    message("Deleting old .gz file...")
    file.remove(gzipped)
    message("DONE\n")
  }
  
  # Gzip new one
  # in case it fails (it will on windows - you will be left with a .csv file)
  message(paste0("gzipping file to: ", gzipped))
  cmd <- paste0("gzip -f ", "'", file , "'") # use quotes in case of spaces in file name
  try(system(cmd))
  message("Done")
}
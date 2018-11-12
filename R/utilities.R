#' Add NZ season to a data.table
#'
#' \code{addNZSeason} returns a dt with SOUTHERN hemisphere season added.
#'
#' @param dt the data table
#' @param r_dateTime the column in the dt which is a date that lubridate::month() will work on
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#'
addNZSeason <- function(dt,r_dateTime = r_dateTime){
  dt <- dt[, tmpM := lubridate::month(r_dateTime)] # sets 1 (Jan) - 12 (Dec). May already exist but we can't rely on it
  dt <- dt[, season := "Summer"] # easiest to set the default to be the one that bridges years
  dt <- dt[tmpM >= 3 & tmpM <= 5, season := "Autumn"]
  dt <- dt[tmpM >= 6 & tmpM <= 8 , season := "Winter"]
  dt <- dt[tmpM >= 9 & tmpM <= 11, season := "Spring"]
  # re-order to make sense
  dt <- dt[, season := factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))]
  table(dt$season, lubridate::month(dt$r_dateTime, label = TRUE))
  dt$tmpM <- NULL
  return(dt)
}

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

#' Get duration
#'
#' \code{getDuration} takes a timediff created using (e.g.)
#'
#'    startTime <- proc.time()
#'    .....
#'    endTime <- proc.time()
#'    t <- endTime - startTime
#'
#'    and returns a formrated extraction of seconds & minutes for use in feedback.
#'
#' @param t a duration
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#'
getDuration <- function(t){
  elapsed <- t[[3]]
  msg <- paste0(round(elapsed,2),
                " seconds ( ",
                round(elapsed/60,2),
                " minutes)"
  )
  return(msg)
}

#' Tidy long numbers
#'
#' \code{tidyNum} reformats long numbers to include commas and prevents scientific formats
#'
#' @param number an input number or list
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#'
tidyNum <- function(number) {
  format(number, big.mark=",", scientific=FALSE)
}

#' Find the path to Parent Directory
#'
#' Equivalent of \code{findParentDirectory}. Is useful for running a project
#'   across multiple computers where the project is stored in different directories.
#'
#' @param Parent the name of the directory to which the function should track back.
#' Should be the root of the GitHub repository
#'
#' @author Mikey Harper, \email{m.harper@@soton.ac.uk}
#' @export
#'
findParentDirectory <- function(Parent){
  directory <-getwd()
  while(basename(directory) != Parent){
    directory <- dirname(directory)

  }
  return(directory)
}

#' Installs and loads packages
#'
#' \code{loadLibraries} checks whether the package is already installed,
#'   installing those which are not preinstalled. All the libraries are then loaded.
#'
#'   Especially useful when running on virtual machines where package installation
#'   is not persistent (Like UoS sve). It will fail if the packages need to be
#'   installed but there is no internet access.
#'
#'   NB: in R 'require' tries to load a package but throws a warning & moves on if it's not there
#'   whereas 'library' throws an error if it can't load the package. Hence 'loadLibraries'
#'   https://stackoverflow.com/questions/5595512/what-is-the-difference-between-require-and-library
#' @param ... A list of packages
#' @param repo The repository to load functions from. Defaults to "https://cran.rstudio.com"
#' @importFrom  utils install.packages
#'
#' @author Luke Blunden, \email{lsb@@soton.ac.uk} (original)
#' @author Michael Harper \email{m.harper@@soton.ac.uk} (revised version)
#' @export
#'
loadLibraries <- function(..., repo = "https://cran.rstudio.com"){

  packages <- c(...)

  # Check if package is installed
  newPackages <- packages[!(packages %in% utils::installed.packages()[,1])]

  # Install if required
  if (length(newPackages)){utils::install.packages(newPackages, dependencies = TRUE)}

  # Load packages
  sapply(packages, require, character.only = TRUE)
}

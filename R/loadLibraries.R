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
#' @family utils
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

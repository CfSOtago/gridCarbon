#' Set up the project
#'
#' \code{setup} sources env.R in the repo/project top level folder.
#'
#' @import here
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family utils
#'
setup <- function() {
  source(here::here("env.R"))
}

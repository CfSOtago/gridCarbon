#' Tidy long numbers
#'
#' \code{tidyNum} reformats long numbers to include commas and prevents scientific formats
#'
#' @param number an input number or list
#'
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family utils
#'
tidyNum <- function(number) {
  format(number, big.mark=",", scientific=FALSE)
}
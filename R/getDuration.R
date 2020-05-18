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
#' @family utils
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
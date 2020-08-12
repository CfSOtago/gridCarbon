#' Creates a plot by weekday from the data.table given
#'
#' `makeWeekdayTimePlot` returns a plot which shows yVar by weekday, time, compareYear and plotPeriod (assumed to be lockdown). 
#' Assumes this is the date aligned data. Unlike makeWeekdayPlot this plots the proportion of the value (not the absolute value) within
#' the categories. This helps to see proportional change.
#'
#' @param dt the data
#' @param xVar the x axis - probably days of the week but you can be creative (needs to be a factor)
#' @param yVar the variable you want to plot
#' @param yForm whether you want an abs(olute) (default) or a prop(ortional) plot
#' @param yLab the label for the y axis
#' @param yDiv the value you want to divide yVar by to make the y axis more sensible. Default = `1`
#' 
#' @return a plot
#' @import ggplot2
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family plot
#'
makeWeekdayTimePlot <- function(dt, xVar, yVar, yForm = "abs", yLab = "y Lab", yDiv = 1){
  # by weekday and hour
  # we can't get HMS to format right so...
  dt[, hmsKludge := lubridate::as_datetime(paste0(today(), hms))]
  if(yForm == "prop"){
    # use proportion to show relative shifts
    # use month as the facet to link to other plots
    dt[, month := lubridate::month(dateFixed, label = TRUE)]
    dt <- dt[, .(pVals = mean(get(yVar))/yDiv), 
             keyby = .(hmsKludge, month, compareYear, xVar = get(xVar))]
    sums <- dt[, .(sum = sum(pVals)), keyby = .(compareYear, month, xVar)]
    setkey(sums, compareYear, month, xVar)
    setkey(dt, compareYear, month, xVar)
    plotDT <- sums[dt]
    plotDT[, yVal := (pVals/sum)*100]
    ok <- TRUE
  }
  if(yForm == "abs"){
    #message("abs")
    dt[, month := lubridate::month(dateFixed, label = TRUE)]
    plotDT <- dt[, .(yVal = mean(get(yVar))/yDiv), 
              keyby = .(hmsKludge, month, compareYear, xVar = get(xVar))]
    ok <- TRUE
  }
  if(yForm != "abs" & yForm != "prop"){# be explicit
    # neither
    e <- "yForm not recognised - function understands 'abs' or 'prop"
    return(e)
  }
  if(ok){
    #message("Building plot with ", yForm)
    p <- ggplot2::ggplot(plotDT, aes(x = hmsKludge, y = yVal, 
                                  colour = compareYear)) + 
      geom_step() +
      #geom_point(size = 1, stroke = 1, shape = 16) +
      #scale_x_time(labels = NULL) +
      #scale_x_time(date_format('%H:%M'))
      #scale_x_datetime(breaks=date_breaks('4 hour'),labels=date_format('%H:%M')) +
      theme(axis.text.x=element_text(angle=90, hjust=1)) +
      theme(legend.position="bottom") +
      scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
      scale_color_discrete(name="Year") +
      facet_grid(month ~ xVar) +
      labs( y = yLab,
            x = "Time")
    return(p)
  }
}
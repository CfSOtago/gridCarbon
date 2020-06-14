#' Re-align 2017-2019 dates so their weekdays match to 2020
#'
#' \code{alignDates} returns a dt with `fixedDate` set to the date needed in 2017-2019 to match the nearest weekday
#' in 2020. Use this for plots which try to compare daily values over time.
#' 
#' The data.table to align *must* have a dateTime variable (bu tyou can tell it which one).
#'
#' @param dt the data.table to align
#' @param dateTime the dateTime variable to use
#' @param toDate return data up to but not including this date
#' 
#' @import lubridate
#' @import data.table
#' @author Ben Anderson, \email{b.anderson@@soton.ac.uk}
#' @export
#' @family data
#' @family utils
#'
alignDates <- function(dt, dateTime, toDate){
  # expects gridGenData from loadGenData as the dt
  # re-use the method from the airQual analysis to align the years
  # so kludgy it should not be allowed
  
  dt[, obsDate := lubridate::date(get(dateTime))]
  dt[, decimalDate := lubridate::decimal_date(obsDate)] # gives year.% of year
  
  # set to 2020 'dates'
  dt[, date2020 := lubridate::as_date(lubridate::date_decimal(2020 + (decimalDate - year)))] # sets 'year' portion to 2020 so the lockdown annotation works
  dt[, day2020 := lubridate::wday(date2020, label = TRUE)] # 
  
  # 2020 Jan 1st = Weds
  dt2020 <- dt[year == 2020] 
  dt2020[, dateFixed := obsDate] # no need to change
  dt2020[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2020$origDoW, dt2020$fixedDoW)
  # head(dt2020[origDoW != fixedDoW])
  
  # shift to the closest aligning day
  # 2019 Jan 1st = Tues
  dt2019 <- dt[year == 2019] 
  dt2019[, dateFixed := date2020 -1]
  dt2019[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2019$origDoW, dt2019$fixedDoW)
  # head(dt2019[origDoW != fixedDoW])
  
  # 2018 Jan 1st = Mon
  dt2018 <- dt[year == 2018] 
  dt2018[, dateFixed := date2020 - 2]
  dt2018[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2018$origDoW, dt2018$fixedDoW)
  # head(dt2018[origDoW != fixedDoW])
  
  # 2017 Jan 1st = Sat
  dt2017 <- dt[year == 2017] 
  dt2017[, dateFixed := date2020 - 3]
  dt2017[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  # table(dt2017$origDoW, dt2017$fixedDoW)
  # head(dt2017[origDoW != fixedDoW])
  
  fixedDT <- rbind(dt2017, dt2018, dt2019, dt2020) # leave out 2016 for now
  
  fixedDT[, dateFixed := lubridate::as_date(dateFixed)]
  fixedDT[, wkdayFixed := lubridate::wday(dateFixed,label = TRUE)]
  
  
  fixedDT[, compareYear := ifelse(year < 2020, "pre 2020", # could be any number of years
                                  "2020"
                                  ) 
          ]
  fixedDT[, wkdayObs := lubridate::wday(obsDate, label = TRUE)]
  fixedDT[, weekDay := ifelse(wkdayFixed == "Sat" | wkdayFixed == "Sun", 
                              "Weekend", # makes some plots clearer
                              "Weekday")
          ]
  return(fixedDT[dateFixed < toDate])
}
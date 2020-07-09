##### CSAFEDataAndAnalysis #####
## Marilette Lötter ##
#### The purpose of this R-file is to read in our generation data and clean it up a bit. 
#### Once that is done, we use this to compute carbon intensities and to compute averaged
#### carbon intensities. After calculating these, we want to draw some plots that will 
#### illustrate the changing behaviour of the carbon footprint of New Zealand's 
#### electricity generation, with a focus of hydro-, wind-, geothermal-, and gas-electricity.
#### The goal is to convert all of this into a Shiny app where the user can adjust the 
#### percentage of electricity produced by each of the fuels of interest to see how it could 
#### change the carbon intensity of electricity generation. 

##### Packages needed for the R-file #####
library(data.table); library(dplyr); library(ggplot2); library(ggpubr)
library(lubridate); library(car); library(TTR); library(forecast); 
library(accelerometry); library(ggridges); library(RColorBrewer)

##### Custom functions used throughout the R-file #####
# The following function is used to merge all of the .csv files in the folder. 
# It has two input parameters, the path of the directory folder and the name of the merged .csv file. 

merge <- function(mypath, fn){
  setwd(mypath) # setting the working directory to our folder of interest
  if (file.exists(fn)) file.remove(fn)
  filenames <- list.files(full.names = T) # finding the names of each file in the folder
  n <- length(filenames) # computes the number of files in the working directory
  merged <- read.csv(filenames[1], header = T, stringsAsFactors = F)
  merged$Trading_date <- as.Date(merged$Trading_date, format = "%Y-%m-%d")
  # the following for-loop reads the next .csv file and the adds it to the already merged dataframe
  for (i in 2:n){
    tmp <- read.csv(filenames[i], header = T)
    tmp$Trading_date <- as.Date(tmp$Trading_date, format = "%Y-%m-%d")
    merged <- rbind(merged, tmp)
  } # for-loop
  write.csv(merged, file = fn) # saving the resulting dataframe as a .csv file 
} # function

## The following funtion adds in some notes to a plot
yMax <- 200
myAlpha <- 0.1
vLineAlpha <- 0.4
vLineCol <- "#0072B2" # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
myTextSize <- 3
annotateAllGenYears <- function(plot){
  plot <- plot +
    annotate("text", x = 2004, # x axis is just a number not a real date
             y = yMax * 0.6, angle = 10,size = myTextSize,
             label = "Huntly 50 MW gas unit added", hjust = 0) +
    geom_vline(xintercept = 2004, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2005,
             y = yMax * 0.5, angle = 10,size = myTextSize,
             label = "Warm dry winters", hjust = 0) +
    geom_vline(xintercept = 2005, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2007,
             y = yMax * 0.4, angle = 10,size = myTextSize,
             label = "Huntly CCGT unit added", hjust = 0) +
    geom_vline(xintercept = 2007, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2007,
             y = yMax * 0.6, angle = 10,size = myTextSize,
             label = "Huntly mainly gas fired", hjust = 0) +
    geom_vline(xintercept = 2007, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2008,
             y = yMax * 0.5, angle = 10,size = myTextSize,
             label = "Dry winter", hjust = 0) +
    geom_vline(xintercept = 2008, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2012,
             y = yMax * 0.4, angle = 10,size = myTextSize,
             label = "1 of 4 Huntly Power Station 250 MW units mothballed", hjust = 0) +
    geom_vline(xintercept = 2012, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2012,
             y = yMax * 0.6, angle = 10,size = myTextSize,
             label = "Dry winter on South Island", hjust = 0) +
    geom_vline(xintercept = 2012, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2013,
             y = yMax * 0.5, angle = 10,size = myTextSize,
             label = "2 of 4 Huntly Power Station 250 MW units in storage", hjust = 0) +
    geom_vline(xintercept = 2013, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2013,
             y = yMax * 0.3, angle = 10,size = myTextSize,
             label = "Warmest winter to date", hjust = 0) +
    geom_vline(xintercept = 2013, alpha = vLineAlpha, colour = vLineCol) +
    annotate("text", x = 2015,
             y = yMax * 0.6, angle = 10,size = myTextSize,
             label = "2 of 4 Huntly Power Station 250 MW units retired", hjust = 0) +
    geom_vline(xintercept = 2015, alpha = vLineAlpha, colour = vLineCol)
  return(plot)
}

## The following function does the same as the genDay function but uses a slightly different
## approach to do this. 
genDaily <- function(fuel, yr, mth, d){
  if (mth < 10){
    mth <- paste("0", mth, sep="")
  }
  if (d < 10){
    d <- paste("0", d, sep="")
  }
  dt <- paste(yr, mth, d, sep="-")
  datDay <- fuel[fuel$Trading_date==dt, 4:51]
  selectGen <- !is.na(datDay$TP1)
  datDay <- datDay[selectGen, ]
  dayGen <- colSums(datDay, na.rm = T)
  return(dayGen)
} # genDaily

## Computes the total carbon emissions for an entire month ##
genMonthly <- function(fuel, yr, mth){
  datMonth <- fuel[year(fuel$Trading_date)==yr & month(fuel$Trading_date)==mth, 4:51]
  selectGen <- !is.na(datMonth$TP1)
  datMonth <- datMonth[selectGen, ]
  monthGen <- colSums(datMonth, na.rm = T)
  return(monthGen)
} # genMonthly

## The following function will draw a plot of the carbon intensity
plotIntensity <- function(CI, dateS, CIcol, ave=rep(0, 48)){
  if (ave[1] == 0){
    x <- seq(1, 48)
    df <- data.frame(xVals=x, ci=CI)
    plotTitle <- paste("Carbon Intensity (gCO2-e/kWh)\n for half-hourly intervals, \n", dateS, sep="")
    plt <- ggplot(df, aes(x=xVals, y=ci)) + geom_point(colour = CIcol) + ggtitle(plotTitle) + 
      labs(x="Half-hourly interval", y="Carbon Intensity (gCO2-e/kWh)")
  } else {
    x <- rep(seq(1, 48), 2)
    grp <- c(rep('daily', 48), rep('average', 48))
    grp <- as.factor(grp)
    df <- data.frame(xVals=x, ci=c(CI, ave), grps=grp)
    plotTitle <- paste("Carbon Intensity (gCO2-e/kWh)\n for half-hourly intervals, \n", dateS, sep="")
    plt <- ggplot(df, aes(x=xVals, y=ci)) + geom_point(aes(shape=grps)) + ggtitle(plotTitle) + 
      labs(x="Half-hourly interval", y="Carbon Intensity (gCO2-e/kWh)") + 
      scale_shape_manual(values=c(16, 3))
  }
} # plotIntensity function

##### Reading in the data and cleaning it up #####
## The purpose of this code is to merge .csv files in the EA_Generation_data folder.
## Note that I am not using the folder directly for now, I am a bit wary of this 
## because I might destroy the data, which would not be ideal. 

# Keeping track of how it takes to read in the data and to clean it up 
startData <- proc.time()

# merging the generation data
genPath <- "C:/Users/maril/OneDrive/Centre for Sustainability SS/Data to be merged"
fnGen <- "merged_Generation_data.csv"
merge(genPath, fnGen)
# merging the embedded generation data
embedGenPath <- "C:/Users/maril/OneDrive/Centre for Sustainability SS/Embedded data to be merged"
fnEmbed <- "merged_Embedded_Generation_data.csv"
merge(embedGenPath, fnEmbed)

## Next, we want to work on the data itself and find "summarised" data set with only what we need.

# Reading in the csv files containing all of the relevant data
setwd(genPath)
genData <- read.csv(fnGen, header=T, stringsAsFactors = F)
setwd(embedGenPath)
embedData <- read.csv(fnEmbed, header=T, stringsAsFactors = F)

csafeWd <- "C:/Users/maril/OneDrive/Centre for Sustainability SS/CSAFE Data and Analysis"
setwd(csafeWd)

# There are some POC duplicates from the two datasets, I want to find those that are unique to the
# embedded generation data and then combine these with the generation dataset. 
genData$POC_Code <- as.factor(genData$POC_Code)
embedData$POC <- as.factor(embedData$POC)

genPOC <- sort(levels(unique(genData$POC_Code)))
embedPOC <- sort(levels(unique(embedData$POC)))

# the following line is used to find the POCs that do not match
notSimPOC <- embedPOC[!genPOC %in% embedPOC]

# now we have to create a subset of embedData which only contains the POCs that are in notSimPOC
embedNotSimData <- embedData[embedData$POC==notSimPOC,]
unique(embedNotSimData$POC) # checking that the above line of code did what it was supposed to do

# Next, we want to construct some summarised dataframes of the larger dataframes, 
# there are some columns that we do not require. 
genDataS <- genData %>% select(-c(X, Site_Code, Nwk_Code, Gen_Code, Tech_Code)) # removing columns we don't need
genDataS$Trading_date <- as.Date.character(genDataS$Trading_date) # converting Trading_date from a character to a date
embedNotSimData <- embedNotSimData %>% select(-c(X, NWK_Code, Participant_Code, Loss_Code, Flow_Direction)) # removing columns we don't need
embedNotSimData$Trading_date <- as.Date.character(embedNotSimData$Trading_date) # converting Trading_Date from a character to a date

# Adding in the fuel types for the data that does not overlap between genData and embedData, 
# i.e. the embedNotSimData dataframe
embedNotSimData <- mutate(embedNotSimData, Fuel_Code='stuff')
unk <- c("ALD0041", "APC0111", "BAN0111", "BUR0011", "CDB0011", "HAR0011", 
         "KEW0011", "MOK0111", "RAN0011", "WCV0111")
hydro <- c("ATI0111", "AVI2201", "BEN0162", "CYD2201", "DRY0011", "FDG0331",
           "KOU0331", "KPO1101", "KUR0331", "KWI0111", "MGR0111", "MGT0331", 
           "MHO0331", "MON0111", "MPH0011", "MTI0011", "OHB2201", "OHC2201", 
           "PRK0111", "PRU0111", "RSD0111", "TRT0111", "WAH0111", "WPA2201", 
           "WPI0112")
geo <- c("WRK0331", "WRK2201")
gas <- c("NPL1101", "SWN2201", "TMU1101", "TRC0331", "WHO0111")
biogas <- "SLF0111"
wind <- "TRH0111"

# Changing the fuel code to unknown for those embedded stations that use unknown fuel for electricity generation
n <- length(unk)
for (i in 1:n){
  embedNotSimData[embedNotSimData$POC==unk[i],]$Fuel_Code <- "Unknown"
}
# Changing the fuel code to hydro for those embedded stations that use hydro fuel for electricity generation
n <- length(hydro)
for (i in 1:n){
  embedNotSimData[embedNotSimData$POC==hydro[i],]$Fuel_Code <- "Hydro"
}
# Changing the fuel code to geo for those embedded stations that use geothermal energy for electricity generation
n <- length(geo)
for (i in 1:n){
  embedNotSimData[embedNotSimData$POC==geo[i],]$Fuel_Code <- "Geo"
}
# Changing the fuel code to gas for those embedded stations that use gas as a fuel for electricity generation
n <- length(gas)
for (i in 1:n){
  embedNotSimData[embedNotSimData$POC==gas[i],]$Fuel_Code <- "Gas"
}
# Changing the fuel code to biogas for those embedded stations that use biogas as a fuel for electricity generation
n <- length(biogas)
for (i in 1:n){
  embedNotSimData[embedNotSimData$POC==biogas[i],]$Fuel_Code <- "Biogas"
}
# Changing the fuel code to wind for those embedded stations that use wind for electricity generation
n <- length(wind)
for (i in 1:n){
  embedNotSimData[embedNotSimData$POC==wind[i],]$Fuel_Code <- "Wind"
}
# Note that I have not changed the Fuel_Code from stuff to the actual fuel for those stations
# using fuels that we are not interested in at the moment. 

# Before I merge genDataS and emnedNotSimData, I have to adjust the ordering of the columns for the latter dataframe
embedNSDReordered <- embedNotSimData[, c(1, 53, 2, seq(3, 52))]
colnames(embedNSDReordered)[1] <- "POC_Code" # renamming the first column

# Finally, the two dataframes can be merged
mergedFinal <- rbind(genDataS, embedNSDReordered)
# Next, I want to save this file, just in case.
if (file.exists("MergedFinal.csv")) file.remove("MergedFinal.csv")
write.csv(mergedFinal, file = "MergedFinal.csv")

(processingData <- proc.time() - startData)



##### Calculating the CO2-e's #####
csafeWd <- "C:/Users/maril/OneDrive/Centre for Sustainability SS/CSAFE Data and Analysis"
setwd(csafeWd)

# timing the process 
startCO2e <- proc.time()

## Getting the data that we need ##
genData1 <- read.csv("MergedFinal.csv", header = T, stringsAsFactors = F)
# genData1$Fuel_Code <- as.factor(genData1$Fuel_Code)

# Daylight savings makes our lives a little bit more complicated and for this reason, I will 
# exclude TP49 and TP50 which represents the extra hour we have with daylight savings
genData <- genData1[,seq(2, 52)]
genData$Trading_date <- as.Date(genData$Trading_date, format = "%Y-%m-%d") # converting Trading_date from a character to a date
class(genData$Trading_date)

class(genData$Fuel_Code)
unique(genData$Fuel_Code)
genData$Fuel_Code <- as.factor(genData$Fuel_Code)
# subsetting the data into separate datasets for each of the 4 relevant fuels
gas <- genData[genData$Fuel_Code == "Gas", ]  
geo <- genData[genData$Fuel_Code == "Geo", ]
hydro <- genData[genData$Fuel_Code == "Hydro", ]
wind <- genData[genData$Fuel_Code == "Wind", ]
coal <- genData[genData$Fuel_Code == "Coal", ]

# plant efficiencies
gasE <- 0.46; geoE <- 0.15; windE <- 0.5; hydroE <- 0.9; coalE <- 0.38
# emission factors
gasCO2 <- 0.194; gasCH4 <- 3/1000000; gasN2O <- 3/10000000
geoCO2 <- 0.115; geoCH4 <- 0; geoN2O <- 0
coalCO2 <- 0.3306; coalCH4 <- 0.00000342; coalN2O <- 0.00000513
windCO2 = windCH4 = windN2O = 0;
hydroCO2 = hydroCH4 = hydroN2O = 0;

## Computing the carbon footprint for gas 
# - for CO2
gefCO2 <- gasCO2/gasE
gasCO2 <- gas[4:51]*gefCO2  
# - for CH4 
gefCH4 <- gasCH4/gasE
gasCH4 <- gas[4:51]*gefCH4
# - for N2O
gefN2O <- gasN2O/gasE
gasN2O <- gas[4:51]*gefN2O
# - Total
gasTotal <- gasCO2 + gasCH4 + gasN2O
gasCarbon <- cbind(gas[1:3], gasTotal)

## Computing the carbon footprint for coal
# - for CO2
gefCO2 <- coalCO2/coalE
coalCO2 <- coal[4:51]*gefCO2
# - for CH4
gefCH4 <- coalCH4/coalE
coalCH4 <- coal[4:51]*gefCH4
# - for N2O
gefN2O <- coalN2O/coalE
coalN2O <- coal[4:51]*gefN2O
# - Total 
coalTotal <- coalCO2 + coalCH4 + coalN2O
coalCarbon <- cbind(coal[1:3], coalTotal)

## Computing the carbon footprint for geo 
# - for CO2
gefCO2 <- 0.123 # geoCO2/geoE
geoCO2 <- geo[4:51]*gefCO2
#geoSumCO2 <- colSums(geoCO2, na.rm=T)
geoTotal <- geoCO2
geoCarbon <- cbind(geo[1:3], geoTotal)

## Computing the carbon footprint for hydro
gefCO2 <- hydroCO2/hydroE
hydroCO2 <- hydro[4:51]*gefCO2
#geoSumCO2 <- colSums(geoCO2, na.rm=T)
hydroTotal <- hydroCO2
hydroCarbon <- cbind(hydro[1:3], hydroTotal)

## Computing the carbon footprint for wind
gefCO2 <- windCO2/windE
windCO2 <- wind[4:51]*gefCO2
#geoSumCO2 <- colSums(geoCO2, na.rm=T)
windTotal <- windCO2
windCarbon <- cbind(wind[1:3], windTotal)

## Combining the 4 carbon footprint datasets that were calculated above
if (file.exists("Carbon_Footprint_Data.csv")) file.remove("Carbon_Footprint_Data.csv")
carbonData <- rbind(gasCarbon, geoCarbon, hydroCarbon, windCarbon, coalCarbon)
write.csv(carbonData, "Carbon_Footprint_Data.csv")

(processingCO2e <- proc.time() - startCO2e)

##### Calculating the carbon intensities #####
### SKIP THIS BIT UP TO ... ###
# Carbon footprint data #  
csafeWd <- "C:/Users/maril/OneDrive/Centre for Sustainability SS/CSAFE Data and Analysis"
carbonData <- read.csv("Carbon_Footprint_Data.csv", header = T, stringsAsFactors = F)
# subsetting the data into seperate datasets for each of the 4 relevant fuels
gasCarbon <- carbonData[carbonData$Fuel_Code=="Gas", 2:52]
geoCarbon <- carbonData[carbonData$Fuel_Code=="Geo", 2:52]
hydroCarbon <- carbonData[carbonData$Fuel_Code=="Hydro", 2:52]
windCarbon <- carbonData[carbonData$Fuel_Code=="Wind", 2:52]
coalCarbon <- carbonData[carbonData$Fuel_Code=="Coal", 2:52]
### ... HERE ###

years <- seq(2006, 2017)
# years <- seq(1998, 2017)
n <- length(years)
yearlyCI <- rep(0, n) # the total yearly carbon intensity for each year will be stored in this
months <- seq(1, 12)
monthlyCI <- data.frame(year=rep(0, (n*12)), month=rep(0, (n*12)), ci=rep(0, (n*12)), xVals = seq(1, n*12)) 
# the monthly CI for each year will be stored in this
dailyCI <- data.frame(yr=0, mth=0, d=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0, tot=0)

# The following for-loop is used to compute the overall carbon intensity #
# for each day of the 20 full years' data that we have # 
startCI <- proc.time()

# Daily CI
trck <- 1
for (i in 1:n){
  for (k in 1:12){
    xY <- gas[year(gas$Trading_date) == years[i], ]
    xM <- xY[month(xY$Trading_date) == months[k], ]
    days <- unique(xM$Trading_date, na.rm = T)
    days <- days[!is.na(days)]
    m <- length(days)
    
    for (j in 1:m){
      dailyCI[trck, 1] <- years[i]; dailyCI[trck, 2] <- months[k]; dailyCI[trck, 3] <- j
      tmp <- genDaily(genData, years[i], months[k], j)
      totalG <- sum(tmp, na.rm=T)
      
      tmp1 <- sum(gasE + geoE + hydroE + windE, na.rm=T) 
      
      dailyCI[trck, 5] <- sum(genDaily(gasCarbon, years[i], months[k], j), na.rm=T)
      dailyCI[trck, 6] <- sum(genDaily(geoCarbon, years[i], months[k], j), na.rm=T)
      dailyCI[trck, 7] <- sum(genDaily(hydroCarbon, years[i], months[k], j), na.rm=T)
      dailyCI[trck, 8] <- sum(genDaily(windCarbon, years[i], months[k], j), na.rm=T)
      dailyCI[trck, 9] <- sum(genDaily(coalCarbon, years[i], months[k], j), na.rm=T)
      
      dailyCI[trck, 10] <- totalG
      # updating trck
      trck <- trck + 1
    }
  }
}

dailyCI$ci <- (dailyCI$gas+dailyCI$geo+dailyCI$hydro+dailyCI$wind+dailyCI$coal)/dailyCI$tot*1000

## Now that we have the daily carbon intenisties for each day of the years 1998-2017, 
## we can compute the monthly and yearly average carbon intensities. 
years <- seq(2006, 2017)
#years <- seq(1998, 2017)
n <- length(years)

# Monthly
months <- seq(1, 12)
monthlyCI <- data.frame(year=rep(0, (n*12)), month=rep(0, (n*12)), ci=rep(0, (n*12)), 
                        xVals = seq(1998, 2018-1/12, 1/12))

for (i in 1:n){
  for (j in 1:12){
    monthlyCI$year[j + (i-1)*12] <- years[i]
    monthlyCI$month[j+(i-1)*12] <- j 
    
    monthlyCI$ci[j+(i-1)*12] <- mean(dailyCI$ci[dailyCI$yr==years[i] & dailyCI$mth==j])
  }
}
# Yearly 
yearlyCI <- rep(0, n) # the total yearly carbon intensity for each year will be stored in this
for (i in 1:n){
  yearlyCI[i] <- mean(dailyCI$ci[dailyCI$yr==years[i]])
}

(processingCI <- proc.time() - startCI)

##### Creating plots #####
## Plot of yearly CI ##
yearDF <- data.frame(yrs = years, ci = yearlyCI)
(yearPlot <- ggplot(yearDF, aes(x=yrs, y=ci)) + geom_point(shape=19, size=1) + 
    geom_line(linetype=4) + # ggtitle("Carbon Intensity (gCO2-e/kWh) for the years 1998-2017") + 
    labs(x="Year", y="Carbon Intensity (gCO2-e/kWh)")
)

## Plot of monthly CI without yearly CI superimposed ##
monthPlot1 <- ggplot(monthlyCI, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="red") + geom_line() + 
  ggtitle("Monthly Carbon Intensity (gCO2-e/kWh) for the years 1998-2017") + 
  labs(x="Month", y="Carbon Intensity (gCO2-e/kWh)")
  
# The following for-loop creates the line-segments needed to distinguish between the years #
for (i in 1998:2017){
  xVal <- i
  monthPlot1 <- monthPlot1 + geom_vline(xintercept=xVal, linetype=4) + 
    geom_text(aes_string(x=i+0.5, y=15, label=as.character(i)), size=3.5)
} # for-loop
(monthPlot1 <- monthPlot1 + geom_vline(xintercept=2018, linetype=4))

## Plot of monthly CI with yearly CI superimposed ##
years <- seq(2006, 2017)
n <- length(years)
monthPlot2 <- ggplot(monthlyCI, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line() + 
    labs(x="Month", y="Carbon Intensity (gCO2-e/kWh)")

yrs <- rep(yearlyCI[1], 12)
for (i in 2:n){
  yrs <- c(yrs, rep(yearlyCI[i], 12))
}
yrDF <- data.frame(xV = seq(years[1], 2018-1/12, 1/12), yV = yrs)

(monthPlot2 <- monthPlot2 + geom_line(aes(x=yrDF$xV, y=yrDF$yV), linetype=5, colour="red"))

## Plot of daily CI for every day of the month with monthly and yearly CI superimposed ##
# 1998
t <- dailyCI$ci[dailyCI$yr==1998]
xV <- seq(1, length(t))
dat1998 <- data.frame(xVals=xV, ci=dailyCI$ci[dailyCI$yr==1998])
mnthCI <- monthlyCI$ci[monthlyCI$year==1998]
tmp <- c(rep(mnthCI[1], 31), rep(mnthCI[2], 28))
odd <- c(3, 5, 7, 8, 10, 12)
even <- c(4, 6, 9, 11)
for (i in 3:12){
  if (i%in%odd){
    tmp <- c(tmp, rep(mnthCI[i], 31))
  } else {
    tmp <- c(tmp, rep(mnthCI[i], 30))
  }
}
ciMonth <- data.frame(xVals=xV, ci=tmp)
ciYr <- data.frame(xVals=xV, ci=rep(yearlyCI[1], length(t)))

dayPlot1998 <- ggplot(dat1998, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line()+ 
  ggtitle("Daily Carbon Intensity (gCO2-e/kWh) for the year 1998 
            with Yearly Carbon Intensity superimposed (red)
            and Monthly Carbon Intensity superimposed (blue)") + 
  labs(x="Day", y="Carbon Intensity (gCO2-e/kWh)") + 
  geom_line(aes(x=ciYr$xVals, y=ciYr$ci), linetype=5, colour="red") +
  geom_line(aes(x=ciMonth$xVals, y=ciMonth$ci), linetype=5, colour="blue")
dayPlot1998
# 2001
t <- dailyCI$ci[dailyCI$yr==2001]
xV <- seq(1, length(t))
dat2001 <- data.frame(xVals=xV, ci=dailyCI$ci[dailyCI$yr==2001])
mnthCI <- monthlyCI$ci[monthlyCI$year==2001]
tmp <- c(rep(mnthCI[1], 31), rep(mnthCI[2], 28))
odd <- c(3, 5, 7, 8, 10, 12)
even <- c(4, 6, 9, 11)
for (i in 3:12){
  if (i%in%odd){
    tmp <- c(tmp, rep(mnthCI[i], 31))
  } else {
    tmp <- c(tmp, rep(mnthCI[i], 30))
  }
}
ciMonth <- data.frame(xVals=xV, ci=tmp)
ciYr <- data.frame(xVals=xV, ci=rep(yearlyCI[4], length(t)))

dayPlot2001<- ggplot(dat2001, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line()+ 
  ggtitle("Daily Carbon Intensity (gCO2-e/kWh) for the year 2001 
            with Yearly Carbon Intensity superimposed (red)
            and Monthly Carbon Intensity superimposed (green)") + 
  labs(x="Day", y="Carbon Intensity (gCO2-e/kWh)") + 
  geom_line(aes(x=ciYr$xVals, y=ciYr$ci), linetype=5, colour="red") +
  geom_line(aes(x=ciMonth$xVals, y=ciMonth$ci), linetype=5, colour="blue")
dayPlot2001
# 2004 
t <- dailyCI$ci[dailyCI$yr==2004]
xV <- seq(1, length(t))
dat2004 <- data.frame(xVals=xV, ci=dailyCI$ci[dailyCI$yr==2004])
mnthCI <- monthlyCI$ci[monthlyCI$year==2004]
tmp <- c(rep(mnthCI[1], 31), rep(mnthCI[2], 29))
odd <- c(3, 5, 7, 8, 10, 12)
even <- c(4, 6, 9, 11)
for (i in 3:12){
  if (i%in%odd){
    tmp <- c(tmp, rep(mnthCI[i], 31))
  } else {
    tmp <- c(tmp, rep(mnthCI[i], 30))
  }
}
ciMonth <- data.frame(xVals=xV, ci=tmp)
ciYr <- data.frame(xVals=xV, ci=rep(yearlyCI[7], length(t)))

dayPlot2004 <- ggplot(dat2004, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line()+ 
  ggtitle("Daily Carbon Intensity (gCO2-e/kWh) for the year 2004
          with Yearly Carbon Intensity superimposed (red)
          and Monthly Carbon Intensity superimposed (green)") + 
  labs(x="Day", y="Carbon Intensity (gCO2-e/kWh)") + 
  geom_line(aes(x=ciYr$xVals, y=ciYr$ci), linetype=5, colour="red") +
  geom_line(aes(x=ciMonth$xVals, y=ciMonth$ci), linetype=5, colour="blue")
dayPlot2004
# 2007 
t <- dailyCI$ci[dailyCI$yr==2007]
xV <- seq(1, length(t))
dat2007 <- data.frame(xVals=xV, ci=dailyCI$ci[dailyCI$yr==2007])
mnthCI <- monthlyCI$ci[monthlyCI$year==2007]
tmp <- c(rep(mnthCI[1], 31), rep(mnthCI[2], 28))
odd <- c(3, 5, 7, 8, 10, 12)
even <- c(4, 6, 9, 11)
for (i in 3:12){
  if (i%in%odd){
    tmp <- c(tmp, rep(mnthCI[i], 31))
  } else {
    tmp <- c(tmp, rep(mnthCI[i], 30))
  }
}
ciMonth <- data.frame(xVals=xV, ci=tmp)
ciYr <- data.frame(xVals=xV, ci=rep(yearlyCI[14], length(t)))

dayPlot2007 <- ggplot(dat2007, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line()+ 
  ggtitle("Daily Carbon Intensity (gCO2-e/kWh) for the year 2007 
          with Yearly Carbon Intensity superimposed (red)
          and Monthly Carbon Intensity superimposed (green)") + 
  labs(x="Day", y="Carbon Intensity (gCO2-e/kWh)") + 
  geom_line(aes(x=ciYr$xVals, y=ciYr$ci), linetype=5, colour="red") +
  geom_line(aes(x=ciMonth$xVals, y=ciMonth$ci), linetype=5, colour="blue")
dayPlot2007
# 2011
t <- dailyCI$ci[dailyCI$yr==2011]
xV <- seq(1, length(t))
dat2011 <- data.frame(xVals=xV, ci=dailyCI$ci[dailyCI$yr==2011])
mnthCI <- monthlyCI$ci[monthlyCI$year==2011]
tmp <- c(rep(mnthCI[1], 31), rep(mnthCI[2], 28))
odd <- c(3, 5, 7, 8, 10, 12)
even <- c(4, 6, 9, 11)
for (i in 3:12){
  if (i%in%odd){
    tmp <- c(tmp, rep(mnthCI[i], 31))
  } else {
    tmp <- c(tmp, rep(mnthCI[i], 30))
  }
}
ciMonth <- data.frame(xVals=xV, ci=tmp)
ciYr <- data.frame(xVals=xV, ci=rep(yearlyCI[14], length(t)))

dayPlot2011<- ggplot(dat2011, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line()+ 
  ggtitle("Daily Carbon Intensity (gCO2-e/kWh) for the year 2011 
          with Yearly Carbon Intensity superimposed (red)
          and Monthly Carbon Intensity superimposed (green)") + 
  labs(x="Day", y="Carbon Intensity (gCO2-e/kWh)") + 
  geom_line(aes(x=ciYr$xVals, y=ciYr$ci), linetype=5, colour="red") +
  geom_line(aes(x=ciMonth$xVals, y=ciMonth$ci), linetype=5, colour="blue")
dayPlot2011
# 2014
t <- dailyCI$ci[dailyCI$yr==2014]
xV <- seq(1, length(t))
dat2014 <- data.frame(xVals=xV, ci=dailyCI$ci[dailyCI$yr==2014])
mnthCI <- monthlyCI$ci[monthlyCI$year==2014]
tmp <- c(rep(mnthCI[1], 31), rep(mnthCI[2], 28))
odd <- c(3, 5, 7, 8, 10, 12)
even <- c(4, 6, 9, 11)
for (i in 3:12){
  if (i%in%odd){
    tmp <- c(tmp, rep(mnthCI[i], 31))
  } else {
    tmp <- c(tmp, rep(mnthCI[i], 30))
  }
}
ciMonth <- data.frame(xVals=xV, ci=tmp)
ciYr <- data.frame(xVals=xV, ci=rep(yearlyCI[17], length(t)))

dayPlot2014<- ggplot(dat2014, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line()+ 
  ggtitle("Daily Carbon Intensity (gCO2-e/kWh) for the year 2014 
          with Yearly Carbon Intensity superimposed (red)
          and Monthly Carbon Intensity superimposed (green)") + 
  labs(x="Day", y="Carbon Intensity (gCO2-e/kWh)") + 
  geom_line(aes(x=ciYr$xVals, y=ciYr$ci), linetype=5, colour="red") +
  geom_line(aes(x=ciMonth$xVals, y=ciMonth$ci), linetype=5, colour="blue")
dayPlot2014
# 2017 
t <- dailyCI$ci[dailyCI$yr==2017]
xV <- seq(1, length(t))
dat2017 <- data.frame(xVals=xV, ci=dailyCI$ci[dailyCI$yr==2017])
mnthCI <- monthlyCI$ci[monthlyCI$year==2017]
tmp <- c(rep(mnthCI[1], 31), rep(mnthCI[2], 28))
odd <- c(3, 5, 7, 8, 10, 12)
even <- c(4, 6, 9, 11)
for (i in 3:12){
  if (i%in%odd){
    tmp <- c(tmp, rep(mnthCI[i], 31))
  } else {
    tmp <- c(tmp, rep(mnthCI[i], 30))
  }
}
ciMonth <- data.frame(xVals=xV, ci=tmp)
ciYr <- data.frame(xVals=xV, ci=rep(yearlyCI[20], length(t)))

dayPlot2017<- ggplot(dat2017, aes(x=xVals, y = ci)) + 
  geom_point(shape=19, size=1, colour="black") + geom_line()+ 
  ggtitle("Daily Carbon Intensity (gCO2-e/kWh) for the year 2017 
          with Yearly Carbon Intensity superimposed (red)
          and Monthly Carbon Intensity superimposed (green)") + 
  labs(x="Day", y="Carbon Intensity (gCO2-e/kWh)") + 
  geom_line(aes(x=ciYr$xVals, y=ciYr$ci), linetype=5, colour="red") +
  geom_line(aes(x=ciMonth$xVals, y=ciMonth$ci), linetype=5, colour="blue")
dayPlot2017 

## Fitting some models to the monthly average of the carbon intensities
yearCI <- data.frame(year=seq(2006, 2017), ci=yearlyCI)
# yearCI <- data.frame(year=seq(1998, 2017), ci=yearlyCI)

monthSimpleMod <- lm(ci~year, data=monthlyCI)
(monthSimpleSum <- summary(monthSimpleMod))

yearSimpleMod <- lm(ci~year, data=yearCI)
(yearSimpleSum <- summary(monthSimpleMod))

## Fitting a model with months (dummy variables) ##
monthlyCI1 <- monthlyCI
monthlyCI1$month <- as.factor(monthlyCI1$month)
monthMod <- lm(ci~year + month, data=monthlyCI1)
(monthSum <- summary(monthMod))

## Fitting a model with seasons (dummy variables) ##
monthlyCI2 <- monthlyCI
monthlyCI2$season <- "Summer"
monthlyCI2$season[monthlyCI2$month==3 | monthlyCI2$month==4 | monthlyCI2$month==5] <- "Autumn"
monthlyCI2$season[monthlyCI2$month==6 | monthlyCI2$month==7 | monthlyCI2$month==8] <- "Winter"
monthlyCI2$season[monthlyCI2$month==9 | monthlyCI2$month==10 | monthlyCI2$month==11] <- "Spring"
monthlyCI2$season <- as.factor(monthlyCI2$season)
monthlyCI2$season <- relevel(monthlyCI2$season, ref="Summer")
seasonMod <- lm(ci~year+season, data=monthlyCI2)
(seasonSum <- summary(seasonMod))

## Modelling association between demand and carbon intensity ##
years <- seq(2006, 2017)
n <- length(years)
# years <- seq(1998, 2017)
totalDemand <- data.frame(matrix(vector(), 0, 6, 
                                 dimnames=list(c(), c("yr", "mth", "season", 
                                                      "morning", "evening", "total"))), 
                          stringsAsFactors=FALSE)
for (k in 1:n){
  seasons <- c("Spring", "Summer", "Autumn", "Winter")
  for (i in 1:12){
    totTmp <- genData[year(genData$Trading_date)==years[k] & 
                        month(genData$Trading_date)==i, ]
    mornTmp <- genData[year(genData$Trading_date)==years[k] & 
                         month(genData$Trading_date)==i, 18:25]
    eveTmp <- genData[year(genData$Trading_date)==years[k] & 
                        month(genData$Trading_date)==i, 38:45]
    ind <- i + (k-1)*12
    totalDemand[ind, 1] <- years[k]; totalDemand[ind, 2] <- i
    totalDemand[ind, 4] <- sum(colSums(mornTmp, na.rm = T))/1000000
    totalDemand[ind, 5] <- sum(colSums(eveTmp, na.rm = T))/1000000
    totalDemand[ind, 6] <- sum(colSums(totTmp[, 4:51], na.rm = T))/1000000
  } # for (i in 1:12)
} # for (k in 1:n)

totalDemand$season <- "Summer"
totalDemand$season[totalDemand$mth==3 | totalDemand$mth==4 | totalDemand$mth==5] <- "Autumn"
totalDemand$season[totalDemand$mth==6 | totalDemand$mth==7 | totalDemand$mth==8] <- "Winter"
totalDemand$season[totalDemand$mth==9 | totalDemand$mth==10 | totalDemand$mth==11] <- "Spring"
totalDemand$season <- as.factor(totalDemand$season)
totalDemand$season <- relevel(totalDemand$season, ref="Summer")

ci <- monthlyCI$ci
totalDemand <- cbind(totalDemand, ci)
head(totalDemand)
totalDemand$totalS <- scale(totalDemand$total)
totalDemand$yrS <- scale(totalDemand$yr)

demandYrModel <- lm(ci~yr + total, data=totalDemand)
(demandYrSum <- summary(demandYrModel))
demandYrStModel <- lm(ci~yrS + totalS, data=totalDemand)
(demandYrStSum <- summary(demandYrStModel))
demandModel <- lm(ci~total, data=totalDemand)
(demandSum <- summary(demandModel))

demandSeasonModel <- lm(ci~total + season, data=totalDemand)
(demandSeasonSum <- summary(demandSeasonModel))
AIC(demandSeasonModel)

totalDemand1 <- totalDemand
totalDemand1$mth <- as.factor(totalDemand1$mth)
demandMthModel <- lm(ci~total + mth, data=totalDemand1)
(demandMthSum <- summary(demandMthModel))
AIC(demandMthModel)

yearlyDemand <- data.frame(matrix(
  vector(), 0, 4, dimnames=list(c(), c("yr", "morning", "evening", "total"))), 
  stringsAsFactors=FALSE)
for (i in 1:n){
  yearlyDemand[i, 1] <- years[i]
  yearlyDemand[i, 2:4] <- colSums(totalDemand[ totalDemand$yr==years[i], 4:6])
}

(pltDemYr <- ggplot() + geom_point(aes(x=yearlyDemand$yr, y=yearlyDemand$total)) + 
    labs(x="Year", y="Demand"))
cor(yearlyDemand$yr, yearlyDemand$total)

## Adding in whether a winter was dry or not each year ##
monthlyCI3 <- monthlyCI2
monthlyCI3$dry <- 0
dryWinters <- c(2005, 2008, 2012)
monthlyCI3$dry[monthlyCI3$year %in% dryWinters] <- 1
dryMod <- lm(ci~year+season+dry, data=monthlyCI3)
(drySum <- summary(dryMod))
fVals <- fitted(dryMod)

fittedDF <- data.frame(xVals = seq(1998, 2018-1/12, 1/12), pred = fVals)
(monthPlot3 <- monthPlot2 + geom_line(aes(x=seq(1998, 2018-1/12, 1/12), y=fVals), colour = "blue", 
                                     linetype=8))
# Add coal and geothermal - DONE
# Include a stack plot
## Creating dataframes that contain the separated carbon intensities for each of the fuels
# - daily
dailyCISep <- dailyCI
dailyCISep$gas <- dailyCISep$gas/dailyCISep$tot*1000
dailyCISep$geo <- dailyCISep$geo/dailyCISep$tot*1000
dailyCISep$hydro <- dailyCISep$hydro/dailyCISep$tot*1000
dailyCISep$wind <- dailyCISep$wind/dailyCISep$tot*1000
dailyCISep$coal <- dailyCISep$coal/dailyCISep$tot*1000
# - monthly and yearly

years <- seq(2006, 2017)
# years <- seq(1998, 2017)
n <- length(years)

monthlyCISep <- data.frame(yr=0, mth=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0)
trck <- 1

for (i in 2006:2017){
# for (i in 1998:2017){
  for (j in 1:12){
    monthlyCISep[trck, 1] <- i
    monthlyCISep[trck, 2] <- j 
    
    monthlyCISep[trck, 3] <- mean(dailyCISep$ci[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 4] <- mean(dailyCISep$gas[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 5] <- mean(dailyCISep$geo[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 6] <- mean(dailyCISep$hydro[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 7] <- mean(dailyCISep$wind[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 8] <- mean(dailyCISep$coal[dailyCISep$yr==i & dailyCISep$mth==j])
    
    trck <- trck + 1
  }
}
# Yearly 
yearlyCISep <- data.frame(yr=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0)
for (i in 1:n){
  yearlyCISep[i, 1] <- years[i]
  yearlyCISep[i, 2] <- mean(dailyCISep$ci[dailyCISep$yr==years[i]])
  yearlyCISep[i, 3] <- mean(dailyCISep$gas[dailyCISep$yr==years[i]])
  yearlyCISep[i, 4] <- mean(dailyCISep$geo[dailyCISep$yr==years[i]])
  yearlyCISep[i, 5] <- mean(dailyCISep$hydro[dailyCISep$yr==years[i]])
  yearlyCISep[i, 6] <- mean(dailyCISep$wind[dailyCISep$yr==years[i]])
  yearlyCISep[i, 7] <- mean(dailyCISep$coal[dailyCISep$yr==years[i]])
}

# Look at the half-hourly plots - 4pm to 8pm (for every year)
# heatmap plot
# treat year as a factor
# do a time series analysis
# rainfall/flow and min temps
# add yearly(monthly/daily) boxplots
# ggtimeseries package may be helpful with visualisations

# we observe a peak in 2008

### Making plots with carbon footprint divided into coal, geo, and gas ###
## Yearly ##

plotSep <- yearlyCISep
plotSep$geo <- plotSep$gas + plotSep$geo
plotSep$coal <- plotSep$coal + plotSep$geo

yearSep <- ggplot(plotSep, aes(x, y)) + 
  geom_area(aes(x=yr, y=coal), fill="chartreuse4") + 
  geom_area(aes(x=yr, y=geo), fill="grey33") + 
  geom_area(aes(x=yr, y=gas), fill="purple") + 
  #ggtitle("Yearly carbon intensity(gCO2-e/kWh) for the years 1998-2017") + 
  labs(y="Carbon intensity (gCO2-e/kWh)", x="Year")  + 
  annotate("text", x = 2013, 
           y = 200, size = 8, color = "chartreuse4",
           label = "Coal", hjust = 0) + 
  annotate("text", x = 2013, # x axis is just a number not a real date
           y = 180,size = 8, color = "grey33",
           label = "Geothermal", hjust = 0) + 
  annotate("text", x = 2013, # x axis is just a number not a real date
           y = 160, size = 8, color="purple", 
           label = "Gas", hjust = 0)
yearSep  

## Monthly

plotSep <- monthlyCISep
plotSep$geo <- plotSep$gas + plotSep$geo
plotSep$coal <- plotSep$coal + plotSep$geo

tmp <- nrow(plotSep)
plotSep$xVals <- seq(1, tmp)

monthSep <- ggplot(plotSep, aes(x, y)) + 
  geom_area(aes(x=xVals, y=coal), fill="chartreuse4") + 
  geom_area(aes(x=xVals, y=geo), fill="grey33") + 
  geom_area(aes(x=xVals, y=gas), fill="purple") +
  # ggtitle("Monthly Carbon Intensity (gCO2-e/kWh) for the years 1998-2017") + 
  labs(x="Month", y="Carbon Intensity (gCO2-e/kWh)") + 
  #theme(axis.text.x=element_blank()) + 
  annotate("text", x = 100, 
           y = 250, size = 8, color = "chartreuse4",
           label = "Coal", hjust = 0) + 
  annotate("text", x = 100, # x axis is just a number not a real date
           y = 230,size = 8, color = "grey33",
           label = "Geothermal", hjust = 0) + 
  annotate("text", x = 100, # x axis is just a number not a real date
           y = 210, size = 8, color="purple", 
           label = "Gas", hjust = 0)
monthSep

## Daily

daySepFunc <- function(datDay=dailyCISep, y){
  plotSep <- datDay[datDay$yr==y, ]
  plotSep$geo <- plotSep$gas + plotSep$geo
  plotSep$coal <- plotSep$coal + plotSep$geo
  
  t <- c(2000, 2004, 2008, 2012, 2016)
  if (y %in% t){
    plotSep$xVals <- seq(1, 366)
  } else { 
    plotSep$xVals <- seq(1, 365)
  }
  
  plt <- ggplot(plotSep, aes(x, y)) + 
    geom_area(aes(x=xVals, y=coal), fill="chartreuse4") + 
    geom_area(aes(x=xVals, y=geo), fill="grey33") + 
    geom_area(aes(x=xVals, y=gas), fill="purple") +
    ggtitle(y) + 
    labs(x="Day", y="gCO2-e/kWh") 
  return(plt)
}

for (i in 1:n){
  tmp <- daySepFunc(y=years[i])
  nam <- paste("plotSep", years[i], sep="")
  assign(nam, tmp)
}

#ggarrange(plotSep1998, plotSep1999, plotSep2000, plotSep2001, 
#          plotSep2002, plotSep2003, plotSep2004, plotSep2005, 
ggarrange(plotSep2006, plotSep2007, plotSep2008, plotSep2009, 
          plotSep2010, plotSep2011, plotSep2012, plotSep2013, 
          plotSep2014, plotSep2015, plotSep2016, plotSep2017, 
          nrow=3, ncol=4)

##### Running a time series analysis #####

# converting my dataframe into a form that is compatible with ts()
cis <- monthlyCI$ci
monthly <- ts(cis, frequency = 12, start=c(2006, 1))
#monthly <- ts(cis, frequency=12, start=c(1998, 1))
plot.ts(monthly) # basic plot of the data

SMA(monthly, n=12)

# decomposing the time series
monthlyTS <- decompose(monthly)
plot(monthlyTS)

# making forecasts, to find the estimated fit and the acf
monthlyModel <- HoltWinters(log(monthly)) # exponential smoothing
monthlyModel
# alpha = 0.8415615 >> estimate of the level at current time is based on recent and distant past observations
# beta = 0 >> the estimated slope of the trend is not updated with the time series
# gamma = 1 >> estimate of the seasonal component is based on recent and distant observations

monthlyModel$SSE
plot(monthlyModel)

monthlyFC <- forecast(monthlyModel, gamma=FALSE)

monthlyResids <- monthlyFC$residuals
acf(monthly, lag.max=48, na.action=na.pass)
pacf(monthly, lag.max=240, na.action=na.pass)

# checking whether the predictive model can be imporved
# acf(monthlyForecasts$)

##### Drawing separated plots for the peak times #####
genDailya <- function(fuel, yr, mth, d){
  if (mth < 10){
    mth <- paste("0", mth, sep="")
  }
  if (d < 10){
    d <- paste("0", d, sep="")
  }
  dt <- paste(yr, mth, d, sep="-")
  datDay <- fuel[fuel$Trading_date==dt, 4:11]
  selectGen <- !is.na(datDay[, 4])
  datDay <- datDay[selectGen, ]
  dayGen <- colSums(datDay, na.rm = T)
  return(dayGen)
} # genDailya
## Morning ##

years <- seq(2006, 2017)
# years <- seq(1998, 2017)
n <- length(years)
yearlyCIa <- rep(0, n) # the total yearly carbon intensity for each year will be stored in this
months <- seq(1, 12)
monthlyCIa <- data.frame(year=rep(0, (n*12)), month=rep(0, (n*12)), ci=rep(0, (n*12)), xVals = seq(1, n*12)) 
# the monthly CI for each year will be stored in this
dailyCIa <- data.frame(yr=0, mth=0, d=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0, tot=0)

# The following for-loop is used to compute the overall carbon intensity #
# for each day of the 20 full years' data that we have # 
startCIa <- proc.time()

# Daily CI
trck <- 1
for (i in 1:n){
  for (k in 1:12){
    xY <- gas[year(gas$Trading_date) == years[i], ]
    xM <- xY[month(xY$Trading_date) == months[k], ]
    days <- unique(xM$Trading_date, na.rm = T)
    days <- days[!is.na(days)]
    m <- length(days)
    
    gasC <- gasCarbon[, c(1:3, 18:25)]
    geoC <- geoCarbon[, c(1:3, 18:25)]
    hydroC <- hydroCarbon[, c(1:3, 18:25)]
    windC <- windCarbon[, c(1:3, 18:25)]
    coalC <- coalCarbon[, c(1:3, 18:25)]
    for (j in 1:m){
      dailyCIa[trck, 1] <- years[i]; dailyCIa[trck, 2] <- months[k]; dailyCIa[trck, 3] <- j
      tmp <- genDailya(genData, years[i], months[k], j)
      totalG <- sum(tmp, na.rm=T)
      
      tmp1 <- sum(gasE + geoE + hydroE + windE, na.rm=T) 
      
      dailyCIa[trck, 5] <- sum(genDailya(gasC, years[i], months[k], j), na.rm=T)
      dailyCIa[trck, 6] <- sum(genDailya(geoC, years[i], months[k], j), na.rm=T)
      dailyCIa[trck, 7] <- sum(genDailya(hydroC, years[i], months[k], j), na.rm=T)
      dailyCIa[trck, 8] <- sum(genDailya(windC, years[i], months[k], j), na.rm=T)
      dailyCIa[trck, 9] <- sum(genDailya(coalC, years[i], months[k], j), na.rm=T)
      
      dailyCIa[trck, 10] <- totalG
      # updating trck
      trck <- trck + 1
    }
  }
}

dailyCIa$ci <- (dailyCIa$gas+dailyCIa$geo+dailyCIa$hydro+dailyCIa$wind+dailyCIa$coal)/dailyCIa$tot*1000

## Now that we have the daily carbon intenisties for each day of the years 1998-2017, 
## we can compute the monthly and yearly average carbon intensities. 
years <- seq(2006, 2017)
# years <- seq(1998, 2017)
n <- length(years)

# Monthly
months <- seq(1, 12)
monthlyCIa <- data.frame(year=rep(0, (n*12)), month=rep(0, (n*12)), ci=rep(0, (n*12)), 
                        xVals = seq(1998, 2018-1/12, 1/12))

for (i in 1:n){
  for (j in 1:12){
    monthlyCIa$year[j + (i-1)*12] <- years[i]
    monthlyCIa$month[j+(i-1)*12] <- j 
    
    monthlyCIa$ci[j+(i-1)*12] <- mean(dailyCIa$ci[dailyCIa$yr==years[i] & dailyCIa$mth==j])
  }
}
# Yearly 
yearlyCIa <- rep(0, n) # the total yearly carbon intensity for each year will be stored in this
for (i in 1:n){
  yearlyCIa[i] <- mean(dailyCIa$ci[dailyCIa$yr==years[i]])
}

(processingCI <- proc.time() - startCI)

## Creating dataframes that contain the separated carbon intensities for each of the fuels
# - daily
dailyCISep <- dailyCIa
dailyCISep$gas <- dailyCISep$gas/dailyCISep$tot*1000
dailyCISep$geo <- dailyCISep$geo/dailyCISep$tot*1000
dailyCISep$hydro <- dailyCISep$hydro/dailyCISep$tot*1000
dailyCISep$wind <- dailyCISep$wind/dailyCISep$tot*1000
dailyCISep$coal <- dailyCISep$coal/dailyCISep$tot*1000
# - monthly and yearly

years <- seq(2006, 2017)
# years <- seq(1998, 2017)
n <- length(years)

monthlyCISep <- data.frame(yr=0, mth=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0)
trck <- 1

for (i in 2006:2017){
# for (i in 1998:2017){
  for (j in 1:12){
    monthlyCISep[trck, 1] <- i
    monthlyCISep[trck, 2] <- j 
    
    monthlyCISep[trck, 3] <- mean(dailyCISep$ci[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 4] <- mean(dailyCISep$gas[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 5] <- mean(dailyCISep$geo[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 6] <- mean(dailyCISep$hydro[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 7] <- mean(dailyCISep$wind[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 8] <- mean(dailyCISep$coal[dailyCISep$yr==i & dailyCISep$mth==j])
    
    trck <- trck + 1
  }
}
# Yearly 
yearlyCISep <- data.frame(yr=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0)
for (i in 1:n){
  yearlyCISep[i, 1] <- years[i]
  yearlyCISep[i, 2] <- mean(dailyCISep$ci[dailyCISep$yr==years[i]])
  yearlyCISep[i, 3] <- mean(dailyCISep$gas[dailyCISep$yr==years[i]])
  yearlyCISep[i, 4] <- mean(dailyCISep$geo[dailyCISep$yr==years[i]])
  yearlyCISep[i, 5] <- mean(dailyCISep$hydro[dailyCISep$yr==years[i]])
  yearlyCISep[i, 6] <- mean(dailyCISep$wind[dailyCISep$yr==years[i]])
  yearlyCISep[i, 7] <- mean(dailyCISep$coal[dailyCISep$yr==years[i]])
}

# Look at the half-hourly plots - 4pm to 8pm (for every year)
# heatmap plot
# treat year as a factor
# do a time series analysis
# rainfall/flow and min temps
# add yearly(monthly/daily) boxplots
# ggtimeseries package may be helpful with visualisations

# we observe a peak in 2008

### Making plots with carbon footprint divided into coal, geo, and gas ###
## Yearly ##

plotSep <- yearlyCISep
plotSep$geo <- plotSep$gas + plotSep$geo
plotSep$coal <- plotSep$coal + plotSep$geo

yearSep <- ggplot(plotSep, aes(x, y)) + 
  geom_area(aes(x=yr, y=coal), fill="chartreuse4") + 
  geom_area(aes(x=yr, y=geo), fill="grey33") + 
  geom_area(aes(x=yr, y=gas), fill="purple") + 
  # ggtitle("Yearly carbon intensity(gCO2-e/kWh) for the years 1998-2017 (morning peak)") + 
  labs(y="Carbon intensity (gCO2-e/kWh)", x="Year")  + 
  annotate("text", x = 2012, color="chartreuse4",
           y = 300,size = 8,
           label = "Coal", hjust = 0) + 
  annotate("text", x = 2012, # x axis is just a number not a real date
           y = 280, size = 8, color="grey33",
           label = "Geothermal", hjust = 0) + 
  annotate("text", x = 2012, color="purple",
           y = 260, size = 8,
           label = "Gas", hjust = 0)
yearSep  

## Monthly

plotSep <- monthlyCISep
plotSep$geo <- plotSep$gas + plotSep$geo
plotSep$coal <- plotSep$coal + plotSep$geo

tmp <- nrow(plotSep)
plotSep$xVals <- seq(1, tmp)

monthSep <- ggplot(plotSep, aes(x, y)) + 
  geom_area(aes(x=xVals, y=coal), fill="chartreuse4") + 
  geom_area(aes(x=xVals, y=geo), fill="grey33") + 
  geom_area(aes(x=xVals, y=gas), fill="purple") +
  # ggtitle("Monthly Carbon Intensity (gCO2-e/kWh) for the years 1998-2017 (morning peak)") + 
  labs(x="", y="Carbon Intensity (gCO2-e/kWh)") + 
  theme(axis.text.x=element_blank()) + 
  annotate("text", x = 1, 
           y = 400, size = 6, colour="chartreuse4",
           label = "Coal", hjust = 0) + 
  annotate("text", x = 1, # x axis is just a number not a real date
           y = 380, size = 6,  colour="grey33",
           label = "Geothermal", hjust = 0) + 
  annotate("text", x =1, # x axis is just a number not a real date
           y = 360, size = 6,  colour="purple",
           label = "Gas", hjust = 0)
monthSep

## Daily

daySepFunc <- function(datDay=dailyCISep, y){
  plotSep <- datDay[datDay$yr==y, ]
  plotSep$geo <- plotSep$gas + plotSep$geo
  plotSep$coal <- plotSep$coal + plotSep$geo
  
  t <- c(2000, 2004, 2008, 2012, 2016)
  if (y %in% t){
    plotSep$xVals <- seq(1, 366)
  } else { 
    plotSep$xVals <- seq(1, 365)
  }
  
  plt <- ggplot(plotSep, aes(x, y)) + 
    geom_area(aes(x=xVals, y=coal), fill="chartreuse4") + 
    geom_area(aes(x=xVals, y=geo), fill="grey33") + 
    geom_area(aes(x=xVals, y=gas), fill="purple") +
    ggtitle(y) + 
    labs(x="Day", y="gCO2-e/kWh") 
  return(plt)
}

for (i in 1:n){
  tmp <- daySepFunc(y=years[i])
  nam <- paste("plotSep", years[i], sep="")
  assign(nam, tmp)
}

#ggarrange(plotSep1998, plotSep1999, plotSep2000, plotSep2001, 
#          plotSep2002, plotSep2003, plotSep2004, plotSep2005, 
ggarrange(plotSep2006, plotSep2007, plotSep2008, plotSep2009, 
          plotSep2010, plotSep2011, plotSep2012, plotSep2013, 
          plotSep2014, plotSep2015, plotSep2016, plotSep2017, 
          nrow=5, ncol=4)


## Afternoon ##

years <- seq(2006, 2017)
# years <- seq(1998, 2017)
n <- length(years)
yearlyCIb <- rep(0, n) # the total yearly carbon intensity for each year will be stored in this
months <- seq(1, 12)
monthlyCIb <- data.frame(year=rep(0, (n*12)), month=rep(0, (n*12)), ci=rep(0, (n*12)), xVals = seq(1, n*12)) 
# the monthly CI for each year will be stored in this
dailyCIb <- data.frame(yr=0, mth=0, d=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0, tot=0)

# The following for-loop is used to compute the overall carbon intensity #
# for each day of the 20 full years' data that we have # 
startCIb <- proc.time()

# Daily CI
trck <- 1
for (i in 1:n){
  for (k in 1:12){
    xY <- gas[year(gas$Trading_date) == years[i], ]
    xM <- xY[month(xY$Trading_date) == months[k], ]
    days <- unique(xM$Trading_date, na.rm = T)
    days <- days[!is.na(days)]
    m <- length(days)
    
    gasC <- gasCarbon[, c(1:3, 38:45)]
    geoC <- geoCarbon[, c(1:3, 38:45)]
    hydroC <- hydroCarbon[, c(1:3, 38:45)]
    windC <- windCarbon[, c(1:3, 38:45)]
    coalC <- coalCarbon[, c(1:3, 38:45)]
    for (j in 1:m){
      dailyCIb[trck, 1] <- years[i]; dailyCIb[trck, 2] <- months[k]; dailyCIb[trck, 3] <- j
      tmp <- genDailya(genData, years[i], months[k], j)
      totalG <- sum(tmp, na.rm=T)
      
      tmp1 <- sum(gasE + geoE + hydroE + windE, na.rm=T) 
      
      dailyCIb[trck, 5] <- sum(genDailya(gasC, years[i], months[k], j), na.rm=T)
      dailyCIb[trck, 6] <- sum(genDailya(geoC, years[i], months[k], j), na.rm=T)
      dailyCIb[trck, 7] <- sum(genDailya(hydroC, years[i], months[k], j), na.rm=T)
      dailyCIb[trck, 8] <- sum(genDailya(windC, years[i], months[k], j), na.rm=T)
      dailyCIb[trck, 9] <- sum(genDailya(coalC, years[i], months[k], j), na.rm=T)
      
      dailyCIb[trck, 10] <- totalG
      # updating trck
      trck <- trck + 1
    }
  }
}

dailyCIb$ci <- (dailyCIb$gas+dailyCIb$geo+dailyCIb$hydro+dailyCIb$wind+dailyCIb$coal)/dailyCIb$tot*1000

## Now that we have the daily carbon intenisties for each day of the years 1998-2017, 
## we can compute the monthly and yearly average carbon intensities. 
years <- seq(2006, 2017)
#years <- seq(1998, 2017)
n <- length(years)

# Monthly
months <- seq(1, 12)
monthlyCIb <- data.frame(year=rep(0, (n*12)), month=rep(0, (n*12)), ci=rep(0, (n*12)), 
                         xVals = seq(2006, 2018-1/12, 1/12)) #seq(1998, 2018-1/12, 1/12))

for (i in 1:n){
  for (j in 1:12){
    monthlyCIb$year[j + (i-1)*12] <- years[i]
    monthlyCIb$month[j+(i-1)*12] <- j 
    
    monthlyCIb$ci[j+(i-1)*12] <- mean(dailyCIb$ci[dailyCIb$yr==years[i] & dailyCIb$mth==j])
  }
}
# Yearly 
yearlyCIb <- rep(0, n) # the total yearly carbon intensity for each year will be stored in this
for (i in 1:n){
  yearlyCIb[i] <- mean(dailyCIb$ci[dailyCIb$yr==years[i]])
}

(processingCI <- proc.time() - startCI)

## Creating dataframes that contain the separated carbon intensities for each of the fuels
# - daily
dailyCISep <- dailyCIb
dailyCISep$gas <- dailyCISep$gas/dailyCISep$tot*1000
dailyCISep$geo <- dailyCISep$geo/dailyCISep$tot*1000
dailyCISep$hydro <- dailyCISep$hydro/dailyCISep$tot*1000
dailyCISep$wind <- dailyCISep$wind/dailyCISep$tot*1000
dailyCISep$coal <- dailyCISep$coal/dailyCISep$tot*1000
# - monthly and yearly
years <- seq(2006, 2017)
#years <- seq(1998, 2017)
n <- length(years)

monthlyCISep <- data.frame(yr=0, mth=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0)
trck <- 1

for (i in 2006:2017){
# for (i in 1998:2017){
  for (j in 1:12){
    monthlyCISep[trck, 1] <- i
    monthlyCISep[trck, 2] <- j 
    
    monthlyCISep[trck, 3] <- mean(dailyCISep$ci[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 4] <- mean(dailyCISep$gas[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 5] <- mean(dailyCISep$geo[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 6] <- mean(dailyCISep$hydro[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 7] <- mean(dailyCISep$wind[dailyCISep$yr==i & dailyCISep$mth==j])
    monthlyCISep[trck, 8] <- mean(dailyCISep$coal[dailyCISep$yr==i & dailyCISep$mth==j])
    
    trck <- trck + 1
  }
}
# Yearly 
yearlyCISep <- data.frame(yr=0, ci=0, gas=0, geo=0, hydro=0, wind=0, coal=0)
for (i in 1:n){
  yearlyCISep[i, 1] <- years[i]
  yearlyCISep[i, 2] <- mean(dailyCISep$ci[dailyCISep$yr==years[i]])
  yearlyCISep[i, 3] <- mean(dailyCISep$gas[dailyCISep$yr==years[i]])
  yearlyCISep[i, 4] <- mean(dailyCISep$geo[dailyCISep$yr==years[i]])
  yearlyCISep[i, 5] <- mean(dailyCISep$hydro[dailyCISep$yr==years[i]])
  yearlyCISep[i, 6] <- mean(dailyCISep$wind[dailyCISep$yr==years[i]])
  yearlyCISep[i, 7] <- mean(dailyCISep$coal[dailyCISep$yr==years[i]])
}

# Look at the half-hourly plots - 4pm to 8pm (for every year)
# heatmap plot
# treat year as a factor
# do a time series analysis
# rainfall/flow and min temps
# add yearly(monthly/daily) boxplots
# ggtimeseries package may be helpful with visualisations

# we observe a peak in 2008

### Making plots with carbon footprint divided into coal, geo, and gas ###
## Yearly ##

plotSep <- yearlyCISep
plotSep$geo <- plotSep$gas + plotSep$geo
plotSep$coal <- plotSep$coal + plotSep$geo

yearSep <- ggplot(plotSep, aes(x, y)) + 
  geom_area(aes(x=yr, y=coal), fill="chartreuse4") + 
  geom_area(aes(x=yr, y=geo), fill="grey33") + 
  geom_area(aes(x=yr, y=gas), fill="purple") + 
  # ggtitle("Yearly carbon intensity(gCO2-e/kWh) for the years 1998-2017 (evening peak)") + 
  labs(y="Carbon intensity (gCO2-e/kWh)", x="Year")  + 
  annotate("text", x = 2012, 
           y = 300, size = 8, color="chartreuse4",
           label = "Coal", hjust = 0) + 
  annotate("text", x = 2012, # x axis is just a number not a real date
           y = 280, size = 8, color="grey33",
           label = "Geothermal", hjust = 0) + 
  annotate("text", x = 2012, # x axis is just a number not a real date
           y = 260, size = 8, color="purple",
           label = "Gas", hjust = 0)
yearSep  

## Monthly

plotSep <- monthlyCISep
plotSep$geo <- plotSep$gas + plotSep$geo
plotSep$coal <- plotSep$coal + plotSep$geo

tmp <- nrow(plotSep)
plotSep$xVals <- seq(1, tmp)

monthSep <- ggplot(plotSep, aes(x, y)) + 
  geom_area(aes(x=xVals, y=coal), fill="chartreuse4") + 
  geom_area(aes(x=xVals, y=geo), fill="grey33") + 
  geom_area(aes(x=xVals, y=gas), fill="purple") +
  # ggtitle("Monthly Carbon Intensity (gCO2-e/kWh) for the years 1998-2017 (evening peak)") + 
  labs(x="", y="Carbon Intensity (gCO2-e/kWh)") + 
  theme(axis.text.x=element_blank()) + 
  annotate("text", x = 1, 
           y = 400, size = 6, colour="chartreuse4",
           label = "Coal", hjust = 0) + 
  annotate("text", x = 1, # x axis is just a number not a real date
           y = 380, size = 6,  colour="grey33",
           label = "Geothermal", hjust = 0) + 
  annotate("text", x =1, # x axis is just a number not a real date
           y = 360, size = 6,  colour="purple",
           label = "Gas", hjust = 0)

monthSep

## Daily

daySepFunc <- function(datDay=dailyCISep, y){
  plotSep <- datDay[datDay$yr==y, ]
  plotSep$geo <- plotSep$gas + plotSep$geo
  plotSep$coal <- plotSep$coal + plotSep$geo
  
  t <- c(2000, 2004, 2008, 2012, 2016)
  if (y %in% t){
    plotSep$xVals <- seq(1, 366)
  } else { 
    plotSep$xVals <- seq(1, 365)
  }
  
  plt <- ggplot(plotSep, aes(x, y)) + 
    geom_area(aes(x=xVals, y=coal), fill="chartreuse4") + 
    geom_area(aes(x=xVals, y=geo), fill="grey33") + 
    geom_area(aes(x=xVals, y=gas), fill="purple") +
    ggtitle(y) + 
    labs(x="Day", y="gCO2-e/kWh") 
  return(plt)
}

for (i in 1:n){
  tmp <- daySepFunc(y=years[i])
  nam <- paste("plotSep", years[i], sep="")
  assign(nam, tmp)
}

# ggarrange(plotSep1998, plotSep1999, plotSep2000, plotSep2001, 
#          plotSep2002, plotSep2003, plotSep2004, plotSep2005, 
ggarrange(plotSep2006, plotSep2007, plotSep2008, plotSep2009, 
          plotSep2010, plotSep2011, plotSep2012, plotSep2013, 
          plotSep2014, plotSep2015, plotSep2016, plotSep2017, 
          nrow=5, ncol=4)

##### Calculating the mean CI of morning and evening peaks for each season #####
### Computing/reading in the carbon intensity for each of the 48 half-hourly intervals ###
## Creating daily dataframes ##

start <- proc.time()

bin <- file.exists("dailyCIgas.csv") && 
  file.exists("dailyCIgeo.csv") && 
  file.exists("dailyCIhydro.csv") && 
  file.exists("dailyCIwind.csv") && 
  file.exists("dailyCIcoal.csv")

dailyCI <- data.frame(matrix(vector(), 0, 51, 
                             dimnames=list(c(), c("yr", "mth", "d", 
                                                  paste("TP", 1:48, sep="")))), 
                      stringsAsFactors=FALSE)

if (!bin) {
  dailyCIgas <- data.frame(matrix(vector(), 0, 51, 
                                  dimnames=list(c(), c("yr", "mth", "d", 
                                                       paste("TP", 1:48, sep="")))), 
                           stringsAsFactors=FALSE)
  dailyCIgeo=dailyCIhydro=dailyCIwind=dailyCIcoal=dailyCIgas
  dailyCI=dailyCIgas
  
  # Filling in the above data frames #
  years <- c(2006, 2017)
  #years <- seq(1998, 2017)
  n <- length(years)
  trck <- 1
  for (i in 1:n){
    for (k in 1:12){
      # determining how many unique days we have in the current iteration #
      xY <- carbonData[year(carbonData$Trading_date)==years[i], ]
      xM <- xY[month(xY$Trading_date)==k, ]
      days <- unique(xM$Trading_date, na.rm=T)
      days <- days[!is.na(days)]
      m <- length(days)
      
      for (j in 1:m){
        # assigning the dates #
        dailyCIgas[trck,1]=dailyCIgeo[trck,1]=dailyCIhydro[trck,1]=years[i]
        dailyCIwind[trck,1]=dailyCIcoal[trck,1]=dailyCIgas[trck, 1]
        dailyCI[trck, 1]=dailyCIgas[trck, 1]
        
        dailyCIgas[trck,2]=dailyCIgeo[trck,2]=dailyCIhydro[trck,2]=k
        dailyCIwind[trck,2]=dailyCIcoal[trck,2]=dailyCIgas[trck, 2]
        dailyCI[trck, 2]=dailyCIgas[trck, 2]
        
        dailyCIgas[trck,3]=dailyCIgeo[trck,3]=dailyCIhydro[trck,3]=j
        dailyCIwind[trck,3]=dailyCIcoal[trck,3]=dailyCIgas[trck, 3]
        dailyCI[trck, 3]=dailyCIgas[trck, 3]
        
        # computing the total electricity generation for the day #
        tmpGen <- genDaily(genData, years[i], k, j)
        
        # computing the total carbon emissions for each of the five fuels for each half-hourly interval # 
        tmpGas <- genDaily(gasCarbon, years[i], k, j)
        tmpGeo <- genDaily(geoCarbon, years[i], k, j)
        tmpHydro <- genDaily(hydroCarbon, years[i], k, j)
        tmpWind <- genDaily(windCarbon, years[i], k, j)
        tmpCoal <- genDaily(coalCarbon, years[i], k, j)
        
        # computing the total carbon intensity for each of the five fuels for each half-hourly interval #
        dailyCIgas[trck, 4:51] <- tmpGas/tmpGen*1000
        dailyCIgeo[trck, 4:51] <- tmpGeo/tmpGen*1000
        dailyCIhydro[trck, 4:51] <- tmpHydro/tmpGen*1000
        dailyCIwind[trck, 4:51] <- tmpWind/tmpGen*1000
        dailyCIcoal[trck, 4:51] <- tmpCoal/tmpGen*1000
        
        trck <- trck + 1
      } # for (j in 1:m)
    } # for (k in 1:12)
  } # for (i in 1:n)
  
  # saving the above data frames, this enables us to skip all of the computing once we've computed #
  # all of the relevant carbon intensities # 
  write.csv(dailyCIgas, file="dailyCIgas.csv")
  write.csv(dailyCIgeo, file="dailyCIgeo.csv")
  write.csv(dailyCIhydro, file="dailyCIhydro.csv")
  write.csv(dailyCIwind, file="dailyCIwind.csv")
  write.csv(dailyCIcoal, file="dailyCIcoal.csv")
  write.csv(dailyCI, file="dailyCI.csv")
} # if  

dailyCIgas <- read.csv("dailyCIgas.csv"); dailyCIgas[is.na(dailyCIgas)] <- 0; dailyCIgas <- dailyCIgas[, 2:52]
dailyCIgeo <- read.csv("dailyCIgeo.csv"); dailyCIgeo[is.na(dailyCIgeo)] <- 0; dailyCIgeo <- dailyCIgeo[, 2:52]
dailyCIhydro <- read.csv("dailyCIhydro.csv"); dailyCIhydro[is.na(dailyCIhydro)] <- 0; dailyCIhydro <- dailyCIhydro[, 2:52]
dailyCIwind <- read.csv("dailyCIwind.csv"); dailyCIwind[is.na(dailyCIwind)] <- 0; dailyCIwind <- dailyCIwind[, 2:52]
dailyCIcoal <- read.csv("dailyCIcoal.csv"); dailyCIcoal[is.na(dailyCIcoal)] <- 0; dailyCIcoal <- dailyCIcoal[, 2:52]

r <- nrow(dailyCIcoal); c <- ncol(dailyCIcoal)
for (i in 1:r){
  dailyCI[i ,] <- rep(0, c)
} # for

dailyCI[, 1:3] <- dailyCIgas[, 1:3]

dailyCI[, 4:51] <- dailyCIgas[, 4:51] + dailyCIgeo[, 4:51] + dailyCIhydro[, 4:51] + 
  dailyCIwind[, 4:51] + dailyCIcoal[, 4:51]

(endCIdaily <- proc.time() - start)

## Creating monthly dataframes ##
start <- proc.time()

bin <- file.exists("monthlyCIgas.csv") && 
  file.exists("monthlyCIgeo.csv") && 
  file.exists("monthlyCIhydro.csv") && 
  file.exists("monthlyCIwind.csv") && 
  file.exists("monthlyCIcoal.csv")

monthlyCI <- data.frame(matrix(vector(), 0, 50, 
                               dimnames=list(c(), c("yr", "mth", 
                                                    paste("TP", 1:48, sep="")))), 
                        stringsAsFactors=FALSE)

if (!bin){
  years <- seq(2006, 2017)
  # years <- seq(1998, 2017)
  n <- length(years)
  monthlyCIgas = monthlyCIgeo = monthlyCIhydro = monthlyCIwind = monthlyCIcoal = monthlyCI
  
  trck <- 1
  for (i in 1:n){
    for (j in 1:12){
      monthlyCIgas[trck, 1]=monthlyCIgeo[trck, 1]=monthlyCIhydro[trck, 1]=years[i] 
      monthlyCIwind[trck, 1]=monthlyCIcoal[trck, 1]=monthlyCIgas[trck, 1]
      monthlyCI[trck, 1]=monthlyCIgas[trck, 1]
      
      monthlyCIgas[trck, 2]=monthlyCIgeo[trck, 2]=monthlyCIhydro[trck, 2]=j 
      monthlyCIwind[trck, 2]=monthlyCIcoal[trck, 2]=monthlyCIgas[trck, 2]
      monthlyCI[trck, 2]=monthlyCIgas[trck, 2]
      
      # computing the total electricity generation for the month #
      tmpGen <- genMonthly(genData, years[i], j)
      
      # computing the total carbon emissions for each of the five fuels for each half-hourly interval # 
      tmpGas <- genMonthly(gasCarbon, years[i], j)
      tmpGeo <- genMonthly(geoCarbon, years[i], j)
      tmpHydro <- genMonthly(hydroCarbon, years[i], j)
      tmpWind <- genMonthly(windCarbon, years[i], j)
      tmpCoal <- genMonthly(coalCarbon, years[i], j)
      
      # computing the total carbon intensity for each of the five fuels for each half-hourly interval #
      monthlyCIgas[trck, 3:50] <- tmpGas/tmpGen*1000
      monthlyCIgeo[trck, 3:50] <- tmpGeo/tmpGen*1000
      monthlyCIhydro[trck, 3:50] <- tmpHydro/tmpGen*1000
      monthlyCIwind[trck, 3:50] <- tmpWind/tmpGen*1000
      monthlyCIcoal[trck, 3:50] <- tmpCoal/tmpGen*1000
      
      trck <- trck + 1
      
      print(trck)
    } # for (j in 1:12)
  } # for (i in 1:n)
  
  # saving the above data frames, this enables us to skip all of the computing once we've computed #
  # all of the relevant carbon intensities # 
  write.csv(monthlyCIgas, file="monthlyCIgas.csv")
  write.csv(monthlyCIgeo, file="monthlyCIgeo.csv")
  write.csv(monthlyCIhydro, file="monthlyCIhydro.csv")
  write.csv(monthlyCIwind, file="monthlyCIwind.csv")
  write.csv(monthlyCIcoal, file="monthlyCIcoal.csv")
} # if

monthlyCIgas <- read.csv("monthlyCIgas.csv"); monthlyCIgas[is.na(monthlyCIgas)] <- 0; monthlyCIgas <- monthlyCIgas[, 2:51]
monthlyCIgeo <- read.csv("monthlyCIgeo.csv"); monthlyCIgeo[is.na(monthlyCIgeo)] <- 0; monthlyCIgeo <- monthlyCIgeo[, 2:51]
monthlyCIhydro <- read.csv("monthlyCIhydro.csv"); monthlyCIhydro[is.na(monthlyCIhydro)] <- 0; monthlyCIhydro <- monthlyCIhydro[, 2:51]
monthlyCIwind <- read.csv("monthlyCIwind.csv"); monthlyCIwind[is.na(monthlyCIwind)] <- 0; monthlyCIwind <- monthlyCIwind[, 2:51]
monthlyCIcoal <- read.csv("monthlyCIcoal.csv"); monthlyCIcoal[is.na(monthlyCIcoal)] <- 0; monthlyCIcoal <- monthlyCIcoal[, 2:51]

r <- nrow(monthlyCIcoal); c <- ncol(monthlyCIcoal)
for (i in 1:r){
  monthlyCI[i ,] <- rep(0, c)
} # for

monthlyCI[, 1:2] <- monthlyCIgas[, 1:2]

monthlyCI[, 3:50] <- monthlyCIgas[, 3:50] + monthlyCIgeo[, 3:50] + monthlyCIhydro[, 3:50] + 
  monthlyCIwind[, 3:50] + monthlyCIcoal[, 3:50]

(endCImonthly <- proc.time() - start)

### Seasonal carbon intensities for the morning and evening peaks ###
## Morning ##
monthlyCIMorning <- data.frame(matrix(vector(), 0, 11, 
                                      dimnames=list(c(), c("yr", "season", 
                                                           paste("TP", 15:22, sep=""), "mean"))), 
                               stringsAsFactors=FALSE)
monthlyCIEvening <- data.frame(matrix(vector(), 0, 11, 
                                      dimnames=list(c(), c("yr", "season", 
                                                           paste("TP", 35:42, sep=""), "mean"))), 
                               stringsAsFactors=FALSE)
monthlyCIDay <- data.frame(matrix(vector(), 0, 51, 
                                  dimnames=list(c(), c("yr", "season", 
                                                       paste("TP", 1:48, sep=""), "mean"))), 
                           stringsAsFactors=FALSE)
years <- seq(2006, 2017)
#years <- seq(1998, 2017)
n <- length(years)
totalDemand <- data.frame(matrix(vector(), 0, 5, 
                                 dimnames=list(c(), c("yr", "season", 
                                                      "morning", "evening", "total"))), 
                          stringsAsFactors=FALSE)

for (k in 1:n){
  seasons <- c("Spring", "Summer", "Autumn", "Winter")
  tmp1 <- monthlyCI[monthlyCI$yr==years[k], c(1, 2, 17:24)]
  tmp2 <- monthlyCI[monthlyCI$yr==years[k], c(1, 2, 37:44)]
  t1 <- monthlyCI[monthlyCI$yr==years[k], ]
  tmp3 <- tmp1
  for (i in 1:4){
    if (seasons[i]=="Spring"){
      tmp3 <- tmp1[tmp1$mth%in%c(9, 10, 11), ]
      tmp4 <- tmp2[tmp2$mth%in%c(9, 10, 11), ]
      t2 <- t1[t1$mth%in%c(9, 10, 11), ]
      totTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(9, 10, 11), ]
      mornTmp <- genData[year(genData$Trading_date)==years[k] & 
                           month(genData$Trading_date)%in%c(9, 10, 11), 18:25]
      eveTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(9, 10, 11), 38:45]
    } else if (seasons[i]=="Summer"){
      tmp3 <- tmp1[tmp1$mth%in%c(12, 1, 2), ]
      tmp4 <- tmp2[tmp2$mth%in%c(12, 1, 2), ]
      t2 <- t1[t1$mth%in%c(12, 1, 2), ]
      totTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(12, 1, 2), ]
      mornTmp <- genData[year(genData$Trading_date)==years[k] & 
                           month(genData$Trading_date)%in%c(12, 1, 2), 18:25]
      eveTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(12, 1, 2), 38:45]
    } else if (seasons[i]=="Autumn"){
      tmp3 <- tmp1[tmp1$mth%in%c(3, 4, 5), ]
      tmp4 <- tmp2[tmp2$mth%in%c(3, 4, 5), ]
      t2 <- t1[t1$mth%in%c(3, 4, 5), ]
      totTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(3, 4, 5), ]
      mornTmp <- genData[year(genData$Trading_date)==years[k] & 
                           month(genData$Trading_date)%in%c(3, 4, 5), 18:25]
      eveTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(3, 4, 5), 38:45]
    } else if (seasons[i]=="Winter"){
      tmp3 <- tmp1[tmp1$mth%in%c(6, 7, 8), ]
      tmp4 <- tmp2[tmp2$mth%in%c(6, 7, 8), ]
      t2 <- t1[t1$mth%in%c(6, 7, 8), ]
      totTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(6, 7, 8), ]
      mornTmp <- genData[year(genData$Trading_date)==years[k] & 
                           month(genData$Trading_date)%in%c(6, 7, 8), 18:25]
      eveTmp <- genData[year(genData$Trading_date)==years[k] & 
                          month(genData$Trading_date)%in%c(6, 7, 8), 38:45]
    } # if ... else if
    tmp5 <- c(mean(tmp3[, 3]), mean(tmp3[, 4]), mean(tmp3[, 5]), mean(tmp3[, 6]), 
              mean(tmp3[, 7]), mean(tmp3[, 8]), mean(tmp3[, 9]), mean(tmp3[, 10]))
    tmp6 <- c(mean(tmp4[, 3]), mean(tmp4[, 4]), mean(tmp4[, 5]), mean(tmp4[, 6]), 
              mean(tmp4[, 7]), mean(tmp4[, 8]), mean(tmp4[, 9]), mean(tmp4[, 10]))
    ind <- i + (k-1)*4
    monthlyCIMorning[ind, 1] = monthlyCIEvening[ind, 1] = totalDemand[ind, 1] = years[k]
    monthlyCIMorning[ind, 2] =monthlyCIEvening[ind, 2] = totalDemand[ind, 2] = seasons[i]
    monthlyCIMorning[ind, 3:10] <- tmp5
    monthlyCIEvening[ind, 3:10] <- tmp6
    monthlyCIMorning[ind, 11] <- mean(c(tmp3[, 3], tmp3[, 4], tmp3[, 5], tmp3[, 6], 
                                      tmp3[, 7], tmp3[, 8], tmp3[, 9], tmp3[, 10]))
    monthlyCIEvening[ind, 11] <- mean(c(tmp4[, 3], tmp4[, 4], tmp4[, 5], tmp4[, 6], 
                                      tmp4[, 7], tmp4[, 8], tmp4[, 9], tmp4[, 10]))
    totalDemand[ind, 3] <- sum(colSums(mornTmp, na.rm = T))/1000000
    totalDemand[ind, 4] <- sum(colSums(eveTmp, na.rm = T))/1000000
    totalDemand[ind, 5] <- sum(colSums(totTmp[, 4:51], na.rm = T))/1000000
  } # for (i in 1:4)
} # for (k in 1:n)

### Plotting the mean carbon intensity for each season over the 20 years ###
## Morning peak ##
xSummer <- seq(1, n*4, 4); xAutumn <- seq(2, n*4, 4); xWinter <- seq(3, n*4, 4); xSpring <- seq(4, n*4, 4)

ySpring <- monthlyCIMorning[monthlyCIMorning$season=="Spring", ]$mean
ySummer <- monthlyCIMorning[monthlyCIMorning$season=="Summer", ]$mean
yAutumn <- monthlyCIMorning[monthlyCIMorning$season=="Autumn", ]$mean
yWinter <- monthlyCIMorning[monthlyCIMorning$season=="Winter", ]$mean

tmp <- data.frame(xVals=c(xSpring, xSummer, xAutumn, xWinter), 
                  yVals = c(ySpring, ySummer, yAutumn, yWinter))
yStuff <- tmp[order(tmp$xVals), ]

colours <- brewer.pal(11, "RdYlGn")
colours2 <- brewer.pal(11, "RdYlBu")

(pltMorn <- ggplot() + geom_line(aes(x=yStuff$xVals, y=yStuff$yVals), linetype=2) + 
    geom_point(aes(x=xSpring, y=ySpring), colour=colours[10]) + 
    geom_point(aes(x=xSummer, y=ySummer), colour=colours[2]) + 
    geom_point(aes(x=xAutumn, y=yAutumn), colour=colours[3], size=2) + 
    geom_point(aes(x=xWinter, y=yWinter), colour=colours2[10]) + 
    labs(y = "Carbon intensity (g CO2e/kWh)") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
    # ggtitle("Morning Peak") + 
    annotate("text", x = 70, 
             y = 150, size = 6, colour=colours[10],
             label = "Spring", hjust = 0) + 
    annotate("text", x = 70, # x axis is just a number not a real date
             y = 140, size = 6,  colour=colours[2],
             label = "Summer", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 130, size = 6,  colour=colours[3],
             label = "Autumn", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 120, size = 6,  colour=colours2[10],
             label = "Winter", hjust = 0))

(pltAutumnMorn <- ggplot() + geom_line(aes(x=x, y=yMorn), linetype=2) +
    geom_point(aes(x=xAutumn, y=yAutumn), colour=colours[3], size=2))


## Evening peak ##
ySpring <- monthlyCIEvening[monthlyCIEvening$season=="Spring", ]$mean
ySummer <- monthlyCIEvening[monthlyCIEvening$season=="Summer", ]$mean
yAutumn <- monthlyCIEvening[monthlyCIEvening$season=="Autumn", ]$mean
yWinter <- monthlyCIEvening[monthlyCIEvening$season=="Winter", ]$mean

tmp <- data.frame(xVals=c(xSpring, xSummer, xAutumn, xWinter), 
                  yVals = c(ySpring, ySummer, yAutumn, yWinter))
yStuff <- tmp[order(tmp$xVals), ]

(pltEve <- ggplot() + geom_line(aes(x=yStuff$xVals, y=yStuff$yVals), linetype=2) + 
    geom_point(aes(x=xSpring, y=ySpring), colour=colours[10]) + 
    geom_point(aes(x=xSummer, y=ySummer), colour=colours[2]) + 
    geom_point(aes(x=xAutumn, y=yAutumn), colour=colours[3], size=2) + 
    geom_point(aes(x=xWinter, y=yWinter), colour=colours2[10]) +
    labs(y = "Carbon intensity (g CO2e/kWh)") + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    # ggtitle("Evening Peak")+ 
    annotate("text", x = 70, 
             y = 150, size = 6, colour=colours[10],
             label = "Spring", hjust = 0) + 
    annotate("text", x = 70, # x axis is just a number not a real date
             y = 140, size = 6,  colour=colours[2],
             label = "Summer", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 130, size = 6,  colour=colours[3],
             label = "Autumn", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 120, size = 6,  colour=colours2[10],
             label = "Winter", hjust = 0))

(pltAutumnEve <- ggplot() + geom_line(aes(x=x, y=yMorn), linetype=2) +
    geom_point(aes(x=xAutumn, y=yAutumn), colour=colours[3], size=2))

## Autumn mean ##
tmpSpring <- rowMeans(monthlyCI[monthlyCI$mth%in%c(9, 10, 11), 3:50], na.rm=T)
tmpSummer <- rowMeans(monthlyCI[monthlyCI$mth%in%c(12, 1, 2), 3:50], na.rm=T)
tmpAutumn <- rowMeans(monthlyCI[monthlyCI$mth%in%c(3, 4, 5), 3:50], na.rm=T)
tmpWinter <- rowMeans(monthlyCI[monthlyCI$mth%in%c(6, 7, 8), 3:50], na.rm=T)

ySpring <- c(mean(tmpSpring[1:3]), mean(tmpSpring[4:6]), mean(tmpSpring[7:9]), 
             mean(tmpSpring[10:12]), mean(tmpSpring[13:15]), mean(tmpSpring[16:18]), 
             mean(tmpSpring[19:21]), mean(tmpSpring[22:24]), mean(tmpSpring[25:27]), 
             mean(tmpSpring[28:30]), mean(tmpSpring[31:33]), mean(tmpSpring[34:36]), 
             mean(tmpSpring[37:39]), mean(tmpSpring[40:42]), mean(tmpSpring[43:45]), 
             mean(tmpSpring[46:48]), mean(tmpSpring[49:51]), mean(tmpSpring[52:54]), 
             mean(tmpSpring[55:57]), mean(tmpSpring[58:60]))
ySummer <- c(mean(tmpSummer[1:3]), mean(tmpSummer[4:6]), mean(tmpSummer[7:9]), 
             mean(tmpSummer[10:12]), mean(tmpSummer[13:15]), mean(tmpSummer[16:18]), 
             mean(tmpSummer[19:21]), mean(tmpSummer[22:24]), mean(tmpSummer[25:27]), 
             mean(tmpSummer[28:30]), mean(tmpSummer[31:33]), mean(tmpSummer[34:36]), 
             mean(tmpSummer[37:39]), mean(tmpSummer[40:42]), mean(tmpSummer[43:45]), 
             mean(tmpSummer[46:48]), mean(tmpSummer[49:51]), mean(tmpSummer[52:54]), 
             mean(tmpSummer[55:57]), mean(tmpSummer[58:60]))
yAutumn <- c(mean(tmpAutumn[1:3]), mean(tmpAutumn[4:6]), mean(tmpAutumn[7:9]), 
             mean(tmpAutumn[10:12]), mean(tmpAutumn[13:15]), mean(tmpAutumn[16:18]), 
             mean(tmpAutumn[19:21]), mean(tmpAutumn[22:24]), mean(tmpAutumn[25:27]), 
             mean(tmpAutumn[28:30]), mean(tmpAutumn[31:33]), mean(tmpAutumn[34:36]), 
             mean(tmpAutumn[37:39]), mean(tmpAutumn[40:42]), mean(tmpAutumn[43:45]), 
             mean(tmpAutumn[46:48]), mean(tmpAutumn[49:51]), mean(tmpAutumn[52:54]), 
             mean(tmpAutumn[55:57]), mean(tmpAutumn[58:60]))
yWinter <- c(mean(tmpWinter[1:3]), mean(tmpWinter[4:6]), mean(tmpWinter[7:9]), 
             mean(tmpWinter[10:12]), mean(tmpWinter[13:15]), mean(tmpWinter[16:18]), 
             mean(tmpWinter[19:21]), mean(tmpWinter[22:24]), mean(tmpWinter[25:27]), 
             mean(tmpWinter[28:30]), mean(tmpWinter[31:33]), mean(tmpWinter[34:36]), 
             mean(tmpWinter[37:39]), mean(tmpWinter[40:42]), mean(tmpWinter[43:45]), 
             mean(tmpWinter[46:48]), mean(tmpWinter[49:51]), mean(tmpWinter[52:54]), 
             mean(tmpWinter[55:57]), mean(tmpWinter[58:60]))

xSummer <- seq(1, n*4, 4); xAutumn <- seq(2, n*4, 4); xWinter <- seq(3, n*4, 4); xSpring <- seq(4, n*4, 4)

tmp <- data.frame(xVals=c(xSpring, xSummer, xAutumn, xWinter), 
                  yVals = c(ySpring, ySummer, yAutumn, yWinter))
yAll <- tmp[order(tmp$xVals), ]

(pltAutumn <- ggplot() + geom_line(aes(x=yAll$xVals, y=yAll$yVals), linetype=2) + 
    geom_point(aes(x=xSpring, y=ySpring), colour=colours[10]) + 
    geom_point(aes(x=xSummer, y=ySummer), colour=colours[2]) + 
    geom_point(aes(x=xAutumn, y=yAutumn), colour=colours[3], size=2) + 
    geom_point(aes(x=xWinter, y=yWinter), colour=colours2[10]) + 
    labs(y = "Carbon intensity (g CO2e/kWh)") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
    # ggtitle("Morning Peak") + 
    annotate("text", x = 70, 
             y = 150, size = 6, colour=colours[10],
             label = "Spring", hjust = 0) + 
    annotate("text", x = 70, # x axis is just a number not a real date
             y = 140, size = 6,  colour=colours[2],
             label = "Summer", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 130, size = 6,  colour=colours[3],
             label = "Autumn", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 120, size = 6,  colour=colours2[10],
             label = "Winter", hjust = 0))

pltAutumn + geom_vline(xintercept=4) + geom_vline(xintercept=8) + geom_vline(xintercept=12) + 
  geom_vline(xintercept=16) + geom_vline(xintercept=20) + geom_vline(xintercept=24) + 
  geom_vline(xintercept=28) + geom_vline(xintercept=32) + geom_vline(xintercept=36) + 
  geom_vline(xintercept=40) + geom_vline(xintercept=44) + geom_vline(xintercept=48) + 
  geom_vline(xintercept=52) + geom_vline(xintercept=56) + geom_vline(xintercept=60) + 
  geom_vline(xintercept=64) + geom_vline(xintercept=68) + geom_vline(xintercept=72) + 
  geom_vline(xintercept=76) + geom_vline(xintercept=80)
##### Computing correlation coefficient between total generation and mean carbon intensity #####
### Getting the data ready ###
monthlyGen <-  data.frame(matrix(vector(), 0, 5, 
                                 dimnames=list(c(), c("yr", "month", 
                                                      "morning", "evening", "total"))), 
                          stringsAsFactors=FALSE)
combinedDailyCI <- data.frame(matrix(vector(), 0, 6, 
                                     dimnames=list(c(), c("yr", "month", "d",
                                                          "morning", "evening", "total"))), 
                              stringsAsFactors=FALSE)
combinedMonthlyCI <- data.frame(matrix(vector(), 0, 5, 
                                       dimnames=list(c(), c("yr", "month", 
                                                            "morning", "evening", "total"))), 
                                stringsAsFactors=FALSE)
trck <- 1
for (i in 1:n){
  tmp1 <- monthlyCI[monthlyCI$yr==years[i], c(1, 2, 17:24)] # morning peak
  tmp2 <- monthlyCI[monthlyCI$yr==years[i], c(1, 2, 37:44)] # evening peak
  for (j in 1:12){
    totTmp <- genData[year(genData$Trading_date)==years[i] & month(genData$Trading_date)==j, 4:51]
    mornTmp <- genData[year(genData$Trading_date)==years[i] & month(genData$Trading_date)==j, 18:25]
    eveTmp <- genData[year(genData$Trading_date)==years[i] & month(genData$Trading_date)==j, 38:45]
    tmp3 <- tmp1[tmp1$mth==j, ]
    tmp4 <- tmp2[tmp2$mth==j, ]
    tmp7 <- genData[month(genData$Trading_date)==j, ]
    
    tmp5 <- c(mean(tmp3[, 3]), mean(tmp3[, 4]), mean(tmp3[, 5]), mean(tmp3[, 6]), 
              mean(tmp3[, 7]), mean(tmp3[, 8]), mean(tmp3[, 9]), mean(tmp3[, 10]))
    tmp6 <- c(mean(tmp4[, 3]), mean(tmp4[, 4]), mean(tmp4[, 5]), mean(tmp4[, 6]), 
              mean(tmp4[, 7]), mean(tmp4[, 8]), mean(tmp4[, 9]), mean(tmp4[, 10]))
    ind <- j + (i-1)*12
    
    combinedMonthlyCI[ind, 1] = monthlyGen[ind, 1] = years[i]
    combinedMonthlyCI[ind, 2] = monthlyGen[ind, 2] = j
    combinedMonthlyCI[ind, 3] <- mean(c(tmp3[, 3], tmp3[, 4], tmp3[, 5], tmp3[, 6], 
                                        tmp3[, 7], tmp3[, 8], tmp3[, 9], tmp3[, 10]))
    combinedMonthlyCI[ind, 4] <- mean(c(tmp4[, 3], tmp4[, 4], tmp4[, 5], tmp4[, 6], 
                                        tmp4[, 7], tmp4[, 8], tmp4[, 9], tmp4[, 10]))
    combinedMonthlyCI[ind, 5] <- rowMeans(monthlyCI[monthlyCI$yr==years[i] & monthlyCI$mth==j, 3:50])
    monthlyGen[ind, 3] <- sum(colSums(mornTmp, na.rm = T))/1000000
    monthlyGen[ind, 4] <- sum(colSums(eveTmp, na.rm = T))/1000000
    monthlyGen[ind, 5] <- sum(colSums(totTmp, na.rm = T))/1000000 
    
    m <- 30
    if (j%in%c(1, 3, 5, 7, 8, 10, 12)){
      m <- 31
    } else if (j==2){
      if (years[i]%%4==0){m <- 29}
      else if (years[i]%%4!=0){m <- 28}
    } 
    
    for (k in 1:m){
      combinedDailyCI[trck, 1] <- years[i]; combinedDailyCI[trck, 2] <-j; combinedDailyCI[trck, 3] <- k
      
      t1 <- dailyCI[dailyCI$yr==years[i] & dailyCI$mth ==j & dailyCI$d==k, 4:51]
      t2 <- dailyCI[dailyCI$yr==years[i] & dailyCI$mth ==j & dailyCI$d==k, 18:25]
      t3 <- dailyCI[dailyCI$yr==years[i] & dailyCI$mth ==j & dailyCI$d==k, 38:45]
      
      combinedDailyCI[trck, 4] <- rowMeans(t2)
      combinedDailyCI[trck, 5] <- rowMeans(t3)
      combinedDailyCI[trck, 6] <- rowMeans(t1)
      
      trck <- trck + 1
    }
  } # for (j in 1:12)
} # for (i in 1:n)

### For all seasons - by months ###
ord <- order(monthlyGen$total)
monthlyGen <- monthlyGen[ord,] # electricity demand
combinedMonthlyCI <- combinedMonthlyCI[ord,]

### correlation between carbon intensity and demand ###
## overall ##
(pltCorOverall <- ggplot() + geom_point(aes(x=monthlyGen$total, y=combinedMonthlyCI$total)) + 
  labs(x="Electricity demand (MW)", y="Carbon intensity (g CO2-e/kWh)"))
cor(monthlyGen$total, combinedMonthlyCI$total) # 0.4004173
## morning peak ##
(pltCorMorning <- ggplot() + geom_point(aes(x=monthlyGen$morning, y=combinedMonthlyCI$morning)) + 
    labs(x="Electricity demand (MW)", y="Carbon intensity (g CO2-e/kWh)"))
cor(monthlyGen$morning, combinedMonthlyCI$morning) # 0.3409178
## evening peak ##
(pltCorEvening<- ggplot() + geom_point(aes(x=monthlyGen$evening, y=combinedMonthlyCI$evening)) + 
    labs(x="Electricity demand (MW)", y="Carbon intensity (g CO2-e/kWh)"))
cor(monthlyGen$evening, combinedMonthlyCI$evening) # 0.389138

### Computing the correlation between ci and demand for each month, season, and year
dailyGen <- data.frame(yr=0, mth=0, d=0, gas=0, geo=0, hydro=0, wind=0, coal=0, tot=0)

# Daily CI
trck <- 1
for (i in 1:n){
  for (k in 1:12){
    xY <- gas[year(gas$Trading_date) == years[i], ]
    xM <- xY[month(xY$Trading_date) == months[k], ]
    days <- unique(xM$Trading_date, na.rm = T)
    days <- days[!is.na(days)]
    m <- length(days)
    
    for (j in 1:m){
      dailyCI[trck, 1] <- years[i]; dailyCI[trck, 2] <- months[k]; dailyCI[trck, 3] <- j
      tmp <- genDaily(genData, years[i], months[k], j)
      totalG <- sum(tmp, na.rm=T)
      
      tmp1 <- sum(gasE + geoE + hydroE + windE, na.rm=T) 
      
      dailyGen[trck, 1] <- years[i]; dailyGen[trck, 2] <- k; dailyGen[trck, 3] <- j
      dailyGen[trck, 4] <- sum(genDaily(gasCarbon, years[i], months[k], j), na.rm=T)
      dailyGen[trck, 5] <- sum(genDaily(geoCarbon, years[i], months[k], j), na.rm=T)
      dailyGen[trck, 6] <- sum(genDaily(hydroCarbon, years[i], months[k], j), na.rm=T)
      dailyGen[trck, 7] <- sum(genDaily(windCarbon, years[i], months[k], j), na.rm=T)
      dailyGen[trck, 8] <- sum(genDaily(coalCarbon, years[i], months[k], j), na.rm=T)
      
      dailyGen[trck, 9] <- totalG
      # updating trck
      trck <- trck + 1
    }
  }
}

dailyGen[, 4:9] <- dailyGen[, 4:9]/1000000 

corMonth <- data.frame(matrix(vector(), 0, 5, 
                                  dimnames=list(c(), c("yr", "mth", "total", "mthFactor", "seasonFactor"))), 
                           stringsAsFactors=FALSE)
corSeason <- data.frame(matrix(vector(), 0, 3, 
                                dimnames=list(c(), c("yr", "season", "total"))), 
                         stringsAsFactors=FALSE)
corYear <- data.frame(matrix(vector(), 0, 2, 
                                dimnames=list(c(), c("yr", "total"))), 
                         stringsAsFactors=FALSE)

n <- length(years)
spring <- c(9, 10, 11); summer <- c(12, 1, 2); autumn <- c(3, 4, 5); winter <- c(6, 7, 8)
seasons <- c("Summer", "Autumn", "Winter", "Spring")

for (i in 1:n){
  ## seasonal correlation
  for (j in 1:4){
    tmp1 <- combinedDailyCI[combinedDailyCI$yr == years[i] & combinedDailyCI$month%in%spring, 6]
    tmp2 <- dailyGen[dailyGen$yr == years[i] & dailyGen$mth%in%spring, 9]
    if (j == 1){
      tmp1 <- combinedDailyCI[combinedDailyCI$yr == years[i] & combinedDailyCI$month%in%summer, 6]
      tmp2 <- dailyGen[dailyGen$yr == years[i] & dailyGen$mth%in%summer, 9]
    } else if (j == 2){
      tmp1 <- combinedDailyCI[combinedDailyCI$yr == years[i] & combinedDailyCI$month%in%autumn, 6]
      tmp2 <- dailyGen[dailyGen$yr == years[i] & dailyGen$mth%in%autumn, 9]
    } else if (j == 3){
      tmp1 <- combinedDailyCI[combinedDailyCI$yr == years[i] & combinedDailyCI$month%in%winter, 6]
      tmp2 <- dailyGen[dailyGen$yr == years[i] & dailyGen$mth%in%winter, 9]
    } # if ... else if
    ind <- j + (i-1)*4
    
    corSeason[ind, 1] <- years[i]; corSeason[ind, 2] <- seasons[j]; 
    corSeason[ind, 3] <- cor(tmp1, tmp2)
  } # for (j in 1:4)
  
  ## monthly correlation
  for (k in 1:12){
    ind <- k + (i-1)*12
    tmp1 <- combinedDailyCI[combinedDailyCI$yr == years[i] & combinedDailyCI$month == k, 6]
    tmp2 <- dailyGen[dailyGen$yr == years[i] & dailyGen$mth == k, 9]
    
    corMonth[ind, 1] <- years[i]; corMonth[ind, 2] <- k; 
    corMonth[ind, 3] <- cor(tmp1, tmp2)
  }
  
  tmp1 <- combinedDailyCI[combinedDailyCI$yr == years[i], 6]
  tmp2 <- dailyGen[dailyGen$yr == years[i], 9]
  corYear[i, 1] <- years[i]; corYear[i, 2] <- cor(tmp1, tmp2)
} # for (i in 1:n)

(pltCorMonth <- ggplot() + geom_point(aes(x=seq(1, 240), y=corMonth$total)) + 
    geom_line(aes(x=seq(1, 240), y=corMonth$total),  linetype=2) +
    labs(x="Month", y="Correlation coefficient"))


(pltCorSeason <- ggplot() + geom_point(aes(x=seq(1, 80), y=corSeason$total)) + 
    geom_line(aes(x=seq(1, 80), y=corSeason$total),  linetype=2) +
    labs(x="Season", y="Correlation coefficient"))

(plotCorSeasonCols <- ggplot() + 
    geom_line(aes(x=seq(1, 80), y=corSeason$total), linetype=2) + 
    geom_point(aes(x=xSpring, y=corSeason[corSeason$season=="Spring", 3]), colour=colours[10]) + 
    geom_point(aes(x=xSummer, y=corSeason[corSeason$season=="Summer", 3]), colour=colours[2]) + 
    geom_point(aes(x=xAutumn, y=corSeason[corSeason$season=="Autumn", 3]), colour=colours[3]) + 
    geom_point(aes(x=xWinter, y=corSeason[corSeason$season=="Winter", 3]), colour=colours2[10]) +
    labs(y = "Correlation coefficient") + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    # ggtitle("Evening Peak")+ 
    annotate("text", x = 70, 
             y = 1, size = 6, colour=colours[10],
             label = "Spring", hjust = 0) + 
    annotate("text", x = 70, # x axis is just a number not a real date
             y = 0.9, size = 6,  colour=colours[2],
             label = "Summer", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 0.8, size = 6,  colour=colours[3],
             label = "Autumn", hjust = 0) + 
    annotate("text", x =70, # x axis is just a number not a real date
             y = 0.7, size = 6,  colour=colours2[10],
             label = "Winter", hjust = 0))

(pltCorYear <- ggplot() + geom_point(aes(x=seq(1, 20), y=corYear$total)) + 
    geom_line(aes(x=seq(1, 20), y=corYear$total),  linetype=2) +
    labs(x="Year", y="Correlation coefficient"))

## Setting up a factor for month
corMonth[corMonth$mth==1,]$mthFactor <- "January"
corMonth[corMonth$mth==2,]$mthFactor <- "February"
corMonth[corMonth$mth==3,]$mthFactor <- "March"
corMonth[corMonth$mth==4,]$mthFactor <- "April"
corMonth[corMonth$mth==5,]$mthFactor <- "May"
corMonth[corMonth$mth==6,]$mthFactor <- "June"
corMonth[corMonth$mth==7,]$mthFactor <- "July"
corMonth[corMonth$mth==8,]$mthFactor <- "August"
corMonth[corMonth$mth==9,]$mthFactor <- "September"
corMonth[corMonth$mth==10,]$mthFactor <- "October"
corMonth[corMonth$mth==11,]$mthFactor <- "November"
corMonth[corMonth$mth==12,]$mthFactor <- "December"
corMonth$mthFactor <- as.factor(corMonth$mth)
levels(corMonth$mthFactor) <- c("January", "February", "March", 
                                "April", "May", "June", "July", 
                                "August", "September", "October", 
                                "November", "December")
## Setting up a factor for season
corMonth[corMonth$mth%in%c(12, 1, 2),]$seasonFactor <- "Summer"
corMonth[corMonth$mth%in%c(3, 4, 5),]$seasonFactor <- "Autumn"
corMonth[corMonth$mth%in%c(6, 7, 8),]$seasonFactor <- "Winter"
corMonth[corMonth$mth%in%c(9, 10, 11),]$seasonFactor <- "Spring"
corMonth$seasonFactor <- as.factor(corMonth$seasonFactor)
levels(corMonth$seasonFactor) <- c("Summer", "Autumn", "Winter", "Spring")

## Box and whisker plots for each month
ggplot(aes(y = total, x=mthFactor), data=corMonth) + geom_boxplot() + 
  labs(x="Month", y="Correlation coefficient")

## Box and whisker plots for each season
ggplot(aes(y = total, x=seasonFactor), data=corMonth) + geom_boxplot() + 
  labs(x="Season", y="Correlation coefficient")

## Box and whisker plots for each year
corMonth$yr <- as.factor(corMonth$yr)
ggplot(aes(y = total, x=yr), data=corMonth) + geom_boxplot()+ 
  labs(x="Year", y="Correlation coefficient")

## Box and whicker plot for each season of each year
# cor1 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==1998,]) + geom_boxplot() + 
#   ggtitle("1998") + theme(plot.title = element_text(hjust = 0.5))
# cor2 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==1999,]) + geom_boxplot() + 
#   ggtitle("1999") + theme(plot.title = element_text(hjust = 0.5))
# cor3 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2000,]) + geom_boxplot() + 
#   ggtitle("2000") + theme(plot.title = element_text(hjust = 0.5))
# cor4 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2001,]) + geom_boxplot() + 
#   ggtitle("2001") + theme(plot.title = element_text(hjust = 0.5))
# cor5 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2002,]) + geom_boxplot() + 
#   ggtitle("2002") + theme(plot.title = element_text(hjust = 0.5))
# cor6 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2003,]) + geom_boxplot() + 
#   ggtitle("2003") + theme(plot.title = element_text(hjust = 0.5))
# cor7 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2004,]) + geom_boxplot() + 
#   ggtitle("2004") + theme(plot.title = element_text(hjust = 0.5))
# cor8 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2005,]) + geom_boxplot() + 
#   ggtitle("2005") + theme(plot.title = element_text(hjust = 0.5))
cor9 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2006,]) + geom_boxplot() + 
  ggtitle("2006") + theme(plot.title = element_text(hjust = 0.5))
cor10 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2007,]) + geom_boxplot() + 
  ggtitle("2007") + theme(plot.title = element_text(hjust = 0.5))
cor11 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2008,]) + geom_boxplot() + 
  ggtitle("2008") + theme(plot.title = element_text(hjust = 0.5))
cor12 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2009,]) + geom_boxplot() + 
  ggtitle("2009") + theme(plot.title = element_text(hjust = 0.5))
cor13 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2010,]) + geom_boxplot() + 
  ggtitle("2010") + theme(plot.title = element_text(hjust = 0.5))
cor14 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2011,]) + geom_boxplot() + 
  ggtitle("2011") + theme(plot.title = element_text(hjust = 0.5))
cor15 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2012,]) + geom_boxplot() + 
  ggtitle("2012") + theme(plot.title = element_text(hjust = 0.5))
cor16 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2013,]) + geom_boxplot() + 
  ggtitle("2013") + theme(plot.title = element_text(hjust = 0.5))
cor17 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2014,]) + geom_boxplot() + 
  ggtitle("2014") + theme(plot.title = element_text(hjust = 0.5))
cor18 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2015,]) + geom_boxplot() + 
  ggtitle("2015") + theme(plot.title = element_text(hjust = 0.5))
cor19 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2016,]) + geom_boxplot() + 
  ggtitle("2016") + theme(plot.title = element_text(hjust = 0.5))
cor20 <- ggplot(aes(y=total, x=seasonFactor), data=corMonth[corMonth$yr==2017,]) + geom_boxplot() + 
  ggtitle("2017") + theme(plot.title = element_text(hjust = 0.5))

# ggarrange(cor1, cor2, cor3, cor4, ncol=2, nrow=2)
# ggarrange(cor5, cor6, cor7, cor8, ncol=2, nrow=2)
ggarrange(cor9, cor10, cor11, cor12, ncol=2, nrow=2)
ggarrange(cor13, cor14, cor15, cor16, ncol=2, nrow=2)
ggarrange(cor17, cor18, cor19, cor20, ncol=2, nrow=2)

## Boxplot of summer for every year

ySummer <- rep(0, n)

for(i in 1:n){ 
  tmp <- corMonth[corMonth$seasonFactor=="Summer" & corMonth$yr==years[i],]$total
  ySummer[i] <- mean(tmp)
}

ggplot() +  geom_line(aes(x=seq(2006, 2017), y=ySummer), linetype=2) + 
  geom_point(aes(x=seq(2006, 2017), y=ySummer)) + 
  labs(y = "Correlation coefficient", x="Year")

## Boxplot of autumn for every year
yAutumn <- rep(0, n)

for(i in 1:n){ 
  tmp <- corMonth[corMonth$seasonFactor=="Autumn" & corMonth$yr==years[i],]$total
  yAutumn[i] <- mean(tmp)
}

ggplot() +  geom_line(aes(x=seq(2006, 2017), y=yAutumn), linetype=2) + 
  geom_point(aes(x=seq(2006, 2017), y=yAutumn)) + 
  labs(y = "Correlation coefficient", x="Year") 

## Boxplot of winter for every year
yWinter<- rep(0, n)

for(i in 1:n){ 
  tmp <- corMonth[corMonth$seasonFactor=="Winter" & corMonth$yr==years[i],]$total
  yWinter[i] <- mean(tmp)
}

ggplot() +  geom_line(aes(x=seq(2006, 2017), y=yWinter), linetype=2) + 
  geom_point(aes(x=seq(2006, 2017), y=yWinter)) + 
  labs(y = "Correlation coefficient", x="Year") 

## Boxplot of sping for every year
ySpring <- rep(0, n)

for(i in 1:n){ 
  tmp <- corMonth[corMonth$seasonFactor=="Spring" & corMonth$yr==years[i],]$total
  ySpring[i] <- mean(tmp)
}

ggplot() +  geom_line(aes(x=seq(2006, 2017), y=ySpring), linetype=2) + 
  geom_point(aes(x=seq(2006, 2017), y=ySpring)) + 
  labs(y = "Correlation coefficient", x="Year") 

#corMonth$yrSeason <- interaction(corMonth$yr, corMonth$mthFactor)

#ggplot(aes(y = total, x = yrSeason), data =corMonth) + geom_boxplot() + 
#  labs(x="Year and Season", y="Correlation coefficient")

#ggplot(corMonth) + boxplot(aes(x=yr, y=total)) + facet_wrap(.~seasonFactor, nrow=5)


modMonthMornTot <- lm(combinedMonthlyCI$morning~monthlyGen$total)
summary(modMonthMornTot)
modMonthEveTot <- lm(combinedMonthlyCI$evening~monthlyGen$total)
summary(modMonthEveTot)
modMonthMorn <- lm(combinedMonthlyCI$morning~monthlyGen$morning)
summary(modMonthMorn)
modMonthEve <- lm(combinedMonthlyCI$evening~monthlyGen$evening)
summary(modMonthEve)
modTotal <- lm(combinedMonthlyCI$total~monthlyGen$total)
summary(modTotal)
### For autumn only - by months ###
autumnGen <- monthlyGen[monthlyGen$month%in%c(3, 4, 5), ]
combinedAutumn <- combinedMonthlyCI[combinedMonthlyCI$month%in%c(3, 4, 5), ]

ord <- order(autumnGen$total)
autumnGen <- autumnGen[ord,]
combinedAutumn <- combinedAutumn[ord,]

plot(autumnGen$total, combinedAutumn$morning)
plot(autumnGen$total, combinedAutumn$evening)
plot(autumnGen$total, combinedAutumn$total)

cor(autumnGen$total, combinedAutumn$morning)
cor(autumnGen$total, combinedAutumn$evening)
cor(autumnGen$total, combinedAutumn$total)


ord <- order(autumnGen$morning)
autumnGen <- autumnGen[ord,]
combinedAutumn <- combinedAutumn[ord,]
plot(autumnGen$morning, combinedAutumn$morning)
cor(autumnGen$morning, combinedAutumn$morning)

ord <- order(autumnGen$evening)
autumnGen <- autumnGen[ord,]
combinedAutumn <- combinedAutumn[ord,]
plot(autumnGen$evening, combinedAutumn$evening)
cor(autumnGen$evening, combinedAutumn$evening)

# When using each month separately, we don't seem to have strong correlations between
# electricity demand and carbon intensity

### For all seasons - by season ###
## Morning Peak ##
yMorn <- monthlyCIMorning$mean
ySpring <- monthlyCIMorning[monthlyCIMorning$season=="Spring", ]$mean
ySummer <- monthlyCIMorning[monthlyCIMorning$season=="Summer", ]$mean
yAutumn <- monthlyCIMorning[monthlyCIMorning$season=="Autumn", ]$mean
yWinter <- monthlyCIMorning[monthlyCIMorning$season=="Winter", ]$mean

plot(totalDemand$total, yMorn)
cor(totalDemand$total, yMorn)

plot(totalDemand$morning, yMorn)
cor(totalDemand$morning, yMorn)
## Evening Peak ##
yEve <- monthlyCIEvening$mean
ySpring <- monthlyCIEvening[monthlyCIEvening$season=="Spring", ]$mean
ySummer <- monthlyCIEvening[monthlyCIEvening$season=="Summer", ]$mean
yAutumn <- monthlyCIEvening[monthlyCIEvening$season=="Autumn", ]$mean
yWinter <- monthlyCIEvening[monthlyCIEvening$season=="Winter", ]$mean

plot(totalDemand$total, yEve)
cor(totalDemand$total, yEve)

plot(totalDemand$evening, yEve)
cor(totalDemand$evening, yEve)

### For autumn only - by season ###
## Morning Peak
yAutumn <- monthlyCIMorning[monthlyCIMorning$season=="Autumn", ]$mean
tot <- totalDemand[totalDemand$season=="Autumn", ]

plot(tot$total, yAutumn)
cor(tot$total, yAutumn)

modAutumnMorn1 <- lm(yAutumn~tot$total)
summary(modAutumnMorn1)

plot(tot$morning, yAutumn)
cor(tot$morning, yAutumn)

modAutumnMorn2 <- lm(yAutumn~tot$morning)
summary(modAutumnMorn2)

## Evening Peak
yAutumn <- monthlyCIEvening[monthlyCIEvening$season=="Autumn", ]$mean
tot <- totalDemand[totalDemand$season=="Autumn", ]

plot(tot$total, yAutumn)
cor(tot$total, yAutumn)

plot(tot$evening, yAutumn)
cor(tot$evening, yAutumn)

##### To-do list #####

## For each season, calculate the mean carbon intensity at morning and evening peak. 
## We'll obtain a trend in time, there could be a correlation between demand and carbon intensity.

## Level of demand and carbon intensity is not correlated - this might not be true. 
## Plotting this for each season for the 20 years and computing that correlation to see if what Imran
## says is indeed true. 

## Fig 7, half GW on x-axis. Colour-code data-points rather than arrows at the top
## BUT do box-and-whisker plots. Do this by season. Plot correlation coefficient for each season 
## and plot them. Worry about peaks in autumn because there is less/no water around

## We want to understand how we can go deeper than Imran did and look at seasons and trends over time.

## Shiny app to create plots - interactivity is good :) watermark the plots (how?)

## Get onto github
yearSep + #geom_vline(xintercept=1998) + geom_vline(xintercept=1999) + geom_vline(xintercept=2000) + 
  #geom_vline(xintercept=2001) + geom_vline(xintercept=2002) + geom_vline(xintercept=2003) + 
  #geom_vline(xintercept=2004) + geom_vline(xintercept=2005) + 
  geom_vline(xintercept=2006) + 
  geom_vline(xintercept=2007) + geom_vline(xintercept=2008) + geom_vline(xintercept=2009) +
  geom_vline(xintercept=2010) + geom_vline(xintercept=2011) + geom_vline(xintercept=2012) + 
  geom_vline(xintercept=2013) + geom_vline(xintercept=2014) + geom_vline(xintercept=2015) + 
  geom_vline(xintercept=2016) + geom_vline(xintercept=2017)

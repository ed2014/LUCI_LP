#' ---
#' title: "TDR data analysis L&P"
#' author: "Edmar Teixeira"
#' date: "May 3rd, 2014"
#' output: html_document
#' ---

# FIXME: Code not ready yet (EIT)
# Purpose: Read data from TDR loggers for data-cleaning and analysis
library("chron")
library(plyr)
library(ggplot2)
#Change the directory workspace
setwd("K:\\CPDiary\\Data\\Lucerne & pasture drought trial 2011-2012\\Analysis")

# Lookup of plots blocks treatments
lookupExp = read.table("lookUp.txt", sep="\t", header=T)

# Read files from stats folder load it to a variable
info  = read.table("testLP.txt", sep="\t", header=F)
head(info)

headTDR = c("time", "record", "1_t", "1_b", "2_t", "2_b", "3_t", "3_b", "4_t", "4_b", 
            "5_t", "5_b", "6_t", "6_b", "7_t", "7_b", "8_t", "8_b", "9_t", "9_b", "10_t", 
            "10_b", "11_t", "11_b", "12_t", "12_b", "13_t", "13_b", "14_t", "14_b", "15_t", 
            "15_b", "16_t", "16_b", "17_t", "17_b", "18_t", "18_b", "19_t", "19_b", "20_t", 
            "20_b", "21_t", "21_b", "22_t", "22_b", "23_t", "23_b", "24_t", "24_b", "25_t", 
            "25_b", "26_t", "26_b", "27_t", "27_b", "28_t", "28_b", "29_t", "29_b", "30_t", 
            "30_b", "31_t", "31_b", "32_t", "32_b")
 #)

colnames(info) = headTDR

summary (info)
head(info)


# form list of datatypes
col_types<-c(c("character","integer"),rep("integer",65))

# Find duplicates and delete them
dups<-info[duplicated(info$time),]
dups[,1:2]
info = info[!duplicated(info$time),]
info[duplicated(info$time),]


# Include new date and time columns
Date = NULL
Time = NULL
JulDay = NULL
for (i in 1:length(info$time)) {
  theText = unlist(strsplit(as.character(info[i,1])," "))
  theTime= paste(theText[2],":00", sep = "", collapse = NULL)
  dateTime = chron(dates = theText[1], times = theTime, format = c(dates = "d/m/y", times = "h:m:s"))
  JulDay[i] = as.numeric(dates(dateTime))
  Date[i] = as.character(dates(dateTime))
  Time[i] = as.numeric(hours(dateTime))
}

info$newDate = Date
info$newTime = Time
info$newJulDay = JulDay

info_new = data.frame()
for (i in 3:length(headTDR)){
thisPlot = unlist(strsplit(as.character(colnames(info[i])),"_"))[1]
thisDepth = unlist(strsplit(as.character(colnames(info[i])),"_"))[2]
thisCrop = lookupExp$Crop[lookupExp$Plot == thisPlot]
thisTreat = lookupExp$IrrigFreq[lookupExp$Plot == thisPlot] 
thisBlock = lookupExp$Block[lookupExp$Plot == thisPlot]
moist_temp = info[i]
colnames(moist_temp) = 'SMC' # FIXME: this issue with name must be later simplified

if (i == 3) {
 info_new = data.frame(julD = info$newJulDay, date = info$newDate, hourOfDay = info$newTime, plot = thisPlot , block = thisBlock, depth = thisDepth, crop = thisCrop, treat = thisTreat, Moisture = moist_temp)
 }else{
  temp_df = data.frame(julD = info$newJulDay, date = info$newDate, hourOfDay = info$newTime, plot = thisPlot , block = thisBlock, depth = thisDepth, crop = thisCrop, treat = thisTreat, SMC = moist_temp)
  info_new = rbind(info_new, temp_df)
 }
}
head(info_new)
summary(info_new)

# Summarise data by daily values
info_new[is.na(info_new)] = 0

# Summarise data by treatment (FIXME: idea that can be here implemented is to use SMC at a given time of the day instead)
dailyData = ddply(info_new, c("julD", "date", "crop", "treat", "depth"), summarise, SMC = mean(as.numeric(SMC)))

head(dailyData)

ggplot(dailyData, aes(x=julD, y=SMC)) +  geom_point() + facet_wrap(depth ~ crop + treat)
ggplot(dailyData, aes(x=julD, y=SMC)) +  geom_line() + facet_wrap(depth ~ crop + treat)

# Isolating block values too
dailyData2 = ddply(info_new, c("julD", "date", "crop", "treat", "depth", "block"), summarise, SMC = mean(as.numeric(SMC)))

head(dailyData2)
ggplot(dailyData2, aes(x=julD, y=SMC)) +  geom_line(aes(colour=block)) + facet_wrap(depth ~ crop + treat)
ggplot(dailyData2, aes(x=julD, y=SMC)) +  geom_point(aes(colour=block)) + facet_wrap(depth ~ crop + treat)
ggplot(dailyData2, aes(x=julD, y=SMC)) +  geom_line() + facet_wrap(depth ~ crop + treat)

#TODO: Subset data by plot and make relationship between means of each block
summary(dailyData2$SMC)
#i = 1
#Cleaning data from bad readings
for (i in 1:length(dailyData2$julD)) {
  if (is.na(dailyData2$SMC[i])) {print(paste(c(dailyData2$date[i], dailyData2$block[i], dailyData2$SMC[i])}
}

# FIXME: This logic is imcomplete - need to have a code for treatment/depth/block and do the analysis like that

# Do relationship among data from different blocks
daily_recent = subset(dailyData2, daily_recent$julD > 15800)
daily_block1 = subset(daily_recent, daily_recent$block == 1)
daily_block2 = subset(daily_recent, daily_recent$block == 2)
daily_block3 = subset(daily_recent, daily_recent$block == 3)
daily_block4 = subset(daily_recent, daily_recent$block == 4)

SMC_b1 = mean(daily_block1$SMC, na.rm = TRUE)
SMC_b2 = mean(daily_block2$SMC, na.rm = TRUE)
SMC_b3 = mean(daily_block3$SMC, na.rm = TRUE)
SMC_b4 = mean(daily_block4$SMC, na.rm = TRUE)

adj = matrix(rep(1, 16), nrow = 4, ncol = 4)
# Relationships among plots
adj[1,2] = SMC_b1/SMC_b2
adj[1,3] = SMC_b1/SMC_b3
adj[1,4] = SMC_b1/SMC_b4
adj[2,3] = SMC_b2/SMC_b3
adj[2,4] = SMC_b2/SMC_b4
adj[3,4] = SMC_b3/SMC_b4

# Exclude data beyond acceptable ranges
countNA = 0
countTooHigh = 0
countTooLow = 0
for (i in 1:nrow(dailyData2)) {
  if (is.na(dailyData2$SMC[i])) {
    countNA = countNA + 1
    dailyData2$block[i] 
    
  }
  else {
    if (dailyData2$SMC[i] < 0.05) {      
      countTooLoW = countTooLow + 1
      
    }
    if (dailyData2$SMC[i] > 0.4) {countTooHigh = countTooHigh + 1}
  }
}
countNA
countTooHigh
countTooLow

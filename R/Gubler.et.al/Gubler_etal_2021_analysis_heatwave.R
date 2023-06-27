#### 1. DATA PREPARATION ####

## clean Memory

rm(list = ls())

## Set working directory 
setwd("C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave")

## load packages 

library(stringr)
library(tidyverse)
library(lubridate)
library(naniar)
library(yardstick)
library(StreamMetabolism)
library(dplyr)
library(data.table)
library(reshape)
library(xts)
library(gridExtra)
library(plyr)
library(colorRamps)
library(egg)
library (viridis)
library(purrr)
library(expss)

## load data
data_log <- read.csv("data_heatwave_log.csv",header = TRUE,na=TRUE)
data_ref <- read.csv("data_heatwave_ref.csv",header=TRUE,na=TRUE)
data_uhi_full <- read.csv("data_UHI_full.csv",header=TRUE,na=TRUE)

## check data visually
summary(data_log)
summary(data_ref)
summary(data_uhi_full)

plot(data_log$Log_15) # Log_22: cleared out 7h on 5.8. duet to too high values (12:00 - 18:50; 42 values)
plot(data_ref$BOLL_STRAHLUNG_GLOBAL)

## Date conversion to POSIXct format


data_log$Date_time_CET <- strptime(data_log$Date_time_CET, format="%d.%m.%Y %H:%M")
data_ref$Date_time_CET <- strptime(data_ref$Date_time_CET, format="%d.%m.%Y %H:%M")
data_uhi_full$Date_time_CET <- strptime(data_uhi_full$Date_time_CET, format="%d.%m.%Y %H:%M")

typeof(data_log$Date_time_CET)
typeof(data_ref$Date_time_CET)
typeof(data_uhi_full$Date_time_CET)

## Define NA's

data_ref[data_ref==-9999] <- NA
data_uhi_full[data_uhi_full==-9999] <- NA

## Radiation: Negative values to zero (Bollwerk)

data_ref$BOLL_STRAHLUNG_GLOBAL[data_ref$BOLL_STRAHLUNG_GLOBAL<0] <- 0
plot(data_ref$BOLL_STRAHLUNG_GLOBAL)

data_uhi_full$BOLL_STRAHLUNG_GLOBAL[data_uhi_full$BOLL_STRAHLUNG_GLOBAL<0] <- 0
plot(data_uhi_full$BOLL_STRAHLUNG_GLOBAL)


#### 2. a) Calculations HEATWAVE ####

## Calculate differences (delta T) for Reference measurements

data<-data_ref 

# Bollwerk (Dach)
data$diff_BOLL <- data$BOLL_Log_64 - data$BOLL_TEMP
summary(data$diff_BOLL)

# Zollikofen: 2m, 3m, Stevenson Screen

data$diff_ZOLL_2m <- data$ZOLL_Log_98_2m - data$ZOLL_TEMP
summary(data$diff_ZOLL_2m)

data$diff_ZOLL_3m <- data$ZOLL_Log_99_3m - data$ZOLL_TEMP
summary(data$diff_ZOLL_3m)

data$diff_ZOLL_KHaus <- data$ZOLL_Log_999_KLIMAHAUS - data$ZOLL_TEMP # Differenz Stevenson zu Station
summary(data$diff_ZOLL_KHaus)

data$diff_ZOLL_stvlog <- data$ZOLL_Log_98_2m - data$ZOLL_Log_999_KLIMAHAUS # Differenz Logger zu Stevenson
summary(data$diff_ZOLL_stvlog)

# Amt fÃ¼r Umwelt (Wankdorf)
data$diff_AFU <- data$AFU_Log_83_3m - data$AFU_TEMP
summary(data$diff_AFU)

## Calculate UHI bias on 10min 

# AFU vs. ZOLL
UHI_AFU_ZOLL_Log <- (data$AFU_Log_83_3m - data$ZOLL_Log_99_3m) # UHI LCD
UHI_AFU_ZOLL_AWS <- (data$AFU_TEMP - data$ZOLL_TEMP_2m) # UHI AWS
data$UHI_bias_AFU_ZOLL <- UHI_AFU_ZOLL_Log - UHI_AFU_ZOLL_AWS # UHI bias: LCD - AWS
summary (data$UHI_bias_AFU_ZOLL)
plot(data$UHI_bias_AFU_ZOLL)

# # BOLL vs. ZOLL
UHI_BOLL_ZOLL_Log <- (data$BOLL_Log_64 - data$ZOLL_Log_99_3m) # UHI LCD
UHI_BOLL_ZOLL_AWS <- (data$BOLL_TEMP - data$ZOLL_TEMP_2m) # UHI AWS
data$UHI_bias_BOLL_ZOLL <- UHI_BOLL_ZOLL_Log - UHI_BOLL_ZOLL_AWS # UHI bias: LCD - AWS
summary (data$UHI_bias_BOLL_ZOLL)

data_ref<-data


#### b) PARAMETER CALCULATIONS ####

## Hourly averages

# Logger Data (total 76)

data_num <- data_log
data_num$DateHour <- droplevels(cut(data_num$Date_time_CET, breaks='hour')) # create col with day only
data_log_hourly<-aggregate(x=data_num[,6:81], by=list(data_num$DateHour), FUN=mean,na.rm=TRUE) ## aggregate to hourly mean
write.csv(data_log_hourly,"C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave/temp_heatwave_hourly_averaged.csv")


# Reference Data (hourly average)

data_num<- data_ref
data_num$DateHour <- droplevels(cut(data_num$Date_time_CET, breaks='hour')) # create col with day only
data_ref_hourly <- aggregate(x=data_num[,6:39], by=list(data_num$DateHour), FUN=mean,na.rm=TRUE) ## aggregate to hourly mean

# Reference: Drop Precipitation cols
data_ref_hourly<- subset(data_ref_hourly, select = -c(ZOLL_NIEDERSCHLAG,BOLL_NIEDERSCHLAG,ZOLL_SONNE))

# Refernce: subset precipitation & sunshine duration (hourly sums)
data_ref_precip <- select(data_ref,BOLL_NIEDERSCHLAG,ZOLL_NIEDERSCHLAG,ZOLL_SONNE,Date_time_CET)
data_num <- data_ref_precip
data_num$DateHour <- droplevels(cut(data_num$Date_time_CET, breaks='hour'))
data_ref_precip_hourly <- aggregate(x=data_num[,1:3], by=list(data_num$DateHour), FUN=sum,na.rm=TRUE)


data_ref_hourly<-cbind(data_ref_hourly,data_ref_precip_hourly[,2:4]) # add them to hourly averages

summary(data_ref_hourly$UHI_bias_AFU_ZOLL)
summary(data_ref_hourly$UHI_bias_BOLL_ZOLL)

## UHI-Intensity: Urban - Rural (Zollikofen); 10min-values

data_uhi <- data_log[,6:81] - data_log$Log_99_ZOLL_3m # difference between Zollikofen and eacht site
data_uhi <- cbind(data_uhi,data_log[,1:5]) # add date hour col's again
summary(data_uhi)
plot(data_uhi$Log_1)

# aggregate to hourly UHI-Intensity

data_uhi$DateHour <- droplevels(cut(data_uhi$Date_time_CET, breaks='hour')) # create col with hour only
uhi_hourly_avg <- aggregate(x=data_uhi[,1:81], by=list(data_uhi$DateHour), FUN=mean,na.rm=TRUE)

write.csv(uhi_hourly_avg,"C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave/UHI_intensity_hourly_averaged.csv")
plot(uhi_hourly_avg$Log_1~uhi_hourly_avg$Group.1)

# select specific daytime hours

t22<-subset(uhi_hourly_avg,Hour=="22") 
t23<-subset(uhi_hourly_avg,Hour=="23") 

### Cooling rates: Temperature difference between 18:00 and 22:00!

# Cooling rate based on HOURLY AVERAGES

data_log$DateHour <- droplevels(cut(data_log$Date_time_CET, breaks='hour')) # create col with hour only

cool_10min<-subset(data_log,format(Date_time_CET, '%H') %in% c('18', '19', '20', '21')) # choose 18:00-21:50 by DateTime column
cool_h<-aggregate(x=cool_10min[,1:82], by=list(cool_10min$DateHour), FUN=mean,na.rm=TRUE) ## aggregate to hourly mean
cool_t1<-subset(cool_h, (Hour=="18")) # choose t1 (18 Uhr)
cool_t2<-subset(cool_h, (Hour=="21")) # choose t2 (21 Uhr)

cooling_h <- (cool_t2[,7:82] - cool_t1[,7:82])/4 # subtraction and averaging to get deltaT/h
cooling_h <- t(cooling_h) # change orientation of dataframe
colnames(cooling_h) <- c("30/7","31/7","1/8","2/8","3/8","4/8","5/8","6/8","7/8") #rename column names with exact date
average<-rowMeans(cooling_h) # mean over whole heatwave
cooling_h <- cbind(cooling_h,average) # add as last column

write.csv(cooling_h,"C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave/UHI_cooling_rate_hourly_averaged_t18_t22.csv")


# Cooling rates based on ABSOLUTE values

# subsets of different daytime hours

cool_tt1<-subset(data_log, (Hour=="18") & (Minute=="0")) # choose time: 18:00
cool_tt2<-subset(data_log, (Hour=="22") & (Minute=="0")) # choose time: 23:00

cooling_abs <- (cool_tt2[,6:81] - cool_tt1[,6:81])/4 #subtraction
cooling_abs <- t(cooling_abs) # transpose
colnames(cooling_abs) <- c("30/7","31/7","1/8","2/8","3/8","4/8","5/8","6/8","7/8")
average<-rowMeans(cooling_abs) # mean over whole heatwave
cooling_abs <- cbind(cooling_abs,average) # add as last column

write.csv(cooling_abs,"C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave/UHI_cooling_rate_absolute_t18_t22.csv")
summary(cooling_abs)

# Difference between MEAN (less cooling) und ABSOLUTE (stronger cooling)
test<-cooling_h - cooling_abs
summary (test)

### Warming rates: Temperature difference between 6:00 and 10:00!

# Warming rate based on HOURLY AVERAGES

warm_10min<-subset(data_log,format(Date_time_CET, '%H') %in% c( '06', '07', '08', '09')) # choose 06:00 - 09:50
warm_h<-aggregate(x=warm_10min[,1:82], by=list(cool_10min$DateHour), FUN=mean,na.rm=TRUE) ## aggregate to hourly mean
warm_t1<-subset(warm_h, (Hour=="6")) # choose t1 (6 Uhr) 
warm_t2<-subset(warm_h, (Hour=="9")) # choose t2 (9 Uhr)

warming_h <- (warm_t2[,7:82] - warm_t1[,7:82])/4 # subtraction and averaging to get deltaT/h
warming_h <- t(warming_h) # change orientation of dataframe
colnames(warming_h) <- c("30/7","31/7","1/8","2/8","3/8","4/8","5/8","6/8","7/8") #rename column names with exact date
average<-rowMeans(warming_h) # mean over whole heatwave
warming_h <- cbind(warming_h,average) # add as last column

write.csv(warming_h,"C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave/UHI_warming_rate_hourly_averaged_t6_t10.csv")


## Warming rates based on ABSOLUTE VALUES

warm_tt1<-subset(data_log, (Hour=="6") & (Minute=="0")) # choose time: 06:00
warm_tt2<-subset(data_log, (Hour=="10") & (Minute=="0")) # choose time: 10:00 

warming_abs <- (warm_tt2[,6:81] - warm_tt1[,6:81])/4
warming_abs <- t(warming_abs)
colnames(warming_abs) <- c("30/7","31/7","1/8","2/8","3/8","4/8","5/8","6/8","7/8")
average<-rowMeans(warming_abs) # mean over whole heatwave
warming_abs <- cbind(warming_abs,average) # add as last column

write.csv(warming_abs,"C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave/UHI_warming_rate_absolute_t6_t10.csv")


# Difference between MEAN und ABSOLUTE
test<-warming_h - warming_abs
summary (test)


# Summary of UHI-Bias hourly averaged
summary(data_ref_hourly$UHI_bias_AFU_ZOLL)
summary(data_ref_hourly$UHI_bias_BOLL_ZOLL)





#### 3. PARAMETER CALCULATIONS UHI ENTIRE STUDY PERIOD ####

## UHI-Intensity: Urban - Rural (Zollikofen); 10min-values

data_uhi_period <- data_uhi_full[,32:107] - data_uhi_full$Log_99_ZOLL_3m # difference between Zollikofen and eacht site
data_uhi_period <- cbind(data_uhi_period,data_uhi_full[,1:31]) # add date hour & reference parameters again
summary(data_uhi_period)


# aggregate to hourly UHI-Intensity

data_uhi_period$DateHour <- droplevels(cut(data_uhi_period$Date_time_CET, breaks='hour')) # create col with hour only
uhi_period_hourly_avg <- aggregate(x=data_uhi_period, by=list(data_uhi_period$DateHour), FUN=mean,na.rm=TRUE)

write.csv(uhi_period_hourly_avg,"C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_UHI_heatwave/UHI_intensity_entire_period_hourly_averaged.csv")
plot(uhi_period_hourly_avg$Log_1~uhi_period_hourly_avg$Group.1)
summary(uhi_period_hourly_avg)


# create new data frame with nighttime UHI only (22-06)
uhi_period_night_hourly <- subset(uhi_period_hourly_avg,uhi_period_hourly_avg$ZOLL_STRAHLUNG_GLOBAL<=1)
summary(uhi_period_night_hourly)

# create new data frame with UHI & wind data (ZOLL) during nighttime only
data_uhi_night <- uhi_period_night_hourly[,2:77]
data_uhi_night <- cbind(data_uhi_night,uhi_period_night_hourly[,102])
colnames(data_uhi_night)[77] <- "ZOLL_WIND_mean"

# Add mean wind speed at BOLL
data_uhi_night <- cbind(data_uhi_night,uhi_period_night_hourly[,89])
colnames(data_uhi_night)[78] <- "BOLL_WIND_mean"


#data_uhi_night <- as.numeric(as.character(unlist(data_uhi_night)))

# create levels (groups) based on wind speed of ZOLL
data_uhi_night$wind_group_ZOLL <- cut(data_uhi_night$ZOLL_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(data_uhi_night$wind_group_ZOLL)
levels(data_uhi_night$wind_group_ZOLL) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels

# create levels (groups) based on wind speed of BOLL
data_uhi_night$wind_group_BOLL <- cut(data_uhi_night$BOLL_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(data_uhi_night$wind_group_BOLL)
levels(data_uhi_night$wind_group_BOLL) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels

# Add AWS UHI intensity at BOLL: AWS BOLL - AWS ZOLL
data_uhi_night$AWS_BOLL <- uhi_period_night_hourly$BOLL_TEMP - uhi_period_night_hourly$ZOLL_TEMP_2m
summary(data_uhi_night$AWS_BOLL)
# Add AWS UHI intensity at AFU: AWS AFU - AWS ZOLL
data_uhi_night$AWS_AFU <- uhi_period_night_hourly$AFU_TEMP - uhi_period_night_hourly$ZOLL_TEMP_2m
summary(data_uhi_night$AWS_AFU)


#### 5. PLOTTING ####


#### 5.1 SHORTTIME PERFORMANCE during heatwave (Figure 5 OLD): XTS-Approach (wie gehabt) ####

## Data preparation: Create dataframe with all variables

# define individual data frames for time series plots

data_temp_uhi <- data_log_hourly[,1:77] # temperature data
data_uhi <- uhi_hourly_avg[1:77] # uhi data
data_stations <- merge(data_log_hourly[,1:77],uhi_hourly_avg[1:77],by="Group.1") # comined temp & uhi; temp=.x; uhi=.y
data_AWS_temp <- select(data_ref_hourly,ZOLL_TEMP_2m,AFU_TEMP,BOLL_TEMP) # AWS temperatures
data_delta <- select(data_ref_hourly,diff_ZOLL_3m,diff_AFU,diff_BOLL) # deltaT
data_delta_UHI <- select(data_ref_hourly,UHI_bias_AFU_ZOLL, UHI_bias_BOLL_ZOLL) # UHI Bias 
data_wind <- select(data_ref_hourly,ZOLL_WIND_mean,AFU_WIND_mean,BOLL_WIND_mean) # wind speed
data_rad <- select(data_ref_hourly,BOLL_STRAHLUNG_GLOBAL,ZOLL_STRAHLUNG_GLOBAL,AFU_STRAHLUNG_GLOBAL) #global radiation
data_precip <- data_ref_precip_hourly[,1:2] # precipitation sums
data_sun <- select(data_ref_hourly,Group.1,ZOLL_SONNE)

# merge them onto 1 

ts_all_data<-do.call("cbind", list(data_temp_uhi,data_uhi,data_stations,data_AWS_temp,data_delta,data_delta_UHI,data_wind,data_rad,data_precip))

# define event lines in UTC!see: https://rdrr.io/rforge/xts/man/addEventLines.html 

# event lines for start cooling rate on 30.7.2018 18:00 (local time)
events1 <- xts(letters[1:1], 
               as_datetime(c("2018-07-30 16:00:00")), c("foo", "bar")) # Eingabe in UTC (default)

# event lines for night marker (22:00 and 06:00) throughout heatwave
events2 <- xts(letters[1:16], 
               as_datetime(c("2018-07-31 04:00:00", "2018-07-31 20:00:00", "2018-08-01 04:00:00", "2018-08-01 20:00:00", "2018-08-02 04:00:00", "2018-08-02 20:00:00", "2018-08-03 04:00:00", "2018-08-03 20:00:00", "2018-08-04 04:00:00", "2018-08-04 20:00:00" 
                             , "2018-08-05 04:00:00", "2018-08-05 20:00:00", "2018-08-06 04:00:00", "2018-08-06 20:00:00", "2018-08-07 04:00:00", "2018-08-07 20:00:00")), c("foo", "bar")) # Eingabe in UTC (default)

# event line for end cooling rate AND for night marker on 30.7.2018 22:00  
events3 <- xts(letters[1:1], 
               as_datetime(c("2018-07-30 20:00:00")), c("foo", "bar")) # Eingabe in UTC (default)

## Time-series with all stations: Individual plots ##

myColors <- c("cadetblue2", "cadetblue","darkorchid")

# a) Hourly averaged air temperatures

rownames(data_log_hourly) = data_log_hourly[,1] #define first col. as date/time in xts format
xts.temp <- as.xts(data_log_hourly[,2:77]) # turn into xts (selected rows)
plot1<-plot.xts(xts.temp,main="Air temperatures at all LCS, hourly averaged", lwd=0.05, minor.ticks = NULL)
plot1<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot1<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot1<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot1


# b) Hourly averaged UHI-Intensity

rownames(uhi_hourly_avg) = uhi_hourly_avg[,1] #define first col. as date/time in xts format
xts.uhi <- as.xts(uhi_hourly_avg[,2:77]) # turn into xts (selected rows)
plot2<-plot.xts(xts.uhi,main="UHI-Intensity all LCS, hourly averaged",lwd=0.05,minor.ticks = "hours")
plot2<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot2<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot2<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot2

# c) Hourly averaged air temperature at AWS
data_delta <- select(data_ref_hourly,Group.1,ZOLL_TEMP_2m,AFU_TEMP,BOLL_TEMP)
rownames(data_delta) = data_delta[,1] #define first col. as date/time in xts format
xts.AWStemp <- as.xts(data_delta[2:4]) # turn into xts (selected rows)
plot3<-plot.xts(xts.AWStemp,main="Temperature at AWS, hourly averaged",lwd=0.05,minor.ticks = "hours",col=myColors)
plot3<-addLegend("top", on=1, 
                 legend.names = c("ZOLLIKOFEN","WANKDORF","BOLLWERK"), 
                 lty=c(1, 1), lwd=c(2, 1),col=myColors)
plot3<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot3<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot3<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot3

# d) Hourly averaged air temperature at AWS
data_delta <- select(data_ref_hourly,Group.1,ZOLL_Log_99_3m, AFU_Log_83_3m, BOLL_Log_64)
rownames(data_delta) = data_delta[,1] #define first col. as date/time in xts format
xts.LCDtemp <- as.xts(data_delta[2:4]) # turn into xts (selected rows)
plot4<-plot.xts(xts.LCDtemp,main="Temperature at LCD, hourly averaged",lwd=0.05,minor.ticks = "hours",col=myColors)
plot4<-addLegend("top", on=1, 
                 legend.names = c("ZOLL","AFU","BOLL"), 
                 lty=c(1, 1), lwd=c(2, 1),col=myColors)
plot4<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot4<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot4<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot4

# e) Hourly averaged Delta T between Loggers and reference stations
data_delta <- select(data_ref_hourly,Group.1,diff_ZOLL_3m,diff_AFU,diff_BOLL)
rownames(data_delta) = data_delta[,1] #define first col. as date/time in xts format
xts.delta <- as.xts(data_delta[2:4]) # turn into xts (selected rows)
plot5<-plot.xts(xts.delta,main="Delta T at reference stations, hourly averaged",lwd=0.05,minor.ticks = "hours",col=myColors)
plot5<-addLegend("top", on=1, 
          legend.names = c("ZOLLIKOFEN","WANKDORF","BOLLWERK"), 
          lty=c(1, 1), lwd=c(2, 1),col=myColors)
plot5<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot5<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot5<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot5

# f) Hourly averaged UHI bias between Log and AWS
data_delta_UHI <- select(data_ref_hourly,Group.1,UHI_bias_AFU_ZOLL, UHI_bias_BOLL_ZOLL) # UHI Bias 
rownames(data_delta_UHI) = data_delta_UHI[,1] #define first col. as date/time in xts format
xts.delta_UHI <- as.xts(data_delta_UHI[2:3]) # turn into xts (selected rows)
plot6<-plot.xts(xts.delta_UHI,main="UHI bias AFU vs. ZOLL and BOLL vs. ZOLL",lwd=0.05,minor.ticks = "hours")
plot6<-addLegend("topright", on=1, 
                 legend.names = c("AFU/ZOLL","BOLL/ZOLL"), 
                 lty=c(1, 1), lwd=c(2, 1))
plot6<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot6<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot6<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot6

# g) Hourly averaged wind speed at reference stations
data_delta <- select(data_ref_hourly,Group.1,ZOLL_WIND_mean,AFU_WIND_mean,BOLL_WIND_mean)
rownames(data_delta) = data_delta[,1] #define first col. as date/time in xts format
xts.wind <- as.xts(data_delta[2:4]) # turn into xts (selected rows)
plot7<-plot.xts(xts.wind,main="Wind speed (m/s) at AWS, hourly averaged",lwd=0.05,minor.ticks = "hours",col=myColors)
plot7<-addLegend("topleft", on=1, 
          legend.names = c("ZOLLIKOFEN","WANKDORF","BOLLWERK"), 
          lty=c(1, 1), lwd=c(2, 1),col=myColors)
plot7<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot7<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot7<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot7

# h) Global radiation (w/m2) at reference stations
data_delta <- select(data_ref_hourly,Group.1,ZOLL_STRAHLUNG_GLOBAL,AFU_STRAHLUNG_GLOBAL,BOLL_STRAHLUNG_GLOBAL)
rownames(data_delta) = data_delta[,1] #define first col. as date/time in xts format
xts.rad <- as.xts(data_delta[2:4]) # turn into xts (selected rows)
plot8<-plot.xts(xts.rad,main="Global radiation (m/s) at AWS, hourly averaged",lwd=0.05,minor.ticks = "hours",col=myColors)
plot8<-addLegend("topright", on=1,  lty=c(1, 1), legend.names = c("ZOLLIKOFEN","WANKDORF","BOLLWERK"), lwd=c(2, 1),col=myColors)
plot8<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot8<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot8<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot8

# j) Sunshine sum (minutes per hour) at Zollikofen
data_delta <- select(data_ref_hourly,Group.1,ZOLL_SONNE)
rownames(data_delta) = data_delta[,1] #define first col. as date/time in xts format
xts.sun <- as.xts(data_delta[2]) # turn into xts (selected rows)
plot9<-plot.xts(xts.sun,main="Sunshine duration at Zollikofen, hourly sums",lwd=0.05,minor.ticks = "hours")
plot9<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot9<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot9<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot9

# k) Precipitation sums at Bollwerk and Zollikofen   
data_delta <- select(data_ref_hourly,Group.1,BOLL_NIEDERSCHLAG,ZOLL_NIEDERSCHLAG)
rownames(data_delta) = data_delta[,1] #define first col. as date/time in xts format
xts.prec <- as.xts(data_delta[3:3]) # turn into xts (selected rows)
plot10<-plot.xts(xts.prec,type="h",main="Precipitation Bollwerk, hourly sum",ylab="BLABLA")
plot10<-addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
plot10<-addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
plot10<-addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)

plot10



## multiple plots on one page ## data: ts_all_data; see: https://www.rdocumentation.org/packages/xts/versions/0.11-2/topics/plot.xts


## FIGURE 5: panel with all parameters and event lines

UHIColors <- c("chocolate1", "chocolate4") # color scheme for UHI bias plot
myColors <- c("cadetblue2", "cadetblue","darkorchid")
#plot(xts.temp,main="Temperature all stations",sub="BLABLA")



jpeg(file="Figure_5_old.jpeg",width=4000,height=4000, res=350)

plot(xts.delta, main=expression(paste(bold("(a) "),Delta,italic("T")," at reference sites")),col=myColors, lwd=3, pin=c(10,5))
addEventLines(events1, on = 1,lty=2 , lwd = 1, col = 9, cex=0.001)
addEventLines(events2, on = 1,lty=3 , lwd = 1, col = 9, cex=0.001)
addEventLines(events3, on = 1,lty=4 , lwd = 1, col = 9, cex=0.001)
#addSeries(xts.LCDtemp, main="Air temperature LCD", col=myColors)
#addEventLines(events1, on = 3,lty=2 , lwd = 1, col = 9)
#addEventLines(events2, on = 3,lty=3 , lwd = 1, col = 9, cex=0.001)
#addEventLines(events3, on = 3,lty=4 , lwd = 1, col = 9, cex=0.001)
#addSeries(xts.AWStemp, main="Air temperature AWS")
#addEventLines(events1, on = 3,lty=2 , lwd = 1, col = 9)
#addEventLines(events2, on = 3,lty=3 , lwd = 1, col = 9, cex=0.001)
#addEventLines(events3, on = 3,lty=4 , lwd = 1, col = 9, cex=0.001)
addSeries(xts.delta_UHI, main=expression(paste(bold("(b) "),Delta,italic("T")[UHI] ," at reference sites")),col= UHIColors,lwd=3, yaxis.left=TRUE, yaxis.right=FALSE, yaxis="same",pin=c(10,5))
addEventLines(events1, on = 2,lty=2 , lwd = 1, col = 9, cex=0.001)
addEventLines(events2, on = 2,lty=3 , lwd = 1, col = 9, cex=0.001)
addEventLines(events3, on = 2,lty=4 , lwd = 1, col = 9, cex=0.001)
addSeries(xts.uhi, main=expression(paste(bold("(c) "),"UHI intensity at all LCD sites")), yaxis.left=TRUE, yaxis.right=FALSE) 
addEventLines(events1, on = 3,lty=2 , lwd = 1, col = 9, cex=0.001)
addEventLines(events2, on = 3,lty=3 , lwd = 1, col = 9, cex=0.001)
addEventLines(events3, on = 3,lty=4 , lwd = 1, col = 9, cex=0.001)
addSeries(xts.wind, main=expression(paste(bold("(d) "), "Wind speed at reference sites")),col=myColors, yaxis.left=TRUE, yaxis.right=FALSE, lwd=2)
addEventLines(events1, on = 4,lty=2 , lwd = 1, col = 9, cex=0.001)
addEventLines(events2, on = 4,lty=3 , lwd = 1, col = 9, cex=0.001)
addEventLines(events3, on = 4,lty=4 , lwd = 1, col = 9, cex=0.001)
addSeries (xts.sun, main=expression(paste(bold("(e) "),"Sunshine duration at ZOLL")), col="cadetblue2", yaxis.left=TRUE, yaxis.right=FALSE, yaxis="same", lwd=2)
addEventLines(events1, on = 5,lty=2 , lwd = 1, col = 9, cex=0.001)
addEventLines(events2, on = 5,lty=3 , lwd = 1, col = 9, cex=0.001)
addEventLines(events3, on = 5,lty=4 , lwd = 1, col = 9, cex=0.001)
addSeries(xts.prec,main=expression(paste(bold("(f) "),"Precipitation sum at BOLL")),type="h", yaxis.left=TRUE, yaxis.right=FALSE, col= "darkorchid", lwd=3)
addEventLines(events1, on = 6,lty=2 , lwd = 1, col = 9, cex=0.001)
addEventLines(events2, on = 6,lty=3 , lwd = 1, col = 9, cex=0.001)
addEventLines(events3, on = 6,lty=4 , lwd = 1, col = 9, cex=0.001)

dev.off()






#### 5.2 SHORTTIME PERFORMANCE during heatwave (Figure 5 OLD): GG-PLOT-Approach (vgl. Vorlage Moritz B) for reviews ####



# Data preparation
data_temp_uhi <- data_log_hourly[,1:77] # temperature data
data_uhi <- uhi_hourly_avg[1:77] # uhi data
data_stations <- merge(data_log_hourly[,1:77],uhi_hourly_avg[1:77],by="Group.1") # comined temp & uhi; temp=.x; uhi=.y
data_AWS_temp <- select(data_ref_hourly,ZOLL_TEMP_2m,AFU_TEMP,BOLL_TEMP) # AWS temperatures
data_delta <- select(data_ref_hourly,Group.1,diff_ZOLL_3m,diff_AFU,diff_BOLL) # deltaT
data_delta_UHI <- select(data_ref_hourly,Group.1,UHI_bias_AFU_ZOLL, UHI_bias_BOLL_ZOLL) # UHI Bias 
data_wind <- select(data_ref_hourly,Group.1,ZOLL_WIND_mean,AFU_WIND_mean,BOLL_WIND_mean) # wind speed
data_rad <- select(data_ref_hourly,Group.1,BOLL_STRAHLUNG_GLOBAL,ZOLL_STRAHLUNG_GLOBAL,AFU_STRAHLUNG_GLOBAL) #
data_precip <- data_ref_precip_hourly[,1:2]
data_sun <- select(data_ref_hourly,Group.1,ZOLL_SONNE)

summary(data_uhi)


# Plot 1: DeltaT at reference sites

data <- data_delta
class(data$Group.1)  
data$Group.1<-as_datetime(data$Group.1,tz="CET")

plot_data <-melt(data, id="Group.1")

p1<-ggplot(plot_data, aes(x=Group.1, y= value, color=variable, group=variable))+
  geom_line(lwd=1, show.legend = TRUE) +
  labs(fill = "VARIABLE")+
  theme(legend.position = "none") +
  theme(panel.background=element_blank()) +
  geom_hline(yintercept = 0, lty=1,lwd=0.5) +
  theme_bw()+
  scale_y_continuous(n.breaks = 10)+ # sec.axis = sec_axis(~ . * 1.0,n.breaks = 10) einfügen für zweite y-Achse
  scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d %H:%M")+
  labs(x="Date & time (CET)",y="K", title=expression(paste(bold("(a) "),Delta,italic("T")," at reference sites"))) +
  scale_color_manual(" ", values = c("diff_ZOLL_3m" = "cornflowerblue", "diff_AFU" = "darkolivegreen4", "diff_BOLL" = "darkorchid"), labels =c("ZOLL_3m", "AFU","BOLL"))+
  theme(axis.title.x = element_blank())+ # Hide x-axis Label 
  # add all vertical reference lines
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 16:00:00"))), linetype="dashed", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 20:00:00"))), linetype="dotdash", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 20:00:00"))), linetype="dotted", color = "black", size=0.5)


p1


# Plot 2: Delta T UHI at reference sites

data <- data_delta_UHI
class(data$Group.1)  
data$Group.1<-as_datetime(data$Group.1,tz="CET")

plot_data <-melt(data, id="Group.1")

p2<-ggplot(plot_data, aes(x=Group.1, y= value, color=variable, group=variable))+
  geom_line(lwd=1, show.legend = TRUE) +
  #labs(fill = "variable")+
  theme(legend.position = "none") +
  theme(panel.background=element_blank()) +
  geom_hline(yintercept = 0, lty=1,lwd=0.5) +
  theme_bw()+
  scale_y_continuous(n.breaks = 10)+ # sec.axis = sec_axis(~ . * 1.0,n.breaks = 10) einfügen für zweite y-Achse
  scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d %H:%M")+
  labs(x="Date & time (CET)",y="K", title=expression(paste(bold("(b) "),Delta,italic("T")[UHI] ," at reference sites"))) +
  scale_color_manual(" ", values = c("UHI_bias_AFU_ZOLL" = "chocolate1", "UHI_bias_BOLL_ZOLL" = "chocolate4"), labels =c("AFU - ZOLL", "BOLL - ZOLL"))+
  theme(axis.title.x = element_blank())+ # Hide x-axis Label 
  # add all vertical reference lines
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 16:00:00"))), linetype="dashed", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 20:00:00"))), linetype="dotdash", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 20:00:00"))), linetype="dotted", color = "black", size=0.5)


p2

# Plot 3: UHI intensities at all LCD sites

data <- data_uhi
class(data$Group.1)  
data$Group.1<-as_datetime(data$Group.1,tz="CET")

list<-select(data,Log_1, Log_11, Log_21, Log_31, Log_41, Log_51, Log_61, Log_71, Log_80, Log_101) # create list with Logs to be displayed in Legend

plot_data <-melt(data, id="Group.1")

p3<-ggplot(plot_data, aes(x=Group.1, y= value, color=variable, group=variable))+
  geom_line(lwd=0.5, show.legend = TRUE) +
  #labs(fill = "variable")+
  theme(panel.background=element_blank()) +
  geom_hline(yintercept = 0, lty=1,lwd=0.5) +
  theme_bw()+
  scale_y_continuous(n.breaks = 6)+ # sec.axis = sec_axis(~ . * 1.0,n.breaks = 10) einfügen für zweite y-Achse
  scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d %H:%M")+
  labs(x="Date & time (CET)",y="K", title=expression(paste(bold("(c) "),"UHI intensity at all LCD network sites"))) +
  scale_color_manual(breaks = list, limits = "Log_1")+
  #theme(legend.position = "none") + # remove legend from plot
  #scale_colour_grey()+ # use grey scales instead of colours
  scale_color_viridis(discrete = TRUE)+
  theme(axis.title.x = element_blank())+ # Hide x-axis Label 
  # add all vertical reference lines
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 16:00:00"))), linetype="dashed", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 20:00:00"))), linetype="dotdash", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 20:00:00"))), linetype="dotted", color = "black", size=0.5)


p3


# Plot 4: Wind speed at reference sites

data <- data_wind
class(data$Group.1)  
data$Group.1<-as_datetime(data$Group.1,tz="CET")

plot_data <-melt(data, id="Group.1")

p4<-ggplot(plot_data, aes(x=Group.1, y= value, color=variable, group=variable))+
  geom_line(lwd=1, show.legend = TRUE) +
  #labs(fill = "variable")+
  theme(panel.background=element_blank()) +
  geom_hline(yintercept = 0, lty=1,lwd=0.5) +
  theme_bw()+
  scale_y_continuous(n.breaks = 10)+ # sec.axis = sec_axis(~ . * 1.0,n.breaks = 10) einfügen für zweite y-Achse
  scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d %H:%M")+
  labs(x="Date & time (CET)",y=expression(paste("m s"^"-1")), title=expression(paste(bold("(d) "), "Wind speed at reference sites"))) +
  scale_color_manual(" ", values = c("ZOLL_WIND_mean" = "cornflowerblue", "AFU_WIND_mean" = "darkolivegreen4", "BOLL_WIND_mean" = "darkorchid"),labels =c("ZOLL", "AFU","BOLL"))+
  #theme(legend.position = "none") + # remove legend from plot
  #scale_colour_grey()+ # use grey scales instead of colours
  theme(axis.title.x = element_blank())+ # Hide x-axis Label 
  # add all vertical reference lines
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 16:00:00"))), linetype="dashed", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 20:00:00"))), linetype="dotdash", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 20:00:00"))), linetype="dotted", color = "black", size=0.5)


p4


# Plot 5: Sunshine duration at ZOLL

data <- data_sun 
class(data$Group.1)  
data$Group.1<-as_datetime(data$Group.1,tz="CET")

plot_data <-melt(data, id="Group.1")

p5<-ggplot(plot_data, aes(x=Group.1, y= value, colour=variable))+
  geom_line(lwd=1, show.legend = TRUE) +
  #labs(fill = "variable")+
  theme(panel.background=element_blank()) +
  geom_hline(yintercept = 0, lty=1,lwd=0.5) +
  theme_bw()+
  scale_y_continuous(n.breaks =10)+ # sec.axis = sec_axis(~ . * 1.0,n.breaks = 10) einfügen für zweite y-Achse
  scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d %H:%M")+
  labs(x="Date & time (CET)",y=expression(paste("min h"^"-1")), title=expression(paste(bold("(e) "),"Sunshine duration at ZOLL"))) +
  scale_color_manual(" ",labels =c("ZOLL"), values = c("ZOLL_SONNE" = "cornflowerblue"))+
  #theme(legend.position = "none") + # remove legend from plot
  #scale_colour_grey()+ # use grey scales instead of colours
  theme(axis.title.x = element_blank())+ # Hide x-axis Label 
  # add all vertical reference lines
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 16:00:00"))), linetype="dashed", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 20:00:00"))), linetype="dotdash", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 20:00:00"))), linetype="dotted", color = "black", size=0.5)


p5

# Plot 6: Precipitation sums at BOLL and ZOLL

data <- data_precip
class(data$Group.1)  
data$Group.1<-as_datetime(data$Group.1,tz="CET")

plot_data <-melt(data, id="Group.1")

p6<-ggplot(plot_data, aes(x=Group.1, y= value, fill=variable))+
  geom_bar(stat="identity") +
  #labs(fill = "variable")+
  theme(panel.background=element_blank()) +
  geom_hline(yintercept = 0, lty=1,lwd=0.5) +
  theme_bw()+
  scale_y_continuous(n.breaks =5)+ # sec.axis = sec_axis(~ . * 1.0,n.breaks = 10) einfügen für zweite y-Achse
  scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d %H:%M")+
  labs(x="Date & time (CET)",y=expression(paste("mm h"^"-1")), title=expression(paste(bold("(f) "),"Precipitation sum at BOLL"))) +
  scale_fill_manual(" ",labels =c("BOLL"), values = c("BOLL_NIEDERSCHLAG" = "darkorchid"))+
  #theme(legend.position = "none") + # remove legend from plot
  #scale_colour_grey()+ # use grey scales instead of colours
  # add all vertical reference lines
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 16:00:00"))), linetype="dashed", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-30 20:00:00"))), linetype="dotdash", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-07-31 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-01 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-02 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-03 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-04 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-05 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-06 20:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 04:00:00"))), linetype="dotted", color = "black", size=0.5)+
  geom_vline(aes(xintercept=as_datetime(c("2018-08-07 20:00:00"))), linetype="dotted", color = "black", size=0.5)


p6


# Combine all Plots into one figure

jpeg(file="Figure_5_NEW.jpeg",width=5000,height=4000, res=350)
ggarrange(p1, p2, p3, p4, p5, p6 ,ncol = 1, nrow = 6)
dev.off()







#### b) PLOT XY: Nocturnal UHI Variability by LCDs over entire Study Period grouped by wind speed 

dat<-data_uhi_night
summary(dat$AWS_BOLL)
dat %>% split(.$wind_group_ZOLL) %>% map(summary) # summary by factor levels


## Boxplots grouped by Wind speed at ZOLL
# Stack all UHI cols into one in order to plot it: 8-Tung! Genau schauen, welche cols alle gestacked werden!
dat2a <- data.frame(dat[,79], stack(dat[1:76]))
summary(dat2a)

jpeg(file="Figure_XY.jpeg",width=2500,height=4000, res=375)
par(mfrow=c(3,1))

# Boxplot of all LCD UHI intensities grouped by wind speed at ZOLL
boxplot(dat2a[,2]~dat2a[,1],
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab="UHI intensity (K)", 
        #range=3, # set interquartile range
        ylim = c(-7, 7),
        #col="chocolate1",
        #border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(a) "),"All LCD network sites")), adj=0)
lapply(seq(-8,10,1), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

## Boxplot of UHI intensitiy at AWS AFU grouped by wind speed at Zoll
boxplot(dat$AWS_AFU~dat$wind_group_ZOLL,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab="UHI intensity (K)", 
        #range=3, # set interquartile range
        ylim = c(-7, 7),
        col="darkolivegreen4",
        #border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(b) "),"Reference site AFU (AWS)")), adj=0)
lapply(seq(-8,10,1), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


## Boxplot of UHI intensitiy at AWS BOLL grouped by wind speed at Zoll
boxplot(dat$AWS_BOLL~dat$wind_group_ZOLL,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab="UHI intensity (K)", 
        #range=3, # set interquartile range
        ylim = c(-7, 7),
        col="darkorchid",
        #border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(c) "),"Reference site BOLL (AWS)")), adj=0)
lapply(seq(-8,10,1), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


par(mfrow=c(1,1))


dev.off()



#### 6. STATISTICAL TESTS ####
summary(data_uhi)
summary(data_ref_hourly)
cor.test(cooling_h[,1],uhi_hourly_avg[,2])


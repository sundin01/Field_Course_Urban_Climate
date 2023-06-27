#### 1. DATA PREPARATION ####

## clean Memory

rm(list = ls())

## Set working directory and load packages

setwd("C:/Users/PHBEMOR10556/OneDrive - PHBern/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_AMT/Analysis/Analysis_Reference_Measurements")

## load packages 

library(stringr)
library(tidyverse)
library(lubridate)
library(naniar)
library(yardstick)
library(StreamMetabolism)
library(dplyr)
library(data.table)
library(yardstick)
library(xlsx)
library(Metrics)
library(doBy)
library(nortest)

## load data

data <- read.csv("data_ref.csv",header = TRUE)

summary(data)

## Date conversion to POSIXct format

typeof(data$Date_time_CET)

data$Date_time_CET <- strptime(data$Date_time_CET, format="%d.%m.%Y %H:%M")

typeof(data$Date_time_CET)

## Define NA's

data[data==-9999] <- NA

## Radiation: Negative values to zero (Bollwerk)

data$BOLL_STRAHLUNG_GLOBAL[data$BOLL_STRAHLUNG_GLOBAL<0] <- 0
plot(data$AFU_STRAHLUNG_GLOBAL)



#### 2. DATA OVERVIEW ####

## Boxplots

boxplot(data$diff_BOLL,data$diff_ZOLL_2m,data$diff_ZOLL_3m,data$diff_AFU)
boxplot(data$diff_ZOLL_3m~data$Hour) # plot grouped by variable, z.B. Stunde oder Tag

## Time Series Plots

ts.plot(data$diff_BOLL)


#### 3. CALCULATE DIFFERENCES: Logger - Station ####

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
hist(data$diff_ZOLL_KHaus)

data$diff_ZOLL_stvlog <- data$ZOLL_Log_98_2m - data$ZOLL_Log_999_KLIMAHAUS # Differenz Logger zu Stevenson
summary(data$diff_ZOLL_stvlog)
hist(data$diff_ZOLL_stvlog)

# Amt für Umwelt (Wankdorf)
data$diff_AFU <- data$AFU_Log_83_3m - data$AFU_TEMP
summary(data$diff_AFU)
hist(data$diff_AFU)


#### 4. CREATE SUBSETS (Day vs. Night) ####

## DAY vs. NIGHT

# Add a date column (with whatever timezone you want)
data$date <- as.Date(data$Date_time_CET, tz = 'CET')

# calculate sunrise and sunset times for Zollikofen over whole study period (=123 days)
Sunrise <- sunrise.set(46.9908,7.46403, "2018/05/16" ,timezone="CET", num.days = 123)


# Generation of dataframe with date, sunrise and sunset
sun <- data.frame(date = as.Date(Sunrise$sunrise, tz = 'CET')) 
sun <- cbind(sun, Sunrise)
sun

# Join the two tables and compute night/day
data <- merge(sun, data, by = "date") #merge two data frames

data$dayNight <- ifelse(data$Date_time_CET > data$sunrise & data$Date_time_CET < data$sunset, 'day', 'night') # compute day/night

# Remove columns: sunset, sunrise and date

data <-subset(data, select = -c(sunrise, sunset, date))

# Create subsets Day and Night

data_night <- subset(data,data$dayNight=="night")
summary(data_night)
ts.plot(data_night$ZOLL_STRAHLUNG_GLOBAL)

data_day <- subset(data,data$dayNight=="day")
summary(data_day)
ts.plot(data_day$BOLL_STRAHLUNG_GLOBAL)


#### 5. AGGREGATION (hourly, daily) ####

## Hourly aggregation (avg, min, max) : Average values (Temp, Strahlung etc.)

data_num <- data

data_num$DateHour <- droplevels(cut(data_num$Date_time_CET, breaks='hour')) # create col with day & hour only
data_hourly_avg <- aggregate(cbind(Hour,BOLL_Log_64,BOLL_TEMP,BOLL_STRAHLUNG_GLOBAL,BOLL_WIND_mean,BOLL_WIND_dir,
                                   ZOLL_Log_999_KLIMAHAUS,ZOLL_Log_98_2m,ZOLL_Log_99_3m,ZOLL_TEMP_2m,ZOLL_STRAHLUNG_GLOBAL,ZOLL_WIND_mean,ZOLL_WIND_dir,
                                   AFU_Log_83_3m,AFU_TEMP,AFU_STRAHLUNG_GLOBAL,AFU_WIND_mean,AFU_WIND_dir)~DateHour,data=data_num, FUN=mean,na.rm=TRUE) # does it for all selected col

data_hourly_min <- aggregate(cbind(Hour,BOLL_Log_64,BOLL_TEMP,diff_BOLL,BOLL_STRAHLUNG_GLOBAL,BOLL_WIND_mean,BOLL_WIND_dir,
                                   ZOLL_Log_999_KLIMAHAUS,ZOLL_Log_98_2m,ZOLL_Log_99_3m,ZOLL_TEMP_2m,diff_ZOLL_2m,diff_ZOLL_3m,ZOLL_STRAHLUNG_GLOBAL,ZOLL_WIND_mean,ZOLL_WIND_dir,
                                   AFU_Log_83_3m,AFU_TEMP,diff_AFU,AFU_STRAHLUNG_GLOBAL,AFU_WIND_mean,AFU_WIND_dir)~DateHour,data=data_num, FUN=min,na.rm=TRUE) # does it for all selected col

data_hourly_max <- aggregate(cbind(Hour,BOLL_Log_64,BOLL_TEMP,diff_BOLL,BOLL_STRAHLUNG_GLOBAL,BOLL_WIND_mean,BOLL_WIND_dir,
                                   ZOLL_Log_999_KLIMAHAUS,ZOLL_Log_98_2m,ZOLL_Log_99_3m,ZOLL_TEMP_2m,diff_ZOLL_2m,diff_ZOLL_3m,ZOLL_STRAHLUNG_GLOBAL,ZOLL_WIND_mean,ZOLL_WIND_dir,
                                   AFU_Log_83_3m,AFU_TEMP,diff_AFU,AFU_STRAHLUNG_GLOBAL,AFU_WIND_mean,AFU_WIND_dir)~DateHour,data=data_num, FUN=max,na.rm=TRUE) # does it for all selected col

## Hourly aggregation (hourly sums): Precipitation

#data_precip <- select(data,BOLL_NIEDERSCHLAG,ZOLL_NIEDERSCHLAG,ZOLL_SONNE,Date_time_CET)
#data_num <- data_precip
#data_num$DateHour <- droplevels(cut(data_num$Date_time_CET, breaks='hour'))
#data_precip_hourly <- aggregate(x=data_num[,1:3], by=list(data_num$DateHour), FUN=sum,na.rm=TRUE)

#names(data_precip_hourly)[1]<-"DateHour"

#data_hourly_avg<-cbind(data_hourly_avg,data_precip_hourly[,2:4]) # add them to hourly averages --> unterschiedliche Anzahl rows --> WHY???

#summary(data_precip_hourly)

# calculate differences in hourly average

# Bollwerk (Dach)
data_hourly_avg$diff_BOLL <- data_hourly_avg$BOLL_Log_64 - data_hourly_avg$BOLL_TEMP
summary(data_hourly_avg)

# Zollikofen: 2m, 3m. Klimahaus, Stevenson vs. Radshield, 3m vs. 2m

data_hourly_avg$diff_ZOLL_2m <- data_hourly_avg$ZOLL_Log_98_2m - data_hourly_avg$ZOLL_TEMP_2m
summary(data_hourly_avg$diff_ZOLL_2m)

data_hourly_avg$diff_ZOLL_3m <- data_hourly_avg$ZOLL_Log_99_3m - data_hourly_avg$ZOLL_TEMP_2m
summary(data_hourly_avg$diff_ZOLL_3m)

data_hourly_avg$diff_ZOLL_Khaus <- data_hourly_avg$ZOLL_Log_999_KLIMAHAUS - data_hourly_avg$ZOLL_TEMP_2m
summary(data_hourly_avg$diff_ZOLL_Khaus)

data_hourly_avg$diff_ZOLL_stvlog <- data_hourly_avg$ZOLL_Log_98_2m- data_hourly_avg$ZOLL_Log_999_KLIMAHAUS
summary(data_hourly_avg$diff_ZOLL_stvlog)

data_hourly_avg$diff_ZOLL_3m_2m <- data_hourly_avg$ZOLL_Log_99_3m- data_hourly_avg$ZOLL_Log_98_2m
summary(data_hourly_avg$diff_ZOLL_3m_2m)


# Amt für Umwelt (Wankdorf)

data_hourly_avg$diff_AFU <- data_hourly_avg$AFU_Log_83_3m - data_hourly_avg$AFU_TEMP
summary(data_hourly_avg$diff_AFU)



## Daily aggregation (avg, min, max)

data_num$DateDay <- droplevels(cut(data_num$Date_time_CET, breaks='day')) # create col with day only
data_daily_avg <- aggregate(cbind(BOLL_Log_64,BOLL_TEMP,diff_BOLL,BOLL_STRAHLUNG_GLOBAL,BOLL_WIND_mean,BOLL_WIND_dir,
                                  ZOLL_Log_999_KLIMAHAUS,ZOLL_Log_98_2m,ZOLL_Log_99_3m,ZOLL_TEMP_2m,diff_ZOLL_2m,diff_ZOLL_3m,ZOLL_STRAHLUNG_GLOBAL,ZOLL_WIND_mean,ZOLL_WIND_dir,
                                  AFU_Log_83_3m,AFU_TEMP,diff_AFU,AFU_STRAHLUNG_GLOBAL,AFU_WIND_mean,AFU_WIND_dir)~DateDay,data=data_num, FUN=mean,na.rm=TRUE) # does it for one col

data_daily_min <- aggregate(cbind(BOLL_Log_64,BOLL_TEMP,diff_BOLL,BOLL_STRAHLUNG_GLOBAL,BOLL_WIND_mean,BOLL_WIND_dir,
                                  ZOLL_Log_999_KLIMAHAUS,ZOLL_Log_98_2m,ZOLL_Log_99_3m,ZOLL_TEMP_2m,diff_ZOLL_2m,diff_ZOLL_3m,ZOLL_STRAHLUNG_GLOBAL,ZOLL_WIND_mean,ZOLL_WIND_dir,
                                  AFU_Log_83_3m,AFU_TEMP,diff_AFU,AFU_STRAHLUNG_GLOBAL,AFU_WIND_mean,AFU_WIND_dir)~DateDay,data=data_num, FUN=min,na.rm=TRUE) # does it for one col

data_daily_max <- aggregate(cbind(BOLL_Log_64,BOLL_TEMP,diff_BOLL,BOLL_STRAHLUNG_GLOBAL,BOLL_WIND_mean,BOLL_WIND_dir,
                                  ZOLL_Log_999_KLIMAHAUS,ZOLL_Log_98_2m,ZOLL_Log_99_3m,ZOLL_TEMP_2m,diff_ZOLL_2m,diff_ZOLL_3m,ZOLL_STRAHLUNG_GLOBAL,ZOLL_WIND_mean,ZOLL_WIND_dir,
                                  AFU_Log_83_3m,AFU_TEMP,diff_AFU,AFU_STRAHLUNG_GLOBAL,AFU_WIND_mean,AFU_WIND_dir)~DateDay,data=data_num, FUN=max,na.rm=TRUE) # does it for one col





# Subset for UHI & heatwave analysis

data_heatwave_ref <- subset(data, Date_time_CET >= as.POSIXct('2018-07-30 00:00') &
                              Date_time_CET <= as.POSIXct('2018-08-07 23:50'))

write.csv(data_heatwave_ref, "C:/Users/PHBEMOR10556/Desktop/Dissertation/Stadtklima/Analyse/1_Paper_Reference/Analysis/Analysis_UHI_heatwave/data_heatwave_ref.csv", row.names=T)


# UHI Bias 

# AFU vs. ZOLL
UHI_AFU_ZOLL_Log <- (data_hourly_avg$AFU_Log_83_3m - data_hourly_avg$ZOLL_Log_99_3m) # UHI LCD
UHI_AFU_ZOLL_AWS <- (data_hourly_avg$AFU_TEMP - data_hourly_avg$ZOLL_TEMP_2m) # UHI AWS
data_hourly_avg$UHI_bias_AFU_ZOLL <- UHI_AFU_ZOLL_Log - UHI_AFU_ZOLL_AWS # UHI bias: LCD - AWS
summary (data_hourly_avg$UHI_bias_AFU_ZOLL)

# BOLL vs. ZOLL
UHI_BOLL_ZOLL_Log <- (data_hourly_avg$BOLL_Log_64 - data_hourly_avg$ZOLL_Log_99_3m) # UHI LCD
UHI_BOLL_ZOLL_AWS <- (data_hourly_avg$BOLL_TEMP - data_hourly_avg$ZOLL_TEMP_2m) # UHI AWS
data_hourly_avg$UHI_bias_BOLL_ZOLL <- UHI_BOLL_ZOLL_Log - UHI_BOLL_ZOLL_AWS # UHI bias: LCD - AWS
summary (data_hourly_avg$UHI_bias_BOLL_ZOLL)

# BOLL vs. AFU
UHI_BOLL_AFU_Log <- (data_hourly_avg$BOLL_Log_64 - data_hourly_avg$AFU_Log_83_3m) # UHI LCD
UHI_BOLL_AFU_AWS <- (data_hourly_avg$BOLL_TEMP - data_hourly_avg$AFU_TEMP) # UHI AWS
data_hourly_avg$UHI_bias_BOLL_AFU <- UHI_BOLL_AFU_Log - UHI_BOLL_AFU_AWS # UHI bias: LCD - AWS
summary (data_hourly_avg$UHI_bias_BOLL_AFU)



## Split hourly averages between day- and nighttime

reg_day <- subset(data_hourly_avg,data_hourly_avg$ZOLL_STRAHLUNG_GLOBAL>1)

reg_night <- subset(data_hourly_avg,data_hourly_avg$ZOLL_STRAHLUNG_GLOBAL<=1)


## Add a dummy variable for grouping solar irradiance and wind (for boxplots in Fig. 4 and S1):  http://www.unige.ch/ses/sococ/cl/r/groups.e.html
# Daytime: Solar Irradiance

reg_day$solar_group_ZOLL <- cut(reg_day$ZOLL_STRAHLUNG_GLOBAL, c(0,200,400,600,800,1000))
summary(reg_day$solar_group_ZOLL)
levels(reg_day$solar_group_ZOLL) <- c("0 - 200", "200 - 400", "400 - 600", "600 - 800","800 - 1000") # label groups/levels

reg_day$solar_group_AFU <- cut(reg_day$AFU_STRAHLUNG_GLOBAL, c(0,200,400,600,800,1000))
summary(reg_day$solar_group_AFU)
levels(reg_day$solar_group_AFU) <- c("0 - 200", "200 - 400", "400 - 600", "600 - 800","800 - 1000") # label groups/levels

reg_day$solar_group_BOLL <- cut(reg_day$BOLL_STRAHLUNG_GLOBAL, c(0,200,400,600,800,1000))
summary(reg_day$solar_group_BOLL)
levels(reg_day$solar_group_BOLL) <- c("0 - 200", "200 - 400", "400 - 600", "600 - 800","800 - 1000") # label groups/levels

# Daytime: Wind speed
reg_day$wind_group_ZOLL <- cut(reg_day$ZOLL_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(reg_day$wind_group_ZOLL)
levels(reg_day$wind_group_ZOLL) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels

reg_day$wind_group_AFU <- cut(reg_day$AFU_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(reg_day$wind_group_AFU)
levels(reg_day$wind_group_AFU) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels

reg_day$wind_group_BOLL <- cut(reg_day$BOLL_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(reg_day$wind_group_BOLL)
levels(reg_day$wind_group_BOLL) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels

# Night-time: Wind speed
reg_night$wind_group_ZOLL <- cut(reg_night$ZOLL_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(reg_night$wind_group_ZOLL)
levels(reg_night$wind_group_ZOLL) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels

reg_night$wind_group_AFU<- cut(reg_night$AFU_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(reg_night$wind_group_AFU)
levels(reg_night$wind_group_AFU) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels

reg_night$wind_group_BOLL <- cut(reg_night$BOLL_WIND_mean, c(0,1,2,3,4,5,6,7))
summary(reg_night$wind_group_BOLL)
levels(reg_night$wind_group_BOLL) <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4","4 - 5", "5 - 6", "6 - 7") # label groups/levels


summary(reg_day$diff_ZOLL_stvlog)

#### 6. ERROR ANALYSIS ####
error_large <- subset(data_hourly_avg,data_hourly_avg$diff_BOLL>(3*(IQR(data_hourly_avg$diff_BOLL)))) # subset on hourly diff > X
cor(reg_day$diff_ZOLL_3m,reg_day$ZOLL_STRAHLUNG_GLOBAL)

## summary statistics 
data_err <- reg_night$UHI_bias_AFU_ZOLL # choose data set 
summary(data_err) # mean, min, max

## Extreme values (calculate 95% quantiles)
quantile(data_err,probs=c(.05,.95)) # 95% quantiles
shapiro.test(data_err)

## RMSE

log<- reg_day$ZOLL_Log_99_3m
AWS <- reg_day$ZOLL_TEMP_2m

rmse_vec(AWS,log, na_rm = TRUE)

## Test for normality (Shapiro-wilkinson)
shapiro.test(log)
shapiro.test(AWS)

# --> data is NOT normally distributed -.- --> non-parametric test

## T-Test: paired two-sample t-tests for dependent samples --> WRONG (data is not normally distributed)
#t.test(log,AWS, paired=TRUE)

## Testing the significance of differences in means: Wilcoxon signed rank test for paired samples (non-parametric as )
wilcox.test (log,AWS, paired=TRUE, alternative = "two.sided")


### LINEAR MODELLING ###

## linear regression models for DAYTIME AWS TEMP without wind attentuation effect

model_a1<-lm(reg_day$diff_ZOLL_3m~reg_day$ZOLL_STRAHLUNG_GLOBAL+reg_day$ZOLL_WIND_mean) # ZOLL 3m
summary(model_a1)
#plot(model_a1)
rmse(reg_day$diff_ZOLL_3m,model_a1$fitted.values)

model_a2 <- lm(reg_day$diff_ZOLL_2m~reg_day$ZOLL_STRAHLUNG_GLOBAL+reg_day$ZOLL_WIND_mean) # ZOLL 3m
summary(model_a2)
#plot(model_a2)
rmse(reg_day$diff_ZOLL_2m,model_a2$fitted.values)

model_b <- lm(reg_day$diff_AFU~reg_day$AFU_STRAHLUNG_GLOBAL+reg_day$AFU_WIND_mean) # AFU
summary(model_b)
#plot(model_b)
rmse(reg_day$diff_AFU,model_b$fitted.values)

model_c <- lm(reg_day$diff_BOLL~reg_day$BOLL_STRAHLUNG_GLOBAL+reg_day$BOLL_WIND_mean) # BOLL
summary(model_c)
#plot(model_c)
rmse(reg_day$diff_BOLL,model_c$fitted.values)

## Calculate wind attenuation terms (Auchmann & Br?nnimann 2012) for radiation correction

reg_day$ZOLL_wind_att <-  (1-exp(-2.6/reg_day$ZOLL_WIND_mean))*reg_day$ZOLL_STRAHLUNG_GLOBAL
summary(reg_day$ZOLL_wind_att)
plot(reg_day$ZOLL_wind_att~reg_day$ZOLL_WIND_mean )


reg_day$AFU_wind_att <- (1-exp(-2.6/reg_day$AFU_WIND_mean))*reg_day$AFU_STRAHLUNG_GLOBAL
summary(reg_day$AFU_wind_att)
plot(reg_day$AFU_wind_att~reg_day$AFU_WIND_mean )

reg_day$BOLL_wind_att <- (1-exp(-2.6/reg_day$BOLL_WIND_mean))*reg_day$BOLL_STRAHLUNG_GLOBAL
summary(reg_day$BOLL_wind_att)
plot(reg_day$BOLL_wind_att~reg_day$BOLL_WIND_mean )


## linear Models for DAYTIME ERROR with wind attenuation & global radiation


# Zollikofen
model_ZOLL<-lm(reg_day$diff_ZOLL_2m~reg_day$ZOLL_wind_att)
summary(model_ZOLL)
#plot(model_ZOLL)
summary(model_ZOLL$residuals)
rmse(reg_day$diff_ZOLL_3m,model_ZOLL$fitted.values)

# Wankdorf
model_AFU<-lm(reg_day$diff_AFU~reg_day$AFU_wind_att)
summary(model_AFU)
#plot(model_AFU)
summary(model_AFU$residuals)
rmse(reg_day$diff_AFU,model_AFU$fitted.values)

# Bollwerk
model_BOLL<-lm(reg_day$diff_BOLL~reg_day$BOLL_wind_att)
summary(model_BOLL)
#plot(model_BOLL)
summary(model_BOLL$residuals)
rmse(reg_day$diff_BOLL,model_BOLL$fitted.values)


## general linear regression models for NIGHT-TIME ERRORS

model_a<-lm(reg_night$diff_ZOLL_2m~reg_night$ZOLL_WIND_mean)
summary(model_a)
#plot(model_a)
rmse(reg_night$diff_ZOLL_3m,model_a$fitted.values)

model_b <- lm(reg_night$diff_AFU~reg_night$AFU_WIND_mean)
summary(model_b)
rmse(reg_night$diff_AFU,model_b$fitted.values)

model_c <- glm(reg_night$diff_BOLL~reg_night$BOLL_WIND_mean)
summary(model_c)
rmse(reg_night$diff_BOLL,model_c$fitted.values)

## Calculate wind attenuation terms (Auchmann & Br?nnimann 2012) for radiation correction --> Nighttime

reg_night$ZOLL_wind_att <-  1-exp(-2.6/reg_night$ZOLL_WIND_mean)
summary(reg_night$ZOLL_wind_att)

reg_night$AFU_wind_att <- 1-exp(-2.6/reg_night$AFU_WIND_mean)
summary(reg_night$AFU_wind_att)

reg_night$BOLL_wind_att <- 1-exp(-2.6/reg_night$BOLL_WIND_mean)
summary(reg_night$BOLL_wind_att)


## general linear Models for NIGHTTIME AWS TEMP with wind attenuation

# Zollikofen

model_ZOLL<-glm(reg_night$ZOLL_TEMP_2m~reg_night$ZOLL_Log_99_3m + reg_night$ZOLL_wind_att)
summary(model_ZOLL)
#plot(model_ZOLL)
summary(model_ZOLL$fitted.values)
reg_night$ZOLL_diff_modelled <- model_ZOLL$fitted.values - reg_night$ZOLL_TEMP_2m # calculate difference between AWS and modelled "AWS" values
plot(reg_night$ZOLL_diff_modelled~reg_night$ZOLL_WIND_mean) # plot modelled differences vs. wind speed

RMSE_mod<- rmse(reg_night$ZOLL_TEMP_2m,model_ZOLL$fitted.values) # calculate RMSE between modelled and measured AWS temperatures
RMSE_mod
RMSE_reduction <- 100 - (RMSE_mod/(0.34 /100))
RMSE_reduction

# Amt f?r Umwelt
model_AFU<-glm(reg_night$AFU_TEMP~reg_night$AFU_Log_83_3m + reg_night$AFU_wind_att)
summary(model_AFU)
#plot(model_AFU)
summary(model_AFU$fitted.values)
reg_night$AFU_diff_modelled <- model_AFU$fitted.values - reg_night$AFU_TEMP# calculate difference between AWS and modelled "AWS" values
plot(reg_night$AFU_diff_modelled~reg_night$AFU_WIND_mean) # plot modelled differences vs. wind speed

RMSE_mod<- rmse(reg_night$AFU_TEMP,model_AFU$fitted.values) # calculate RMSE between modelled and measured AWS temperatures
RMSE_mod
RMSE_reduction <- 100 - (RMSE_mod/(0.2 /100))
RMSE_reduction

# Bollwerk
model_BOLL<-glm(reg_night$BOLL_TEMP~reg_night$BOLL_Log_64 + reg_night$BOLL_wind_att)
summary(model_BOLL)
#plot(model_BOLL)
summary(model_BOLL$fitted.values)
reg_night$BOLL_diff_modelled <- model_BOLL$fitted.values - reg_night$BOLL_TEMP# calculate difference between AWS and modelled "AWS" values
plot(reg_night$BOLL_diff_modelled~reg_night$BOLL_WIND_mean) # plot modelled differences vs. wind speed

RMSE_mod<- rmse(reg_night$BOLL_TEMP,model_BOLL$fitted.values) # calculate RMSE between modelled and measured AWS temperatures
RMSE_mod
RMSE_reduction <- 100 - (RMSE_mod/(0.19 /100))
RMSE_reduction




#### 7. PLOTS ####

## Boxplots 


## Daytime: create dataframe with raw temperature data only: 10MIN

data_day_boxplot <- data.frame(data_day$BOLL_Log_64,data_day$BOLL_TEMP,data_day$ZOLL_Log_99_3m,data_day$ZOLL_TEMP_2m,data_day$AFU_Log_83_3m,data_day$AFU_TEMP)

myList <- list(data_day_boxplot)

df <- melt(myList)  



# Daytime: Plot with 3 locations (colours): Logger (light) and professional (dark): 10MIN

ggplot(df, aes(x=variable,y=value,fill=variable)) + # definition 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot()+ 
  theme(legend.position = "none")+ # omit whole legend
  title("blabla")+
  scale_fill_manual(values=c("lightblue","darkblue","green","darkgreen","red", "darkred")) #colours of boxplots

summary(data_day_boxplot) # look up exact numbers



## Nighttime: create dataframe with raw temperature data only

data_night_boxplot <- data.frame(data_night$BOLL_Log_64,data_night$BOLL_TEMP,data_night$ZOLL_Log_99_3m,data_night$ZOLL_TEMP_2m,data_night$AFU_Log_83_3m,data_night$AFU_TEMP)

myList <- list(data_night_boxplot)

df <- melt(myList) 

# Nighttime: Plot with 3 locations (colours): Logger (light) and professional (dark): 10MIN

ggplot(df, aes(x=variable,y=value,fill=variable)) + # definition 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot()+ 
  theme(legend.position = "none")+ # omit whole legend
  scale_fill_manual(values=c("lightblue","darkblue","green","darkgreen","red", "darkred")) #colours of boxplots
summary(data_night_boxplot)




## Differences (dT zwischen Logger und Station) by daytime hours for each station: 10MIN

# create subset of deltaT of all 3 stations
data_delta <- subset(data, select=c("Date_time_CET", "Hour", "diff_BOLL","diff_ZOLL_3m","diff_AFU"))
means <- by(data$diff_BOLL, data$Hour, mean,na.omit=FALSE)

# Plot for BOLLWERK 

boxplot(diff_BOLL~Hour, data=data_delta,
        main="10-MIN deltaT for Bollwerk",
        xlab="Daytime hour",
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        col="lightblue",
        border="darkblue")
points(means, pch = 23, cex = 0.75,
       bg = "red")
# Plot für Zollikofen 

boxplot(diff_ZOLL_3m~Hour, data=data_delta,
        main="10-MIN deltaT for Zollikofen",
        xlab="Daytime hour",
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        col="green",
        border="darkgreen")

# Plot für AFU / Wankdorf 

boxplot(diff_AFU~Hour, data=data_delta,
        main="10-MIN deltaT for Wankdorf",
        xlab="Daytime hour",
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        col="red",
        border="darkred")



#### Scatter Plots weather variables: hourly mean deltaT vs. Windspeed, wind direction and global radiation ###


reg <- data_hourly_avg


### FULL DAY PLOTS

## Plots BOLL: Windspeed & radiation


par(mfrow=c(3,2))
plot(reg$diff_BOLL~reg$BOLL_STRAHLUNG_GLOBAL,
     main="Bollwerk: deltaT vs. radiation",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     pch=3)

plot(reg$diff_BOLL~reg$BOLL_WIND_mean,
     main="Bollwerk: deltaT vs. windspeed",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)


## Plots ZOLL: Windspeed & radiation


plot(reg$diff_ZOLL_3m~reg$ZOLL_STRAHLUNG_GLOBAL,
     main="Zollikofen: deltaT vs. radiation",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     pch=3)

plot(reg$diff_ZOLL_3m~reg$ZOLL_WIND_mean,
     main="Zollikofen: deltaT vs. windspeed",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)


## Plots Wankdorf: Windspeed & radiation


plot(reg$diff_AFU~reg$AFU_STRAHLUNG_GLOBAL,
     main="Wankdorf: deltaT vs. radiation",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     pch=3)

plot(reg$diff_AFU~reg$AFU_WIND_mean,
     main="Wankdorf: deltaT vs. windspeed",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),  
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)



par(mfrow=c(1,1))




#### 8. PLOTS PAPER ####


## FIGURE 3: Differences (dT zwischen Logger und Station) by daytime hours for each station: HOUR


# create subset of deltaT of all 3 stations
data_delta <- subset(data_hourly_avg, select=c("DateHour", "Hour", "diff_BOLL","diff_ZOLL_3m","diff_ZOLL_2m","diff_AFU"))

jpeg(file="Figure_3.jpeg",width=2500,height=4000, res=375)
par(mfrow=c(4,1))


# Plot für Zollikofen 3 Meter

boxplot(diff_ZOLL_3m~Hour, data=data_delta,
        #main="ZOLL_3m",
        xlab="Daytime hour (CET)",
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        ylim = c(-1.5, 3),
        #range=3, # set interquartile range
        col="cornflowerblue",
        border="black",
        notch=FALSE)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)
title(main=expression(paste(bold("(a) "),"ZOLL_3m")), adj=0)

# Calculate Median values of for daytime hours

summaryBy(diff_ZOLL_3m ~ Hour, data = data_delta, 
          FUN = list(median))

# Plot für Zollikofen 2 Meter

boxplot(diff_ZOLL_2m~Hour, data=data_delta,
        #main="ZOLL_2m",
        xlab="Daytime hour (CET)",
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        ylim = c(-1.5, 3),
        #range=3, # set interquartile range
        col="cornflowerblue",
        border="black",
        notch=FALSE)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)
title(main=expression(paste(bold("(b) "),"ZOLL_2m")), adj=0)

# Calculate Median values of for daytime hours
summaryBy(diff_ZOLL_2m ~ Hour, data = data_delta, 
          FUN = list(median))


# Plot für AFU / Wankdorf 

boxplot(diff_AFU~Hour, data=data_delta,
        #main="AFU",
        xlab="Daytime hour (CET)",
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        ylim = c(-1.5, 3),
        #range=3, # set interquartile range
        col="darkolivegreen4",
        border="black",
        notch=FALSE)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)
title(main=expression(paste(bold("(c) "),"AFU")), adj=0)

# Calculate Median values of for daytime hours
summaryBy(diff_AFU ~ Hour, data = data_delta, 
          FUN = list(median))

# Plot for BOLLWERK 

boxplot(diff_BOLL~Hour, data=data_delta,
        #main="BOLL",
        xlab="Daytime hour (CET)",
        ylab=expression(paste(Delta,italic("T") ," (K)")) ,
        #range=3, # set interquartile range
        ylim = c(-1.5, 3),
        col="darkorchid",
        border="black",
        notch=FALSE)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)
title(main=expression(paste(bold("(d) "),"BOLL")), adj=0)

# Calculate Median values of for daytime hours
summaryBy(diff_BOLL ~ Hour, data = data_delta, 
          FUN = list(median))


par(mfrow=c(1,1))


dev.off()




### FIGURE 4: DAY / NIGHT PLOTS SEPERATED


# OLD VERSION!!!!!


## PLOT

jpeg(file="Figure_4_old.jpeg",width=4500,height=3500, res=350)

par(mfrow=c(4,3))



## Plots ZOLL 3 Meter: Windspeed & radiation


plot(reg_day$diff_ZOLL_3m~reg_day$ZOLL_STRAHLUNG_GLOBAL,
     #main="ZOLL_3m daytime",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     pch=3)
title(main=expression(paste(bold("(a) "),"ZOLL_3m daytime")), adj=0)
#abline(lm(reg_day$diff_ZOLL_3m~reg_day$ZOLL_STRAHLUNG_GLOBAL),col="red")

plot(reg_day$diff_ZOLL_3m~reg_day$ZOLL_WIND_mean,
     #main="ZOLL_3m daytime",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(e) "),"ZOLL_3m daytime")), adj=0)

plot(reg_night$diff_ZOLL_3m~reg_night$ZOLL_WIND_mean,
     #main="ZOLL_3m night-time",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(i) "),"ZOLL_3m night-time")), adj=0)

## Plots Zollikofen 2 Meter: Windspeed & radiation

plot(reg_day$diff_ZOLL_2m~reg_day$ZOLL_STRAHLUNG_GLOBAL,
     #main="ZOLL_2m daytime",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     pch=3)
title(main=expression(paste(bold("(b) "),"ZOLL_2m daytime")), adj=0)
#abline(lm(reg_day$diff_ZOLL_3m~reg_day$ZOLL_STRAHLUNG_GLOBAL),col="red")

plot(reg_day$diff_ZOLL_2m~reg_day$ZOLL_WIND_mean,
     #main="ZOLL_2m daytime",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(f) "),"ZOLL_2m daytime")), adj=0)

plot(reg_night$diff_ZOLL_2m~reg_night$ZOLL_WIND_mean,
     #main="ZOLL_2m night-time",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(j) "),"ZOLL_2m night-time")), adj=0)

## Plots Wankdorf: Windspeed & radiation


plot(reg_day$diff_AFU~reg_day$AFU_STRAHLUNG_GLOBAL,
     #main="AFU daytime",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     pch=3)
title(main=expression(paste(bold("(c) "),"AFU daytime")), adj=0)
#abline(lm(reg_day$diff_AFU~reg_day$AFU_STRAHLUNG_GLOBAL),col="red")

plot(reg_day$diff_AFU~reg_day$AFU_WIND_mean,
     #main="AFU daytime",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(g) "),"AFU daytime")), adj=0)

plot(reg_night$diff_AFU~reg_night$AFU_WIND_mean,
     #main="AFU night-time",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(k) "),"AFU night-time")), adj=0)

## Plots Bollwerk: Windspeed & radiation


plot(reg_day$diff_BOLL~reg_day$BOLL_STRAHLUNG_GLOBAL,
     #main="BOLL daytime",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     pch=3)
title(main=expression(paste(bold("(d) "),"BOLL daytime")), adj=0)
#abline(lm(reg_day$diff_BOLL~reg_day$BOLL_STRAHLUNG_GLOBAL),col="red")

plot(reg_day$diff_BOLL~reg_day$BOLL_WIND_mean,
     #main="BOLL daytime",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(h) "),"BOLL daytime")), adj=0)

plot(reg_night$diff_BOLL~reg_night$BOLL_WIND_mean,
     #main="BOLL night-time",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(l) "),"BOLL night-time")), adj=0)

par(mfrow=c(1,1))

dev.off()


## NEW VERSION (boxplots)!!!!

jpeg(file="Figure_4_new.jpeg",width=4500,height=3500, res=350)

par(mfrow=c(4,3))

## ZOLL_3m

# Daytime: Radiation
boxplot(diff_ZOLL_3m~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(a) "),"ZOLL_3m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# Daytime: Wind
boxplot(diff_ZOLL_3m~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(e) "),"ZOLL_3m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# NIght-time: Wind
boxplot(diff_ZOLL_3m~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(i) "),"ZOLL_3m night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



## ZOLL_2m

# Daytime: Radiation
boxplot(diff_ZOLL_2m~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(b) "),"ZOLL_2m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# Daytime: Wind 
boxplot(diff_ZOLL_2m~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(f) "),"ZOLL_2m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# Night-time: Wind
boxplot(diff_ZOLL_2m~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(j) "),"ZOLL_2m night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



## AFU

# Daytime: Radiation
boxplot(diff_AFU~solar_group_AFU, data=reg_day,
        #main="AFU",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkolivegreen4",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(c) "),"AFU daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# Daytime: Wind 
boxplot(diff_AFU~wind_group_AFU, data=reg_day,
        #main="AFU",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkolivegreen4",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(g) "),"AFU daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# Nighttime: Wind
boxplot(diff_AFU~wind_group_AFU, data=reg_night,
        #main="AFU",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkolivegreen4",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(k) "),"AFU night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



## BOLL

# Daytime: Radiation
boxplot(diff_BOLL~solar_group_BOLL, data=reg_day,
        #main="AFU",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkorchid",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(d) "),"BOLL daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# Daytime: Wind
boxplot(diff_BOLL~wind_group_BOLL, data=reg_day,
        #main="BOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkorchid",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(h) "),"BOLL daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# Night-time: Wind
boxplot(diff_BOLL~wind_group_BOLL, data=reg_night,
        #main="BOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkorchid",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(l) "),"BOLL night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

par(mfrow=c(1,1))

dev.off()





#### 9. ADDITIONAL PLOTS ####

## FIGURE S2 OLD: Differenz Logger 2m vs. AWS

jpeg(file="Figure_S1_old.jpeg",width=3300,height=1500, res=350)
par(mfrow=c(2,3))

## Differenz Logger vs. Stevenson

rmse(data_hourly_avg$ZOLL_Log_99_3m,data_hourly_avg$ZOLL_Log_999_KLIMAHAUS) # all data
rmse(reg_night$ZOLL_Log_99_3m,reg_night$ZOLL_Log_999_KLIMAHAUS) # night data
rmse(reg_day$ZOLL_Log_99_3m,reg_day$ZOLL_Log_999_KLIMAHAUS) # day data

summary(reg_night$diff_ZOLL_3m_2m)
# Tag hourly


plot(reg_day$diff_ZOLL_stvlog~reg_day$ZOLL_STRAHLUNG_GLOBAL,
     #main="ZOLL daytime: LCD_2m vs. Stevenson",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     pch=3)
title(main=expression(paste(bold("(a) "),"ZOLL daytime: LCD_2m vs. Stevenson")), adj=0)

plot(reg_day$diff_ZOLL_stvlog~reg_day$ZOLL_WIND_mean,
     #main="ZOLL daytime: LCD_2m vs. Stevenson",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     pch=3)
title(main=expression(paste(bold("(b) "),"ZOLL daytime: LCD_2m vs. Stevenson")), adj=0)

# Nacht hourly
plot(reg_night$diff_ZOLL_stvlog~reg_night$ZOLL_WIND_mean,
     #main="ZOLL night-time: LCD_2m vs. Stevenson",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(c) "),"ZOLL night-time: LCD_2m vs. Stevenson")), adj=0)


## Differenz Logger 3m vs. 2m


plot(reg_day$diff_ZOLL_3m_2m~reg_day$ZOLL_STRAHLUNG_GLOBAL,
     #main="ZOLL daytime: LCD_3m vs. LCD_2m",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     pch=3)
title(main=expression(paste(bold("(d) "),"ZOLL daytime: LCD_3m vs. LCD_2m")), adj=0)

#abline(lm(reg_day$diff_ZOLL_3m~reg_day$ZOLL_STRAHLUNG_GLOBAL),col="red")

plot(reg_day$diff_ZOLL_3m_2m~reg_day$ZOLL_WIND_mean,
     #main="ZOLL daytime: LCD_3m vs. LCD_2m",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(e) "),"ZOLL daytime: LCD_3m vs. LCD_2m")), adj=0)

plot(reg_night$diff_ZOLL_3m_2m~reg_night$ZOLL_WIND_mean,
     #main="ZOLL night-time: LCD_3m vs. LCD_2m",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")),
     ylim = c(-1.5, 3),
     xlim=c(0,8.5),
     pch=3)
title(main=expression(paste(bold("(f) "),"ZOLL night-time: LCD_3m vs. LCD_2m")), adj=0)

par(mfrow=c(1,1))

dev.off()


## FIGURE S2 NEW: Differenz Logger 2m vs. AWS

jpeg(file="Figure_S1_new.jpeg",width=4500,height=1500, res=350)
par(mfrow=c(2,3))


## DAYTIME: SOLAR

# ZOLL LCD_2m vs. Stevenson screen
boxplot(diff_ZOLL_stvlog~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1.5, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(a) "),"ZOLL daytime: LCD_2m vs. Stevenson screen")), adj=0)
lapply(seq(-1.5,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


# ZOLL LCD_2m vs. Stevenson screen
boxplot(diff_ZOLL_stvlog~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1.5, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(b) "),"ZOLL daytime: LCD_2m vs. Stevenson screen")), adj=0)
lapply(seq(-1.5,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# ZOLL LCD_2m vs. Stevenson screen
boxplot(diff_ZOLL_stvlog~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1.5, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(c) "),"ZOLL night-time: LCD_2m vs. Stevenson screen")), adj=0)
lapply(seq(-1.5,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# ZOLL LCD_3m vs. LCD_2m
boxplot(diff_ZOLL_3m_2m~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1.5, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(d) "),"ZOLL daytime: LCD_3m vs. LCD_2m")), adj=0)
lapply(seq(-1.5,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



# ZOLL LCD_3m vs. LCD_2m
boxplot(diff_ZOLL_3m_2m~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1.5, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(e) "),"ZOLL daytime: LCD_3m vs. LCD_2m")), adj=0)
lapply(seq(-1.5,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


# ZOLL LCD_3m vs. LCD_2m
boxplot(diff_ZOLL_3m_2m~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1.5, 3),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(f) "),"ZOLL night-time: LCD_3m vs. LCD_2m")), adj=0)
lapply(seq(-1.5,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


par(mfrow=c(1,1))

dev.off()


## Figure XY: 10min-values

# Tag 10min
plot(data_day$diff_ZOLL_stvlog~data_day$ZOLL_STRAHLUNG_GLOBAL,
     main="Zollikofen Day: deltaT Log/Stevenson vs. radiation",
     xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-5, 6))

plot(data_day$diff_ZOLL_stvlog~data_day$ZOLL_WIND_mean,
     main="Zollikofen Day: deltaT Log/Stevenson vs. WIND",
     xlab=expression(paste("Wind speed (m s"^"-1",")")),
     ylab=expression(paste(Delta,italic("T") ," (K)")), 
     ylim = c(-5, 6),
     pch = 3)


## FIGURE S2 ##

# Windgeschwindigkeit nach Tageszeit

jpeg(file="Figure_S2.jpeg",width=2500,height=4000, res=375)

par(mfrow=c(3,1))


boxplot(ZOLL_WIND_mean~Hour, data=data_hourly_avg,
        #main="ZOLL",
        xlab="Daytime hour (CET)",
        ylab=expression(paste("Wind speed (m s"^"-1",")")),
        #range=3, # set interquartile range
        ylim = c(0, 10),
        col="cornflowerblue",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(a) "),"ZOLL")), adj=0)
lapply(seq(0,10,2), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html

boxplot(AFU_WIND_mean~Hour, data=data_hourly_avg,
        #main="AFU",
        xlab="Daytime hour (CET)",
        ylab=expression(paste("Wind speed (m s"^"-1",")")),
        #range=3, # set interquartile range
        ylim = c(0, 10),
        col="darkolivegreen4",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(b) "),"AFU")), adj=0)
lapply(seq(0,10,2), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html

boxplot(BOLL_WIND_mean~Hour, data=data_hourly_avg,
        #main="BOLL",
        xlab="Daytime hour (CET)",
        ylab=expression(paste("Wind speed (m s"^"-1",")")),
        #range=3, # set interquartile range
        ylim = c(0, 10),
        col="darkorchid",
        border="black",
        notch=FALSE)
title(main=expression(paste(bold("(c) "),"BOLL")), adj=0)
lapply(seq(0,10,2), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
par(mfrow=c(1,1))

dev.off()






## NEW (SINGLE) PLOTS FIG. 4

## DAYTIME: SOLAR

# ZOLL_3m
boxplot(diff_ZOLL_3m~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(a) "),"ZOLL_3m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



# ZOLL_2m
boxplot(diff_ZOLL_2m~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(b) "),"ZOLL_2m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)




# AFU
boxplot(diff_AFU~solar_group_AFU, data=reg_day,
        #main="AFU",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(c) "),"AFU daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



# BOLL
boxplot(diff_BOLL~solar_group_BOLL, data=reg_day,
        #main="AFU",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkorchid",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(d) "),"BOLL daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



## DAYTIME: WIND SPEED

# ZOLL_3m
boxplot(diff_ZOLL_3m~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(e) "),"ZOLL_3m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# ZOLL_2m
boxplot(diff_ZOLL_2m~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(f) "),"ZOLL_2m daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# AFU
boxplot(diff_AFU~wind_group_AFU, data=reg_day,
        #main="AFU",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(g) "),"AFU daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# BOLL
boxplot(diff_BOLL~wind_group_BOLL, data=reg_day,
        #main="BOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkorchid",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(h) "),"BOLL daytime")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


## NIGHT-TIME: WIND SPEED

# ZOLL_3m
boxplot(diff_ZOLL_3m~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(i) "),"ZOLL_3m night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# ZOLL_2m
boxplot(diff_ZOLL_2m~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(j) "),"ZOLL_2m night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# AFU
boxplot(diff_AFU~wind_group_AFU, data=reg_night,
        #main="AFU",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(k) "),"AFU night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# BOLL
boxplot(diff_BOLL~wind_group_BOLL, data=reg_night,
        #main="BOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="darkorchid",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(l) "),"BOLL night-time")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)



## NEW (SINGLE) PLOTS FIG. S1

## DAYTIME: SOLAR

# ZOLL LCD_2m vs. Stevenson screen
boxplot(diff_ZOLL_stvlog~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(a) "),"ZOLL daytime: LCD_2m vs. Stevenson")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# ZOLL LCD_3m vs. LCD_2m
boxplot(diff_ZOLL_3m_2m~solar_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Solar irradiance (W m"^"-2",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(d) "),"ZOLL daytime: LCD_3m vs. LCD_2m")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


## DAYTIME: WIND SPEED

# ZOLL LCD_2m vs. Stevenson screen
boxplot(diff_ZOLL_stvlog~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(b) "),"ZOLL daytime: LCD_2m vs. Stevenson")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# ZOLL LCD_3m vs. LCD_2m
boxplot(diff_ZOLL_3m_2m~wind_group_ZOLL, data=reg_day,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(e) "),"ZOLL daytime: LCD_3m vs. LCD_2m")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


## NIGHT-TIME: WIND SPEED

# ZOLL LCD_2m vs. Stevenson screen
boxplot(diff_ZOLL_stvlog~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(c) "),"ZOLL_night-time: LCD_2m vs. Stevenson")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)

# ZOLL LCD_3m vs. LCD_2m
boxplot(diff_ZOLL_3m_2m~wind_group_ZOLL, data=reg_night,
        #main="ZOLL",
        xlab=expression(paste("Wind speed (m s"^"-1",")")),
        ylab=expression(paste(Delta,italic("T") ," (K)")), 
        #range=3, # set interquartile range
        ylim = c(-1, 3),
        col="cadetblue2",
        border="darkslateblue",
        notch=FALSE)
title(main=expression(paste(bold("(f) "),"ZOLL_night-time: LCD_3m vs. LCD_2m")), adj=0)
lapply(seq(-1,3,0.5), function(x) abline(a = x,b = 0, lty = 3, lwd=0.5)) # Add grid lines for orientation: http://howtoinr.weebly.com/add-reference-lines.html
abline(h=0, lty=1, lwd=1)


########################
# # # Get Yo Packs # # #
########################
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "arules", "arulesViz",
               "graphics", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "RMySQL",
               "RSQLite", "wesanderson", "lubridate",
               "tidyverse", "plotly", "ggfortify")

################################
####       LOAD DATA        ####
################################

## connect to remote database 
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')


## Use attribute names to specify specific attributes for download
## 2006
yr2006 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3, global_active_power FROM yr_2006")

## 2007
yr2007 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3, global_active_power FROM yr_2007")
## 2008
yr2008 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3, global_active_power FROM yr_2008")
## 2009
yr2009 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3, global_active_power FROM yr_2009")
## 2010
yr2010 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3, global_active_power FROM yr_2010")


########################################
####       PRE - PROCESS            ####
########################################

################################
###    Order the data frame ####
################################

## Bind whole enitre years together: 2007 - 2009
energy <- bind_rows(yr2007, yr2008, yr2009)

## Combine Date and Time attribute values in a new attribute column
energy$DateTime <- paste(energy$Date,energy$Time)

## rename the columns
colnames(energy)[3:6] <- c("kitchen", "laundry", "climat", "global_kw")

## make an extra column "total" with total submeter energy consumption
energy$total_subs <- energy$kitchen + energy$laundry + energy$climat

## transform "global" from kW to Wh
energy$global_wh <- round(energy$global_kw/60*1000,
                          digits = 1)
## reorder the columns
energy <- energy[c("DateTime", "Date", "Time",
                   "kitchen", "laundry", "climat",
                   "total_subs", "global_wh", "global_kw")]


##########################
##  Specify data types  ##
##          &           ##
## order with Lubridate ##
##########################

## Convert DateTime from POSIXlt to POSIXct 
energy$DateTime <- as.POSIXct(energy$DateTime,
                              "%Y-%m-%d %H:%M:%S", tz = "CET" )

## Create attributes for :
##    year,  quarter, month,
##    week, weekday, day,
##    hour and minute
energy$year <- year(energy$DateTime)
energy$quarter <- quarter(energy$DateTime)
energy$month <- month(energy$DateTime)
energy$week <- week(energy$DateTime)
## weekdays are unordered "names"
## set them as an ordered factor
energy$weekday <- weekdays(energy$DateTime) 
energy$weekday <- factor(energy$weekday,
                         levels = unique(energy$weekday),ordered=TRUE)
energy$day <- day(energy$DateTime)
energy$hour <- hour(energy$DateTime)
energy$minute <- minute(energy$DateTime)



##########################
##        BONUS         ##
##  CALCULATE YEARLY    ##
## ENERGY CONSUMPTION   ##
##########################

## energy$total_subs gives total Wh consumed every minute
## through all submeters
sum_total_subs <- sum(energy$total_subs)
# calculate average yearly consumption in kWh 
yearly_subs_kWh <- sum_total_subs / 3 / 1000 

## energy$global_wh gives total Wh consumed every minute
## in the entire house
sum_global_wh <- sum(energy$global_wh)
# calculate average yearly consumption in kWh 
yearly_global_kwh <- sum_global_wh / 3 / 1000 






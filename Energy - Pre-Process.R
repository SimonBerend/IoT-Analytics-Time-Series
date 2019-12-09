########################
# # # Get Yo Packs # # #
########################
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "arules", "arulesViz",
               "graphics", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "RMySQL",
               "RSQLite", "wesanderson", "lubridate",
               "tidyverse", "plotly", "ggfortify",
               "padr")

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
energy <- bind_rows(yr2007, yr2008, yr2009, yr2010)

## rename the columns
colnames(energy)[3:6] <- c("kitchen", "laundry", "climat", "global_kw")



######################################
## Create and Complete Date Stamps  ##
######################################
## Combine Date and Time attribute values in a new attribute column
energy$DateTime <- paste(energy$Date,energy$Time)

## Convert DateTime from POSIXlt to POSIXct 
energy$DateTime <- as.POSIXct(energy$DateTime,
                              "%Y-%m-%d %H:%M:%S", tz = "CET" )

## COMPLETE TIME STAMPS
energy <- pad(energy,
              interval = "min",
              break_above = 2053503)

## when assigning winter/summer time
## r expects doubble values for some times, which it cannot find
## the result are some NA's in the DateTime column
## remove these (although not ideal data management)
energy <- energy[complete.cases(energy$DateTime), ]


######################################
## for [i] loop that adresses NA's  ##
######################################

for(i in 1:nrow(energy)) {
  if(is.na(energy$laundry[i])) {
    
    #create empty data frame with same columns as energy 
    check_i <- as.data.frame(energy[0,])
    
    # if possible, bind rows of one week before and after
    if(i > 10080){ 
      check_i <- rbind(check_i, energy[i - 10080,])}
    if(i < (nrow(energy) - 10080)) {
      check_i <- rbind(check_i, energy[i + 10080,])}
    
    # calculate the mean of the variables added to the empty df
    mean_i_kitchen <- mean(check_i$kitchen, na.rm = TRUE)
    mean_i_laundry <- mean(check_i$laundry, na.rm = TRUE)
    mean_i_climat <- mean(check_i$climat, na.rm = TRUE)
    mean_i_global_kw <- mean(check_i$global_kw, na.rm = TRUE)
    
    # replace the NA in df with mean value
    energy$kitchen[i] <- mean_i_kitchen
    energy$laundry[i] <- mean_i_laundry
    energy$climat[i] <- mean_i_climat
    energy$global_kw[i] <- mean_i_global_kw
    
  }
}




#################################
##  Create derivative Values   ##
#################################
## make an extra column "total" with total submeter energy consumption
energy$total_subs <- energy$kitchen + energy$laundry + energy$climat

## transform "global" from kW to Wh
energy$global_wh <- round(energy$global_kw/60*1000,
                          digits = 1)
## reorder the columns
energy <- energy[c("DateTime", "Date", "Time",
                   "kitchen", "laundry", "climat",
                   "total_subs", "global_wh", "global_kw")]

###########################################
## Create time attributes with Lubridate ##
###########################################
energy$year <- year(energy$DateTime)
energy$quarter <- quarter(energy$DateTime)
energy$month <- month(energy$DateTime)
energy$week <- week(energy$DateTime)
## weekdays are unordered "characters"
## set them as an ordered factor
energy$weekday <- weekdays(energy$DateTime) 
energy$weekday <- factor(energy$weekday,
                         levels = unique(energy$weekday),ordered=TRUE)
energy$day <- day(energy$DateTime)
energy$hour <- hour(energy$DateTime)
energy$minute <- minute(energy$DateTime)



#############################
## Extra: CALCULATE YEARLY ##
## ENERGY CONSUMPTION      ##
#############################
## energy$global_wh gives total Wh consumed every minute
## in the entire house
sum_global_wh <- energy %>% filter(year != 2010 ) %>%  sum(energy$global_wh)
# calculate average yearly consumption in kWh 
yearly_global_kwh <- sum_global_wh / 3 / 1000 






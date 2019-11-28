   # # # Get Yo Packs # # #
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "arules", "arulesViz",
               "graphics", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "RMySQL",
               "RSQLite", "wesanderson", "lubridate",
               "tidyverse")

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
                     Sub_metering_3 FROM yr_2006")

## 2007
yr2007 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3 FROM yr_2007")
## 2008
yr2008 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3 FROM yr_2008")
## 2009
yr2009 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3 FROM yr_2009")
## 2010
yr2010 <- dbGetQuery(con,"SELECT Date, Time,
                     Sub_metering_1, Sub_metering_2,
                     Sub_metering_3 FROM yr_2010")


        ########################################
       ####       PRE - PROCESS            ####
      ########################################

## Bind whole enitre years together: 2007 - 2009
energy <- bind_rows(yr2007, yr2008, yr2009)

## Combine Date and Time attribute values in a new attribute column
energy$DateTime <- paste(energy$Date,energy$Time)

## make an extra column with total energy consumption
energy$total <- energy$kitchen + energy$laundry + energy$climat
               
## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(energy)
colnames(energy)[3:5] <- c("kitchen", "laundry", "climat")
energy <- energy[c("DateTime", "Date", "Time", "kitchen", "laundry", "climat")]

## Convert DateTime from POSIXlt to POSIXct 
energy$DateTime <- as.POSIXct(energy$DateTime,
                              "%Y-%m-%d %H:%M:%S", tz = "CET" )

## Check time zone
attr(energy$DateTime, "tzone")


  ### Lubridate that Shzz  ###
 ## Add time zone without changing the clock time
## force_tz(energy$DateTime, "CET")

## Create attributes for : year,  quarter, month, week, weekday, day, hour and minute
energy$year <- year(energy$DateTime)
energy$quarter <- quarter(energy$DateTime)
energy$month <- month(energy$DateTime)
energy$week <- week(energy$DateTime)
energy$weekday <- weekdays(energy$DateTime)
energy$weekday <- factor(energy$weekday, levels = c("maandag", "dinsdag",
                                                    "woensdag", "donderdag",
                                                    "vrijdag", "zaterdag", "zondag"))

energy$day <- day(energy$DateTime)
energy$hour <- hour(energy$DateTime)
energy$minute <- minute(energy$DateTime)


    ################################
   ####   Daily (per Hour)     #### 
  ################################

## Group data set by time attributes ##
enHour <- energy %>% na.omit() %>%
                     group_by(hour) %>% 
                     summarise(mKitchen = mean(kitchen),
                               mLaundry = mean(laundry),
                               mClimat = mean(climat))

## line plot: areas / hours
ggplot(enHour, aes(x=hour)) + 
  geom_line(aes(y = mKitchen), color = "darkred", size = 1) +
  geom_line(aes(y = mLaundry), color = "darkblue", size = 1) + 
  geom_line(aes(y = mClimat), color="darkgreen", linetype="twodash", size = 1)

## line plot: areas / hour : tydiverse
# Data preparation
tiHour <- enHour %>%
  select(hour, mKitchen, mLaundry, mClimat) %>%
  pivot_longer(-hour, names_to = "submeter", values_to = "Watth")
  
# Visualization
ggplot(tiHour, aes(x = hour, y = Watth)) + 
  geom_line(aes(color = submeter, 
                linetype = submeter)) + 
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Mean Daily E-Consumption",
       subtitle = "(2007-2009)",
       x = "Time (0-24 h)",
       y = "Consumption (W/h - active e)")+
  theme_bw()


      ################################
     ####    Weekly (per Day)    #### 
    ################################

## Group data per weekday 
enWeek <- energy %>% na.omit() %>%
  group_by(weekday) %>% 
  summarise(mKitchen = mean(kitchen),
            mLaundry = mean(laundry),
            mClimat = mean(climat))

## line plot: areas / weekday : tydiverse
# Data preparation
tiWeek <- enWeek %>%
  select(weekday, mKitchen, mLaundry, mClimat) %>%
  pivot_longer(-weekday, names_to = "submeter", values_to = "Watth")

# Visualization
ggplot(tiWeek, aes(x = weekday, y = Watth, group = submeter)) + 
  geom_line(aes(color = submeter,
                linetype = submeter)) + 
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Mean Weekly E-Consumption",
       subtitle = "(2007-2009)",
       x = "Days",
       y = "Consumption (W/h - active e)")+
  theme_bw()

   ################################
  ####      Monthly (per day) #### 
 ################################

## Group data per day
enMon <- energy %>% na.omit() %>%
  group_by(day) %>% 
  summarise(mKitchen = mean(kitchen),
            mLaundry = mean(laundry),
            mClimat = mean(climat))

## line plot: areas / day : tydiverse
# Data preparation
tiMon <- enMon %>%
  select(day, mKitchen, mLaundry, mClimat) %>%
  pivot_longer(-day, names_to = "submeter", values_to = "Watth")

# Visualization
ggplot(tiMon, aes(x = day, y = Watth)) + 
  geom_line(aes(color = submeter, 
                linetype = submeter)) + 
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Mean Monthly E-Consumption",
       subtitle = "(2007-2009)",
       x = "Days (1-31)",
       y = "Consumption (W/h - active e)")+
  theme_bw()


################################
####        Years to do           #### 
################################

tot2007 <- energy %<% subset()



## Group data set by time attributes ##
enHour <- energy %>% na.omit() %>%
  group_by(hour) %>% 
  summarise(mKitchen = mean(kitchen),
            mLaundry = mean(laundry),
            mClimat = mean(climat))

## line plot: areas / hours
ggplot(enHour, aes(x=hour)) + 
  geom_line(aes(y = mKitchen), color = "darkred", size = 1) +
  geom_line(aes(y = mLaundry), color = "darkblue", size = 1) + 
  geom_line(aes(y = mClimat), color="darkgreen", linetype="twodash", size = 1)

## line plot: areas / hour : tydiverse
# Data preparation
tiHour <- enHour %>%
  select(hour, mKitchen, mLaundry, mClimat) %>%
  pivot_longer(-hour, names_to = "submeter", values_to = "Watth")

# Visualization
ggplot(tiHour, aes(x = hour, y = Watth)) + 
  geom_line(aes(color = submeter, 
                linetype = submeter)) + 
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Mean Weekly E-Consumption",
       subtitle = "(2007-2009)",
       x = "Time (0-24 h)",
       y = "Consumption (W/h - active e)")+
  theme_bw()

month(energy$DateTime)
day()

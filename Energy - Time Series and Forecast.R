#################################################################
#####                 TIME SERIES  and FORECAST            ######
#################################################################

#####################################################
## Run pre-process  ## Packages Needed are There  ###
#####################################################


source(file = "C:/Users/Gebruiker/Desktop/Esgitit/IoT Analytics/Energy - Pre-Process.R",
       local = FALSE)


################################################
##            PACKAGES                       ###
################################################
install.packages("forecast")
library(forecast)





################################################
###           CREATE SUBSETS                 ###
################################################

# group per hour 
en_hour <- energy %>% 
  group_by(
    # create one column for year - month - date - hour
    y_m_d_h = format(energy$DateTime,
                   "%Y-%m-%d %H")) %>% 
  summarise(avg_wh_per_min_kitchen = mean(kitchen),
            avg_wh_per_min_laundry = mean(laundry),
            avg_wh_per_min_climat = mean(climat), 
            avg_wh_per_min_total_subs = mean(total_subs),
            avg_wh_per_min_global = mean(global_wh), 
            avg_kw_global = mean(global_kw)
            ) 

# group per day
en_day <- energy %>% 
  group_by(
    # create one column for year - month - date
    y_m_d = format(energy$DateTime,
                     "%Y-%m-%d")) %>% 
  summarise(avg_wh_per_min_kitchen = round(mean(kitchen),
                                           digits = 2),
            avg_wh_per_min_laundry = round(mean(laundry),
                                           digits = 2),
            avg_wh_per_min_climat = round(mean(climat),
                                          digits = 2),
            avg_wh_per_min_total_subs = round(mean(total_subs),
                                              digits = 2),
            avg_wh_per_min_global = round(mean(global_wh),
                                          digits = 2),
            avg_kw_global = round(mean(global_kw),
                                  digits = 2)
            )

# group per week
en_week <- energy %>% 
  group_by(
    # create one column for year - week
    year_week = format(energy$DateTime,
                       "%Y - %W")) %>% 
  summarise(avg_wh_per_min_kitchen = round(mean(kitchen),
                                           digits = 2),
            avg_wh_per_min_laundry = round(mean(laundry),
                                           digits = 2),
            avg_wh_per_min_climat = round(mean(climat),
                                          digits = 2),
            avg_wh_per_min_total_subs = round(mean(total_subs),
                                              digits = 2),
            avg_wh_per_min_global = round(mean(global_wh),
                                          digits = 2),
            avg_kw_global = round(mean(global_kw),
                                  digits = 2)
  )
# get rid of the "left-over" weeks 00 & 53
# in this way, you can set freq in ts at 52 without issues
en_week <- en_week %>% filter(year_week != "2007 - 53" & year_week != "2008 - 00" &
                              year_week != "2008 - 53" & year_week != "2009 - 00" &
                              year_week != "2009 - 53" & year_week != "2010 - 00" )

# group by month  
en_month <- energy %>% 
  group_by(
    # create one column for year - month
    year_month = format(energy$DateTime,
                       "%Y - %m")) %>% 
  summarise(avg_wh_per_min_kitchen = round(mean(kitchen),
                                           digits = 2),
            avg_wh_per_min_laundry = round(mean(laundry),
                                           digits = 2),
            avg_wh_per_min_climat = round(mean(climat),
                                          digits = 2),
            avg_wh_per_min_total_subs = round(mean(total_subs),
                                              digits = 2),
            avg_wh_per_min_global = round(mean(global_wh),
                                          digits = 2),
            avg_kw_global = round(mean(global_kw),
                                  digits = 2)
  )
    
################################################
###     TURN SUB SETS INTO TS OBJECTS        ###
################################################



#############################
#####    TIME SERIES   ######
#############################

##### plot 1 #####

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
energy_weekly <- filter(energy, weekday == "maandag" & hour == 20 & minute == 0)

## Create TS object with submeter 3: climat
ts_sub3_energy_weekly <- ts(energy_weekly$climat, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(ts_sub3_energy_weekly,
         ts.colour = 'darkred',
         xlab = "Time",
         ylab = "Watt Hours",
         main = "Climat Room") +
  theme_bw()


##### plot 2  #####

## Subset to one observation per hour during the 2008 august dip
energy_aug_2008 <- filter(energy, year == 2008 & week == 33 &
                            minute == 0)

## Create TS object with global in wh
## Note: TS frequency does not recognize '24' (only 12 & 4 ?)
ts_global_wh_aug_2008 <- ts(energy_aug_2008$global_wh,
                            frequency = 24,
                            start = c(1, 1))

## Plot global wh with autoplot - add labels, color
autoplot(ts_global_wh_aug_2008,
         ts.colour = 'darkred',
         xlab = "Time",
         ylab = "Watt Hours",
         main = "Global Consumption in Week 33, 2008",
         xmin = 1,
         xmax = 8) +
  theme_bw()


##############################################################
##                    Forecasting                           ##
##############################################################

## Apply time series linear regression to the sub-meter 3 ts object: 
##  -- ts_sub3_energy_weekly: weekly energy over 3 years for climat room
## use summary to obtain R2 and RMSE from the model you built
fit_sub3 <- tslm(ts_sub3_energy_weekly ~ trend + season) 
summary(fit_sub3)


## Create Forecast for sub-meter 3. 
## Forecast ahead 20 time periods
## confidence levels 80 and 90
forecast_fit_sub3 <- forecast(fit_sub3,
                              h=20,
                              level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecast_fit_sub3, 
     ylim = c(0, 30), 
     ylab = "Watt-Hours", 
     xlab ="Time",
     main = "Forecast of Energy Consumption in Climat Room",
     sub = "based on weely sample of wh, at Mo, 20:00:00")

###############   forecast with a different subset     ###########
##
## Sub-meter 1 with your choice of frequency, time period and confidence levels
##
## Create TS object with submeter 1: kitchen
ts_kitchen_energy_weekly <- ts(energy_weekly$kitchen, frequency=52, start=c(2007,1))


## Apply time series linear regression to the kitchen ts object
fit_kitchen_weekly <- tslm(ts_kitchen_energy_weekly ~ trend + season) 
summary(fit_kitchen_weekly)

## Forecast: ahead 20 time periods (weeks)
## confidence levels 80 and 90
forecast_fit_kitchen <- forecast(fit_kitchen_weekly,
                                 h=20,
                                 level=c(80,90))

## Plot kitchen forecast
plot(forecast_fit_kitchen, 
     #ylim = c(0, 30), 
     ylab = "Watt-Hours", 
     xlab ="Time",
     main = "Forecast of Energy Consumption in Kitchen",
     sub = "based on weely sample, at Mo, 20:00:00")

########################################################
###                      DECOMPOSE                  ####
########################################################

## Decompose sub 3: climat into trend, seasonal and remainder
components_sub3_weekly <- decompose(ts_sub3_energy_weekly)
## Plot decomposed sub-meter 3 
plot(components_sub3_weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components_sub3_weekly)

## adjust original ts set for seasonality
ts_sub3_weekly_season_adj <- ts_sub3_energy_weekly - 
  components_sub3_weekly$seasonal

## Test Seasonal Adjustment by running Decompose again. 
## Note the very, very small scale for Seasonal
plot(decompose(ts_sub3_weekly_season_adj))

## Holt Winters Exponential Smoothing & Plot
ts_sub3_holtwinters <- HoltWinters(ts_sub3_weekly_season_adj,
                                   beta = FALSE,
                                   gamma = FALSE)
plot(ts_sub3_holtwinters, ylim = c(0, 25))

## HoltWinters forecast & plot
## USE FOR TREND
## with deinished confidence levels
ts_sub3_hw_fc <- forecast(ts_sub3_holtwinters,
                          h=25,
                          level = c(10, 25))
plot(ts_sub3_hw_fc,
     ylim = c(0, 20),
     ylab= "Watt-Hours",
     xlab="Time",
     main = "Climat Room",
     sub = "Smooth HW Forecast",
     start (2009))  ## How does this start work?


#####################################################
## Run pre-process  ## Packages Needed are There  ###
#####################################################

source(file = "~/IoT Analytics/Energy - Pre-Process.R",
       local = FALSE)


################################################
##            PACKAGES                       ###
################################################
install.packages("forecast")
library(forecast)



####################
## PLAN OF ATTACK ##
####################

weekEn <- energy %>%
  na.omit %>%
  filter(year == 2008 & week == 2)

## Subset the 9th day of January 2008 - All observations
dayEn <- energy %>%
  na.omit %>%
  filter(year == 2008 &
           month == 1 &
           day == 9)

## installed plotly in 'energy project' script - pacman
## Plot sub-meter 1
plot_ly(dayEn,
        x = ~dayEn$DateTime,
        y = ~dayEn$kitchen, 
        type = 'scatter', 
        mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(dayEn,
        x = ~dayEn$DateTime,y = ~dayEn$kitchen, name = 'Kitchen',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~dayEn$laundry, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~dayEn$climat, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency
dayEn10 <- energy %>% 
  filter(year == 2008 & month == 1 & day == 9 & 
           (minute == 0 | minute == 10 | minute == 20 |
            minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(dayEn10,
        x = ~dayEn10$DateTime,y = ~dayEn10$kitchen, name = 'Kitchen',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~dayEn10$laundry, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~dayEn10$climat, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

### Same as above for a week of my choosing ###
### week 40 2007  ### appears to be pretty avarage ##
## Subset week 40 2007 - 3 hour frequency
weekEn4h <- energy %>% 
  filter(year == 2007 & week == 40 &
           (hour == 0 | hour == 4 | hour == 8 | 
            hour == 12 | hour == 16 | hour == 20))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(weekEn4h,
        x = ~weekEn4h$DateTime,y = ~weekEn4h$kitchen, name = 'Kitchen',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~weekEn4h$laundry, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~weekEn4h$climat, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption week 40, 2007",
         xaxis = list(title = "Time",
                      autotick = FALSE),
         yaxis = list (title = "Power (watt-hours)"))

### And once more for a period of my choice ###
### wednesday mid-day june 2009 ###
## Subset ... - ... frequency
wedEn <- energy %>% 
  filter(year == 2009 & month == 6 & day == 5 &
           (hour == 09 | hour == 10 |hour == 11 | hour == 12 |
            hour == 13 | hour == 14 | hour == 15 | hour == 16) &
           (minute == 0 | minute == 10 | minute == 20 |
            minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(wedEn,
        x = ~wedEn$DateTime,y = ~wedEn$kitchen, name = 'Kitchen',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~wedEn$laundry, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~wedEn$climat, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Wednesday, Mid-day, Oct, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

##########################################
## Extra: grouped bar chart with plotly ##
##########################################

## create the sub-set you want to plot
## in this case: 1ste week of feb 2007
week_feb_2007 <- energy %>%
  filter(year == 2007 & week == 7) %>%
  na.omit() %>%
  group_by(weekday) %>% 
  summarise(sum_kitchen = (sum(kitchen)/1000),
            sum_laundry = (sum(laundry)/1000),
            sum_climat = (sum(climat)/1000))

## and plot
plot_ly(week_feb_2007,x = ~weekday, y = ~sum_kitchen,
        type = 'bar', name = 'Kitchen') %>%
  add_trace(y = ~sum_laundry, name = 'Laundry') %>%
  add_trace(y = ~sum_climat, name = 'Climat') %>%
  layout() %>%
  layout(title = "Total Daily Consumption per Submeter, week 7, 2007",
       xaxis = list(title = "Days of the Week"),
       yaxis = list(title = "Consumption in kWh"),
       barmode = 'group')
 

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





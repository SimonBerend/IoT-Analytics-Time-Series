########################################################
#####        TIME SERIES  and FORECAST            ######
########################################################

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
# in this way, you can set freq in ts at 52
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
    
####################################
###     CREATE TS OBJECTS        ###
####################################

## Create TS object with for climat weekly
ts_week_climat <- ts(en_week$avg_wh_per_min_climat,
                       frequency=52,
                       start=c(2007,1))

## Create TS object with for kitchen weekly
ts_week_kitchen <- ts(en_week$avg_wh_per_min_kitchen,
                       frequency=52,
                       start=c(2007,1))


## autoplot: climat - week
autoplot(ts_week_climat,
         ts.colour = 'darkred',
         xlab = "Time",
         ylab = "Watt Hours",
         main = "Climat Room") +
  theme_bw()


############################################
##              Forecasting               ##
############################################

## ts linear regression: 
##  - ts_week_climat 
## use summary() to obtain the forecast's R2 and RMSE
fit_week_climat <- tslm(ts_week_climat ~ trend + season) 
summary(fit_week_climat)


## Forecast : week climat 
## 5 weeks left in 2010: Forecast 5 time periods ahead
## confidence levels 80 and 90
fc_fit_week_climat <- forecast(fit_week_climat,
                              h=5,
                              level=c(80,90))

## Plot climat fc for end 2010
plot(fc_fit_week_climat, 
     #ylim = c(0, 30),
     xlim = c(2010, 2011),
     ylab = "avg Watt-Hours per minute", 
     xlab ="Time",
     main = "Forecast: Climat Room",
     )


##############################
##      DECOMPOSE           ##
##############################

## Decompose : climat - week 
## trend, seasonal and remainder
## use plot() & summary() to inspect
comp_week_climat <- decompose(ts_week_climat)

## adjust original ts set for seasonality
ts_week_climat_adj_season <- ts_week_climat - comp_week_climat$seasonal
## you can test Seasonal adjustment by running decompose() on new ts 
## seasonality will appear again but with very low values

## Holt Winters Exponential Smoothing & Plot
ts_hw_week_climat <- HoltWinters(ts_week_climat_adj_season,
                                   beta = FALSE,
                                   gamma = FALSE)
plot(ts_hw_week_climat,
     #ylim = c(0, 25)
     )

## HoltWinters forecast & plot
## USE FOR TREND
## with deinished confidence levels
fc_hw_week_climat <- forecast(ts_week_climat_hw,
                          h=5,
                          level = c(10, 25))
plot(fc_hw_week_climat,
     ylim = c(5, 10),
     xlim = c(2010.6, 2011),
     ylab= "avg Watt-Hours per Minute",
     xlab="Time",
     main = "Climat Room")

#################################
##  TIME SERIES  and FORECAST  ##
##      Monthly Trends         ##
#################################
##        Run pre-process      ##
##    Most Packages are There  ##
#################################
source(file = "C:/Users/Gebruiker/Desktop/Esgitit/IoT Analytics/Energy - Pre-Process.R",
       local = FALSE)

## Packages to Forecast and create High Charter
install.packages("forecast")
library(forecast)
install.packages("highcharter")
library(highcharter)

###  SUBSET for Months
# group by month  
en_month <- energy %>% 
  filter(DateTime > "2007-10-31 23:59:00") %>% 
  group_by(
    # create one column for year - month
    year_month = format(DateTime,
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
                                  digits = 2) )


## CREATE TS OBJECTS
# Climat
ts_month_climat <- ts(en_month$avg_wh_per_min_climat,
                      frequency = 12, start = c(2007,11))
# Kitchen
ts_month_kitchen <- ts(en_month$avg_wh_per_min_kitchen,
                       frequency = 12, start = c(2007,11))
# Laundry
ts_month_laundry <- ts(en_month$avg_wh_per_min_laundry,
                       frequency = 12, start = c(2007,11))
# total subs
ts_month_total <- ts(en_month$avg_wh_per_min_total_subs,
                     frequency = 12, start = c(2007,11))
#global
ts_month_global <- ts(en_month$avg_wh_per_min_global,
                      frequency = 12, start = c(2007,11))

ts_month_all <- ts(en_month[2:6],
                   frequency = 12, start = c(2007,11))

## Decompose
dc_month_climat <- decompose(ts_month_climat)
dc_month_kitchen <- decompose(ts_month_kitchen)
dc_month_laundry <- decompose(ts_month_laundry)
dc_month_total <- decompose(ts_month_total)
dc_month_global <- decompose(ts_month_global)

# isolate trends and clear NA's
trend_climat <- na.omit(dc_month_climat$trend)
trend_kitchen <- na.omit(dc_month_kitchen$trend)
trend_laundry <- na.omit(dc_month_laundry$trend)
trend_total <- na.omit(dc_month_total$trend)
trend_global <- na.omit(dc_month_global$trend)

# forecast trends
fc_trend_climat <- forecast(trend_climat, h=7,level = c(10, 25))
fc_trend_kitchen <- forecast(trend_kitchen, h=7,level = c(10, 25))
fc_trend_laundry <- forecast(trend_laundry, h=7,level = c(10, 25))
fc_trend_total <- forecast(trend_total, h=7,level = c(10, 25))
fc_trend_global <- forecast(trend_global, h=7,level = c(10, 25))

# plot:
# High Charter
# 2 series per meter:
# actual  and forecast $ mean
fc_trends_chart <- highchart(type = "stock") %>% 
  hc_title(text = "Energy Consumption Trends: actual and Forecast") %>% 
  hc_subtitle(text = "in avg Wh per Minute") %>% 
  hc_add_series(fc_trend_kitchen$mean, id = "Kitchen", name = "Kitchen", color = "brown") %>% 
  hc_add_series(trend_kitchen, id = "Kitchen", name = "Kitchen", color = "brown") %>% 
  hc_add_series(fc_trend_climat$mean, id = "Climat", name = "Climat", color = "red") %>% 
  hc_add_series(trend_climat, id = "Climat", name = "Climat", color = "red") %>%
  hc_add_series(fc_trend_laundry$mean, id = "Laundry", name = "Laundry", color = "green") %>% 
  hc_add_series(trend_laundry, id = "Laundry", name = "Laundry", color = "green") %>%   
  hc_add_series(fc_trend_total$mean, id = "All Subs", name = "All Subs", color = "blue") %>% 
  hc_add_series(trend_total, id = "All Subs", name = "All Subs", color = "blue") %>%   
  hc_add_series(fc_trend_global$mean, id = "Global", name = "Global", color = "slateblue") %>% 
  hc_add_series(trend_global, id = "Global", name = "Global", color = "slateblue") %>% 
  hc_tooltip(valueDecimals = 2)

# show plot 
fc_trends_chart

# Older Version:
# only 1 series for actual and forecasted values
# problem: shows confidence intervals
never_mind <- highchart(type = "stock") %>% 
  hc_title(text = "Energy Consumption Trends: actual and Forecast") %>% 
  hc_subtitle(text = "in avg Wh per Minute") %>% 
  hc_add_series(fc_trend_kitchen, id = "Kitchen", name = "Kitchen",
                addOriginal = TRUE, color = "brown") %>% 
  hc_add_series(fc_trend_climat, id = "Climat", name = "Climat",
                addOriginal = TRUE, color = "red") %>% 
  hc_add_series(fc_trend_laundry, id = "Laundry", name = "Laundry", 
                addOriginal = TRUE, color = "green") %>% 
  hc_add_series(fc_trend_total, id = "All Subs", name = "All Subs", 
                addOriginal = TRUE, color = "blue") %>% 
  hc_add_series(fc_trend_global, id = "Global", name = "Global", 
                addOriginal = TRUE, color = "yellow") %>% 
  hc_tooltip(valueDecimals = 2)

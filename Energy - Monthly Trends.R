#####################################
##  TIME SERIES  and HighCharter  ##
##      Monthly Trends         ####
##################################
##        Run pre-process      ##
##  Packages Needed are There  #
###############################

source(file = "C:/Users/Gebruiker/Desktop/Esgitit/IoT Analytics/Energy - Pre-Process.R",
       local = FALSE)

## Package to create the chart below
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
                                  digits = 2))

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

# plot
trends_chart <- highchart(type = "chart") %>% 
  hc_title(text = "Energy Consumption Trends") %>% 
  hc_subtitle(text = "in avg Wh per Minute") %>% 
  hc_add_series(trend_kitchen, id = "Kitchen", name = "Kitchen") %>% 
  hc_add_series(trend_climat, id = "Climat", name = "Climat") %>% 
  hc_add_series(trend_laundry, id = "Laundry", name = "Laundry") %>% 
  hc_add_series(trend_total, id = "All Subs", name = "All Subs") %>% 
  hc_add_series(trend_global, id = "Global", name = "Global")

trends_chart

trends_chart <- highchart(type = "stock") %>% 
  hc_title(text = "Energy Consumption Trends") %>% 
  hc_subtitle(text = "in avg Wh per Minute") %>% 
  
  hc_add_series(trend_climat, id = "Climat", name = "Climat", type = "column") %>% 
  hc_add_series(trend_laundry, id = "Laundry", name = "Laundry") %>% 
  hc_add_series(trend_total, id = "All Subs", name = "All Subs") %>% 
  hc_add_series(trend_global, id = "Global", name = "Global") %>% 
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d}
                            
 
                            {point.y:.2f}%')



trends_chart

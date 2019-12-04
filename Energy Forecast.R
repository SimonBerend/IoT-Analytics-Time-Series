
#####################################################
## Run pre-process  ## Packages Needed are There  ###
#####################################################

source(file = "~/IoT Analytics/Energy - Pre-Process.R",
       local = FALSE)


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

## Subset to an observation every half hour during the 2008 august dip
energy_aug_2008 <- filter(energy, year == 2008 & week == 33 &
                             minute == 0)

## Create TS object with submeter 3: climat
ts_global_wh_aug_2008 <- ts(energy_aug_2008$global_wh,
                            frequency = 24,
                            start = c(1, 1))

## Plot global wh with autoplot - add labels, color
autoplot(ts_global_wh_aug_2008,
         ts.colour = 'darkred',
         xlab = "Time",
         ylab = "Watt Hours",
         main = "Global (Wh)") +
  theme_bw()
  


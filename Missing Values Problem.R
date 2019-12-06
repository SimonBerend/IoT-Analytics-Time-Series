
################################################
###           DEAL WITH NA's                 ###
################################################
install.packages("imputeTS")
library(imputeTS)

install.packages("padr")
library(padr)

##### cumbersome way to complete the time stamps
## create seq with complete dates, per minute 
## from 2007-01-01 00:00:00 until 2009-12-31 23:59:00
DateTime <- seq.POSIXt (as.POSIXct("2007-01-01 00:00:00", tz = "CET"),
                        as.POSIXct("2009-12-31 23:59:00", tz = "CET"),
                        by = "1 min")
# make that a data frame 
time_complete <- as.data.frame(DateTime)

# Bind energy to complete time series
complete_en <- left_join(time_complete, energy, by = "DateTime")


#### QUICK WAY TO COMPLETE TIME STAMPS
pad_energy <- pad(energy,
                  interval = "min",
                  break_above = 1578240)



####### find a way to replace NA's ##########
# make a test df
df_test <- pad_energy

# identify the place in a column that has NA's
which(is.na(df_test$kitchen))

# make a for loop (?) that does some shiit to the NA's
for(i in 1:length(df_test)) {
  if(is.na(df_test$kitchen[i])) {
    print("Damn, it's NA")
  }
}







# stats about NA's 
statsNA(complete_en$kitchen)






# identify on what time there are missing values
missing_minutes <- RegularTimeSeries[!RegularTimeSeries %in% energy$DateTime]

# make a data frame of the missing values vectore
df_missing_minutes <- as.data.frame(missing_minutes)

# add to the missing value DateTime variables of year, month, week, hour 
df_missing_minutes$year <- year(df_missing_minutes$missing_minutes)
df_missing_minutes$month <- month(df_missing_minutes$missing_minutes)
df_missing_minutes$day <- day(df_missing_minutes$missing_minutes)
df_missing_minutes$hour <- hour(df_missing_minutes$missing_minutes)


# create one column for date and hour
df_missing_minutes$date_hour <- format(df_missing_minutes$missing_minutes,
                                       "%Y-%m-%d %H")

# summarize the missing values according time variables
sum_mv_date_hour <- df_missing_minutes %>%  group_by(date_hour) %>% 
  summarise(n())

sum_mv <- df_missing_minutes %>%  group_by(month, day, hour) %>% 
  summarise(n())

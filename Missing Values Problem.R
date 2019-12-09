
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
test2 <- pad_energy
# identify the place in a column that has NA's
which(is.na(df_test$kitchen))

# make a for [i] loop that does some shiit to the NA's
# for the kitchen
for(i in 1:nrow(test2)) {
  if(is.na(test2$kitchen[i])) {
    
    check_i <- as.data.frame(test2[0,])
    
    if(i > 100080){ 
      check_i <- rbind(check_i, test2[i - 10080,])}
    
    if(i < (nrow(test2) - 10080)) {
      check_i <- rbind(check_i, test2[i + 10080,])}
    print("step1")
    mean_i <- mean(check_i$kitchen, na.rm = TRUE)
    print("step2")
    test2$kitchen[i] <- mean_i
    print("step3")
        }
}

# for the rest
for(i in 1:nrow(df_test)) {
  if(is.na(df_test$laundry[i])) {
    
    check_i <- as.data.frame(df_test[0,])
    
    if(i > 100080){ 
      check_i <- rbind(check_i, df_test[i - 10080,])}
    
    if(i < (nrow(df_test) - 10080)) {
      check_i <- rbind(check_i, df_test[i + 10080,])}
    print("step1")
    mean_i_laundry <- mean(check_i$laundry, na.rm = TRUE)
    mean_i_climat <- mean(check_i$climat, na.rm = TRUE)
    mean_i_total_subs <- mean(check_i$total_subs, na.rm = TRUE)
    mean_i_global_wh <- mean(check_i$global_wh, na.rm = TRUE)
    mean_i_global_kw <- mean(check_i$global_kw, na.rm = TRUE)
    print("step2")
    
    df_test$laundry[i] <- mean_i_laundry
    df_test$climat[i] <- mean_i_climat
    df_test$total_subs[i] <- mean_i_total_subs
    df_test$global_wh[i] <- mean_i_global_wh
    df_test$global_kw[i] <- mean_i_global_kw
    print("step3")
    
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

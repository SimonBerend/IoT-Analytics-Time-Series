
################################################
###           DEAL WITH NA's                 ###
################################################

## create seq with complete dates, per minute 
## from 2007-01-01 00:00:00 until 2009-12-31 23:59:00
RegularTimeSeries <- seq.POSIXt (as.POSIXct("2007-01-01 00:00:00", tz = "CET"), 
                                     as.POSIXct("2009-12-31 23:59:00", tz = "CET"),
                                     by = "1 min")

missing_minutes <- RegularTimeSeries[!RegularTimeSeries %in% energy$DateTime]

df_missing_minutes <- as.data.frame(missing_minutes)

df_missing_minutes$year <- year(df_missing_minutes$missing_minutes)
df_missing_minutes$month <- month(df_missing_minutes$missing_minutes)
df_missing_minutes$day <- day(df_missing_minutes$missing_minutes)

sum_missing_values <- df_missing_minutes %>%  group_by(year, month, day) %>% 
  summarise(n())


############ create one column 
 df$whatyouwant <- format(DateTime, "%Y-%m-%d %H")


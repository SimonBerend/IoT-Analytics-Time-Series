df_test <- bind_rows(yr2007, yr2008, yr2009)

## rename the columns
colnames(df_test)[3:6] <- c("kitchen", "laundry", "climat", "global_kw")



######################################
## Create and Complete Date Stamps  ##
######################################
## Combine Date and Time attribute values in a new attribute column
df_test$DateTime <- paste(df_test$Date,df_test$Time)

## Convert DateTime from POSIXlt to POSIXct 
df_test$DateTime <- as.POSIXct(df_test$DateTime,
                              "%Y-%m-%d %H:%M:%S", tz = "CET" )

## COMPLETE TIME STAMPS
df_test <- pad(df_test,
              interval = "min",
              break_above = 1578240)

copy_df <- df_test

start_time <- Sys.time()
rows <-  nrow(df_test)
for(i in 1:rows) {
  if(is.na(df_test$laundry[i])) {
    if(i > 10080 & i < rows - 10080) {
      df_test$kitchen[i] <- mean(c(df_test$kitchen[i+10080],
                                   df_test$kitchen[i-10080]),
                                 na.rm = TRUE)
      
      df_test$laundry[i] <- mean(c(df_test$laundry[i+10080],
                                   df_test$laundry[i-10080]),
                                 na.rm = TRUE)
      df_test$climat[i] <- mean(c(df_test$climat[i+10080],
                                  df_test$climat[i-10080]),
                                na.rm = TRUE)
      df_test$global_kw[i] <- mean(c(df_test$global_kw[i+10080],
                                     df_test$global_kw[i-10080]),
                                   na.rm = TRUE)
    }}}
end_time <- Sys.time()
end_time - start_time
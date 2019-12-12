    ################################
   ####   Daily (per Hour)     #### 
  ################################

## Group data set by time attributes ##
enHour <- energy %>% na.omit() %>%
                     group_by(hour) %>% 
                     summarise(mKitchen = mean(kitchen),
                               mLaundry = mean(laundry),
                               mClimat = mean(climat))

## line plot: areas / hours
ggplot(enHour, aes(x=hour)) + 
  geom_line(aes(y = mKitchen), color = "darkred", size = 1) +
  geom_line(aes(y = mLaundry), color = "darkblue", size = 1) + 
  geom_line(aes(y = mClimat), color="darkgreen", linetype="twodash", size = 1)

## line plot: areas / hour : tydiverse
# Data preparation
tiHour <- enHour %>%
  select(hour, mKitchen, mLaundry, mClimat) %>%
  pivot_longer(-hour, names_to = "submeter", values_to = "Watth")
  
# Visualization
ggplot(tiHour, aes(x = hour, y = Watth)) + 
  geom_smooth(aes(color = submeter),
              method = "loess",
              span = 0.3,
              se = FALSE) +
  labs(title = "Average Daily E-Consumption",
       subtitle = "(2007-2009)",
       x = "Time (0-24 h)",
       y = "Consumption (W/h - active e)")+
  theme_bw()


      ################################
     ####    Weekly (per Day)    #### 
    ################################

## Group data per weekday 
enWeek <- energy %>% na.omit() %>%
  group_by(weekday) %>% 
  summarise(mKitchen = mean(kitchen),
            mLaundry = mean(laundry),
            mClimat = mean(climat))

## line plot: areas / weekday : tydiverse
# Data preparation
tiWeek <- enWeek %>%
  select(weekday, mKitchen, mLaundry, mClimat) %>%
  pivot_longer(-weekday, names_to = "submeter", values_to = "Watth")

# Visualization
ggplot(tiWeek, aes(x = weekday, y = Watth, group = submeter)) + 
  geom_line(aes(color = submeter,
                linetype = submeter)) + 
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Mean Weekly E-Consumption",
       subtitle = "(2007-2009)",
       x = "Days",
       y = "Consumption (W/h - active e)")+
  theme_bw()



   ################################
  ####      Monthly (per day) #### 
 ################################

## Group data per day
enMon <- energy %>% na.omit() %>%
  group_by(day) %>% 
  summarise(mKitchen = mean(kitchen),
            mLaundry = mean(laundry),
            mClimat = mean(climat))

## line plot: areas / day : tydiverse
# Data preparation
tiMon <- enMon %>%
  select(day, mKitchen, mLaundry, mClimat) %>%
  pivot_longer(-day, names_to = "submeter", values_to = "Watth")

# Visualization
ggplot(tiMon, aes(x = day, y = Watth)) + 
  geom_line(aes(color = submeter, 
                linetype = submeter)) + 
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Mean Monthly E-Consumption",
       subtitle = "(2007-2009)",
       x = "Days (1-31)",
       y = "Consumption (W/h - active e)")+
  theme_bw()



################################
####     Years (per Day)    ### 
##############################
# create sets: yearly en consumption per month,day
en2009 <- energy %>% 
  na.omit() %>%
  filter(year == 2009) %>%
  group_by(month,day) %>% 
  summarise(total.2009 = mean(total))

## Join that shizz 
enYear <- inner_join(en2007, en2008, by = c("month", "day"))
enYear <- inner_join(enYear, en2009, by = c("month", "day"))
enYear <- inner_join(enYear, en3ytot, by = c("month", "day"))

colnames(enYear)
## line plot: years / month,day : tydiverse
# Data preparation
tiYear <- enYear %>%
  select(month, day,
         total.2007, total.2008,
         total.2009, overall.avg) %>%
  pivot_longer(-c(month, day),
               names_to = "period",
               values_to = "Watth")

## turn month, day into one variable
tiYear$monthDay <- paste( month.abb[tiYear$month],
                          tiYear$day,
                          sep="-" )
### UNIQUE ###
tiYear$monthDay <- factor(tiYear$monthDay,
                          levels=unique(tiYear$monthDay),ordered=TRUE)

str(tiYear$monthDay)
# Visualization
ggplot(tiYear, aes(x = monthDay, y = Watth, group = period)) + 
  geom_smooth(aes(color = period),
              method = "loess",
              span = 0.15,
              se = FALSE) +
  labs(title = "Yearly E-Consumption",
       subtitle = "(Smoothened)",
       y = "Consumption (W/h - active e)",
       x = NULL)+
  theme_bw() + 
  scale_x_discrete(breaks = c("Jan-1", "Feb-1", "Mar-1", "Apr-1",
                              "May-1", "Jun-1", "Jul-1", "Aug-1",
                              "Sep-1", 'Oct-1', "Nov-1", "Dec-1"))



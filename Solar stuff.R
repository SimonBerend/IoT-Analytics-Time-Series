################################
####     Consumption and    ####
####     Green production   ####
####     per Month          ####
################################
# create set: yearly consumption per month
enMonthly <- energy %>% 
  na.omit() %>%
  group_by(month) %>% 
  summarise(consumption = (sum(total)/3000))

# set months as ordered factor
enMonthly$month <- factor(enMonthly$month,
                          levels = enMonthly$month, 
                          ordered = TRUE)

# create column: yearly solar production per month
# sun hours from http://www.zonuren.nl/europa-centr/
enMonthly$sun <- as.numeric(c("2.1",	"3.0", "4.9",
                   "6.2",	"7.2", "7.8",
                   "7.5", "6.6", "5.5",
                   "3.9", "2.1", "1.8"))

# create column: energy production
# Calculate energy production
# Solar panel watts x average hours of sunlight x 75% = daily watt-hours 
# https://www.vivintsolar.com/blog/how-calculate-solar-panel-output
# google: "a domestic solar panel has a power output of around 265 watts"
# install 6 panels of 300W = 1000 W

enMonthly$production <- as.numeric(1350 * enMonthly$sun * 30 /1000)



## line plot: years / month,day : tydiverse
# Data preparation
tiMonthly <- enMonthly %>%
  select(month, consumption,
         production) %>%
  pivot_longer(-month,
               names_to = "cons.prod",
               values_to = "kW.h")


# Visualization
ggplot(tiMonthly, aes(x = month, y = kW.h, group = cons.prod)) + 
  geom_smooth(aes(color = cons.prod),
              method = "loess",
              span = 0.3,
              se = FALSE) +
  labs(title = "Consumption and Solar Panel Production",
       subtitle = "(Prod: 6 x 300 W solar panels)",
       y = "kW/h",
       x = "Months",
       fill = "Energy")+
  theme_bw()



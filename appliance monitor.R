
## filter energy for August 12 2008 (low energy usage)
enAug2008 <- energy %>% 
  filter(DateTime > "2008-12-08 10:00:00" &
         DateTime < "2008-12-08 13:30:00") %>%
  select(DateTime, laundry, minute, Time)



enAug2008$Time <- factor(enAug2008$Time,
                         levels=enAug2008$Time,
                         ordered=TRUE)


ggplot(enAug2008, aes(x = Time, y = laundry, group = 1)) + 
  geom_line() +
  labs(title = "Laundry Room Energy Consumption",
       subtitle = "(Fridge)",
       y = "Consumption (W/h - active e)",
       x = NULL)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("10:00:00", "10:20:00", "10:40:00", "11:00:00",
                              "11:20:00", "11:40:00", "12:00:00", "12:20:00",
                              "12:40:00", "13:00:00", "13:20:00"))

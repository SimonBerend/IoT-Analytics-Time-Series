# Data Frame for plotly
# quarterly consumption per submeter

# group sub consumption per year, month
en_quart_subs <- energy %>% 
  group_by(year, quarter) %>% 
  summarise(kitchen = round(sum(kitchen)/1000, digits = 1),
            laundry = round(sum(laundry)/1000, digits = 1),
            climat = round(sum(climat)/1000, digits = 1))

# pivot sub meter values
en_quart_subs <- en_quart_subs %>%
  select(year, quarter, kitchen, laundry, climat) %>%
  pivot_longer(-c(year, quarter), names_to = "submeter", values_to = "kWh")

en_quart_subs <- en_quart_subs %>% 
pivot_wider(names_from = "year", values_from = "kWh")

colnames(en_quart_subs)[3:6] <- c("yr07", "yr08", "yr09", "yr10")

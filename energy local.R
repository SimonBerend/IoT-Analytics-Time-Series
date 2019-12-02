if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "arules", "arulesViz",
               "graphics", "RColorBrewer", "e1071",
               "dplyr")

setwd("C:/Users/Gebruiker/Downloads/household_power_consumption")

house <- read.delim2("household_power_consumption.txt", 
                     header = TRUE, 
                     sep = ";", 
                     dec = ".",
                     na.strings = "?")

str(house)
summary(house)

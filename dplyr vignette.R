   # # # Get Yo Packs # # #
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "arules", "arulesViz",
               "graphics", "RColorBrewer", "e1071",
               "dplyr", "dbplyr", "RMySQL",
               "RSQLite", "wesanderson", "nycflights13")

### dbplyr Workshop ###

vignette("dbplyr")

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

# copy data into th data set - "quick and dirty"
copy_to(con, nycflights13::flights, "flights",
        temporary = FALSE, 
        indexes = list(
          c("year", "month", "day"), 
          "carrier", 
          "tailnum",
          "dest")
        )

# make a reference to the data copied
flights_db <- tbl(con, "flights")

flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time))

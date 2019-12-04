##################################
## Get pre-process from Github  ##
##################################

source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website
  script <- getURL(u, ssl.verifypeer = FALSE)
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script),
       envir= .GlobalEnv)
}

source_github("https://github.com/SimonBerend/IoT-Analytics-Time-Series/blob/master/Energy%20-%20Pre-Process.R")


######################################
## Get pre-process from local repos ##
######################################

source(file = "C:/Users/Gebruiker/Desktop/Esgitit/IoT Analytics/Energy - Pre-Process.R",
       local = FALSE)

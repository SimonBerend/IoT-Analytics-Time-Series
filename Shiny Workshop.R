# # # Get Yo Packs # # #
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "shiny", "shinydashboard", "dplyr", "lubridate")


####    get rds data to make dash   ###
setwd("C:/Users/Gebruiker/Downloads")
dashdata <- readRDS("data_ready.rds")


#######################################
#######################################

# Tweek the Server
server <- function(input, output) {
  
  output$Line_ActiveEnergy_avg <- renderPlot({
    ggplot(data = dashdata %>% filter(year(date) == input$SelectYear)) + 
      geom_line(aes(x = date, y = ActiveEnergy_avg)) +
      ylab("Active energy") + xlab("Time")})
  
  output$Histo_ActiveEnergy_avg <- renderPlot({
    hist(dashdata$ActiveEnergy_avg, 
         breaks = input$Breaks,
         main = "Distribution of Active E")
  })
    
}

# Tweek the User Interface
ui <- dashboardPage(
      dashboardHeader(
        title = "Energy Dashboard"),
      dashboardSidebar(
        sliderInput(
          inputId = "Breaks",
          label = "Choose the number of breaks in the histogram:",
          min = 10,
          max = 50,
          value = 25),
        selectInput(
          inputId = "SelectYear",
          label = "Choose a year for the line chart to show:",
          choices = list("2007", "2008", "2009", "2010"))),
      dashboardBody(
        title = "Them Graphs",
        box(plotOutput(outputId = "Line_ActiveEnergy_avg")),
        box(plotOutput(outputId = "Histo_ActiveEnergy_avg"))
        ))

# Show Dashboard
shinyApp(ui, server)

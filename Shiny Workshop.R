# # # Get Yo Packs # # #
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "shiny", "shinydashboard", "dplyr", "lubridate")


####    get rds data to make dash   ###
setwd("C:/Users/Gebruiker/Downloads")
dashdata <- readRDS("data_ready.rds")


#####    check Iva's stuff on Slack !!! #####

# Tweek the Server
server <- function(input, output) {
  output$Line_ActiveEnergy_avg <- renderPlot({
    ggplot(data = dashdata) +
      geom_line(aes(x = date[input$dateRangeInput], y = ActiveEnergy_avg))
    })
  output$Histo_ActiveEnergy_avg <- renderPlot({
    hist(dashdata$ActiveEnergy_avg, breaks = input$Breaks)
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
          start = "2007-01-01",
          end = "2010-11-26",
          min = "2007-01-01",
          max = "2010-11-26",
          startview = "year")),
      dashboardBody(
        box(plotOutput(outputId = "Line_ActiveEnergy_avg")),
        box(plotOutput(outputId = "Histo_ActiveEnergy_avg"))
        ))

# Show Dashboard
shinyApp(ui, server)

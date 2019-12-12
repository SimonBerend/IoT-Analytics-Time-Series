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
  
   output$smooth_avg <- renderPlot({
    ggplot(data = energy %>%
             filter(year(Date) == input$SelectYear) %>% 
             group_by(Date) %>% 
             summarise(overall_avg = round(mean(global_wh), digits = 1))
           ) +
       geom_smooth(aes(x = Date, y = overall_avg),
                   span = 0.1, se = F) +
       labs(title = "Yearly E-Consumption",
            subtitle = "(Smoothened)",
            y = "avg Consumption in Wh per Minute",
            x = "Months") +
       scale_x_date(date_breaks = "1 month", date_labels = "%b")
  })
   
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
    box(plotOutput(outputId = "smooth_avg")),
    box(plotOutput(outputId = "Histo_ActiveEnergy_avg"))
  ))

# Show Dashboard
shinyApp(ui, server)

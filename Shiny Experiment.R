# # # Get Yo Packs # # #
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "shiny", "shinydashboard", 
               "dplyr", "lubridate")

# Pre-process
# source(file = "C:/Users/Gebruiker/Desktop/Esgitit/IoT Analytics/Energy - Pre-Process.R",
#       local = FALSE)


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "Overview",
             icon = icon("tachometer")),
    
    menuItem("Trends", tabName = "Trends",
             icon = icon("line-chart"))
    
),
  sliderInput(
    inputId = "Breaks",
    label = "Choose the number of breaks in the histogram:",
    min = 10,
    max = 50,
    value = 25),
  selectInput(
    inputId = "SelectYear",
    label = "Choose a year for the line chart to show:",
    choices = list("2007", "2008", "2009", "2010"))
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Overview",
            h2("Overview of Energy Consumption"),
            box(plotOutput(outputId = "smooth_avg")),
            box(plotOutput(outputId = "Histo_ActiveEnergy_avg"))
    ),
    
    tabItem(tabName = "Trends",
            h2("Trends and Predictions")
    )
  )
)

# Server ------------------------------------------------------------------

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


# Show Dashboard ----------------------------------------------------------

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Your Energy"),
    sidebar,
    body
  ),
  server
)


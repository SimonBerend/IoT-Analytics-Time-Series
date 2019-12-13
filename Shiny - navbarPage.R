# # # Get Yo Packs # # #
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "shiny", "shinydashboard", "dplyr", "lubridate")


# Data?
# source ?

# User Interface ----------------------------------------------------------

ui <- navbarPage("Your Energy", inverse = TRUE,
                 tabPanel("Overview",
                          titlePanel("Overview of Energy Consumption"),
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         
                              selectInput(inputId = "SelectYear",
                                          label = "Choose a year for the line chart to show:",
                                          choices = list("2007", "2008", "2009", "2010"))
                                          ),
                              mainPanel(width = 9,
                                        box(plotOutput(outputId = "smooth_avg"))
                                        )
                                       )
                          ),
                 
                 tabPanel("Trends",
                          sidebarLayout(
                            sidebarPanel(width = 12,
                                         h1("Past and Forecasted Trends in Energy Consumption")),
                            mainPanel(width = 12,
                                      highchartOutput(outputId = "fc_trends_chart")
                                      )
                                       )
                          ),
                          
                 navbarMenu("More",
                            tabPanel("About IoT Analytics"),
                            tabPanel("Contact")
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
  
  output$bar_overview <- renderPlotly({
    plot_ly(data = energy %>%
             filter(year(Date) == input$SelectYear) %>% 
             group_by(Date) %>% 
             summarise(overall_avg = round(mean(global_wh), digits = 1))
    plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
              add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
              layout(yaxis = list(title = 'Count'), barmode = 'group')
            
  })
  
  
  output$fc_trends_chart <- renderHighchart({fc_trends_chart
  })
}

# Run App ----------------------------------------------------------------
shinyApp(ui, server)

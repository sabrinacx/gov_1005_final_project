library(shiny)
library(tidyverse)
library(janitor)
library(ggthemes)
library(readxl)
library(tools)
# library(maps)
# us_states <- map_data("state")

all <- read_rds("crime1.rds")
all_states <- read_rds("crime1_only_states.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("United States Crime Data"),
  
  tabsetPanel(
    tabPanel("State Comparisons", fluid = TRUE,
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("crime_type", "Crime Type:",
                             c("Violent Crime" = "violent_crime_rate_per_100_000",
                               "Murder and Non-Negligent Manslaughter" = "murder_and_nonnegligent_manslaughter_rate_per_100_000",
                               "Robbery" = "robbery_rate_per_100_000",
                               "Aggravated Assault" = "aggravated_assault_rate_per_100_000",
                               "Property Crime" = "property_crime_rate_per_100_000",
                               "Burglary" = "burglary_rate_per_100_000",
                               "Larceny Theft" = "larceny_theft_rate_per_100_000",
                               "Motor Vehicle Theft" = "motor_vehicle_theft_rate_per_100_000"),
                             "Violent Crime"),
                 textInput("area1", "State 1:", "New York"),
                 textInput("area2", "State 2:", "Alabama"),
                 sliderInput("year", "Year:", min = 2010, max = 2017, value = c(2010, 2017), sep = ""),
                 checkboxGroupInput("area3", "Regions:", c("United States" = "United States Total",
                                                           "South", "Northeast", "Midwest", 
                                                           "West", "Pacific"), selected = "United States Total")),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("linePlot")
               )
             )
    ),
    tabPanel("Top 10", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("crime_type2", "Crime Type:",
                             c("Violent Crime" = "violent_crime_rate_per_100_000",
                               "Murder and Non-Negligent Manslaughter" = "murder_and_nonnegligent_manslaughter_rate_per_100_000",
                               "Robbery" = "robbery_rate_per_100_000",
                               "Aggravated Assault" = "aggravated_assault_rate_per_100_000",
                               "Property Crime" = "property_crime_rate_per_100_000",
                               "Burglary" = "burglary_rate_per_100_000",
                               "Larceny Theft" = "larceny_theft_rate_per_100_000",
                               "Motor Vehicle Theft" = "motor_vehicle_theft_rate_per_100_000"),
                             "Violent Crime"),
                 selectInput("year2", "Year:", c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")),
                 sliderInput("slice", "Top:", value = 10, min = 1, max = 25)),
               
               mainPanel(
                 plotOutput("dotPlot")
               )
             )
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    all %>%
      filter(area %in% c(input$area1, input$area2, input$area3),
             crime_type == input$crime_type,
             year >= input$year[1] & year <= input$year[2]) %>%
      
      # draw the histogram with the specified number of bins
      
      ggplot(aes(x = year, y = value, color = area)) + geom_line() + xlab("Year") + ylab("Rate Per 100,000") + theme_minimal()
  })
  
  output$dotPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    all_states %>% 
      filter(year == input$year2,
             crime_type == input$crime_type2) %>% 
      arrange(desc(value)) %>% 
      slice(1:input$slice) %>% 
      ggplot(mapping = aes(x = reorder(area, value), y = value, fill = area)) + geom_col(show.legend = FALSE) + coord_flip() +
      labs(y = "Violent Crime Rate Per 100,000", 
           x = "States",
           title = "States with the Top 10 Highest Violent Crime Rates in",
           caption = "Source: US FBI:UCR Crime Data 2016") + 
      theme_economist_white()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
library(shiny)
library(tidyverse)
library(janitor)
library(ggthemes)
library(readxl)
library(tools)
# library(maps)
# us_states <- map_data("state")

all <- read_rds("crime.rds")
  
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("United States Crime Data"),
   
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
        sliderInput("year", "Year:", min = 2010, max = 2017, value = c(2010, 2017)),
        checkboxGroupInput("area3", "Regions:", c("United States" = "United States Total",
                                                  "South", "Northeast", "Midwest", 
                                                  "West", "Pacific"
                                                  ), selected = "United States Total")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("annualPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$annualPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     all %>%
       filter(area %in% c(input$area1, input$area2, input$area3)) %>%
       #filter(year %in% c(input$year)) %>% 
       select(area, year, input$crime_type) %>% 
       gather(crime_type, y, input$crime_type) %>% 
       
      # draw the histogram with the specified number of bins
     
     ggplot(aes(x = year, y = y, color = area)) + geom_line() + xlab("Year") + ylab(input$crime_type)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(tidyverse)
library(janitor)
library(ggthemes)
library(readxl)
library(tools)
library(shinythemes)

# read in the rds that contains the crime data from script.R
# crime1.rds contains all data (regional and state-specific data)
# crime1_only_states contains only state-specific data

all <- read_rds("crime1.rds")
all_states <- read_rds("crime1_only_states.rds")

# Define UI for application with four tabs

ui <- navbarPage("United States Federal Crime Data", theme = shinytheme("flatly"),
  
  tabPanel("State Comparisons",
           fluidPage(
             titlePanel("State Comparisons"),
  
  # Tab one allows users to compare the crime rates between states and regions on a generated line graph

             
             # Sidebar that enables user to select the crime type, areas, years, and regions they'd like to 
             # see on the line graph. 
             
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
                                                           "West", "Pacific"), selected = "United States Total")
                 ),
               
               # Main panel shows a plot of the generated line plot
               
               mainPanel(
                 plotOutput("states_line_plot")
                 )
               )
             )
           ),
    
    # Tab two allows the user to compare crime rates across the US at a specific year on a bar plot
    
  tabPanel("Annual Comparisons", 
           fluidPage(
             tabsetPanel(
             tabPanel("States with Highest Rates",
             
             # Sidebar layout enables the user to choose the crime type and year they'd like to examine
             # Users can also choose how many states they'd like to look at.
             
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
                 numericInput("year2", "Year:", value = 2017, min = 2010, max = 2017), 
                              #c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")),
                 sliderInput("slice", "Top:", value = 10, min = 1, max = 25)),
               
               # Main panel shows a plot of the generated bar plot
               
               mainPanel(
                 plotOutput("highest_bar_plot")
                 )
               )
             ),
             tabPanel("States with Lowest Rates",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("crime_type3", "Crime Type:",
                                      c("Violent Crime" = "violent_crime_rate_per_100_000",
                                        "Murder and Non-Negligent Manslaughter" = "murder_and_nonnegligent_manslaughter_rate_per_100_000",
                                        "Robbery" = "robbery_rate_per_100_000",
                                        "Aggravated Assault" = "aggravated_assault_rate_per_100_000",
                                        "Property Crime" = "property_crime_rate_per_100_000",
                                        "Burglary" = "burglary_rate_per_100_000",
                                        "Larceny Theft" = "larceny_theft_rate_per_100_000",
                                        "Motor Vehicle Theft" = "motor_vehicle_theft_rate_per_100_000"),
                                      "Violent Crime"),
                          numericInput("year3", "Year:", value = 2017, min = 2010, max = 2017), 
                          #c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")),
                          sliderInput("slice3", "Top:", value = 10, min = 1, max = 25)),
                        
                        # Main panel shows a plot of the generated bar plot
                        
                        mainPanel(
                          plotOutput("lowest_bar_plot")
                        )
                      )
                      )
           ))),
    
    # Tab three shows an animated graphic that shows the change in states with the highest violent crime rates in the US
    
    tabPanel("Top 10 Highest Violent Crime States", 
             fluidPage(
             titlePanel("Top 10 Highest Violent Crime States"),
             sidebarLayout(
               
               # Sidebar panel shows a description of the graphic
               
               sidebarPanel(
               tags$h5(helpText("This is an animated graphic that shows the states with the highest
                                violent crime rates in the United States from 2010 to 2017."))),
               
               # Main panel shows the generated gif
               
               mainPanel(
               imageOutput("violent_gif")
               )
               )
             )),
    
    # Tab four includes basic information about the project
    
    tabPanel("About", 
             fluidPage(
               titlePanel("About"),

             br(),

             p(paste("Definitions:")),

             br(),

             br(),

             p(paste("This project couldn't have been possible without the data provided by the US Federal
                     Bureau of Investigation : Uniform Crime Reports Data. Data can be accessed from their website: https://www.fbi.gov/services/cjis/ucr")),

             br(),

             p(paste("My code can be found here: https://github.com/sabrinacx/us_crime_data")),

             br()

             )))

# Define server logic required to generate the various graphics in the four tabs

server <- function(input, output) {
  
  # Generates the line graph for tab one based on inputs from ui
  
  output$states_line_plot <- renderPlot({
    
    # Prepares data table for line graph by filtering out the user-selected areas, crime types and years 
    
    all %>%
      filter(area %in% c(input$area1, input$area2, input$area3),
             crime_type == input$crime_type,
             year >= input$year[1] & year <= input$year[2]) %>% 
      rename(Area = area) %>% 
      
      # Draw the line graph with the filtered data set 
      
      ggplot(aes(x = year, y = value, color = Area)) + geom_line() + xlab("Year") + theme_fivethirtyeight() + ylab("Rate Per 100,000")
  })
  
  # Generates the bar graph for tab two based on inputs from ui
  
  output$highest_bar_plot <- renderPlot({
    
    # Prepares data table for bar graph by filtering out the user-selected year and crime type 
    
    all_states %>% 
      filter(year == input$year2,
             crime_type == input$crime_type2) %>% 
      arrange(desc(value)) %>% 
      slice(1:input$slice) %>% 
      
      # Draw the bar plot with the filtered data set and add relevant title
      
      ggplot(mapping = aes(x = reorder(area, value), y = value, fill = area)) + geom_col(show.legend = FALSE) + coord_flip() +
      labs(y = "Violent Crime Rate Per 100,000 (%)", 
           x = "States",
           title = "States with the Highest Violent Crime Rates",
           caption = "Source: US FBI:UCR Crime Data") + 
      theme_economist_white()
  })
  
  output$lowest_bar_plot <- renderPlot({
    
    # Prepares data table for bar graph by filtering out the user-selected year and crime type 
    
    all_states %>% 
      filter(year == input$year3,
             crime_type == input$crime_type3) %>% 
      arrange(value) %>% 
      slice(1:input$slice3) %>% 
      
      # Draw the bar plot with the filtered data set and add relevant title
      
      ggplot(mapping = aes(x = reorder(area, -value), y = value, fill = area)) + geom_col(show.legend = FALSE) + coord_flip() +
      labs(y = "Violent Crime Rate Per 100,000 (%)", 
           x = "States",
           title = "States with the Lowest Violent Crime Rates",
           caption = "Source: US FBI:UCR Crime Data") + 
      theme_economist_white()
  })
  
  # Generates the gif that contains the animated bar graph for tab three 
  
  output$violent_gif <- renderImage({
    
    # Pulls the premade gif from the shiny directory
    
    list(src = "violent.gif",
         contentType = 'image/gif'
    )}, deleteFile = FALSE)
  
  # Generates text for tab four
  
  output$about <- renderText("This project visualizes US Crime Data from US Federal Bureau of Investigation : Uniform Crime Reports Data.
                    My code can be found at https://github.com/sabrinacx/gov_1005_final_project. ")
}

# Run the application 
shinyApp(ui = ui, server = server)
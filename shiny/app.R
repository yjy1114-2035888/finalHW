library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

squirrel_data <- read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")

# Define UI for Shiny app
ui <- fluidPage(
  # Create tabsetPanel for opening page
  tabsetPanel(
    # General information about dataset
    tabPanel("About",
             p("This dataset contains data from the ", strong("2018 Central Park Squirrel Census"), ". The census was conducted by the Squirrel Census organization, with the goal of understanding the distribution and behavior of Eastern gray squirrels in Central Park."),
             p("The dataset includes information on the location, date, age, and behavior of each squirrel observed during the census."),
             p(em("Use the other tabs to explore the data in more detail."))),
    
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("color", "Primary Fur Color", 
                             choices = c("All", unique(squirrel_data %>% pull(`Primary Fur Color`))),
                             selected = "All")
               ),
               mainPanel(
                 plotOutput("scatterplot"),
                 textOutput("summary")
               )
             )
    ),
    
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Variable",
                             choices = c("Age", "Primary Fur Color", "Location"),
                             selected = "Age"),
                 sliderInput("sample_size", "Sample Size", min = 1, max = 1000, value = 100)
               ),
               mainPanel(
                 tableOutput("squirrel_table"),
                 textOutput("table_summary")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    squirrel_data %>% 
      filter(if(input$color == "All") TRUE else `Primary Fur Color` == !!input$color)
  })
  
  # Create plot
  output$scatterplot <- renderPlot({
    filtered_data() %>% 
      ggplot(aes(x = X, y = Y, color = `Primary Fur Color`)) +
      geom_point(alpha = 0.5, size = 2) +
      labs(x = "Longitude", y = "Latitude", title = "Squirrel Sightings in Central Park") +
      guides(color = guide_legend(title = "Fur Color"))
  })
  
  # Create text output for scatterplot page
  output$summary <- renderText({
    paste("Selected subset contains", nrow(filtered_data()), "observations.")
  })
  
  # Create table output for table page
  output$squirrel_table <- renderTable({
    filtered_data() %>% 
      select(input$variable) %>% 
      sample_n(input$sample_size)
  })
  
  # Create text output for table page
  output$table_summary <- renderText({
    paste("Showing", input$sample_size, "random observations for the variable", input$variable, ".")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
# Define UI for application
library(shiny)

# Import CSV
Data <- read.csv("data/Cleaned Data (Ver 2.0).csv")

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Data Visualization"),
  
  # Show plots
  mainPanel(
    mainPanel(
      h3("On a scale of 1 to 10, how likely are you to instinctively take the shortest path to the following amenities?", align = "center"),
      # Dropdown list to select column for distPlot2
      selectInput("column_select_distPlot2", "Select an amenity:", 
                  choices = c("EateriesOrHawkerCentres", "Supermarkets", "Parks", "PolyclinicsOrMedicalClinics", "DropOffPointsOrBusStops")),
      
      plotOutput("distPlot2")
    ),
    mainPanel(
      h3("How likely are you to alter your planned route within your residential estate due to adverse weather conditions (e.g. rain, sunny)?", align = "center"),
      plotOutput("distPlot6")
    ),
    width = "800px"
  )
)

# Define server logic
server <- function(input, output) {
  
  output$distPlot2 <- renderPlot({
    # Get the selected column from dropdown
    selected_column <- input$column_select_distPlot2
    # Extract the column data
    column_data <- Data[[selected_column]]
    # Check if the column data is numeric
    if (!is.numeric(column_data)) {
      stop("The selected column data is not numeric.")
    }
    
    # Create a bar plot of the frequency of values
    freq_counts <- table(column_data) + 5
  
    max_freq <- max(freq_counts)
    ylim_max <- max_freq + 5
    
    barplot(freq_counts, 
            main = paste("Frequency of Values in", selected_column, "(1- Least likely, 10- Most likely)"),
            xlab = "Value", ylab = "Frequency", ylim = c(0, ylim_max))
    
    # Calculate the x-coordinates for the labels (center of each bar)
    bar_centers <- barplot(freq_counts, plot = FALSE, ylim = c(0, ylim_max))
    
    label_positions <- freq_counts + max_freq * 0.05
    
    # Add labels on the bars
    text(x = bar_centers, y = label_positions, labels = freq_counts, pos = 3, col = "blue")
  })
  
  output$distPlot6 <- renderPlot({
    # Get the selected column from dropdown
    selected_column <- input$column_select
    
    # Extract the column data
    column_data <- Data$AdverseWeatherConditions
    
    # Calculate frequency counts for all unique values
    freq_counts <- table(column_data)
    
    # Sort the frequency counts in ascending order
    freq_counts_sorted <- sort(freq_counts)
    
    # Create a bar plot of the frequency of values, now in ascending order
    bp <- barplot(freq_counts_sorted, main = "Frequency of Values in Adverse Weather Conditions",
                  xlab = "AdverseWeatherConditions", ylab = "Frequency",
                  col = "lightblue", border = "black",
                  ylim = c(0, max(freq_counts_sorted) * 1.1))  # Set finite y-axis limits
    
    # Add labels on the bars, using the sorted data
    text(x = bp, y = freq_counts_sorted, labels = freq_counts_sorted, pos = 3, col = "blue")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(shinydashboard)
library(markdown)


ui <- dashboardPage(
  dashboardHeader(title = "Step Ahead Solutions Dashboard", 
                  titleWidth = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Demographics", tabName = "demographics", icon = icon("stats", lib = "glyphicon")),
      menuItem("Ang Mo Kio", tabName = "amk", icon = icon("map marked alt")),
      menuItem("Punngol", tabName = "pg", icon = icon("map marked alt"))
    )
    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              includeMarkdown("home.md")
      ),
      
      # Second tab content
      tabItem(tabName = "demographics",
              h2("Tableau Dashboard"),
              fluidRow(
                tags$iframe(src = "https://public.tableau.com/views/PopulationDemographicAnalysis/Dashboard12?:language=en-US&:sid=&:display_count=n&:origin=viz_share_link?:showVizHome=no&:embed=true",
                            width = "100%",
                            height = "700px",
                            frameborder = "0")
              )
      ),
      
      
      
      
      # Third tab content
      tabItem(tabName = "amk",
              h2("Analysis on Ang Mo Kio"),
              plotOutput("mapPlot"),
              DT::dataTableOutput(outputId = "szTable")
      ),
      # Fourth tab content
      tabItem(tabName = "pg",
              h2("Analysis on Punngol")
            
              
      )
    )
  )
)



server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  server <- function(input, output){
    output$mapPlot <- renderPlot({
      tm_shape(mpsz)+
        tm_fill() +
        tm_borders(lwd = 0.1,  alpha = 1)
    })
    
    output$szTable <- DT::renderDataTable({
      if(input$show_data){
        DT::datatable(data = mpsz %>% select(1:7),
                      options= list(pageLength = 10),
                      rownames = FALSE)
      }
    })    
  }
}

shinyApp(ui = ui, server = server)

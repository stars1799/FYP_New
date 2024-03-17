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
library(sf)
library(tmap)
library(tidyverse)

mpsz <- st_read(dsn = "data/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL")

ui <- dashboardPage(
  dashboardHeader(title = "Step Ahead Solutions Dashboard", 
                  titleWidth = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      menuItem("Dashboards", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Ang Mo Kio", tabName = "AngMoKio", icon = icon("th")),
      menuItem("Punngol", tabName = "Punngol", icon = icon("th"))
    )
    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Demographics", "This are the demographics of ...")
              ),
              
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                ), 
                
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("routeSelect"),
                           checkboxGroupInput("directions", "Show",
                                              choices = c(
                                                Northbound = 4,
                                                Southbound = 1,
                                                Eastbound = 2,
                                                Westbound = 3
                                              ),
                                              selected = c(1, 2, 3, 4)
                           )
                       )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "AngMoKio",
              h2("Analysis on Ang Mo Kio"),
              plotOutput("mapPlot"),
              DT::dataTableOutput(outputId = "szTable")
      ),
      # Third tab content
      tabItem(tabName = "Punngol",
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

shinyApp(ui, server)

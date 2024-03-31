######## Loading of Packages ########
library(shiny)
library(sf)
library(tmap)
library(arrow)
library(lubridate)
library(tidyverse)
library(sp)
library(raster)
library(spatstat)
library(classInt)
library(viridis)
library(spNetwork)
library(spatstat)
library(ggplot2)
#install.packages("ggmap")
library(ggmap)




######### Loading of Data ##########

pg_wdl <- st_read(dsn = "data/geospatial", 
                  layer = "pg_wdl")
amk_wdl <- st_read(dsn = "data/geospatial", 
                   layer = "wdl_amk")
dl_pg <- st_read(dsn = "data/geospatial", 
                 layer = "pg_dl")
dl_amk <- st_read(dsn = "data/geospatial", 
                  layer = "dl_amk")

target_buildings <- st_read(dsn = "data/geospatial", 
                            layer = "target_buildings")
tb_cat <- target_buildings["Categorize"]



mpsz <- st_read(dsn = "data/geospatial/MPSZ", layer = "MPSZ-2019")
mpsz3414 <- st_transform(mpsz, 3414)
pa_mpsz <- mpsz3414["PLN_AREA_N"]
mpsz2 <- as_Spatial(pa_mpsz)



pg = mpsz2[mpsz2@data$PLN_AREA_N == "PUNGGOL",]
amk = mpsz2[mpsz2@data$PLN_AREA_N == "ANG MO KIO",]

pg_sp = as(pg, "SpatialPolygons")
amk_sp = as(amk, "SpatialPolygons")

#pg_owin <- as.owin(pg_sp)
#amk_owin <- as.owin(amk_sp)

pg_sf <- st_as_sf(pg)
amk_sf <- st_as_sf(amk)

pg_buildings <- st_intersection(pg_sf, tb_cat)
amk_buildings <- st_intersection(amk_sf, tb_cat)


landuse_pg <- st_read(dsn = "data/geospatial", 
                      layer = "FYP_target_landuse_PG")
landuse_amk <- st_read(dsn = "data/geospatial", 
                       layer = "FYP_target_landuse_AMK")

osm_basemap <- tm_basemap(server = "OpenStreetMap.HOT")
imagery_basemap <- tm_basemap(server = "Esri.WorldImagery")



landuse_pg_sf <- st_as_sf(landuse_pg)
landuse_amk_sf <- st_as_sf(landuse_amk)

landuse_pg_sf <- st_make_valid(landuse_pg_sf)
valid_pg <- st_make_valid(pg_sf)
valid_amk <- st_make_valid(amk_sf)

pg_landuse <- st_intersection(valid_pg, landuse_pg_sf)
amk_landuse <- st_intersection(valid_amk, landuse_amk_sf)

parks_pg <- st_read(dsn = "data/geospatial", 
                    layer = "FYP_walkingtime_parks_PG")
parks_amk <- st_read(dsn = "data/geospatial", 
                     layer = "FYP_walkingtime_parks_AMK")
healthcare_pg <- st_read(dsn = "data/geospatial", 
                         layer = "FYP_walkingtime_healthcare_PG")
healthcare_amk <- st_read(dsn = "data/geospatial", 
                          layer = "FYP_walkingtime_healthcare_AMK_1")
food_pg <- st_read(dsn = "data/geospatial", 
                   layer = "FYP_walkingtime_foodplaces_PG")
food_amk <- st_read(dsn = "data/geospatial", 
                    layer = "FYP_walkingtime_foodplaces_AMK")
smarket_pg <- st_read(dsn = "data/geospatial", 
                      layer = "FYP_walkingtime_smarket_PG")
smarket_amk <- st_read(dsn = "data/geospatial", 
                       layer = "FYP_walkingtime_smarket_AMK")


parks_pg_sf <- st_as_sf(parks_pg)
parks_amk_sf <- st_as_sf(parks_amk)
healthcare_pg_sf <- st_as_sf(healthcare_pg)
healthcare_amk_sf <- st_as_sf(healthcare_amk)
food_pg_sf <- st_as_sf(food_pg)
food_amk_sf <- st_as_sf(food_amk)
smarket_pg_sf <- st_as_sf(smarket_pg)
smarket_amk_sf <- st_as_sf(smarket_amk)


parks_pg_sf <- st_make_valid(parks_pg_sf)
parks_amk_sf <- st_make_valid(parks_amk_sf)
healthcare_pg_sf <- st_make_valid(healthcare_pg_sf)
healthcare_amk_sf <- st_make_valid(healthcare_amk_sf)
food_pg_sf <- st_make_valid(food_pg_sf)
food_amk_sf <- st_make_valid(food_amk_sf)
smarket_pg_sf <- st_make_valid(smarket_pg_sf)
smarket_amk_sf <- st_make_valid(smarket_amk_sf)

pg_parks <- st_intersection(valid_pg, parks_pg_sf)
pg_healthcare <- st_intersection(valid_pg, healthcare_pg_sf)
pg_food <- st_intersection(valid_pg, food_pg_sf)
pg_smarket <- st_intersection(valid_pg, smarket_pg_sf)

amk_parks <- st_intersection(valid_amk, parks_amk_sf)
amk_healthcare <- st_intersection(valid_amk, healthcare_amk_sf)
amk_food <- st_intersection(valid_amk, food_amk_sf)
amk_smarket <- st_intersection(valid_amk, smarket_amk_sf)

pg_b_cat <- pg_buildings["Categorize"]

a_dl_class <- amk_wdl["desired_li"]

## Landuse map 




######### Global Parameters ########

#Area of Study (drop-down)
studyarea <- c(
  "Ang Mo Kio", 
  "Punggol"
)

#Buildings Categories (multi-select)
buildingcat <- c(
  "All", 
  "Educational Facilities", 
  "Industrial", 
  "Multi-Purpose Halls", 
  "Offices", 
  "Other", 
  "Parking", 
  "Public Transport", 
  "Religious Buildings", 
  "Residential", 
  "Road",
  "Sport Facilities", 
  "Water Body"
)

#


#######ui
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        /* Custom CSS to change the navbar color */
        .navbar-default {
          background-color: #FAE9DC; /* Set navbar background color */
          border-color: #ddd; /* Set navbar border color */
           height: 50px;
        
        }
        
        /* Main content padding for better alignment */
        .container-fluid {
          padding-right: 50px;
          padding-left: 50px;
          height: 50px; 
        }
        
        .navbar-brand {
          position: absolute;
          margin-top: -395px;
          left: 100px;
        }
        
        
        "
      )
    )
  ),
  
  # Navbar
  div(class = "navbar navbar-default",
      div(class = "container-fluid",
          imageOutput("top_logo"),
          a(class = "navbar-brand", "STEP AHEAD SOLUTIONS")
      )
  )


  
,
    
    tabPanel("Home",
               div(style = "text-align: center;", imageOutput("logo")), 
               br(),
               hr(),
               br(),
               h4("Welcome to our framework designed to address the discrepancies between current urban plans and community preferences within HDB estates. Our framework aims to enhance accessibility for residents by identifying and mitigating desired pedestrian lines. Navigate through the various sections to explore our research insights, demographic analyses, and proposed solutions."),
               br(),
               h4(strong("Objective Framework")),
               p("Our objective is to develop a comprehensive framework that identifies and mitigates discrepancies between the current urban plans and community preferences, specifically targeting desired pedestrian lines, thereby enhancing accessibility for residents in HDB estates."),
               br(),
               h4(strong("Link to Sponsor")),
               p("By providing a comprehensive framework, our project empowers urban planners to align infrastructure development with the actual movement patterns and preferences of residents. This ensures that current urban layouts of the walkways accurately reflect the community's needs, enhancing convenience, effectiveness, and ultimately improving the overall quality of life within HDB estates."), 
               br(),
               h4(strong("Challenge Statement")), 
               p("To reduce the disparity between urban plans and community preferences, improving accessibility for residents in HDB estates by reducing desired pedestrian lines.")
             ),
    
    navbarMenu("Research Insights",
               tabPanel("Demographics Analysis Insights",
                        div(style = "background-color: #f2f2f2; padding: 10px;",
                            h4(style = "margin-top: 0; margin-bottom: 10px;", strong("Key Insights")),
                            HTML("<ol>
                             <li>Population has been steadily increasing over the years but the population of children is starting to fall</li>
                             <li>There is an increasing trend in the combined population of middle-aged and elderly individuals over the years in Ang Mo Kio (mature estates)</li>
                             <li>There is a significant increase across all age groups over the past decade in Punggol (non-mature estates)</li>
                             <li>Population affects the formation of desire lines</li>
                             </ol>")
                        ),
                        br(), 
                        tabsetPanel(
                          tabPanel(strong("Key Insight 1"),
                                   br(),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Insight 1")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>Population has been steadily increasing over the years but the population of children is starting to fall</h4>")
                                       )
                                   ),
                                   br(),
                                   br(), # Add a line break
                                   div(style = "background-color: #f2f2f2; padding: 10px; text-align: center;",
                                       h4(style = "margin-top: 0; margin-bottom: 8px; text-align: center;", 
                                          strong("Tableau Dashboard")),  # Wrap with strong tag
                                       div(style = "text-align: center;",
                                           HTML('<iframe src="https://public.tableau.com/views/Hypothesis25/Dashboard1?:language=en-GB&:embed=y&:display_count=n&:showVizHome=no" width="1200" height="800" frameborder="0"></iframe>')
                                       )
                                   ),
                                   br(), 
                                   br(), 
                                   p(strong("Description: "), "The dashboard looks to explore the population growth in Singapore from the 1950s to the 2020s with a specific look at the change in population for specific age groups (child - 0 to 19 years old, young adults - 20 to 39 years old, middle-aged - 40 to 59 years old, and elderly - 60 years old and above)."),
                                   p(strong("Analysis:"), "Overall, the trends in the population pyramid suggest that the population in Singapore has been increasing over the last 70 years and this trend is consistent when we break down into the 4 age groups defined above. However, there is a worrying trend that is becoming increasingly apparent for the “Child” age group as they have been trending downwards since the 2010s while for the “Elderly” age group, they have been growing at an almost exponential rate since the turn of the century. This is also evident when looking at the scatter plots, where the number of male and female children in the 2020s are almost at levels that they were in the 1950s. These scatter plots also highlight another concerning trend among “Young Adults” and “Middle-aged” where their numbers are starting to plateau instead of showing any clear increase or decrease.")
                          ),
                          tabPanel(strong("Key Insight 2 & 3"),
                                   br(),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Insight 2")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>There is an increasing trend in the combined population of middle-aged and elderly individuals over the years in Ang Mo Kio (mature estates)</h4>")
                                       )
                                   ),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Insight 3")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>There is a significant increase across all age groups over the past decade in Punggol (non-mature estates)</h4>")
                                       )
                                   ),
                                   br(),
                                   br(), # Add a line break
                                   div(style = "background-color: #f2f2f2; padding: 10px; text-align: center;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Tableau Dashboard")),
                                       div(style = "text-align: center;",
                                           HTML('<iframe src="https://public.tableau.com/views/PopulationDemographicAnalysis/Dashboard12?:embed=y&:showVizHome=no" width="1200" height="800" frameborder="0"></iframe>')
                                       )
                                   ),
                                   br(), 
                                   br(), 
                                   p(strong("Description: for Insight 2 "), "This dashboard aims to provide a comprehensive overview of the Ang Mo Kio demographics population by looking at factors such as age groups, gender, years etc."),
                                   p(strong("Analysis:"), "Overall, the trends suggest a shift in the demographic composition across age groups of child, middle aged and young adults in Ang Mo Kio. With a decrease in children 
                                     and young adults, a steady increase in the elderly population, and a decline in the middle-aged population over the past decade. In terms of age group distribution in AMK, the highest 
                                     population is in the age group 55-59, while the lowest is in the age group 85 and over. "), 
                                   p("In the age group trend of the nested group, the graph suggests a decreasing trend in the combined population of children and young adults, while the combined population of middle-aged and 
                                     elderly individuals shows a slight increase with some fluctuations over the years. Generally, the male and female populations tend to be comparable, but there are a few age groups where one 
                                     gender may slightly outnumber the other. The gender distribution appears relatively balanced in the younger age groups. As individuals age, the gender ratio may exhibit some variations, but 
                                     in general, there is no significant skew toward one gender. In the older age groups, there is a trend of more females than males, especially in the 85 and over category. With that, the analysis 
                                     highlights the importance of adapting urban planning approaches to evolving demographic trends within mature HDB estates like Ang Mo Kio, emphasising the necessity of improving accessibility 
                                     for residents by addressing pedestrian preferences alongside with urban plans."), 
                                   br(), 
                                   br(), 
                                   p(strong("Description for Insight 3: "), "The dashboard looks to explore the population growth in Singapore from the 1950s to the 2020s with a specific look at the change in population for specific age groups 
                                     (child - 0 to 19 years old, young adults - 20 to 39 years old, middle-aged - 40 to 59 years old, and elderly - 60 years old and above)."),
                                   p(strong("Analysis:"), "Overall, the trends in the population pyramid suggest that the population in Singapore has been increasing over the last 70 years and this trend is consistent when we break
                                    down into the 4 age groups defined above. However, there is a worrying trend that is becoming increasingly apparent for the “Child” age 
                                    group as they have been trending downwards since the 2010s while for the “Elderly” age group, they have been growing at an almost exponential rate since the turn of the century. 
                                    This is also evident when looking at the scatter plots, where the number of male and female children in the 2020s are almost at levels that they were in the 1950s. These scatter plots also highlight
                                    another concerning trend among “Young Adults” and “Middle-aged” where their numbers are starting to plateau instead of showing any clear increase or decrease.")
                          ),
                          tabPanel(strong("Key Insight 4"),
                                   br(),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Insight 4")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>Population affects the formation of desire lines</h4>")
                                       )
                                   ),
                                   br(),
                                   br(), # Add a line break
                                   div(style = "background-color: #f2f2f2; padding: 10px; text-align: center;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Tableau Dashboard")),
                                       div(style = "text-align: center;",
                                           HTML('<iframe src="https://public.tableau.com/views/Hypothesis27new/Dashboard12?:embed=y&:showVizHome=no" width="1200" height="800" frameborder="0"></iframe>')
                                       )
                                   ),
                                   br(), 
                                   br(), 
                                   p(strong("Description: "), "This dashboard aims to provide a comprehensive overview by looking at the relationship between the formation of desired lines and variables like population density, area size and population."),
                                   p(strong("Analysis:"), "The first dashboard showcases our desire lines and population density data across the 6 subzones that we collected desire line data from (Cheng San, Chong Boon, Kebun Bahru, Punggol Town Centre, 
                                   Townsville and Waterway East). It also showcases the spread of our data for both population density and desire lines respectively in the form of a box-and-whiskers graph. Selecting the area on either map would allow the 
                                   user to see the exact values for desired lines and population density on the box-and-whiskers graph."), 
                                   p("The second dashboard showcases the relationship between our desired lines and 3 different variables - population density, 
                                   area size and population respectively. Based on the Population Density & Desire Lines chart, although there is an upward trend observed, the p-value of this trend line is at 0.13. This means that the trend is not statically 
                                   significant, indicating that there is no real relationship between Population Density & Desire Lines."),
                                   p("As such, we would explore with the other variables to see if they have a statically significant relationship with desired lines.Firstly, based on the Area Size & Desire Lines chart, we noticed that there is an upward 
                                   trend as well. However, plotting the trend line once again gives us a p-value of 0.27 which also implies that this trend is not statically significant, indicating that there is no real relationship between Area Size & Desire Lines."),
                                   p("Lastly, based on the Population & Desire Lines chart, we observe that there is an upward trend as well. However, plotting the trend line this time gives us a significant p-value of only 0.007. This means that, with our alpha at 
                                     0.05, this trend is statistically significant, indicating that there is a real relationship between Population & Desire Lines. From this, we can see that population is a factor that determines the formation of desire lines and the 
                                     framework will delve deeper into the various demographics to see how they affect desire line formation.")
                          )
                        )
               ),
               tabPanel("Geographical Analysis Insights",
                        div(style = "background-color: #f2f2f2; padding: 10px;",
                            h4(style = "margin-top: 0; margin-bottom: 10px;", strong("Key Insights")),
                            HTML("<ol>
                             <li>Desired lines in Punggol (non-mature estates) are often found near main road infrastructure and areas beneath HDB blocks, often leading to pick-up points and recreational spots like playgrounds and exercise corners.</li>
                             <li>Ang Mo Kio (mature estates) have fewer and more dispersed desired lines, and within the HDB estate itself, there is a noticeable absence of desired lines.</li>
                             <li>/</li>
                             <li>/</li>
                             </ol>")
                        ),
                        br(), 
                        tabsetPanel(
                          tabPanel(strong("Key Insight 1"),
                                   br(),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Insight 1")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>Desired lines in Punggol (non-mature estates) are often found near main road infrastructure and areas beneath HDB blocks, often leading to pick-up points and recreational 
                                                spots like playgrounds and exercise corners.</h4>")
                                       )
                                   ),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Insight 2")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>Ang Mo Kio (mature estates) have fewer and more dispersed desired lines, and within the HDB estate itself, there is a noticeable absence of desired lines.</h4>")
                                       )
                                   ),
                                   br(),
                                   br(), # Add a line break
                                   div(style = "background-color: #f2f2f2; padding: 10px; text-align: center;",
                                       h4(style = "margin-top: 0; margin-bottom: 8px; text-align: center;", 
                                          strong("Interactive Map")),  # Wrap with strong tag
                                       fluidRow(
                                         column(8, # Map takes up 8/12 of the width
                                                tmapOutput("dl_amk")),
                                         column(4, # Selection panel takes up 4/12 of the width
                                                selectInput("mapSelection", "Select a Location:", choices = studyarea))
                                   )),
                                   br(), 
                                   br(), 
                                   p(strong("Description: "), "These 2 maps pinpoint the locations of desired lines within the urban landscape in Ang Mo Kio and Punggol. Which represent mature and non-mature estates respectively."),
                                   p(strong("Key Insight 1 Analysis:"), "Our observations of desired lines in Punggol reveal a clustering effect along main roads and beneath HDB blocks, suggesting a natural emergence influenced by 
                                     the convenience of crossing road infrastructure and accessibility to amenities like roundabouts, playgrounds, and exercise corners. This leads us to infer that desired lines naturally emerge 
                                     based on the convenience of crossing road infrastructure and the accessibility to amenities such as roundabouts, playgrounds, and exercise corners. This allowed us to direct more attention to 
                                     these specific locations, enabling a more focused investigation into preventive measures aimed at mitigating the emergence of desired lines."), 
                                   p(strong("Key Insight 2 Analysis:"), "Key insight 2 can be seen through the comparison of the comparison of the desired lines in Ang Mo Kio and Punggol, where Ang Mo Kio has fewer and more 
                                     dispersed desired lines. While the desired lines still emerge predominantly along the road infrastructure, the count is still notably lower compared to Punggol. Additionally, within the HDB 
                                     estate of Ang Mo Kio, there is a noticeable absence of desired lines. "), 
                                   p("Though we recognise that it is important to acknowledge that numerous mature estates have already undergone retrofitting processes to meet the resident’s needs. Which meant that, the recorded
                                     number of desired lines in our study may not entirely reflect the original situation accurately. Nevertheless, this insight spurred further research into the potential disparities between 
                                     infrastructure design and the preferences of demographics in mature and non-mature estates.")
                                   )
                          
                          )
    )),
    
    tabPanel("Framework", 
             div(br())
    ),
    
    tabPanel("Conclusion", 
             div(br())
    )
  )



server <- function(input, output, session) {
  
  ##################### Images ######################
  
  ### Home Page banner  
  output$logo <- renderImage({
    list(src = "images/homepage.png",
         width = 1250,
         height = 435,
         style = "display: block; margin-left: auto; margin-right: auto; margin-top: 0px; margin-bottom: 0px;")
  }, deleteFile = FALSE)
  
  output$top_logo <- renderImage(
    list(src = "images/top_logo.png",
         width = 50,
         height = 40,
         onclick = "window.location.href='#home'"), deleteFile = FALSE)
  
  
  ###################################################
  tmap_mode("view")
  
  map_loading <- eventReactive(input$studyarea, {
    data <- NULL  # Initialize to NULL to ensure data has a default state
    if (!is.null(input$studyarea)) {
      if (input$studyarea == "Punggol") {
        data <- pg_landuse
      } else {
        data <- a_dl_class
      }
    }
    
    if (!is.null(data)) {
      return(dl_map <- osm_basemap +
               tm_shape(a_dl_class) +
               tm_lines(col = "red", lwd = 2) +  # Adjust line color and width as needed
               tmap_options(check.and.fix = TRUE))
    }
  }, ignoreNULL = FALSE)  # Use this to decide whether to ignore the initial NULL state
  
  # Ensure you render the map correctly in the UI
  output$landuseMap <- renderTmap({
    map_loading()  # This will now call and render the map generated by map_loading
  })
  
  output$dl_map_2 <- renderTmap({
    
    tmap_mode("view")
    # Check the selection and prepare the respective map
    if (input$studyarea == "Ang Mo Kio") {
      dl_map <- osm_basemap +
        tm_shape(a_dl_class) +  # Assuming 'a_dl_class' is your spatial object for AMK
        tm_lines(col = "red", lwd = 2) +
        tmap_options(check.and.fix = TRUE)
    } else {
      # Prepare another map for 'other_map' selection
      # This is just a placeholder, replace with actual code for the other map
      dl_map <- osm_basemap +
        tm_shape(dl_class) +  # Replace 'other_spatial_object' with the actual data
        tm_lines(col = "blue", lwd = 2) +  # Example styling
        tmap_options(check.and.fix = TRUE)
    } 
    return(dl_map)
  })
  
  output$dl_amk <- renderTmap({
    
    dl_map <- tm_basemap(server = "OpenStreetMap") +  # Use tm_basemap to specify the basemap
      tm_shape(mpsz[mpsz$PLN_AREA_N=="ANG MO KIO", ]) + #Replace by studyarea choices
      tm_borders()+
      tm_shape(dl_amk)+ #replace by combining the map of dl_amk & dl_pg
      tm_lines(col = "red", lwd = 2) +  # Adjust line color and width as needed
      tmap_options(check.and.fix = TRUE)
    
    dl_map  # Return the map for rendering
  })
  
}






shinyApp(ui = ui, server = server)


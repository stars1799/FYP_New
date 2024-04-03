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
library(ggmap)
library(tidymodels)
library(glmnet)






######### Loading of Data ##########

###### EDA Data ##### 

Data <- read.csv("data/aspatial/Cleaned_Data_v2.csv")


###### Geospatial ######
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

#Choices for EDA(Amanda) Framework #1 
amenities <- c(
  "Eateries Or Hawker Centres" = "EateriesOrHawkerCentres",
  "Supermarkets" = "Supermarkets",
  "Parks" = "Parks",
  "Polyclinics Or Medical Clinics" = "PolyclinicsOrMedicalClinics",
  "Drop-Off Points Or Bus Stops" = "DropOffPointsOrBusStops"
)





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
          padding-right: 30px;
          padding-left: 30px;
          height: 50px; 
        }
      
        
       #top_logo {
          height: 50px;
          margin-top: -10px;
          margin-left: -36px;
          margin-right: 120px; /* Add margin to separate image and text */
        }

        .navbar-text{
        margin-left: 36px;
        margin-top: -50px;
        z-index: 100000;
        position: absolute; /* Ensure the z-index works properly */
        }
        
        
        "
      )
    )
  ),
  
  
  
  # Navbar

  navbarPage(
    
    column(
      width = 2,
      imageOutput("top_logo")
    ),
    
    tags$p(class = "navbar-text", "STEP AHEAD SOLUTIONS"),
    
    tabPanel("Home",
               div(style = "text-align: center;", imageOutput("logo")), 
               hr(),
               h4("Welcome to our framework designed to address the discrepancies between current urban plans and community preferences within HDB estates. Our framework aims to enhance accessibility for residents by identifying and mitigating desired pedestrian lines. Navigate through the various sections to explore our research insights, demographic analyses, and proposed solutions."),
               br(),
               h4(strong("Objective Framework")),
               p("Our objective is to develop a comprehensive framework that identifies and mitigates discrepancies between the current urban plans and community preferences, specifically targeting desired pedestrian lines, thereby enhancing accessibility for residents in HDB estates."),
               h4(strong("Link to Sponsor")),
               p("By providing a comprehensive framework, our project empowers urban planners to align infrastructure development with the actual movement patterns and preferences of residents. This ensures that current urban layouts of the walkways accurately reflect the community's needs, enhancing convenience, effectiveness, and ultimately improving the overall quality of life within HDB estates."),
               h4(strong("Challenge Statement")), 
               p("To reduce the disparity between urban plans and community preferences, improving accessibility for residents in HDB estates by reducing desired pedestrian lines.")
             ),
    
    tabPanel("Framework", 
             div(style = "background-color: #f2f2f2; padding: 10px;",
                 h4(style = "margin-top: 0; margin-bottom: 10px;", strong("Key Actionables")),
                 HTML("<ol>
                             <li>Constructing direct paths that are easily accessible for residents to reduce the walking time and to facilitate easier access to 
                                     transport, roundabouts, supermarkets and eateries, reducing the necessity for residents to resort to creating unofficial shortcuts.</li>
                             <li>To construct more direct sheltered built paths with better drainage systems in order to incentivize their usage.</li>
                             <li>Develop simple HDB layouts that ensure direct and intuitive pathways for residents, in order to prevent residents from needing to
                                create desired paths in order to prevent the use of the long-winding routes within the HDB estate.</li>
                             <li>To pave the sides of the roads, especially those near bus stops to increase convenience and to prevent the formation of desired paths.</li>
                             </ol>")
             ),
             br(), 
             tabsetPanel(
               tabPanel(strong("Key Actionable 1"),
                        br(), 
                        div(style = "background-color: #f2f2f2; padding: 10px;",
                            h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Actionable 1")),
                            div(style = "text-align: center;",
                                HTML("<h4>Constructing direct paths that are easily accessible for residents to reduce the walking time and to facilitate easier access to 
                                     transport, roundabouts, supermarkets and eateries, reducing the necessity for residents to resort to creating unofficial shortcuts.</h4>")
                            )
                        ),
                        br(),
                        br(), 
                        tabsetPanel(
                          tabPanel(strong("Sub-Challenge"),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Sub-Challenge")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>Which specific amenities contribute to the emergence of desired lines, and what are the underlying reasons driving this?</h4>")
                                       ), 
                                   ),
                                   br(), 
                                   p(strong("Purpose of Sub-Challenge:")),
                                   p(strong("1. Many desired lines in Punggol (non-mature estates) are often found in areas beneath HDB blocks, often leading to pick-up points and 
                                            recreational spots like playgrounds and exercise corners.")), 
                                   p("Our observations of desired lines in Punggol reveal a clustering effect beneath HDB blocks, suggesting a natural emergence influenced by the 
                                     convenience of crossing road infrastructure and accessibility to amenities like roundabouts, playgrounds, and exercise corners. This led us to 
                                     infer that desired lines naturally emerge based on the accessibility to amenities such as roundabouts, playgrounds, and exercise corners. This 
                                     allowed us to direct more attention to these amenities."),
                                   br() 
                                   ),
                          tabPanel(
                            strong('Evidence'), 
                            br(),
                            p(strong("Supporting Evidence")),
                            p("Our survey findings indicate that bus stops, supermarkets, and eateries are the top amenities for which respondents strongly prefer shorter walking times, 
                                thus considering them more convenient. Additionally, these same amenities constitute the top three amenities for which respondents are inclined to take 
                                desired paths or shortcuts."),
                            br(), 
                            column(
                                width = 12,
                                h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", 
                                   strong("On a scale of 1 to 10, how likely are you to instinctively take the shortest path to the following amenities?")),
                                selectInput("column_select_distPlot2", "Select an amenity:", 
                                            choices = amenities),
                                fluidRow(
                                  column(offset = 2, width = 8, # Adjust the offset and width as needed
                                         plotOutput("distPlot2", height = "600px")
                                  )
                                ),
                            ),
                            br(), 
                            p("Our survey findings also revealed the importance of convenience in residents' decision-making regarding their walking routes for daily activities. With 
                              convenience ranking as the primary factor influencing their choice of route, it becomes evident that residents prioritise efficiency when accessing amenities 
                              within their neighbourhoods."),
                            br(), 
                            div(style = "text-align: center;", imageOutput("eda_choices")),
                            p("Additionally, the subsequent top-ranking factors—short walking distance and accessibility—further brings out the aspects needed to create this path. This further
                              emphasises the need to create the most accessible and direct routes possible, minimising travel time."), 
                            p("Upon further exploration of our top-ranking factor of convenience through demographic segmentation, our findings reveal that convenience holds the highest 
                              significance among the young adult population compared to the other demographic groups. This insight holds particular significance as our research also highlighted 
                              a trend of increasing young adult population in non-mature estates over the years. This demographic shift suggests an increasing need to cater to the needs and 
                              preferences of this demographic segment. Thus emphasizing on the increasing need to prioritize convenience when designing built paths. "), 
                            fluidRow(
                              column(6, div(style = "text-align: center;", imageOutput("eda_convenience"))),
                              column(6, div(style = "text-align: center;", imageOutput("pg_dashboard")))
                            ),
                            p("Constructing direct paths aligns closely with residents' prioritisation of convenience and accessibility. By implementing this strategy, we can effectively reduce 
                              desired lines by optimising residents' mobility."), 
                            br(), 
                            br()
                              )
                            )
                          ), 
               tabPanel(strong("Key Actionable 2"),
                        
                        br(), 
                        div(style = "background-color: #f2f2f2; padding: 10px;",
                            h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Actionable 2")),
                            div(style = "text-align: center;",
                                HTML("<h4>To construct more direct sheltered built paths with better drainage systems in order to incentivize their usage.</h4>")
                            )
                        ),
                        br(),
                        br(),
                        tabsetPanel(
                          tabPanel(strong("Sub-Challenge"),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Sub-Challenge")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>How can we enhance & improve pedestrian walkways within HDB estates to encourage higher utilisation of built paths?</h4>")
                                       ), 
                                   ),
                                   br(), 
                                   p(strong("Purpose of Sub-Challenge:")),
                                   p(strong("1. Built paths are sometimes under-utilised due to a lack of directness of sheltered paths, which forces residents to take long detours 
                                            to their intended destination in the event of inclement weather conditions. ")), 
                                   p(strong("2. In our interview with residents during our on-the-ground research in Punggol, residents indicated that sheltered walkways often do 
                                            not provide the most direct routes to facilities such as MRT stations or eateries, even though they’re useful during adverse weather. 
                                            They need to detour around to reach the nearest sheltered path. Therefore, these paths are typically utilised only during poor weather 
                                            conditions, as they are not the shortest path to such amenities. ")), 
                                   p((strong("3. In our interview with a resident in Punggol, he mentioned the drainage system from the pathway of Punggol MRT to the nearby HDB estate 
                                             is poor, especially during adverse weather. The water is flooded on the pathway which makes it very slippery.")),
                                   br(),
                          )),
                          tabPanel(
                            strong('Evidence'), 
                            br(),
                            p(strong("Supporting Evidence")),
                            p("Using sentiment analysis, we have discovered that shelters significantly impact an individual’s sentiment and preference regarding their choice of 
                              walkways. Through word cloud analysis, it has revealed that many residents are expressing dissatisfaction with the lack of shelter in their estate. Upon 
                              further investigation into the data, we have discovered that this feedback extends to concerns about excessive detours along paths and sudden interruptions 
                              in sheltered routes. Therefore, many residents express a desire for more direct sheltered paths that do not compromise on convenience."),
                            br(), 
                            fluidRow(
                              column(6, div(style = "text-align: center;", imageOutput("dislike_bigram"))),
                              column(6, div(style = "text-align: center;", imageOutput("dislike_unigram")))
                            ),
                            p("Furthermore, our data analysis revealed that adverse weather conditions, such as intense sunlight and rainfall, significantly influence residents to alter 
                              their routes. This would suggest a natural inclination among people to seek sheltered paths to avoid exposure to the sun, which is prevalent in Singapore’s 
                              climate."),
                            br(), 
                            h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", 
                               strong("How likely are you to alter your planned route within your residential estate due to adverse weather conditions (e.g., rain, sunny)?")), 
                            fluidRow(
                              column(offset = 2, width = 8, # Adjust the offset and width as needed
                                     plotOutput("distPlot6", height = "600px")
                              )
                            ),
                            p("On the flipside, heavy rain may result in ponding in sections of sheltered walkways, which renders these walkways slippery and unsafe to use. This could be
                              due to poor drainage systems on sections of these walkways, which includes leaking of water from the roof of shelters or overflowing of water from the roads
                              to the walkways. In our survey, a few of our residents also reported that walkway shelters do not adequately protect against heavy rain due to sustained wind
                              or narrowly-built shelters, which contributes to the overall decrease of safety on these walkways."),
                            p("Thus, reinforcing the recommendation to prioritise the construction of more direct built paths with adequate shelter. Hence, indicating that residents would 
                              be more inclined to use a sheltered direct built path as their route.")
                          )
                          )
                        ), 
               tabPanel(strong("Key Actionable 3"),
                        br(), 
                        div(style = "background-color: #f2f2f2; padding: 10px;",
                            h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Actionable 3")),
                            div(style = "text-align: center;",
                                HTML("<h4>Develop simple HDB layouts that ensure direct and intuitive pathways for residents, in order to prevent residents from needing to
                                create desired paths in order to prevent the use of the long-winding routes within the HDB estate.</h4>")
                            )
                        ),
                        br(),
                        br(),
                        tabsetPanel(
                          tabPanel(strong("Sub-Challenge"),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Sub-Challenge")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>Does the layout of the HDB estate influence the emergence of desired lines?</h4>")
                                       ), 
                                   ),
                                   br(), 
                                   p(strong("Purpose of Sub-Challenge:")),
                                   p(strong("1. Ang Mo Kio (mature estates) have fewer and more dispersed desired lines, and within the HDB estate itself, there is a noticeable 
                                            absence of desired lines.")), 
                                   p("This can be seen through the comparison of the desired lines in Ang Mo Kio and Punggol, where Ang Mo Kio has fewer and more dispersed desired 
                                     lines. While the desired lines still emerge predominantly along the road infrastructure, the count is still notably lower compared to Punggol. 
                                     Additionally, within the HDB estate of Ang Mo Kio, there is a noticeable absence of desired lines."), 
                                   br(),
                                   div(style = "text-align: center;", imageOutput("dl_dashboard_amk")),
                                   p("Though we recognise that it is important to acknowledge that numerous mature estates have already undergone retrofitting processes to meet the 
                                     resident’s needs. Which meant that, the recorded number of desired lines in our study may not entirely reflect the original situation accurately. 
                                     Nevertheless, this insight spurred further research into the potential disparities between infrastructure design and the preferences of demographics 
                                     in mature and non-mature estates.")
                                   ),
                          tabPanel(strong('Evidence'), 
                            br(),
                            p(strong("Supporting Evidence")),
                            p("Through our research on walkway data, we found that residents are more inclined to adhere to constructed paths if they are straight and intuitive, as opposed 
                              to the winding routes often found in non-mature estates. Unlike non-mature estates, mature estates have simplified layouts that enable residents to navigate a 
                              long direct and intuitive pathways with ease."),
                            p("Additionally, the complex design of the newer HDB estates has led to less intuitive walking pavements. Therefore, residents often resort to walking along the 
                              main roads to reach their destination within the estate before creating a desired line to reconnect with the designated walking pavements."),
                            #<insert map showing the walking data>
                            #<insert on-the-ground research – Karina’s video & picture showing people walking on car-based roads>
                            div(style = "text-align: center;", imageOutput("otg_pg_dl")),
                            p("Therefore, in more complex HDB layouts, our findings suggest that residents are more inclined to create their own pathways. These custom routes serve to simplify 
                              their journey, allowing them to effortlessly traverse the HDB estate and reach their destination."),
                          ))),
                        
                tabPanel(strong("Key Actionable 4"),
                           br(), 
                           div(style = "background-color: #f2f2f2; padding: 10px;",
                               h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Actionable 4")),
                               div(style = "text-align: center;",
                                   HTML("<h4>To pave the sides of the roads, especially those near bus stops to increase convenience and to prevent the formation of desired paths.</h4>")
                               )
                           ),
                         br(),
                         br(),
                         tabsetPanel(
                           tabPanel(strong("Sub-Challenge"),
                                    div(style = "background-color: #f2f2f2; padding: 10px;",
                                        h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Sub-Challenge")),
                                        div(style = "text-align: center;",
                                            HTML("<h4>How does the current state of road sides, especially those near bus stops, contribute to the formation of desired lines, and what 
                                                 measures can be taken to reduce such instances?</h4>")
                                        )), 
                                        br(), 
                                        p(strong("Purpose of Sub-challenge:")), 
                                        p(strong("1. Road infrastructures exhibit the highest frequency of desired walkways.")),
                                        #<insert desired lines map in pg> 
                                        p("Our observations of desired lines in Punggol reveal a clustering effect along main roads, suggesting a natural emergence influenced by the 
                                          convenience of crossing road infrastructure.")
                                        ), 
                           tabPanel(strong("Evidence"),
                                    br(),
                                    p(strong("Supporting Evidence")),
                                    p("Based on our analysis, residents exhibit a tendency to create desired paths for convenient access to cross the road, particularly towards bus stops
                                      and MRT stations where traffic lights may be less accessible compared to jaywalking. However, merely increasing the number of traffic lights or zebra 
                                      crossings may not effectively address this issue, as people naturally opt for the most convenient route, as evidenced by the clustering of desired 
                                      lines around bus stops."),
                                    div(style = "text-align: center;", imageOutput("a4_amk_dl")),
                                    fluidRow(
                                      column(6, div(style = "text-align: center;", imageOutput("a4_pg_dl_1"))),
                                      column(6, div(style = "text-align: center;", imageOutput("a4_pg_dl_2")))
                                    ),
                                    p("Our survey analysis suggests a strong preference among residents for the shortest walking time to public transport, with an average reasonable walking 
                                      time to bus stops perceived to be around 5 minutes, which was the shortest among surveyed amenities. Therefore, our recommended course of action is not 
                                      to implement path-blocking measures, as they would reduce the convenience for residents accessing their commute. Instead, we advise against actions that 
                                      might provoke negative sentiments towards walkways and suggest initiatives aimed at enhancing pedestrian access to public transport hubs while maintaining
                                      convenience and efficiency."), 
                                    fluidRow(
                                      column(6, div(style = "text-align: center;", imageOutput("eda_a4_ranking"))),
                                      column(6, div(style = "text-align: center;", imageOutput("eda_a4_walkingtime")))
                                    ),
                                    p("As previously mentioned, residents have emphasised convenience as a crucial factor when selecting a route. Hence, I propose that we design walkways that 
                                      cater to the specific needs and preferences of the community."), 
                                    div(style = "text-align: center;", imageOutput("eda_a4_convenience"))
                                  
                                    )) 
                                    
                        ))),
    
    tabPanel("Desired Lines Calculator",
             div(style = "background-color: #f2f2f2; padding: 10px;",
                 h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Desired Lines Calculator")),
                 div(style = "text-align: center;",
                     p(strong("This calculator gives a prediction for how likely one is to walk on desire paths based on the given parameters in a logistic regression analysis.")),
                     p(strong("Simply enter the following parameters then click the “Predict” button to create a prediction and a confidence level:"))
                 )
             ),
             br(),
             div(style = "background-color: #e9ecef; padding: 20px; border-radius: 5px;", # Shaded box for inputs
                 fluidRow(
                   column(4,
                          selectInput("input1", "Throughout the various times of the day, do you tend to use different routes when navigating through your residential estate?", choices = c("Yes", "No")),
                          selectInput("input2", "Do well-lit walkways affect your choice of route when navigating through your residential estate?", choices = c("Yes", "No"))
                   ),
                   column(4,
                          selectInput("input3", "Does terrain elevation (e.g., hills/big slopes) affect your choice of route when navigating through your residential estate?", choices = c("Yes", "No")),
                          selectInput("input4", "Does convenience affect your choice of route when navigating through your residential estate?", choices = c("Yes", "No"))
                   ),
                   column(4,
                          numericInput("input5", "What is your age?", value = 25),
                          selectInput("input6", "What type of housing do you currently live in Singapore?", choices = c("HDB", "Condominium", "Landed Property")),
                          selectInput("input7", "How often do you walk in your immediate neighbourhood?", choices = c("Always", "Frequently", "Sometimes", "Seldom", "Never")),
                          actionButton("predictButton", "Predict", style = "margin-top: 20px; width: 100%; background-color: #ffffff; color: black; border: none;") # Custom styles for button
                   )
                 )
             ),
             br(), # Space between input box and result box
             div(style = "background-color: #f2f2f2; padding: 10px; text-align: center;", # New shaded box for results with text centered
                 h4(style = "margin-top: 0; margin-bottom: 10px;", strong("Results")), # Centered results header
                 div( # Additional div for text outputs, if needed for further styling
                   strong(textOutput("predictionText")),
                   strong(textOutput("confidenceText"))
                 )
             )
    ),
             
   
             
    
    
    navbarMenu("Appendix",
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
                                           HTML('<iframe id="tableauDashboard" src="https://public.tableau.com/views/Hypothesis25/Dashboard1?:language=en-GB&:embed=y&:display_count=n&:showVizHome=no" 
              width="100%" height="800" frameborder="0"></iframe>')
                                           
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
                                           HTML('<iframe id="tableauDashboard" src="https://public.tableau.com/views/Hypothesis27new/Dashboard12?:embed=y&:showVizHome=no" width="100%" height="800" frameborder="0"></iframe>')
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
                             <li>In non-mature estates, buildings often exhibit a smaller, squarish, and jagged design, whereas mature estates tend to feature a more rectangular building structure characterised by elongated corridors.</li>
                             </ol>")
                        ),
                        br(), 
                        tabsetPanel(
                          tabPanel(strong("Key Insight 1 & 2"),
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
                                                uiOutput("dynamicMap")),
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
                                   ),
                          tabPanel(strong("Key Insight 3"),
                                   br(),
                                   div(style = "background-color: #f2f2f2; padding: 10px;",
                                       h4(style = "margin-top: 0; margin-bottom: 10px; text-align: center;", strong("Key Insight 3")),
                                       div(style = "text-align: center;",
                                           HTML("<h4>In non-mature estates, buildings are often exhibit a smaller, squarish, and jaggs design, whereas mature estates tend to feature a more rectangular builind structure characterised 
                                            by elongated corridors</h4>")
                                       )
                                   ),
                                   br(),
                                   br(),
                                   div(style = "background-color: #f2f2f2; padding: 10px; text-align: center;",
                                       h4(style = "margin-top: 0; margin-bottom: 8px; text-align: center;", 
                                          strong("Interactive Map")), 
                                       fluidRow(
                                         column(
                                           8, uiOutput("dynamicMap2")),
                                         column(
                                           4,
                                           selectInput("mapSelection2", "Select a Location:", choices = studyarea))
                                       )),
                                   br(), 
                                   p(strong("Description:"), "overlay color-coded building categorization onto an OpenStreetMap (OSM) layer, enabling users to explore both the geographical distribution of buildings and their respective 
                                     categories while retaining the ability to zoom in for detailed views and identification of individual building names. "), 
                                   p(strong("Analysis:"), "Key insight 3 can be seen through the comparison of the comparison of the building layout in Ang Mo Kio and Punggol. Which suggests that non-mature estates and mature estates 
                                     feature distinct HDB layouts. In mature estates, the focus is often on basic, functional designs aimed at optimising space utilisation. Conversely, non-mature estates typically boast more contemporary 
                                     layouts tailored to foster a conducive environment for community interaction."),
                                   
                          )
                          ) 
                          
                                    )
    )
  )

)





server <- function(input, output, session) {
  
  output$tableauDashboard <- renderUI({
    # Define initial width
    initial_width <- 800  # Set an initial width
    
    # Add JavaScript to update width dynamically
    session$onFlushed(function() {
      runjs(paste0("
        var dashboardWidth = Math.min(window.innerWidth, ", initial_width, ");
        $('#tableauDashboard').width(dashboardWidth);
      "))
    })
  })
  
  ##################### Images ######################
  
  ### Home Page banner  
  output$logo <- renderImage({
    list(src = "images/homepage.png",
         style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto; margin-top: 0px; margin-bottom: 0px;")
  }, deleteFile = FALSE)
  
  ### Top-Logo

  output$top_logo <- renderImage(
    list(src = "images/top_logo.png",
         width = 50,
         height = 45,
         onclick = "window.location.load()"), deleteFile = FALSE)
  

  
  
  ### Framework 1 EDA - James 
  output$eda_choices <- renderImage({
    list(src = "images/EDA_A1_choices.png",
         width = 646,
         height = 376,
         style = "display: block; margin-left: auto; margin-right: auto; margin-top: 0px; margin-bottom: 0px;")
  }, deleteFile = FALSE)
  
  ### Framework 1 EDA - Amanda Convenience  
  output$eda_convenience <- renderImage({
    list(src = "images/EDA_A1_convenience.png",
         style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  ### Framework 1 EDA - Punggol Dashboard 
  output$pg_dashboard <- renderImage({
    list(src = "images/Punggol_dashboard.jpg",
         style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 2 Word Cloud - bigram
  output$dislike_bigram <- renderImage({
    list(src = "images/dislike_bigram_A2.jpg",
         style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 2 Word Cloud - unigram
  output$dislike_unigram <- renderImage({
    list(src = "images/dislike_unigram_A2.jpg",
         style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 3 DL Dashboard
  output$dl_dashboard_amk <- renderImage({
    list(src = "images/DL_amk_dashboard_A3.jpg",
         style = "max-width: 50%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 3 OTG Punggol_Karina 
  output$otg_pg_dl <- renderImage({
    list(src = "images/OTG_pg_A3.jpg",
         style = "max-width: 100%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 4 AMK_DL
  output$a4_amk_dl <- renderImage({
    list(src = "images/A4_AMK.png",
         style = "max-width: 100%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 4 PG_DL_1
  output$a4_pg_dl_1 <- renderImage({
    list(src = "images/A4_Punggol_1.png",
         style = "max-width: 100%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 4 PG_DL_1 
  output$a4_pg_dl_2 <- renderImage({
    list(src = "images/A4_Punggol_2.png",
         style = "max-width: 100%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  ### Framework 4 EDA A4 ranking 
  output$eda_a4_ranking <- renderImage({
    list(src = "images/EDA_A4_ranking.png",
         style = "max-width: 100%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  ### Framework 4 EDA A4 ranking 
  output$eda_a4_walkingtime <- renderImage({
    list(src = "images/EDA_A4_walkingtime.png",
         style = "max-width: 100%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ### Framework 4 EDA A4 ranking 
  output$eda_a4_convenience <- renderImage({
    list(src = "images/EDA_A4_convenience.png",
         style = "max-width: 100%; height: 365px; display: block; margin-left: auto; margin-right: auto;")
  },deleteFile = FALSE)
  
  
  ###################################################
  
  output$amk_landuse_map <- renderTmap(tm_shape(amk_landuse) +
                                         tm_borders() +  # Plot borders of the polygons
                                         tm_fill(col = "Categorize", title = "Landuse Category", alpha = 0.5) +  # Color categorization with lower opacity
                                         tm_layout(legend.show = TRUE) +  # Show legend
                                         osm_basemap  )# Add basemap
  
  output$pg_landuse_map <- renderTmap(tm_shape(pg_landuse) +
                                        tm_borders() +  # Plot borders of the polygons
                                        tm_fill(col = "Categorize", title = "Landuse Category", alpha = 0.5) +  # Color categorization with lower opacity
                                        tm_layout(legend.show = TRUE) +  # Show legend
                                        osm_basemap  )# Add basemap
  
  
  output$dl_amk <- renderTmap({
    
    dl_map <- osm_basemap +  # Use tm_basemap to specify the basemap
      tm_shape(mpsz[mpsz$PLN_AREA_N=="ANG MO KIO", ]) + #Replace by studyarea choices
      tm_borders()+
      tm_shape(dl_amk)+ #replace by combining the map of dl_amk & dl_pg
      tm_lines(col = "red", lwd = 2) +  # Adjust line color and width as needed
      tmap_options(check.and.fix = TRUE)
    
    dl_map  # Return the map for rendering
  })
  
  output$dl_pg <- renderTmap({
    
    dl_map <- osm_basemap +  # Use tm_basemap to specify the basemap
      tm_shape(mpsz[mpsz$PLN_AREA_N=="PUNGGOL", ]) + #Replace by studyarea choices
      tm_borders()+
      tm_shape(dl_pg)+ #replace by combining the map of dl_amk & dl_pg
      tm_lines(col = "blue", lwd = 2) +  # Adjust line color and width as needed
      tmap_options(check.and.fix = TRUE)
    
    dl_map  # Return the map for rendering
  })
  
  output$dynamicMap <- renderUI({
    if (input$mapSelection == "Punggol") {
      tmapOutput("dl_pg")
    } else {
      tmapOutput("dl_amk")
    }
  })
  
  output$dynamicMap2 <- renderUI({
    if (input$mapSelection2 == "Punggol") {
      tmapOutput("pg_landuse_map")
    } else {
      tmapOutput("amk_landuse_map")
    }
  })
  
  ###################################################
  
  ###### EDA (Amanda) for Framework #1 ######
  
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

  ### Desire Line Calculator 
  # Initialize flag to track if data has been loaded
  is_loaded <- reactiveVal(FALSE)
  
  # Initialize trained model
  trained_model <- reactiveVal(NULL)
  
  # Function to load data and train model
  load_and_train <- function() {
    file_path <- "data/aspatial/LR_Data.csv"
    df <- read.csv(file_path, header = TRUE)[,-1]
    y <- df[[ncol(df)]]
    X <- df[, -ncol(df)]
    
    fit <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      fit(as.factor(y) ~ ., data = X)
    trained_model(fit)
    is_loaded(TRUE)
  }
  
  # Make predictions
  observeEvent(input$predictButton, {
    # Load data and train model if not already loaded
    if (!is_loaded()) {
      load_and_train()
    }
    ave <- 35.19708
    std <- 16.25660
    scaled_input5 <- (as.numeric(input$input5)-ave)/std
    # Use new_data for predictions
    new_data <- data.frame(
      input1 = input$input1,
      input2 = input$input2,
      input3 = input$input3,
      input4 = input$input4,
      input5 = scaled_input5,
      input6 = input$input6,
      input7 = input$input7,
      fixed_var1 = 0.948905,
      fixed_var2 = 0.948905,
      fixed_var3 = 0.649635,
      stringsAsFactors = TRUE
    )
    
    
    colnames(new_data) <- c("Throughout.the.various.times.of.the.day..do.you.tend.to.use.different.routes.when.navigating.through.your.residential.estate.", 
                            "Do.well.lit.walkways.affect.your.choice.of.route.when.navigating.through.your.residential.estate.",
                            "Does.terrain.elevation..e.g..hills.big.slopes..affect.your.choice.of.route.when.navigating.through.your.residential.estate.",
                            "Does.convenience.affect.your.choice.of.route.when.navigating.through.your.residential.estate.", 
                            "What.is.your.age.", 
                            "What.type.of.housing.do.you.currently.live.in.Singapore.", 
                            "How.often.do.you.walk.in.your.immediate.neighbourhood.", 
                            "Q32_Encoded", "Q33_Encoded", "Q34_Encoded")
    
    # Make predictions
    if (!is.null(trained_model())) {
      prediction <- predict(trained_model(), new_data = new_data, type = "class")
      
      if (prediction == 1) {
        confidence <- predict(trained_model(), new_data = new_data, type = "prob")[, 2]  # Probability of class 1
        confidence_text <- paste("Confidence (Likely to walk on desire path):", round(confidence * 100, 2), "%")
      } else {
        confidence <- predict(trained_model(), new_data = new_data, type = "prob")[, 1]  # Probability of class 0
        confidence_text <- paste("Confidence (Not likely to walk on desire path):", round(confidence * 100, 2), "%")
      }
      
      # Convert prediction to desired format
      prediction_text <- ifelse(prediction == 1, "Likely to walk on desire path", "Not likely to walk on desire path")
      
      output$predictionText <- renderText(paste("Prediction:", prediction_text))
      output$confidenceText <- renderText(confidence_text)
    } else {
      output$predictionText <- renderText("Model not trained yet.")
      output$confidenceText <- renderText("")
    }
    
  })

}






shinyApp(ui = ui, server = server)


library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        /* Custom CSS to change the navbar color */
        .navbar-default {
          background-color: #FAE9DC; /* Set navbar background color */
          border-color: #ddd; /* Set navbar border color */
        }
        "
      )
    )
  ),
  
  # Navbar
  navbarPage(
    title = div(
      img(
        src = "images/homepage.png",
        height = 40, # Adjusted height
        width = 120  # Original width
      ),
      "STEP AHEAD SOLUTIONS",
      onclick = "window.location.href='#home'"
    ),
    
    tabPanel("Home", 
             div(
               div(style = "text-align: center;", 
                   imageOutput("logo")), 
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
             )
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
                                       div(style = "text-align: center;",
                                           HTML('Interactive Map')
                                       )
                                   ),
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
                          
               )
    ),
    
    tabPanel("Framework", 
             div(br())
    ),
    
    tabPanel("Conclusion", 
             div(br())
    )
  )
)


server <- function(input, output, session){
  
  ##################### images ######################

  ### Home Page banner  
  output$logo <- renderImage({
    list(src = "images/homepage.png",
         width = 1250,
         height = 435,
         style="display: block; margin-left: auto; margin-right: auto; margin-top: 0px; margin-bottom: 0px;") # Add margin-top and margin-bottom to 0px
  }, deleteFile = FALSE)
  
  ###################################################  
  
}

shinyApp(ui = ui, server = server)

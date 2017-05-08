library(shiny)
library(leaflet)

ui <- fluidPage(
    titlePanel("People Killed by Police in 2015"),
    div(p("Over 1,000 people in the United States were killed by police in 2015. Explore these incidents."),
        tabsetPanel( 
            tabPanel("Map of Incidents", p("Select one or more varibles to filter the data by. Click on points on the map to see information about an incident."),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("variable1Selector", label = h4("Select Variable to filter on"), 
                                         choices = list("Select a variable" = "none","Median Income" = "median_household_Income",
                                                        "Percent age 25+ Highschool Graduates or Higher" = "percent_25_or_older_HighSchool_grads",
                                                        "Race/Ethnicity" = "raceethnicity", "Month" = "month",
                                                        "Armed with:" = "armed", "In Majority" = "in_majority", "Is Armed (TRUE/FALSE)" = "is_armed"),
                                         selected = 1),
                             conditionalPanel(
                                 condition = "input.variable1Selector == 'median_household_Income'",
                                 sliderInput("householdIncome1", "Median Household Income Range:", 
                                             min=0, max=150000, value=c(30000,60000), step = 1000)
                             ),
                             conditionalPanel(
                                 condition = "input.variable1Selector == 'percent_25_or_older_HighSchool_grads'",
                                 sliderInput("grad1", "Decimal:", 
                                             min = 0, max = 100, value = c(0,100), step= 1)
                             ),
                             conditionalPanel(
                                 condition = ("input.variable1Selector != 'median_household_Income' &&
                                              input.variable1Selector !=  'percent_25_or_older_HighSchool_grads'"),
                                 selectInput("factor1Selector", label = "Select Value to filter on", choices = list())
                             ),
                             
                             
                             
                             selectInput("variable2Selector", label = h4("Select Variable to filter on"), 
                                         choices = list("Select a variable" = "none", "Median Income" = "median_household_Income",
                                                        "Percent age 25+ Highschool Graduates or Higher" = "percent_25_or_older_HighSchool_grads",
                                                        "Race/Ethnicity" = "raceethnicity", "Month" = "month",
                                                        "Armed with:" = "armed", "In Majority" = "in_majority", "Is Armed (TRUE/FALSE)" = "is_armed"),
                                         selected = 1),
                             conditionalPanel(
                                 condition = "input.variable2Selector == 'median_household_Income'",
                                 sliderInput("householdIncome2", "Median Household Income Range:", 
                                             min=0, max=150000, value=c(30000,60000), step = 1000)
                             ),
                             conditionalPanel(
                                 condition = "input.variable2Selector == 'percent_25_or_older_HighSchool_grads'",
                                 sliderInput("grad2", "Decimal:", 
                                             min = 0, max = 100, value = c(0,100), step= 1)
                             ),
                             conditionalPanel(
                                 condition = ("input.variable2Selector != 'median_household_Income' &&
                                              input.variable2Selector !=  'percent_25_or_older_HighSchool_grads'"),
                                 selectInput("factor2Selector", label = "Select Value to filter on", choices = list())
                             ),
                             
                             
                             selectInput("variable3Selector", label = h4("Select Variable to filter on"), 
                                         choices = list("Select a variable" = "none", "Median Income" = "median_household_Income",
                                                        "Percent age 25+ Highschool Graduates or Higher" = "percent_25_or_older_HighSchool_grads",
                                                        "Race/Ethnicity" = "raceethnicity", "Month" = "month",
                                                        "Armed with:" = "armed", "In Majority" = "in_majority", "Is Armed (TRUE/FALSE)" = "is_armed"),
                                         selected = 1),
                             conditionalPanel(
                                 condition = "input.variable3Selector == 'median_household_Income'",
                                 sliderInput("householdIncome3", "Median Household Income Range:", 
                                             min=0, max=150000, value=c(30000,60000), step = 1000)
                             ),
                             conditionalPanel(
                                 condition = "input.variable3Selector == 'percent_25_or_older_HighSchool_grads'",
                                 sliderInput("grad3", "Decimal:", 
                                             min = 0, max = 100, value = c(0,100), step= 1)
                             ),
                             conditionalPanel(
                                 condition = ("input.variable3Selector != 'median_household_Income' &&
                                      input.variable3Selector !=  'percent_25_or_older_HighSchool_grads'"),
                                 selectInput("factor3Selector", label = "Select Value to filter on", choices = list())
                             )
                         ),
                         mainPanel(leafletOutput("mymap"), textOutput("text"))
                     )  
            ),
            
            
            tabPanel("Interactive Plots", plotOutput("myplots"),
                     selectInput("xvalue", label = h4("Select variable to create plot"),
                                 choices = list("Race/Ethnicity" = "raceethnicity", 
                                                "Race/Ethnicity Normalized" = "normalizedrace",
                                                "Median Household Income" = "median_household_Income",
                                                "Month" = "month", "In Majority" = "in_majority",
                                                "Is Armed" = "is_armed","Armed with:" = "armed", "Age" = "age"),
                                 selected = 1),
                     conditionalPanel(
                         condition = ("input.xvalue !=  'normalizedrace'"),
                         checkboxInput("genderCheck", "Show genders", TRUE)
                     )
            )
        ), class = "span12"
    )
)

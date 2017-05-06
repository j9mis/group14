library(shiny)
library(leaflet)

ui <- fluidPage(
    mainPanel(
        tabsetPanel(
            tabPanel("Map of Incidents", leafletOutput("mymap"), 
                     selectInput("variableSelector", label = h4("Select Variable to filter on"), 
                                 choices = list("Median Income" = "median_household_Income",
                                                "Percent age 25+ Highschool Graduates or Higher" = "percent_25_or_older_HighSchool_grads",
                                                "Race/Ethnicity" = "raceethnicity", "Month" = "month",
                                                "Armed" = "armed", "In Majority" = "in_majority", "Is Armed" = "is_armed"),
                                                 selected = 1),
                     conditionalPanel(
                         condition = "input.variableSelector == 'median_household_Income'",
                         sliderInput("householdIncome", "Median Household Income Range:", 
                                     min=0, max=150000, value=c(30000,60000), step = 1000)
                     ),
                     conditionalPanel(
                         condition = "input.variableSelector == 'percent_25_or_older_HighSchool_grads'",
                         sliderInput("grad", "Decimal:", 
                                     min = 0, max = 100, value = c(0,100), step= 1)
                     ),
                     conditionalPanel(
                         condition = ("input.variableSelector != 'median_household_Income' &&
                input.variableSelector !=  'percent_25_or_older_HighSchool_grads'"),
                         selectInput("factorSelector", label = "Select Value to filter on", choices = list())
                     )
            ),
           
            tabPanel("Interactive Plots", plotOutput("myplots"),
                     selectInput("xvalue", label = h4("Select variable to create plot"),
                                 choices = list("Race/Ethnicity" = "raceethnicity", 
                                                "Race/Ethnicity Normalized" = "normalizedrace",
                                                "Median Household Income" = "median_household_Income",
                                                "Month" = "month", "In Majority" = "in_majority",
                                                "Is Armed" = "is_armed","Age" = "age"),
                                 selected = 1),
                     conditionalPanel(
                         condition = ("input.xvalue !=  'normalizedrace'"),
                             checkboxInput("genderCheck", "Show genders", TRUE),
                     verbatimTextOutput("genderValue")
                     )
            )
        )
    )
)
library(shiny)
library(leaflet)
library(maps)
library(readr)
library(htmltools)
library(lazyeval)
library(dplyr)
library(tidyverse)
library(stringr)

server <- function(input, output, session) {
    dataframe <- read_csv('14_data_3.csv') 
    dataframe <- mutate(dataframe, median_household_Income = as.numeric(median_household_Income))
    dataframe <- mutate(dataframe, percent_25_or_older_HighSchool_grads = as.numeric(percent_25_or_older_HighSchool_grads))
    names(dataframe)[names(dataframe) == 'name'] <- 'person_name'
    dataframe <- mutate(dataframe, is_armed = as.logical(is_armed))
    date_count <-  dataframe %>% group_by(month) %>% count()
    date_count_gender <-  dataframe %>% group_by(month,gender) %>% count()
 
    pop_2015 <-  321418820

    asian_share <-  0.056
    black_share <-  0.133
    hispanic_share <-  0.176
    native_share <-  0.012
    white_share <-  0.616

    per_million_counts <- function(race_count){
        race_count$per_million <- NA
        for (i in 1:nrow(race_count)){
            if (race_count$raceethnicity[i] == "Asian/Pacific Islander"){
                race_count$per_million[i] = ((race_count$n[race_count$raceethnicity=="Asian/Pacific Islander"])*1000000)/(pop_2015*asian_share)
            } else if (race_count$raceethnicity[i] == "Black"){
                race_count$per_million[i] =     ((race_count$n[race_count$raceethnicity=="Black"])*1000000)/(pop_2015*black_share)
            } else if (race_count$raceethnicity[i] == "Hispanic/Latino"){
                race_count$per_million[i] = ((race_count$n[race_count$raceethnicity=="Hispanic/Latino"])*1000000)/(pop_2015*hispanic_share)
            } else if (race_count$raceethnicity[i] == "Native American"){
                race_count$per_million[i] =
                    ((race_count$n[race_count$raceethnicity=="Native American"])*1000000)/(pop_2015*native_share)
            } else if (race_count$raceethnicity[i] == "White"){
                race_count$per_million[i] = ((race_count$n[race_count$raceethnicity=="White"])*1000000)/(pop_2015*white_share)
            } else race_count$per_million[i] = NA
        }
        return(race_count)
    }

    race_count <- dataframe %>% group_by(raceethnicity) %>% count()
    race_count <- per_million_counts(race_count)
    
    output$mymap <- renderLeaflet({
        var <- input$variableSelector
        if ((var != "median_household_Income") && (var != "percent_25_or_older_HighSchool_grads")){
            val <- input$factorSelector
            data <- dataframe %>% filter_(interp(quote(column == value), column = as.name(var), value = val))
            leaflet(data) %>%
                addTiles() %>% addMarkers(~long, ~lat, popup = 
                paste("Armed with: ", as.character(data$armed),
                    "<br>Age: ", as.character(data$age),
                    "<br>Race: ", as.character(data$raceethnicity),
                    "<br>Date of incident: ", as.character(data$month), " ",
                    as.character(dataframe$day), ", ", as.character(data$year) 
                    
                ) 
            )
        }else if (var == "median_household_Income"){
            val1 = as.numeric(input$householdIncome[1])
            val2 = as.numeric(input$householdIncome[2])
            data <- dataframe %>% filter_(interp(quote(column >= value), column = as.name(var), value = val1))
            data <- data %>% filter_(interp(quote(column <= value), column = as.name(var), value = val2))
            leaflet(data) %>%
                addTiles() %>% addMarkers(~long, ~lat, popup = 
                paste("Armed with: ", as.character(data$armed),
                "<br>Age: ", as.character(data$age),
                 "<br>Race: ", as.character(data$raceethnicity),
                 "<br>Date of incident: ", as.character(data$month), " ",
                 as.character(dataframe$day), ", ", as.character(data$year),
                "<br>Median Income of Location: ", as.character(data$median_household_Income), " "
                )
            )
        }else if (var == "percent_25_or_older_HighSchool_grads"){
                val1 = as.numeric(input$grad[1])
                val2 = as.numeric(input$grad[2])
                print(val1)
                data <- dataframe %>% filter_(interp(quote(column >= value), column = as.name(var), value = val1))
                data <- data %>% filter_(interp(quote(column <= value), column = as.name(var), value = val2))
                leaflet(data) %>%
                    addTiles() %>% addMarkers(~long, ~lat, popup = 
                            paste("Armed with: ", as.character(data$armed),
                            "<br>Age: ", as.character(data$age),
                            "<br>Race: ", as.character(data$raceethnicity),
                            "<br>Date of incident: ", as.character(data$month), " ",
                            as.character(dataframe$day), ", ", as.character(data$year),
                            "<br>Percent Age 25+ That are Highschool grads: ", as.character(data$percent_25_or_older_HighSchool_grads), " "
                             )
                    )            
            }
    })
    observe({
        x <- quote(input$variableSelector)
        factors <- unique(dataframe[,eval(x)])
       
        
        updateSelectInput(session, "factorSelector",
                          choices = factors,
                          selected = tail(factors, 1)
        )
    })
    
    
    output$myplots <- renderPlot({
        xval <- input$xvalue
        if (xval == "normalizedrace"){
            ggplot(race_count) + geom_bar(aes(x = raceethnicity, y = per_million), stat="identity", fill = 16, color = "black")  +
                theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                      panel.background = element_blank()) + 
                labs(y = "Police Killings per Million in 2015")
        }
        if (input$genderCheck == FALSE){
                if (xval == 'age'){
                    ggplot(dataframe) + geom_histogram(aes_string(x=xval), binwidth = 3, fill = 16, color = "black") +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                          panel.background = element_blank()) + 
                    labs(y = "Number of Police Killings in 2015")
                }else if (xval == 'month'){
                    ggplot(date_count) + geom_bar(aes(x=month, y=n), stat = "identity", fill = 16, color = "black") + 
                    ggtitle('Police Killings in 2015') +
                    labs(x = 'Month', y = '# of people killed by police') + 
                    theme(axis.line = element_line(colour = "black", size=.1), 
                     panel.background = element_blank()) +
                    scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
                }else if (xval == "median_household_Income"){
                    ggplot(dataframe) + geom_histogram(aes_string(x=xval), binwidth = 8000, fill = 16, color = "black") +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                          panel.background = element_blank()) + 
                    labs(y = "Number of Police Killings in 2015")
                }else{
                    ggplot(dataframe) + geom_bar(aes_string(x=xval), fill = 16, color = "black") +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                          panel.background = element_blank())+ 
                    labs(y = "Number of Police Killings in 2015")
                }
            }else{
                if ((xval == "raceethnicity") || (xval == "in_majority")||(xval == "is_armed")){
                    ggplot(dataframe) + geom_histogram(aes_string(x=xval, fill = 'gender'), stat = "count", color = "black") +
                        scale_fill_manual(values = c('hotpink', 'blue', 'green')) +
                        theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                              panel.background = element_blank())+ 
                        labs(y = "Number of Police Killings in 2015")
                }else if (xval == "age"){
                    ggplot(dataframe) + geom_histogram(aes_string(x=xval, fill = 'gender'), binwidth = 3, color = "black") +
                        scale_fill_manual(values = c('hotpink', 'blue', 'green')) +
                        theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                              panel.background = element_blank())+ 
                        labs(y = "Number of Police Killings in 2015")
                }else if (xval == 'month'){
                    ggplot(date_count_gender) + geom_bar(aes(x=month, y=n, fill = gender), stat = "identity", color = "black") + 
                        ggtitle('Police Killings in 2015') +
                        labs(x = 'Month', y = '# of people killed by police') + 
                        theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                              panel.background = element_blank())+ 
                        scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
                        scale_fill_manual(values = c('hotpink', 'blue', 'green')) 
                }else if (xval == "median_household_Income"){
                    ggplot(dataframe) + geom_histogram(aes_string(x=xval, fill = "gender"), binwidth = 8000, color = "black") +
                        scale_fill_manual(values = c('hotpink', 'blue', 'green')) +
                        theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                              panel.background = element_blank()) + 
                        labs(y = "Number of Police Killings in 2015")
                }
            }

    })
}

# TO NORMALIZE THE DATA BASED ON POPULATION PERCENTAGES
## Based on 2015 US Census data
## Took out arab because the Cenuss does not include it (counted as white sometimes)
pop_2015 = 321418820

asian_share = 0.056
black_share = 0.133
hispanic_share = 0.176
native_share = 0.012
white_share = 0.616

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

## Find Lat/Long
#Our data for December was missing the latitude and longitude for the incidents, so we created this function to add them to 
#the dataframe. This function is designed to work on "the-counted-2015.csv" data set for the months of December.


library(RDSTK)
library(readr)
library(dplyr)
library(tibble)
library(stringr)

#data <- read_csv("the-counted-2015.csv")
lat_long_finder <-function(data){
    latList <- c()
    longList <- c()
    count = 0
    for (i in 1:nrow(data)){
        tryCatch({
            street <- data$streetaddress[i]
            state <- data$state[i]
            city <- data$city[i]
            if (!is.null(street) && !is.null(state) && !is.null(city)){
                address <- str_c(street, " ", city, " ", state)
                coords <- street2coordinates(address)
                longitude <- coords$longitude
                latitude <- coords$latitude
                if(!is.null(latitude) && !is.null(longitude)){
                    latList <- c(latList, latitude)
                    longList <- c(longList, longitude)
                } else {
                    latList <- c(latList, 0)
                    longList <- c(longList, 0)
                }
            }else{
                latList <- c(latList, 0)
                longList <- c(longList, 0)
            }
        }, error = function(err){
            count <<- count+1
            latList <<- c(latList, 0)
            longList <<- c(longList, 0)
        })
    }
    data <- add_column(data, lat = latList, long = longList)
    return(data)
}
#data <- data[1:10,]
#data <- lat_long_finder(data)

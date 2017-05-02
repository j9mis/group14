completeData <- read_csv('14_data.csv')

# Add "Region" variable
completeData$region <- NA
NE = c("ME","NH","VT","MA","RI","CT","NY","PA","NJ", "DE")
MW = c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
South = c("FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")
West = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")
completeData$region[completeData$state %in% NE] = "Northeast"
completeData$region[completeData$state %in% MW] = "Midwest"
completeData$region[completeData$state %in% South] = "South"
completeData$region[completeData$state %in% West] = "West"


#Add Binary "Armed" Variable
#1 if yes (any type), 0 if no

completeData$is_armed <- ifelse(completeData$armed == "No", 0, 1)


#Add racial majority variables

completeData$white_majority <- ifelse(completeData$percent_white_population >= 50, 1, 0)
completeData$black_majority <- ifelse(completeData$percent_black_population >= 50, 1, 0)
completeData$hispanic_majority <- ifelse(completeData$percent_hispanic_population >= 50, 1, 0)

completeData <- mutate(completeData, in_majority = ifelse(completeData$raceethnicity == "White" & completeData$white_majority == 1, "Yes",
                                                          ifelse(completeData$raceethnicity == "Black" & completeData$black_majority == 1, "Yes",
                                                                 ifelse(completeData$raceethnicity == "Hispanic/Latino" & completeData$hispanic_majority == 1, "Yes", "No"))))


data <- completeData

#Add date variable
#Format months
data <- mutate(data, month_num = ifelse(data$month == "January", '01', 
                                        ifelse(data$month == "February", '02', 
                                               ifelse(data$month == "March", '03', 
                                                      ifelse(data$month == "April", '04', 
                                                             ifelse(data$month == "May", '05', 
                                                                    ifelse(data$month == "June", '06', 
                                                                           ifelse(data$month == "July", '07',
                                                                                  ifelse(data$month == "August", '08',
                                                                                         ifelse(data$month == "September", '09', 
                                                                                                ifelse(data$month == "October", '10', 
                                                                                                       ifelse(data$month == "November", '11', '12'))))))))))))


#Format days                                                                                                                
data <- mutate(data, day_num = ifelse(data$day == 1, '01',
                                      ifelse(data$day == 2, '02',
                                             ifelse(data$day == 3, '03',
                                                    ifelse(data$day == 4, '04',
                                                           ifelse(data$day == 5, '05',
                                                                  ifelse(data$day == 6, '06',
                                                                         ifelse(data$day == 7, '07',
                                                                                ifelse(data$day == 8, '08',
                                                                                       ifelse(data$day == 9, '09', as.character(data$day)))))))))))

#Make date variable
data$date <- as.Date(paste(data$year, data$month_num, data$day_num, sep = "-" ))


#Save to csv
write_csv(data, '14_data.csv')

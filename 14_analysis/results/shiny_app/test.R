pop_2015 = 321418820
dataframe <- read_csv('14_data_3.csv') 
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

race_count <- dataframe %>% group_by(raceethnicity) %>% count()
race_count <- per_million_counts(race_count)

ggplot(dataframe) + geom_histogram(mapping = aes(x = median_household_Income), stat = "count", binwidth = 10000)
# +
    # theme(axis.text.x = element_text(angle = 60, hjust = 1))